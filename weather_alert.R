
library(httr)
library(jsonlite)
library(tidyverse)   # includes lubridate
library(glue)
library(blastula)

# ---- Settings -----------------------------------------------------------------
# Email settings come from environment variables, so no address or password is
# stored in the code: GitHub secrets on GitHub, a git-ignored .Renviron file locally.
sender     <- Sys.getenv("SMTP_USER")                                # Gmail account that sends the alert
recipients <- str_split_1(Sys.getenv("EMAIL_TO"), ",\\s*")  # comma-separated list
if (sender == "" || Sys.getenv("SMTP_PASSWORD") == "" || all(recipients == "")) {
  stop("SMTP_USER, SMTP_PASSWORD or EMAIL_TO is not set")
}

lat <- 48.2085                  # Vienna, Stephansplatz
lon <- 16.3721
prob_threshold <- as.numeric(Sys.getenv("RAIN_PROB_THRESHOLD", "50"))  # rain probability in % that counts as "rain" (set 0 to test the email)
mm_threshold   <- 0.3           # ... or this much rain in mm per hour
day_hours      <- 7:21          # only hours you are out (07:00 to 21:59)

log_file     <- "data/rain-alert-log.txt"
history_file <- "data/rain-forecasts.csv"
dir.create("data", showWarnings = FALSE)

write_log <- function(text) {
  write_lines(paste(format(Sys.time(), tz = "Europe/Vienna", usetz = TRUE), text),
              log_file, append = TRUE)
}

get_credentials <- function() {
  creds_envvar(user = sender, pass_envvar = "SMTP_PASSWORD", provider = "gmail")
}

# ---- A napi, időjáráshoz illő szöveg kiválasztása --------------------------------
daily_greeting <- function(target_day, total_mm, day_word, max_prob) {
  messages <- if (is.finite(total_mm) && total_mm >= 20) {
    c(
      "Your motivation called in sick. Your degree didn’t. Get to class — let’s not make this another resit.",
      "Noah has the ark. You have the U-Bahn. Plan accordingly.",
      "Vienna’s auditioning for Titanic {day_word}. Bring an umbrella — Rose still isn’t sharing that door.",
      "You’re a heartthrob {day_word}, but you’re definitely not why the girls are getting wet.",
      "Skip the toilet paper {day_word}. One step outside and the sky will finish the job.",
      "John the Baptist has taken over the weather {day_word}. Expect a surprise baptism."
    )
  } else if (!is.finite(max_prob) || max_prob > 20) {
    c(
      "You’re made of sugar, buttercup. Bring an umbrella.",
      "Free outdoor showers {day_word}. Towels not included.",
      "I ain’t gonna lie to you — it’s going to be a really bad day for you.",
      "Take an umbrella. Looking dramatic in the rain is optional.",
      "It’s raining men! Hallelujah! It’s raining men! Nope, just water. Bring an umbrella."
    )
  } else {
    c(
      "The sky is flirting with rain. Don’t get too attached to your hairstyle.",
      "The forecast is indecisive. Pack an umbrella and feel superior either way.",
      "Not quite Noah weather. Still worth packing an umbrella.",
      "The sky might spit {day_word}. Try not to take it personally.",
      "Sunny {day_word} — allegedly. There’s still a higher chance of rain than of you passing Berger’s class."
    )
  }

  # A dátum naponta másik mondatot választ; az aznapi újrafuttatás ugyanazt adja.
  index <- (as.integer(target_day) %% length(messages)) + 1L
  gsub("{day_word}", day_word, messages[[index]], fixed = TRUE)
}

# ---- Main ---------------------------------------------------------------------
run_check <- function() {

  # 1. Download the forecast (GET request, like the Eurostat example)
  response <- GET(
    "https://api.open-meteo.com/v1/forecast",
    query = list(
      latitude      = lat,
      longitude     = lon,
      hourly        = "precipitation_probability,precipitation,temperature_2m",
      timezone      = "Europe/Vienna",
      forecast_days = 2
    )
  )
  if (status_code(response) != 200) {
    stop(glue("Open-Meteo returned status code {status_code(response)}"))
  }
  weather <- fromJSON(content(response, as = "text", encoding = "UTF-8"))

  # 2. Process: one row per hour
  forecast <- tibble(
    time = ymd_hm(weather$hourly$time, tz = "Europe/Vienna"),
    prob = weather$hourly$precipitation_probability,
    mm   = weather$hourly$precipitation,
    temp = weather$hourly$temperature_2m
  )

  # The check runs in the morning, so it looks at today. If it runs after 18:00
  # (e.g. a manual test in the evening), it looks at tomorrow instead.
  now <- with_tz(Sys.time(), "Europe/Vienna")
  today <- as.Date(now, tz = "Europe/Vienna")
  target_day <- if (hour(now) < 18) today else today + 1

  day <- forecast |>
    filter(as.Date(time, tz = "Europe/Vienna") == target_day,
           hour(time) %in% day_hours)

  # Success check: did we get usable data?
  if (nrow(day) < 10 || all(is.na(day$prob))) {
    stop(glue("Forecast for {target_day} is missing or empty ({nrow(day)} rows)"))
  }

  # 3. Save the forecast, so it can later be compared with the actual weather
  forecast |>
    mutate(run_time = now, target_day = target_day) |>
    write_csv(history_file, append = file.exists(history_file))

  # 4. Find rain windows: consecutive hours above the thresholds
  day <- day |>
    mutate(rain  = coalesce(prob >= prob_threshold, FALSE) | coalesce(mm >= mm_threshold, FALSE),
           block = cumsum(rain != lag(rain, default = FALSE)))

  # Esőjelzés nélkül csak naplózunk; email nem készül.
  if (!any(day$rain)) {
    write_log(glue("OK  {target_day}: no rain expected (max {max(day$prob, na.rm = TRUE)}%), no email"))
    return(invisible(NULL))
  }

  windows <- day |>
    filter(rain) |>
    group_by(block) |>
    summarise(from     = min(time),
              to       = max(time) + hours(1),
              max_prob = max(prob),
              total_mm = sum(mm))

  # 5. A napi angol szöveg és az esős időszakok összeállítása
  day_word <- if (target_day > today) "tomorrow" else "today"
  greeting <- daily_greeting(target_day, sum(day$mm), day_word,
                             max(day$prob, na.rm = TRUE))

  window_lines <- windows |>
    mutate(line = glue("- **{format(from, '%H:%M')}–{format(to, '%H:%M')}**: ",
                       "up to {max_prob}% chance, {round(total_mm, 1)} mm")) |>
    pull(line) |>
    paste(collapse = "\n")

  plot <- ggplot(day, aes(x = time, y = prob, fill = rain)) +
    geom_col() +
    geom_hline(yintercept = prob_threshold, linetype = "dashed") +
    scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = "steelblue"), guide = "none") +
    scale_x_datetime(date_labels = "%H:%M") +
    labs(x = NULL, y = "Rain probability (%)",
         title = glue("Vienna, {format(target_day, '%Y-%m-%d')}")) +
    theme_minimal()

  text <- glue(
    "## {greeting}\n\n",
    "Rain forecast for Vienna on **{format(target_day, '%Y-%m-%d')}**:\n\n",
    "{window_lines}\n\n",
    "Temperature during the day: {round(min(day$temp))}–{round(max(day$temp))} °C."
  )
  # add_ggplot() turns the plot into an HTML image that goes into the email body
  plot_html <- add_ggplot(plot_object = plot, width = 7, height = 3)

  email <- compose_email(
    body   = md(c(text, plot_html)),
    footer = md(glue("Forecast: Open-Meteo, checked {format(now, '%d.%m.%Y %H:%M')}"))
  )

  smtp_send(
    email,
    to          = recipients,
    from        = sender,
    subject     = glue("{greeting} | Vienna | {format(target_day, '%Y-%m-%d')}"),
    credentials = get_credentials()
  )

  write_log(glue("OK  {target_day}: rain expected, email sent ({nrow(windows)} window(s))"))
}

# ---- Run with a success check ---------------------------------------------------
tryCatch(
  run_check(),
  error = function(e) {
    write_log(glue("ERROR {conditionMessage(e)}"))
    # try to tell you that the check failed
    try(smtp_send(
      compose_email(body = md(glue("The rain check failed:\n\n`{conditionMessage(e)}`"))),
      to = recipients, from = sender, subject = "Rain check failed",
      credentials = get_credentials()
    ))
    stop(e)   # exit with an error, so Task Scheduler / GitHub shows the run as failed
  }
)
