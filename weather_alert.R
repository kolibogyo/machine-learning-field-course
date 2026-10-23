#helyszin es idozona
location_name <- "Bécs"
latitude <- 48.2082
longitude <- 16.3738
local_timezone <- "Europe/Vienna"

# az ertesites beallitasai
rain_threshold_mm <- 0.1
# A futtatási mód beolvasása a környezeti beállításokból
run_mode <- Sys.getenv("RUN_MODE", unset = "evening")
dry_run <- Sys.getenv("DRY_RUN", unset = "true") != "false"

# A bemutatóhoz használt tesztpéldák futtatásának beállítása
run_tests <- FALSE

run_mode

#idojarasi elorejelzes lekerese
response <- httr::GET(
  url = "https://api.open-meteo.com/v1/forecast",
  query = list( latitude = latitude,
                longitude = longitude,
                hourly = "rain,showers",
                timezone = local_timezone,
                forecast_days = 2)
)


#keres statuszanak ellenorzese
httr::status_code(response)

#API tartalmanak beolvasasa
response_text <- httr::content(
  response,
  as = "text",
  encoding = "UTF-8"
)

forecast <- jsonlite::fromJSON(response_text)

#feldolgozott valasz fo reszeinek megtekintese
names(forecast)

#az orankenti adatok mertekegysegenek ellenorzese
forecast$hourly_units

#orankenti elorejelzes tablazatba helyezese
weather <- data.frame(
  time = forecast$hourly$time,
  rain_mm = forecast$hourly$rain,
  showers_mm = forecast$hourly$showers
)

#tablazat elso sorainak megjelenitese
head(weather)

#idobelyegek atalakitasa R-ben kezelheto idopontokka
weather$time <- as.POSIXct(
  weather$time, 
  format = "%Y-%m-%dT%H:%M",
  tz = local_timezone
)

#teljes esomennyiseg kiszamitasa orankent
weather$total_rain_mm <- weather$rain_mm + weather$showers_mm

#tablazat szerkezetenek ellenorzese
str(weather)


#a vizsgalando nap meghatarozasa futtatasi kod alapjan
today_local <- as.Date(Sys.time(), tz = local_timezone)

if (run_mode == "evening") {
  target_date <- today_local +1
} else if (run_mode == "morning") {
  target_date <- today_local
} else {
  stop ("A run_mode értéke evening vagy morning lehet.")
}
target_date

#vizsgalt idoablak hatarainak meghatarozasa
window_start <- as.POSIXct(
  paste(target_date, "07:00:00"),
  tz = local_timezone
)
window_end <- as.POSIXct(
  paste(target_date, "22:00:00"),
  tz = local_timezone
)

#az idoablakba tartozo es meg le nem zarult orak kivalasztasa
day <- weather[
  weather$time > window_start &
    weather$time <= window_end &
    weather$time > Sys.time(),
]
head(day)

#esoadatok ellenorzese
if (nrow(day) == 0 ||
    any(!is.finite(day$total_rain_mm))||
    any(day$total_rain_mm < 0)) {
  stop("Hiányzó vagy hibás esőadatok érkeztek.")
}

#az esos orak kivalasztasa es az osszes esomennyiseg kiszamitasa
rainy_hours <- day[
  day$total_rain_mm >= rain_threshold_mm,
]

total_expected_rain_mm <- sum(day$total_rain_mm)

#az eredmenyek megtekintese
nrow(day)
rainy_hours
total_expected_rain_mm

# Az esőjelzés és az összefoglaló szöveg elkészítése
rain_expected <- nrow(rainy_hours) > 0

if (rain_expected) {
  rain_summary <- paste0(
    "Figyelj jól kiskakukk, mert mára ennyi csapadékot jósoltak: ",
    round(total_expected_rain_mm, 1),
    " mm."
  )
} else {
  rain_summary <- paste0(
    "Nem kell esernyõ töki"
  )
}

# Az elkészült szöveg megtekintése
rain_summary

# A bemutatóhoz használt tesztpéldák feltételes futtatása
if (run_tests) {
  
  # Tesztadatok létrehozása az esős időszakok feldolgozásához
  rain_test <- data.frame(
    time = as.POSIXct(
      paste(target_date, c("09:00:00", "10:00:00", "15:00:00")),
      tz = local_timezone
    ),
    total_rain_mm = c(0.3, 0.5, 1.2)
  )
  
  # Az egyes órás időszakok kezdetének kiszámítása
  rain_test$interval_start <- rain_test$time - 3600
  rain_test
  
  #az egymast koveto esos orak csoportositasa
  time_gaps <- as.numeric(
    diff(rain_test$time),
    units = "hours"
  )
  rain_test$period_id <- cumsum(
    c(TRUE, time_gaps > 1)
  )
  rain_test
  
  #az esos idoszakok osszevonasa es esomennyiseguk osszegzese
  rain_periods <- data.frame()
  for(id in unique(rain_test$period_id)){
    period <- rain_test[rain_test$period_id == id, ]
    
    rain_periods <- rbind(
      rain_periods,
      data.frame(
        start = min(period$interval_start),
        end = max(period$time),
        rain_mm = sum(period$total_rain_mm)
      )
    )
  }
  rain_periods
  
  #az esos idoszakok szovegge alakitasa
  period_lines <- sprintf(
    "%s-%s: %.1f mm",
    format(rain_periods$start, "%H:%M", tz = local_timezone),
    format(rain_periods$end, "%H:%M", tz = local_timezone),
    rain_periods$rain_mm
  )
  cat(period_lines, sep = "\n")
}

#valodi elorejelzes esos idoszakainak feldolgozasa
if (rain_expected) {
  rainy_hours$interval_start <- rainy_hours$time - 3600
  
  time_gaps <- as.numeric(
    diff(rainy_hours$time),
    units = "hours"
  )
  rainy_hours$period_id <- cumsum(c(TRUE, time_gaps > 1))
  
  rain_periods <- data.frame ()
  for (id in unique(rainy_hours$period_id)) {
    period <- rainy_hours[rainy_hours$period_id == id, ]
    rain_periods <- rbind(
      rain_periods,
      data.frame(
        start = min(period$interval_start),
        end = max(period$time),
        rain_mm = sum(period$total_rain_mm)
      )
    )
  }
  
  period_lines <- sprintf(
    "%s–%s: %.1f mm",
    format(rain_periods$start, "%H:%M", tz = local_timezone),
    format(rain_periods$end, "%H:%M", tz = local_timezone),
    rain_periods$rain_mm
  )
  
  rain_details <- paste(period_lines, collapse = "\n")
} else {
  rain_details <- "Nincs felsorolható esős időszak."
}

# Az összefoglaló és a részletek megjelenítése
cat(rain_summary, rain_details, sep = "\n\n")

# Az email tárgyának és szövegének összeállítása
email_subject <- paste(
  "Esőjelentés",
  location_name,
  target_date,
  sep = " | "
)

email_body <- paste(
  c(
    paste("Előrejelzés dátuma:", target_date),
    rain_summary,
    if (rain_expected) rain_details
  ),
  collapse = "\n\n"
)

# Az email előnézetének megjelenítése
cat(email_subject, email_body, sep = "\n\n")

# A sortörések előkészítése a formázott emailhez
email_body_md <- gsub("\n", "  \n", email_body, fixed = TRUE)

# Az emailüzenet létrehozása
email_message <- blastula::compose_email(
  body = blastula::md(email_body_md)
)

# Az email előnézetének megnyitása interaktív használat esetén
if (interactive()) {
  print(email_message)
}

# Az email elküldése vagy tesztüzem jelzése
if (dry_run) {
  message("Tesztüzem: az email nem lett elküldve.")
} else {
  blastula::smtp_send(
    email = email_message,
    to = Sys.getenv("EMAIL_TO"),
    from = Sys.getenv("SMTP_USER"),
    subject = email_subject,
    credentials = blastula::creds_envvar(
      user = Sys.getenv("SMTP_USER"),
      pass_envvar = "SMTP_PASSWORD",
      provider = "gmail"
    )
  )
  
  message("A Gmail SMTP-szervere elfogadta a levelet.")
}
