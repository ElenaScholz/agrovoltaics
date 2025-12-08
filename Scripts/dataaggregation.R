rm(list = ls())
renv::activate()
source("utils/dataManipulation.R")
library(dplyr)
#'
#'  Read in Data
#'

DATA_PATH <- "cleaned_data/"

data <- FieldAnalyzeR::read_data(DATA_PATH, add_ID_from_filename = F)
names(data) <- sub("\\.csv$", "", names(data))

# find out time interval between the measurements
timedifference_btw_measurements <- function(df, time_column) {
  df <- df %>% 
    mutate(
      {{ time_column }} := as.POSIXct({{ time_column }},
                                      format = "%Y-%m-%d %H:%M:%S",
                                      tz = "UTC")
    ) %>%
    arrange({{ time_column }}) %>%
    mutate(
      diff_seconds = as.numeric({{ time_column }} - lag({{ time_column }})),
      diff_minutes = diff_seconds / 60
    )
  
  
  
  # Ausgabe des Mittelwertes

  print(paste0("Mean time interval in seconds: ", round(mean(df$diff_seconds, na.rm = TRUE), 2)))
  print(paste0("Median time interval in seconds: ", round(median(df$diff_seconds, na.rm = TRUE), 2)))
  print(paste0("Standard deviation time interval in seconds: ", round(sd(df$diff_seconds, na.rm = TRUE), 2)))
  print(paste0("Minimum time interval in seconds:" ,round(min(df$diff_seconds, na.rm = T), 2)))
  print(paste0("Maximum time interval in seconds:" ,round(max(df$diff_seconds, na.rm = T), 2)))
  

  return(df)
}

meteo_timelag <- timedifference_btw_measurements(data$meteo, timestamp)
temperature_timelag <- timedifference_btw_measurements(data$teplota_vzduch, timestamp)
soil_timelag <- timedifference_btw_measurements(data$teploty_vlhkosti_vZemi, timestamp)
lawn_timelag <- timedifference_btw_measurements(data$travnik, timestamp)

# Histogram of time differences
ggplot(meteo_timelag, aes(x = diff_minutes)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  scale_x_log10() +  # Log scale often helps
  labs(title = "Distribution of Time Lags",
       x = "Time Difference (minutes, log scale)",
       y = "Count") +
  theme_minimal()

# Boxplot to identify outliers
ggplot(meteo_timelag, aes(y = diff_minutes)) +
  geom_boxplot(fill = "lightblue") +
  scale_y_log10() +
  labs(title = "Time Lag Outliers",
       y = "Time Difference (minutes, log scale)") +
  theme_minimal()










meteo_agg_col <- c("bme_temperature" , "bme_pressure",
"bme_altitude","si7021_temperature",
"si7021_humidity" , "uv_intensity" , "wind_speed" ,   "wind_direction_angle" ,
 "sound_intensity_db" , "precipitation_day"  ,  "lightning_count"    ,  "lightning_distance"  )

meteo_min <- aggregate_data(data$meteo, "minute",meteo_agg_col )
