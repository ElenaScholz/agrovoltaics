renv::activate()
source("utils/dataManipulation.R")
aggregate_data <- function(df, aggregation_type, aggregation_columns) {
  # Check if required columns are present in the dataframe
  required_columns <- c("Date", "Year", "Julian", "Month", "Hour", "Minute", "Second")
  if (!all(required_columns %in% colnames(df))) {
    missing_columns <- required_columns[!required_columns %in% colnames(df)]
    stop(paste("Required columns missing in the dataframe:", paste(missing_columns, collapse = ", ")))
  }
  
  # Check if aggregation columns exist
  if (!all(aggregation_columns %in% colnames(df))) {
    missing_agg_cols <- aggregation_columns[!aggregation_columns %in% colnames(df)]
    stop(paste("Aggregation columns missing in the dataframe:", paste(missing_agg_cols, collapse = ", ")))
  }
  
  # Convert comma decimal separators to periods and then to numeric
  df_converted <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(aggregation_columns), 
                                ~as.numeric(gsub(",", ".", .))))
  
  # Define grouping variables based on aggregation type
  group_vars <- switch(aggregation_type,
                       "second" = c("Date", "Hour", "Minute", "Second"),
                       "minute" = c("Date", "Hour", "Minute"),
                       "hourly" = c("Date", "Hour"),
                       "daily" = c("Date"),
                       "monthly" = c("Month", "Year"),
                       "annual" = c("Year"),
                       "seasonal" = c("Year", "Season"),
                       stop("Invalid aggregation_type. Please choose 'second', 'minute', 'hourly', 'daily', 'monthly', 'seasonal', or 'annual'.")
  )
  
  # Handle seasonal aggregation specially
  if (aggregation_type == "seasonal") {
    df_converted <- df_converted %>%
      dplyr::mutate(Season = dplyr::case_when(
        lubridate::month(as.Date(Date)) %in% 3:5 ~ "Spring",
        lubridate::month(as.Date(Date)) %in% 6:8 ~ "Summer",
        lubridate::month(as.Date(Date)) %in% 9:11 ~ "Autumn",
        TRUE ~ "Winter"
      ))
  }
  
  # Perform aggregation
  aggregated_data <- df_converted %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(aggregation_columns),
        list(
          mean = ~mean(., na.rm = TRUE),
          std = ~stats::sd(., na.rm = TRUE),
          min = ~min(., na.rm = TRUE),
          max = ~max(., na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      # Preserve metadata columns
      station_name = dplyr::first(station_name),
      .groups = "drop"
    )
  
  # Add back temporal metadata columns that were used for grouping
  if (aggregation_type %in% c("second", "minute", "hourly", "daily")) {
    if (!"Month" %in% group_vars) {
      aggregated_data <- aggregated_data %>%
        dplyr::mutate(Month = dplyr::first(df_converted$Month[df_converted$Date == Date[1]]))
    }
    if (!"Year" %in% group_vars) {
      aggregated_data <- aggregated_data %>%
        dplyr::mutate(Year = dplyr::first(df_converted$Year[df_converted$Date == Date[1]]))
    }
    if (!"Julian" %in% group_vars) {
      aggregated_data <- aggregated_data %>%
        dplyr::mutate(Julian = dplyr::first(df_converted$Julian[df_converted$Date == Date[1]]))
    }
  }
  
  return(aggregated_data)
}

DATA_PATH <- "cleaned_data/"

data <- FieldAnalyzeR::read_data(DATA_PATH, add_ID_from_filename = F)
names(data) <- sub("\\.csv$", "", names(data))

meteo_agg_col <- c("bme_temperature" , "bme_pressure",
"bme_altitude","si7021_temperature",
"si7021_humidity" , "uv_intensity" , "wind_speed" ,   "wind_direction_angle" ,
 "sound_intensity_db" , "precipitation_day"  ,  "lightning_count"    ,  "lightning_distance"  )

meteo_min <- aggregate_data(data$meteo, "minute",meteo_agg_col )

library(ggplot2)
library(viridis)

# Prepare data
meteo_min_plot <- meteo_min %>%
  mutate(Date = as.Date(Date),
         DateTime = as.POSIXct(paste(Date, sprintf("%02d:%02d", Hour, Minute)), 
                               format = "%Y-%m-%d %H:%M"))

# Heatmap for temperature
ggplot(meteo_min_plot, aes(x = Hour + Minute/60, y = Date, 
                           fill = bme_temperature_mean)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma") +
  scale_x_continuous(breaks = seq(0, 24, 4), 
                     labels = sprintf("%02d:00", seq(0, 24, 4))) +
  labs(title = "BME Temperature - Minute Resolution",
       x = "Time of Day", y = "Date", fill = "°C") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






library(tidyr)
library(tidyr)
library(ggplot2)
library(viridis)

# Reshape for faceting
meteo_long <- meteo_min_plot %>%
  select(Date, Hour, Minute, 
         bmeTemperature = bme_temperature_mean,
         si7021Humidity = si7021_humidity_mean,
         bmePressure = bme_pressure_mean,
         Sound = sound_intensity_db_mean,
         bmeAltitude = bme_altitude_mean,
         si7021Temperature = si7021_temperature_mean,
         uvIntensity = uv_intensity_mean,
         Windspeed = wind_speed_mean,
         Precipitation = precipitation_day_mean) %>%
  pivot_longer(cols = c(bmeTemperature, si7021Humidity, bmePressure, Sound,bmeAltitude,si7021Temperature,Windspeed,Precipitation),
               names_to = "Variable", values_to = "Value")

# Plot with independent color scales
library(patchwork)

# Function to create individual heatmaps
create_heatmap <- function(data, var_name, var_label, color_palette = "viridis") {
  ggplot(data, aes(x = Hour + Minute/60, y = as.Date(Date), fill = .data[[var_name]])) +
    geom_tile() +
    scale_fill_viridis_c(option = color_palette, name = var_label) +
    scale_x_continuous(breaks = seq(0, 24, 6),
                       labels = sprintf("%02d:00", seq(0, 24, 6))) +
    labs(title = var_label, x = "Hour of Day", y = "Date") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, face = "bold"))
}

# Create individual plots with different color scales
p1 <- create_heatmap(meteo_min_plot, "bme_temperature_mean", "Temperature (°C)", "plasma")
p2 <- create_heatmap(meteo_min_plot, "si7021_humidity_mean", "Humidity (%)", "mako")
p3 <- create_heatmap(meteo_min_plot, "bme_pressure_mean", "Pressure (hPa)", "viridis")
p4 <- create_heatmap(meteo_min_plot, "sound_intensity_db_mean", "Sound (dB)", "cividis")

# Combine with patchwork
(p1 | p2) / (p3 | p4)



library(ggplot2)
library(dplyr)
library(viridis)
library(lubridate)
library(ggExtra)
library(tidyr)

# Prepare data with proper date components
meteo_calendar <- meteo_min_plot %>%
  mutate(
    Date = as.Date(Date),
    year = year(Date),
    month = month(Date, label = TRUE),
    day = day(Date)
  )

# Create function for each variable plot
create_calendar_heatmap <- function(data, var_name, var_label, viridis_option = "C") {
  
  p <- ggplot(data, aes(x = day, y = Hour + Minute/60, fill = .data[[var_name]])) +
    geom_tile(color = "white", size = 0.1) + 
    scale_fill_viridis(name = var_label, option = viridis_option, na.value = "grey90")
  
  p <- p + facet_grid(year ~ month)
  p <- p + scale_y_continuous(trans = "reverse", 
                              breaks = seq(0, 23, 2),
                              labels = sprintf("%02d:00", seq(0, 23, 2)))
  p <- p + scale_x_continuous(breaks = c(1, 10, 20, 31))
  p <- p + theme_minimal(base_size = 8)
  p <- p + labs(title = var_label, x = "Day", y = "Hour")
  p <- p + theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, hjust = 0, face = "bold"),
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 7),
    axis.ticks = element_blank(),
    strip.background = element_rect(colour = "white"),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.3, "cm")
  )
  p <- p + removeGrid()
  
  return(p)
}

# Create individual plots with different color schemes
p1 <- create_calendar_heatmap(meteo_calendar, "bme_temperature_mean", 
                              "BME Temperature (°C)", "plasma")

p2 <- create_calendar_heatmap(meteo_calendar, "si7021_temperature_mean", 
                              "Si7021 Temperature (°C)", "inferno")

p3 <- create_calendar_heatmap(meteo_calendar, "si7021_humidity_mean", 
                              "Humidity (%)", "mako")

p4 <- create_calendar_heatmap(meteo_calendar, "bme_pressure_mean", 
                              "Pressure (hPa)", "viridis")

p5 <- create_calendar_heatmap(meteo_calendar, "bme_altitude_mean", 
                              "Altitude (m)", "cividis")

p6 <- create_calendar_heatmap(meteo_calendar, "sound_intensity_db_mean", 
                              "Sound Intensity (dB)", "rocket")

p7 <- create_calendar_heatmap(meteo_calendar, "wind_speed_mean", 
                              "Wind Speed (m/s)", "turbo")

p8 <- create_calendar_heatmap(meteo_calendar, "precipitation_day_mean", 
                              "Precipitation (mm)", "mako")

p9 <- create_calendar_heatmap(meteo_calendar, "uv_intensity_mean", 
                              "UV Intensity", "plasma")

# Display individual plots
p1  # Temperature
p2  # Si7021 Temperature  
p3  # Humidity
p4  # Pressure
p5  # Altitude
p6  # Sound
p7  # Wind Speed
p8  # Precipitation
p9  # UV Intensity

# OR combine multiple plots in a grid
library(patchwork)

# 2x2 grid of key variables
(p1 | p3) / (p4 | p6)

# 3x3 grid of all variables
(p1 | p2 | p3) / (p4 | p5 | p6) / (p7 | p8 | p9)