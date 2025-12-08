renv::activate()
# library(dplyr)
library(dplyr)
source("utils/dataManipulation.R")
DATA_ROOT <-"C:/Users/elena/Documents/RProjects/agrovoltaics/logger_data/"


#'#################
#' READ IN DATA   #
#'#################
raw_data <- FieldAnalyzeR::read_data(DATA_ROOT, csv_sep = ";", add_ID_from_filename = FALSE )


#'####################
#' RENAMLE COLUMNS   #
#'####################
#'Meteo Station
rename_meteo <- c(
  id                   = "id",
  station_name         = "name",
  battery_voltage      = "bat_napeti",
  battery_status       = "bat_stav",
  bme_temperature      = "bmx_teplota",
  bme_pressure         = "bmx_tlak",
  bme_altitude         = "bmx_vyska",
  si7021_temperature   = "si7021_teplota",
  si7021_humidity      = "si7021_vlhkost",
  uv_intensity         = "uv_intenzita",
  wind_speed           = "WindSpeed",
  wind_direction_angle = "wind_dir_angle",
  wind_direction       = "wind_dir",
  sound_intensity_db   = "intenzita_db",
  precipitation_day    = "se_srazky_den",
  battery_temperature  = "bat_temp",
  lightning_count      = "lightning_d",
  lightning_distance   = "lightning_dist",
  error_code           = "kod_chyby",
  signal_strength_rssi = "RSSI",
  reserve0            = "rezerva0",
  reserve1            = "rezerva1",
  reserve2            = "rezerva2",
  timestamp            = "reading_time"
)

#' Air Temperature
rename_teplota <- c(
  id                   = "id",
  station_name         = "name",
  battery_voltage      = "stm_1_bat_napeti",
  solar_panel_voltage  = "stm_1_sp_napeti",
  battery_status       = "stm_1_bat_stav",
  photocell_light      = "stm_1_fotorezistor",
  sensor_error         = "stm_1_sensor_err",
  signal_strength_rssi = "stm_1_RSSI",
  air_temperature_1    = "stm_1_temp_1",
  air_temperature_2    = "stm_1_temp_2",
  air_temperature_3    = "stm_1_temp_3",
  air_temperature_4    = "stm_1_temp_4",
  air_temperature_5    = "stm_1_temp_5",
  air_temperature_6    = "stm_1_temp_6",
  reserve0            = "rezerva0",
  reserve1            = "rezerva1",
  reserve2            = "rezerva2",
  timestamp            = "reading_time"
)

#' Soil Profile Sensor
rename_soil <- c(
  id                   = "id",
  station_name         = "name",
  battery_voltage      = "Ubat",
  solar_panel_voltage  = "Upanel",
  battery_current      = "Cbat",
  battery_energy       = "Ebat",
  battery_temperature  = "Tbat",
  light_sensor         = "Fotorezistor",
  signal_strength_rssi = "RSSI",
  
  soil_temp_1          = "temp1",
  soil_moisture_1      = "moist1",
  soil_conductivity_1  = "vodivost1",
  soil_pH_1            = "pH1",
  soil_nitrogen_1      = "Nko1",
  soil_phosphorus_1    = "Pko1",
  soil_potassium_1     = "Kko1",
  soil_salinity_1      = "salin1",
  soil_TDS_1           = "TDS1",
  
  soil_temp_2          = "temp2",
  soil_moisture_2      = "moist2",
  soil_conductivity_2  = "vodivost2",
  soil_pH_2            = "pH2",
  soil_nitrogen_2      = "Nko2",
  soil_phosphorus_2    = "Pko2",
  soil_potassium_2     = "Kko2",
  soil_salinity_2      = "salin2",
  soil_TDS_2           = "TDS2",
  
  soil_temp_3          = "temp3",
  soil_moisture_3      = "moist3",
  soil_conductivity_3  = "vodivost3",
  soil_pH_3            = "pH3",
  soil_nitrogen_3      = "Nko3",
  soil_phosphorus_3    = "Pko3",
  soil_potassium_3     = "Kko3",
  soil_salinity_3      = "salin3",
  soil_TDS_3           = "TDS3",
  
  soil_temp_4          = "temp4",
  soil_moisture_4      = "moist4",
  soil_conductivity_4  = "vodivost4",
  soil_pH_4            = "pH4",
  soil_nitrogen_4      = "Nko4",
  soil_phosphorus_4    = "Pko4",
  soil_potassium_4     = "Kko4",
  soil_salinity_4      = "salin4",
  soil_TDS_4           = "TDS4",
  
  reserve0            = "rezerva0",
  reserve1            = "rezerva1",
  reserve2            = "rezerva2",
  timestamp            = "reading_time"
)

#' Lawn Sensor
#' Here the Column names are stored in line 191
travnik_colnames <- as.character(raw_data$travnik.csv[190,])
colnames(raw_data$travnik.csv) <- travnik_colnames
raw_data$travnik.csv <- raw_data$travnik.csv[-190,]

rename_travnik <- c(
  id = "id",
  station_name = "name",
  battery_voltage = "Ubat",
  solar_panel_voltage = "Upanel",
  battery_current = "Cbat",
  temperature = "temp",
  humidity = "hum",
  
  
  nitrogen = "Nko",
  phosphorus = "Pko",
  potassium = "Kko",
  
  soil_conductivity = "vodivost",
  pH = "pH",
  salinity = "salin",
  TDS = "TDS",       # total dissolved solids  
  error_code = "chyba",
  
  # Status + Signalqualität
  status = "stav",
  signal_strength = "RSSI",
  
  # Reservierte Felder
  reserve0 = "rezerva0",
  reserve1 = "rezerva1",
  reserve2 = "rezerva2",
  reserve3 = "rezerva3",
  
  timestamp = "reading_time"
 
)


# Create rename maps list
rename_maps <- list(
  "meteo.csv"                  = rename_meteo,
  "teplota_vzduch.csv"         = rename_teplota,
  "teploty_vlhkosti_vZemi.csv" = rename_soil,
  "travnik.csv" = rename_travnik
)


renamed_data <- lapply(names(raw_data), function(name) {
  df <- raw_data[[name]]
  map <- rename_maps[[name]]
  
  df %>% rename(all_of(map))
  
})

names(renamed_data) <- names(raw_data)

#'##################################### 
#' REMOVE COLUMNS THAT ARE NOT NEEDED #
#'##################################### 

drop_columns <- list(
  meteo = c("battery_voltage", "battery_status", 
            "battery_temperature", "error_code", "reserve0", 
            "reserve1", "reserve2", "signal_strength_rssi"),
  teplota_vzduch = c( "battery_voltage" ,  "solar_panel_voltage", "battery_status", "sensor_error",
              "signal_strength_rssi", "reserve0", "reserve1", "reserve2"),
  teploty_vlhkosti_vZemi = c("battery_voltage", "solar_panel_voltage", "battery_current", "battery_energy",
           "battery_temperature", "reserve0", "reserve1", "reserve2"),
  travnik = c( "battery_voltage", "solar_panel_voltage","battery_current", "error_code" ,"status" ,
               "signal_strength", "reserve0", "reserve1", "reserve2", "reserve3"  )
)

# Remove ".csv" from names if necessary
names(renamed_data) <- sub("\\.csv$", "", names(renamed_data))

# Apply mutate_dates and drop columns
cleaned_data <- lapply(names(renamed_data), function(name) {
  df <- renamed_data[[name]]

  df_date <- mutate_dates(df, "timestamp", "%Y-%m-%d %H:%M:%S")
  
  # Drop columns if defined
  if (name %in% names(drop_columns)) {
    df_date <- df_date %>% dplyr::select(-all_of(drop_columns[[name]]))
  }
  
  return(df_date)
})
names(cleaned_data) <- sub("\\.csv$", "", names(renamed_data))

#'################################################
#' Make sure all columns have the right format
#'################################################

library(dplyr)

# Define columns you want to convert
meteo <- c(
  "bme_temperature", "bme_pressure", "bme_altitude",
  "si7021_temperature", "si7021_humidity", "uv_intensity",
  "wind_speed", "wind_direction_angle", "sound_intensity_db",
  "precipitation_day"
)

travnik <- c(
  "id", "temperature", "humidity", "nitrogen", "phosphorus",
  "potassium",  "soil_conductivity",  "pH",  "salinity",  "TDS"                                                            
)


cleaned_data$meteo <- cleaned_data$meteo %>%
  mutate(across(all_of(meteo), ~ as.numeric(gsub(",", ".", .))))

cleaned_data$travnik <- cleaned_data$travnik %>%
  mutate(across(all_of(travnik), ~ as.numeric(gsub(",", ".", .))))


# Define output folder
output_folder <- "cleaned_data"

# Create folder if it doesn't exist
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# Save each cleaned data frame
for (name in names(cleaned_data)) {
  df <- cleaned_data[[name]]
  filename <- paste0(name, ".csv")
  filepath <- file.path(output_folder, filename)
  write.csv(df, filepath, row.names = FALSE)
  message("Saved: ", filepath)
}

