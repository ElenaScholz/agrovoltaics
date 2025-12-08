mutate_dates <- function(df, datetime_column = "timestamp", time_format = "%Y-%m-%d %H:%M:%S") {
  
  mutated_df <- df %>%
    dplyr::mutate(
      !!rlang::sym(datetime_column) := as.POSIXct(
        as.character(!!rlang::sym(datetime_column)), 
        format = time_format
      ),
      Date   = as.Date(!!rlang::sym(datetime_column)),
      Julian = lubridate::yday(!!rlang::sym(datetime_column)),
      Month  = factor(month.name[lubridate::month(!!rlang::sym(datetime_column))], levels = month.name),
      Year   = lubridate::year(!!rlang::sym(datetime_column)),
      Hour   = lubridate::hour(!!rlang::sym(datetime_column)),
      Minute = lubridate::minute(!!rlang::sym(datetime_column)),
      Second = lubridate::second(!!rlang::sym(datetime_column))
    )
  
  return(mutated_df)
}


#' Aggregate Data
#'
#' Perform data aggregation by Day, Month, Season, or Year based on a specified aggregation column.
#'
#' @param df A dataframe containing the Logger data. Columns should include "Logger_ID", "Month", "Date", "Year", "Julian", and the specified aggregation column.
#' @param aggregation_type The type of aggregation to perform: "daily", "monthly", "seasonal", or "annual".
#' @param aggregation_column The name of the column to use for aggregation.
#'
#' @return A dataframe with the aggregated data including mean, standard deviation, minimum, and maximum of the specified aggregation column.
#'
#' @importFrom dplyr group_by filter summarise mutate case_when first
#' @importFrom stats sd
#' @importFrom lubridate month year
#'
#' @export
#'
#'

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

# 
# 
# 
# aggregate_data <- function(df, aggregation_type, aggregation_column) {
#   # Check if required columns are present in the dataframe
#   required_columns <- c("Logger_ID", "Month", "Date", "Year", "Julian", "Hour", "Minute", "Second", aggregation_column)
#   if (!all(required_columns %in% colnames(df))) {
#     missing_columns <- required_columns[!required_columns %in% colnames(df)]
#     stop(paste("Required columns missing in the dataframe:", paste(missing_columns, collapse = ", ")))
#   }
#   
#   # Rename the specified aggregation column within the dataframe
#   colnames(df)[colnames(df) == aggregation_column] <- "Aggregation_Column"
#   
#   # Perform aggregation based on aggregation type
#   if (aggregation_type == "second") {
#     second_data <- df %>%
#       dplyr::group_by(Date, Hour, Minute, Second) %>%
#       dplyr::filter(!is.na(Aggregation_Column)) %>%
#       dplyr::summarise(mean_value = mean(Aggregation_Column),
#                        std_value = stats::sd(Aggregation_Column),
#                        min_value = min(Aggregation_Column),
#                        max_value = max(Aggregation_Column),
#                        Logger_ID = dplyr::first(Logger_ID),
#                        Month = dplyr::first(Month),
#                        Year = dplyr::first(Year),
#                        Julian = dplyr::first(Julian),
#                        .groups = "drop")
#     return(second_data)
#     
#   } else if (aggregation_type == "minute") {
#     minute_data <- df %>%
#       dplyr::group_by(Date, Hour, Minute) %>%
#       dplyr::filter(!is.na(Aggregation_Column)) %>%
#       dplyr::summarise(mean_value = mean(Aggregation_Column),
#                        std_value = stats::sd(Aggregation_Column),
#                        min_value = min(Aggregation_Column),
#                        max_value = max(Aggregation_Column),
#                        Logger_ID = dplyr::first(Logger_ID),
#                        Month = dplyr::first(Month),
#                        Year = dplyr::first(Year),
#                        Julian = dplyr::first(Julian),
#                        .groups = "drop")
#     return(minute_data)
#     
#   } else if (aggregation_type == "hourly") {
#     hourly_data <- df %>%
#       dplyr::group_by(Date, Hour) %>%
#       dplyr::filter(!is.na(Aggregation_Column)) %>%
#       dplyr::summarise(mean_value = mean(Aggregation_Column),
#                        std_value = stats::sd(Aggregation_Column),
#                        min_value = min(Aggregation_Column),
#                        max_value = max(Aggregation_Column),
#                        Logger_ID = dplyr::first(Logger_ID),
#                        Month = dplyr::first(Month),
#                        Year = dplyr::first(Year),
#                        Julian = dplyr::first(Julian),
#                        .groups = "drop")
#     return(hourly_data)
#     
#   } else if (aggregation_type == "daily") {
#     daily_data <- df %>%
#       dplyr::group_by(Date) %>%
#       dplyr::filter(!is.na(Aggregation_Column)) %>%
#       dplyr::summarise(mean_value = mean(Aggregation_Column),
#                        std_value = stats::sd(Aggregation_Column),
#                        min_value = min(Aggregation_Column),
#                        max_value = max(Aggregation_Column),
#                        Logger_ID = dplyr::first(Logger_ID),
#                        Month = dplyr::first(Month),
#                        Year = dplyr::first(Year),
#                        Julian = dplyr::first(Julian),
#                        .groups = "drop")
#     return(daily_data)
#     
#   } else if (aggregation_type == "monthly") {
#     monthly_data <- df %>%
#       dplyr::group_by(Month, Year) %>%
#       dplyr::filter(!is.na(Aggregation_Column)) %>%
#       dplyr::summarise(mean_value = mean(Aggregation_Column),
#                        std_value = stats::sd(Aggregation_Column),
#                        min_value = min(Aggregation_Column),
#                        max_value = max(Aggregation_Column),
#                        Logger_ID = dplyr::first(Logger_ID),
#                        .groups = "drop")
#     return(monthly_data)
#     
#   } else if (aggregation_type == "annual") {
#     annual_data <- df %>%
#       dplyr::group_by(Year) %>%
#       dplyr::filter(!is.na(Aggregation_Column)) %>%
#       dplyr::summarise(mean_value = mean(Aggregation_Column),
#                        std_value = stats::sd(Aggregation_Column),
#                        min_value = min(Aggregation_Column),
#                        max_value = max(Aggregation_Column),
#                        Logger_ID = dplyr::first(Logger_ID),
#                        .groups = "drop")
#     return(annual_data)
#     
#   } else if (aggregation_type == "seasonal") {
#     seasonal_data <- df %>%
#       dplyr::mutate(Season = dplyr::case_when(
#         lubridate::month(Date) %in% 3:5 ~ "Spring",
#         lubridate::month(Date) %in% 6:8 ~ "Summer",
#         lubridate::month(Date) %in% 9:11 ~ "Autumn",
#         TRUE ~ "Winter"
#       )) %>%
#       dplyr::group_by(Year = lubridate::year(Date), Season) %>%
#       dplyr::filter(!is.na(Aggregation_Column)) %>%
#       dplyr::summarise(mean_value = mean(Aggregation_Column),
#                        std_value = stats::sd(Aggregation_Column),
#                        min_value = min(Aggregation_Column),
#                        max_value = max(Aggregation_Column),
#                        Logger_ID = dplyr::first(Logger_ID),
#                        .groups = "drop")
#     return(seasonal_data)
#     
#   } else {
#     stop("Invalid aggregation_type. Please choose 'second', 'minute', 'hourly', 'daily', 'monthly', 'seasonal', or 'annual'.")
#   }
# }