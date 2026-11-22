
# Clean air quality data from one CSV file
# file_path: CSV file location
# o3_unit: "ppm" or "ppb", default is "ppm"
# Returns: Daily average concentrations with one row per day

clean_air <- function(file_path, o3_unit = "ppm") {
  
  # Check O3 unit
  if (!o3_unit %in% c("ppm", "ppb")) {
    stop("Unit must be ppm or ppb")
  }
  
  # Read data
  data <- read.csv(file_path, check.names = FALSE)
  
  # Keep and rename columns
  data <- data[, c("Date",
                   "Daily Max 8-hour CO Concentration",
                   "Daily Mean PM2.5 Concentration",
                   "Daily Max 8-hour Ozone Concentration")]
  
  names(data) <- c("date", "co", "pm25", "o3")
  
  # Convert date
  data$date <- as.Date(data$date, format = "%m/%d/%Y")
  
  # Remove duplicate rows
  data <- unique(data)
  
  # Replace negative values with 0
  for (pollutant in c("co", "pm25", "o3")) {
    data[[pollutant]][!is.na(data[[pollutant]]) &
                        data[[pollutant]] < 0] <- 0
  }
  
  # Calculate daily averages
  daily <- aggregate(
    data[, c("co", "pm25", "o3")],
    by = list(date = data$date),
    FUN = function(x) {
      if (all(is.na(x))) NA_real_
      else mean(x, na.rm = TRUE)
    }
  )
  
  # Convert O3 to ppb
  if (o3_unit == "ppb") {
    daily$o3 <- daily$o3 * 1000
  }
  
  return(daily)
}

