library(ggplot2)
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



# Clean data for each year
aq2018_clean <- clean_air("data/AQ_2018.csv")
aq2019_clean <- clean_air("data/AQ_2019.csv")
aq2020_clean <- clean_air("data/AQ_2020.csv")

# Combine three years
aq_all <- rbind(aq2018_clean, aq2019_clean, aq2020_clean)

# Create summary table
make_table <- function(data) {
  
  pollutants <- c("co", "pm25", "o3")
  
  table <- data.frame(
    Pollutant = c("CO (ppm)", "PM2.5 (ug/m3)", "O3 (ppm)"),
    Minimum = sprintf("%.3f",
                      sapply(data[pollutants], min, na.rm = TRUE)),
    Maximum = sprintf("%.3f",
                      sapply(data[pollutants], max, na.rm = TRUE))
  )
  
  return(table)
}

# Tables for each year
table2018 <- make_table(aq2018_clean)
table2019 <- make_table(aq2019_clean)
table2020 <- make_table(aq2020_clean)

table2018
table2019
table2020

# Plot 1
plot1_data <- data.frame()

for (p in c("co", "o3")) {
  for (m in 1:12) {
    
    x <- aq_all[[p]][as.integer(format(aq_all$date, "%m")) == m]
    x <- x[!is.na(x)]
    
    avg <- mean(x)
    se <- sd(x) / sqrt(length(x))
    
    plot1_data <- rbind(plot1_data, data.frame(
      month = month.abb[m],
      pollutant = p,
      mean = avg,
      lower = avg - 1.96 * se,
      upper = avg + 1.96 * se
    ))
  }
}

# Set month order
plot1_data$month <- factor(plot1_data$month, levels = month.abb)

plot1_data$pollutant <- factor(
  plot1_data$pollutant,
  levels = c("co", "o3"),
  labels = c("CO (ppm)", "O3 (ppm)")
)

head(plot1_data)



plot1 <- ggplot(plot1_data,
                aes(x = month, y = mean, color = pollutant)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2) +
  facet_wrap(~pollutant, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("steelblue", "darkorange3")) +
  labs(
    title = "Monthly Average CO and O3 Concentrations",
    x = "Month",
    y = "Concentration",
    color = "Pollutant"
  ) +
  theme_minimal()

plot1



# Plot 2
plot2_data <- aq_all

# Add year and month
plot2_data$year <- format(plot2_data$date, "%Y")

plot2_data$month <- month.abb[
  as.integer(format(plot2_data$date, "%m"))
]

plot2_data$month <- factor(plot2_data$month, levels = month.abb)

# Calculate monthly averages
plot2_data <- aggregate(
  pm25 ~ year + month,
  data = plot2_data,
  FUN = mean,
  na.rm = TRUE
)

# Create plot
plot2 <- ggplot(plot2_data,
                aes(x = month, y = pm25,
                    color = year, group = year)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "2018" = "steelblue",
    "2019" = "darkorange3",
    "2020" = "darkgreen"
  )) +
  labs(
    title = "Monthly Average PM2.5 Concentrations by Year",
    x = "Month",
    y = "PM2.5 Concentration (ug/m3)",
    color = "Year"
  ) +
  theme_minimal()

plot2

