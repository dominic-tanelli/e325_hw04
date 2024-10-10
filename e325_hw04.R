# Dominic Tanelli
# Prof. Kropp
# ENVST 325
# 10 October 2024

# Homework 4: An introduction to data cleaning
# Starter Code
# install.packages(c("dplyr","ggplot2","lubridate"))
library(dplyr)
library(ggplot2)
library(lubridate)
weather <- read.csv("/cloud/project/campus_weather.csv", na.strings = "#N/A")
metaDat <- read.csv("/cloud/project/meter_weather_metadata.csv", 
                    na.strings = "#N/A")

# In-class prompts
# Prompt 1
weather$Date <- as.POSIXct(weather$Date, format="%m/%d/%y %H:%M")
january_2022 <- weather %>%
  filter(Date >= as.POSIXct("2022-01-01") & Date < 
           as.POSIXct("2022-02-01")) %>%
  select(Date, AirTemp)
january_2022$AirTemp_RollingAvg <- NA
window_size <- 8
for (i in window_size:nrow(january_2022)) {
  january_2022$AirTemp_RollingAvg[i] <- mean(january_2022$AirTemp[(
    i - window_size + 1):i], na.rm = TRUE)
}
cat("\nPrompt 1 Answer (Rolling Average): \n")
print(january_2022)
cat("\nPrompt 1 Answer: \n")
ggplot(january_2022, aes(x = Date)) + geom_line(aes(
  y = AirTemp, color = "Air Temperature"), size = 1) + geom_line(aes(
    y = AirTemp_RollingAvg, color = "Rolling Average"), size = 1) + labs(
      title = "15-Minute Air Temperature and Rolling Average for January 2022",
       x = "Date", y = "Temperature (°C)", color = "Legend") + theme_minimal()

# Prompt 2
may_and_june_2021 <- weather %>%
  filter(Date >= as.POSIXct("2021-05-01") & Date < 
           as.POSIXct("2021-07-01")) %>%
  select(Date, SolRad)
cat("\nPrompt 2 Answer: \n")
ggplot(may_and_june_2021, aes(x = Date, y = SolRad)) + 
  geom_line(color = "blue") + labs(
    title = "Solar Radiation in May and June 2021", x = "Date", 
    y = "Solar Radiation (W/m²)") + theme_minimal()

# Prompt 3
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
  
}
cat("\nPrompt 3 Answer: \n")
timeCheck900(weather$Date)

# Homework
# Question 1
clinton <- weather %>%
  filter(AirTemp >= 0) %>%
  filter(abs(XLevel - YLevel) <= 2)
cat("\nQuestion 1 Answer (Weather): \n")
print(sum(is.na(weather$Precip)))
cat("\nQuestion 1 Answer (Clinton): \n")
print(sum(is.na(clinton$Precip)))

# Question 2
cat("\nQuestion 2 Answer: \n")
clinton$BatVoltFlag <- ifelse(clinton$BatVolt < 8.5, "Low", "Normal")
print(clinton$BatVoltFlag)

# Question 3
check_unrealistic_values <- function(df) {
  min_temp <- -50
  max_temp <- 50
  max_solar_radiation <- 1200
  min_solar_radiation <- 0
  df$unrealistic_air_temp <- ifelse(df$AirTemp < min_temp | df$AirTemp > max_temp, TRUE, FALSE)
  df$unrealistic_solar_radiation <- ifelse(df$SolRad < min_solar_radiation | df$SolRad > max_solar_radiation, TRUE, FALSE)
  return(df)
}
cat("\nQuestion 3 Answer (Weather): \n")
print(check_unrealistic_values(weather)) # No true statements for weather
cat("\nQuestion 3 Answer (Clinton): \n")
print(check_unrealistic_values(clinton)) # No true statements for clinton

# Question 4
jan_and_mar_2021 <- weather %>%
  filter(Date >= as.POSIXct("2021-01-01") & Date < 
           as.POSIXct("2021-04-01")) %>%
  select(Date, AirTemp)
cat("\nQuestion 4 Answer: \n")
ggplot(jan_and_mar_2021, aes(x = Date, y = AirTemp)) + 
  geom_line(color = "blue") + labs(
    title = "Winter Air Temperature in January and March 2021", x = "Month", 
    y = expression("Air Temperature (" * degree * "C)")) + theme_minimal()

# Question 5
mar_and_apr_2021 <- weather %>%
  filter(Date >= as.POSIXct("2021-03-01") & Date < as.POSIXct("2021-05-01")) %>%
  select(Date, Precip, AirTemp)
mar_and_apr_2021 <- mar_and_apr_2021 %>%
  mutate(AirTemp_F = (AirTemp * 9/5) + 32)
for (i in 1:nrow(mar_and_apr_2021)) {
  if (mar_and_apr_2021$AirTemp_F[i] < 35 || 
      (i > 1 && mar_and_apr_2021$AirTemp_F[i - 1] < 35)) {
    mar_and_apr_2021$Precip[i] <- NA
  }
}
cat("\nQuestion 5 Answer: \n")
print(sum(!is.na(mar_and_apr_2021$Precip)))

# Question 6
cat("\nQuestion 6 Answer: See Word PDF\n")
