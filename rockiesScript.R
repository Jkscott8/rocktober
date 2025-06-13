library(baseballr)
library(dplyr)
library(lubridate)

start_date <- as.Date("2023-04-01")
end_date <- Sys.Date()

date_seq <- seq.Date(start_date, end_date, by = "day")

get_daily_data_safe <- function(date, venue_id = 19, retries = 3, sleep_time = 5) {
  attempt <- 1
  while (attempt <= retries) {
    message(paste("Attempt", attempt, "- Getting data for:", date))
    result <- tryCatch({
      statcast_search(start_date = date, end_date = date, venue = venue_id)
    }, error = function(e) {
      message(paste("Attempt", attempt, "error on:", date, "-", e$message))
      NULL
    })
    
    if (!is.null(result)) {
      Sys.sleep(sleep_time)
      return(result)
    } else {
      attempt <- attempt + 1
      Sys.sleep(sleep_time)
    }
  }
  message(paste("Failed to retrieve data for date:", date, "after", retries, "attempts"))
  return(NULL)
}

# Incrementally save data with correct date handling
for (current_date in as.character(date_seq)) {
  
  date_parsed <- as.Date(current_date)
  daily_data <- get_daily_data_safe(date_parsed)
  
  if (!is.null(daily_data) && nrow(daily_data) > 0) {
    file_name <- "coors_statcast_daily_2016_present.csv"
    
    if (!file.exists(file_name)) {
      write.csv(daily_data, file_name, row.names = FALSE)
    } else {
      write.table(daily_data, file_name, row.names = FALSE, col.names = FALSE, sep = ",", append = TRUE)
    }
  }
}


library(dplyr)
library(lubridate)
library(data.table)  # faster handling for large files

# Read your large dataset
data <- fread("coors_statcast_daily_2016_present.csv")

# Convert game_date to Date object if not already
data$game_date <- as.Date(data$game_date)

# Splitting further by month example:
data$year_month <- format(data$game_date, "%Y_%m")

months <- unique(data$year_month)

for (m in months) {
  message(paste("Saving data for month:", m))
  monthly_data <- data %>% filter(year_month == m)
  fwrite(monthly_data, paste0("~/Desktop/Baseball_Predictions/Rockies/games/coors_statcast_", m, ".csv"))
}





library(data.table)
library(dplyr)

# Load your existing large data file
data <- fread("coors_statcast_daily_2016_present.csv")

# Check home teams to confirm column correctness
unique(data$home_team)

# Filter for Rockies home games only (Coors Field)
coors_data <- data %>% filter(home_team == "COL")

# Save filtered data
fwrite(coors_data, "coors_only_statcast_2016_present.csv")







temp = statcast_search_pitchers(start_date = '2025-03-18', end_date = '2025-03-18')
view(temp)


temp = read_csv("~/Desktop/Baseball Modeling/kPredictor/all_seasons.csv")


temp = bref_daily_pitcher("2022-05-10", "2022-06-20")





