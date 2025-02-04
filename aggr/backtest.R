# Load necessary libraries
library(tidyverse)
library(zoo)

rm(list = ls()) # Clean environment

dirpath = dirname(rstudioapi::getSourceEditorContext()$path) # Get script location
setwd(dirpath) # Set working directory

# ---- 1️⃣ Load Data ----
load_data <- function(lst_file, sol_file) {
  df.lst <- read_csv(lst_file) %>%
    select(time, close) %>%
    rename(close_lst = close) %>%
    mutate(time = as.POSIXct(time, origin = "1970-01-01", tz = "UTC"))
  
  df.sol <- read_csv(sol_file) %>%
    select(time, close) %>%
    rename(close_sol = close) %>%
    mutate(time = as.POSIXct(time, origin = "1970-01-01", tz = "UTC"))
  
  return(inner_join(df.lst, df.sol, by = "time") %>%
           mutate(lst_premium = (close_lst / close_sol - 1) * 100))  # Calculate % premium
}

# ---- 2️⃣ Compute Rolling Average (Excluding Outliers) ----
compute_rolling_avg <- function(df, rolling_window = 50, conf_threshold = 2) {
  premium_mean <- mean(df$lst_premium, na.rm = TRUE)
  premium_sd <- sd(df$lst_premium, na.rm = TRUE)
  
  # Filter out extreme outliers (beyond conf_threshold standard deviations)
  df.filtered <- df %>%
    filter(abs(lst_premium - premium_mean) <= conf_threshold * premium_sd)
  
  # Compute rolling average only on filtered data
  df.filtered <- df.filtered %>%
    mutate(rolling_avg_premium = rollapply(lst_premium, width = rolling_window, FUN = mean, fill = NA, align = "right"))
  
  # Merge back with original data
  df <- left_join(df, df.filtered %>% select(time, rolling_avg_premium), by = "time")
  
  return(df)
}

# ---- 3️⃣ Run Analysis for Multiple LSTs ----
lst_files <- c("js.csv")  # List of LST files
sol_file <- "ps.csv"  # Reference SOL file

for (lst_file in lst_files) {
  # Extract LST name from filename
  lst_name <- tools::file_path_sans_ext(lst_file)
  
  # Load and process data
  df.merged <- load_data(lst_file, sol_file)
  df.merged <- compute_rolling_avg(df.merged, rolling_window = 50, conf_threshold = 2)
  
  # ---- 4️⃣ Plot Each LST Premium Over SOL ----
  plot <- ggplot(df.merged, aes(x = time)) +
    geom_line(aes(y = lst_premium), color = "blue", alpha = 0.5) +
    geom_line(aes(y = rolling_avg_premium), color = "black", size = 1.2) +
    labs(title = paste(lst_name, "Premium Over SOL (Raw & Rolling Average)"),
         y = "LST Premium (%)",
         x = "Time") +
    theme_minimal()
  
  # Display plot
  print(plot)
}
