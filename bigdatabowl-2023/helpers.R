library(tidyverse)
library(tidymodels)

read_tracking_data <- function() {
  weeks <- seq(1, 8)
  
  #blank dataframe to store tracking data
  df_tracking <- data.frame()
  
  #iterating through all weeks
  for (w in weeks) {
    #temporary dataframe used for reading week for given iteration
    df_tracking_temp <-
      read_csv(paste0("data/weeks/week", w, ".csv"),
               col_types = cols())
    
    #storing temporary dataframe in full season dataframe
    df_tracking <-
      bind_rows(df_tracking_temp, df_tracking)
    
  }
  
  df_tracking
}

# a helper function to standardize the tracking data for field position where offense is always going left to right
process_tracking_data <- function(df) {
  df <- df %>%
    mutate(
      x = ifelse(playDirection == "left", 120 - x, x),
      y = ifelse(playDirection == "left", 160 / 3 - y, y),
      dir = ifelse(playDirection == "left", dir + 180, dir),
      dir = ifelse(dir > 360, dir - 360, dir),
      o = ifelse(playDirection == "left", o + 180, o),
      o = ifelse(o > 360, o - 360, o)
    )
  
  df
}

# a helper function to remove penalties
process_plays_data <- function(df) {
  #create a is penalty column
  df <- df %>%
    mutate(is_penalty = ifelse(is.na(foulName1), 0, 1))
  
  df <- df %>%
    filter(is_penalty == 0)
  
  df
}