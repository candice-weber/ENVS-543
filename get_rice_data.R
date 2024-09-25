

# A function to get data from the example Rice Rivers Center used in class.
# This function creates a date object, pulls out month/day/weekday, converts 
# inches to centimeters and Celsius to Fahrenheit, drops all columns except 
# those with basic atmospheric and water conditions, and reorder the columns.


get_rice_data <- function() { 
  library(tidyverse)
  library(lubridate)
  
  # Navigating to the data
  url <- "https://docs.google.com/spreadsheets/d/1Mk1YGH9LqjF7drJE-td1G_JkdADOU0eMlrP01WFBT8s/pub?gid=0&single=true&output=csv"
  
  # Reading in the data
  rice <- read_csv(url)
  
  # Formatting the data
  rice %>% 
    # Making date object
    mutate(Date = mdy_hms(DateTime, tz = "EST")) %>% 
    
    # Converting rain inches to centimeters
    mutate(Rain_cm = Rain_in * 2.54) %>% 
    
    # Converting H2O temp C to F
    mutate(H2O_TempF = 1.8 * H2O_TempC + 32) %>% 
    
    # Adding Weekday and Month columns 
    mutate(Month = month(Date, label = TRUE,
                         abbr = FALSE),
           Day = mday(Date),
           Weekday = wday(Date, label = TRUE,
                          abbr = FALSE,
                          week_start = 7)) %>% 
    
    # Removing extraneous columns
    select(-DateTime, -PAR, -Rain_in, -H2O_TempC, -SpCond_mScm, 
           -PH_mv, -Chla_ugl, -BGAPC_CML, -BGAPC_rfu, -ODO_mgl, -Depth_ft,
           -SurfaceWaterElev_m_levelNad83m) %>% 
    
    # Reordering columns
    select(RecordID, Date, Month, Day, Weekday, AirTempF, H2O_TempF, Rain_cm, 
           everything()) -> df
  
  return(df)
  
}
