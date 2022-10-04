library(glue)
library(tidyverse)
library(baseballr)
library(DBI)
library(RPostgreSQL)

# PURPOSE: Preprocess the data and save as single RDS

# Load the data -----------------------------------------------------------

#temp <- scrape_statcast_savant("2015-05-01", "2015-05-08")
#write.csv(temp, file = "data/statcast_data_batters_2015_p1")

# Load by year - Bill Petti function --------------------------------------

annual_statcast_query <- function(season) {
  
  dates <- seq.Date(as.Date(paste0(season, '-03-01')),
                    as.Date(paste0(season, '-12-01')), by = 'week')
  
  date_grid <- tibble(start_date = dates, 
                      end_date = dates + 6)
  
  safe_savant <- safely(scrape_statcast_savant)
  
  payload <- map(.x = seq_along(date_grid$start_date), 
                 ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                   
                   payload <- safe_savant(start_date = date_grid$start_date[.x], 
                                          end_date = date_grid$end_date[.x], type = 'pitcher')
                   
                   return(payload)
                 })
  
  payload_df <- map(payload, 'result')
  
  number_rows <- map_df(.x = seq_along(payload_df), 
                        ~{number_rows <- tibble(week = .x, 
                                                number_rows = length(payload_df[[.x]]$game_date))}) %>%
    filter(number_rows > 0) %>%
    pull(week)
  
  payload_df_reduced <- payload_df[number_rows]
  
  combined <- payload_df_reduced %>%
    bind_rows()
  
  return(combined)
}

year <- seq(2015, 2022, 1)

for (i in 1:8){
  temp <- annual_statcast_query(year[i])
  write_rds(temp, file = glue("V2_data/V2_statcast_data_", as.character(year[i])))
}

# Bind the year-by-year data into one dataframe ---------------------------

statcast_data_2015 <- read_rds("V2_data/V2_statcast_data_2015.rds")
statcast_data_2016 <- read_rds("V2_data/V2_statcast_data_2016.rds")
statcast_data_2017 <- read_rds("V2_data/V2_statcast_data_2017.rds")
statcast_data_2018 <- read_rds("V2_data/V2_statcast_data_2018.rds")
statcast_data_2019 <- read_rds("V2_data/V2_statcast_data_2019.rds")
statcast_data_2020 <- read_rds("V2_data/V2_statcast_data_2020.rds")
statcast_data_2021 <- read_rds("V2_data/V2_statcast_data_2021.rds")
statcast_data_2022 <- read_rds("V2_data/V2_statcast_data_2022.rds")

statcast_data_combined <- rbind(statcast_data_2015, statcast_data_2016,
                                statcast_data_2017, statcast_data_2018,
                                statcast_data_2019, statcast_data_2020,
                                statcast_data_2021, statcast_data_2022)

# Write as single RDS file ------------------------------------------------

write_rds(statcast_data_combined, glue("V2_data/V2_statcast_data_clean.rds"), compress = "gz")
