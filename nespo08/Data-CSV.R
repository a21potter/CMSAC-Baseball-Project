# PURPOSE: Create CSV for Batting/Pitching data sets


# Load the data -----------------------------------------------------------

library(baseballr)
library(tidyverse)
library(DBI)
library(RPostgreSQL)
library(glue)

temp <- scrape_statcast_savant("2015-05-01", "2015-05-08")
write.csv(temp, file = "data/statcast_data_batters_2015_p1")

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
  write.csv(temp, file = glue("data/statcast_data_batters_", as.character(year[i])))
}