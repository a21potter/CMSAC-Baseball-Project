# PURPOSE: Create CSV for Batting/Pitching data sets


# Load the data -----------------------------------------------------------

library(baseballr)
library(tidyverse)
library(DBI)
library(RPostgreSQL)


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

# Batting data  -----------------------------------------------------------


# Pitching data -----------------------------------------------------------


# Combine years into two dataframes ---------------------------------------

statcast_data_batters <- merge(statcast_data_batters_2015, statcast_data_batters_2016,
                               statcast_data_batters_2017, statcast_data_batters_2018,
                               statcast_data_batters_2019, statcast_data_batters_2020,
                               statcast_data_batters_2021, statcast_data_batters_2022)
  
# statcast_data_pitchers <-

# Data cleaning -----------------------------------------------------------

# Clean the batting data - Remove N/As and fix strikeout cols
statcast_data_batters <- statcast_data_batters[ , colSums(is.na(statcast_data_batters)) < nrow(statcast_data_batters)]

statcast_data_batters <- statcast_data_batters %>%
  mutate(pitch_outcome = case_when(events != "" ~ events,
                                   events == "" ~ description)) %>%
  mutate(pitch_outcome = case_when(events == "strikeout" & description == "swinging_strike" ~ "strikeout_swinging",
                                   events == "strikeout" & description == "called_strike" ~ "strikeout_called",
                                   TRUE ~ pitch_outcome),
         game_id = game_pk) %>%
  select(-game_pk)

# Clean the pitching data - Remove N/As and fix strikeout cols
statcast_data_pitchers <- statcast_data_pitchers[ , colSums(is.na(statcast_data_pitchers)) < nrow(statcast_data_pitchers)]

statcast_data_pitchers <- statcast_data_pitchers %>%
  mutate(pitch_outcome = case_when(events != "" ~ events,
                                   events == "" ~ description)) %>%
  mutate(pitch_outcome = case_when(events == "strikeout" & description == "swinging_strike" ~ "strikeout_swinging",
                                   events == "strikeout" & description == "called_strike" ~ "strikeout_called",
                                   TRUE ~ pitch_outcome),
         game_id = game_pk) %>%
  select(-game_pk)


# Create at-bat counter and merge with original dataframe -----------------

# Add at-bat column for batting data (1, 2, 3...)
statcast_data_batters_at_bats <- statcast_data_batters %>%
  count(game_id, batter, at_bat_number) %>%
  group_by(game_id, batter) %>%
  mutate(at_bat_of_game=1:n()) 

# Merge to original dataframe
statcast_data_batters <- statcast_data_batters %>%
  left_join(statcast_data_batters_at_bats, 
            by = c("game_id" = "game_id", "batter" = "batter", "at_bat_number" = "at_bat_number")) %>%
  select(-n)

# Add at-bat column for pitching data (1, 2, 3...)
statcast_data_pitchers_at_bats <- statcast_data_pitchers %>%
  count(game_id, batter, at_bat_number) %>%
  group_by(game_id, batter) %>%
  mutate(at_bat_of_game=1:n()) 

# Merge to original dataframe
statcast_data_pitchers <- statcast_data_pitchers %>%
  left_join(statcast_data_pitchers_at_bats, by = c("game_id" = "game_id", "batter" = "batter", "at_bat_number" = "at_bat_number"))  %>%
  select(-n)
