# PURPOSE: Create CSV for Batting/Pitching data sets


# Load the data -----------------------------------------------------------

library(baseballr)
library(tidyverse)


# Batting data  -----------------------------------------------------------

statcast_data_batters_2015 <- statcast_search_batters(start_date = "2015-04-05", 
                                                 end_date = "2015-11-01")

statcast_data_batters_2016 <- statcast_search_batters(start_date = "2016-04-03", 
                                                      end_date = "2016-11-02")

statcast_data_batters_2017 <- statcast_search_batters(start_date = "2017-04-02", 
                                                      end_date = "2017-11-01")

statcast_data_batters_2018 <- statcast_search_batters(start_date = "2018-03-29", 
                                                      end_date = "2018-10-28")

statcast_data_batters_2019 <- statcast_search_batters(start_date = "2019-03-20", 
                                                      end_date = "2019-10-30")

statcast_data_batters_2020 <- statcast_search_batters(start_date = "2020-07-23", 
                                                      end_date = "2020-10-27")

statcast_data_batters_2021 <- statcast_search_batters(start_date = "2021-04-01", 
                                                      end_date = "2021-11-02")

statcast_data_batters_2022 <- statcast_search_batters(start_date = "2022-04-07", 
                                                      end_date = Sys.Date())



# Pitching data -----------------------------------------------------------


statcast_data_pitchers_2015 <- statcast_search_pitchers(start_date = "2015-04-05", 
                                                      end_date = "2015-11-01")

statcast_data_pitchers_2016 <- statcast_search_pitchers(start_date = "2016-04-03", 
                                                      end_date = "2016-11-02")

statcast_data_pitchers_2017 <- statcast_search_pitchers(start_date = "2017-04-02", 
                                                      end_date = "2017-11-01")

statcast_data_pitchers_2018 <- statcast_search_pitchers(start_date = "2018-03-29", 
                                                      end_date = "2018-10-28")

statcast_data_pitchers_2019 <- statcast_search_pitchers(start_date = "2019-03-20", 
                                                      end_date = "2019-10-30")

statcast_data_pitchers_2020 <- statcast_search_pitchers(start_date = "2020-07-23", 
                                                      end_date = "2020-10-27")

statcast_data_pitchers_2021 <- statcast_search_pitchers(start_date = "2021-04-01", 
                                                      end_date = "2021-11-02")

statcast_data_pitchers_2022 <- statcast_search_pitchers(start_date = "2022-04-07", 
                                                      end_date = Sys.Date())


scrape_statcast_savant()

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
