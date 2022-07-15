# PURPOSE: Preprocess the data and save as single RDS


# Bind the year-by-year data into one dataframe ---------------------------

statcast_data_2015 <- read_rds("data-nick/statcast_data_2015.rds")
statcast_data_2016 <- read_rds("data-nick/statcast_data_2016.rds")
statcast_data_2017 <- read_rds("data-nick/statcast_data_2017.rds")
statcast_data_2018 <- read_rds("data-nick/statcast_data_2018.rds")
statcast_data_2019 <- read_rds("data-nick/statcast_data_2019.rds")
statcast_data_2020 <- read_rds("data-nick/statcast_data_2020.rds")
statcast_data_2021 <- read_rds("data-nick/statcast_data_2021.rds")
statcast_data_2022 <- read_rds("data-nick/statcast_data_2022.rds")

statcast_data_combined <- rbind(statcast_data_2015, statcast_data_2016,
                                statcast_data_2017, statcast_data_2018,
                                statcast_data_2019, statcast_data_2020,
                                statcast_data_2021, statcast_data_2022)

# Data cleaning -----------------------------------------------------------

# Clean the batting data - Remove N/As and fix strikeout cols
statcast_data_combined <- statcast_data_combined[ , colSums(is.na(statcast_data_combined)) < nrow(statcast_data_combined)]

statcast_data_combined <- statcast_data_combined %>%
  mutate(pitch_outcome = case_when(events != "" ~ events,
                                   events == "" ~ description)) %>%
  mutate(pitch_outcome = case_when(events == "strikeout" & description == "swinging_strike" ~ "strikeout_swinging",
                                   events == "strikeout" & description == "called_strike" ~ "strikeout_called",
                                   TRUE ~ pitch_outcome),
         game_id = game_pk) %>%
  select(-game_pk)

# Clean the pitching data - Remove N/As and fix strikeout cols
# statcast_data_pitchers <- statcast_data_pitchers[ , colSums(is.na(statcast_data_pitchers)) < nrow(statcast_data_pitchers)]
# 
# statcast_data_pitchers <- statcast_data_pitchers %>%
#   mutate(pitch_outcome = case_when(events != "" ~ events,
#                                    events == "" ~ description)) %>%
#   mutate(pitch_outcome = case_when(events == "strikeout" & description == "swinging_strike" ~ "strikeout_swinging",
#                                    events == "strikeout" & description == "called_strike" ~ "strikeout_called",
#                                    TRUE ~ pitch_outcome),
#          game_id = game_pk) %>%
#   select(-game_pk)


# Create at-bat counter and merge with original dataframe -----------------

# Add at-bat column for batting data (1, 2, 3...)
statcast_data_combined_at_bats <- statcast_data_combined %>%
  count(game_id, batter, at_bat_number) %>%
  group_by(game_id, batter) %>%
  mutate(at_bat_of_game=1:n()) 

# Merge to original dataframe
statcast_data_combined <- statcast_data_combined %>%
  left_join(statcast_data_combined_at_bats, 
            by = c("game_id" = "game_id", "batter" = "batter", "at_bat_number" = "at_bat_number")) %>%
  select(-n)

# Add at-bat column for pitching data (1, 2, 3...)
# statcast_data_pitchers_at_bats <- statcast_data_pitchers %>%
#   count(game_id, batter, at_bat_number) %>%
#   group_by(game_id, batter) %>%
#   mutate(at_bat_of_game=1:n()) 

# Merge to original dataframe
# statcast_data_pitchers <- statcast_data_pitchers %>%
#   left_join(statcast_data_pitchers_at_bats, by = c("game_id" = "game_id", "batter" = "batter", "at_bat_number" = "at_bat_number"))  %>%
#   select(-n)


# Write as single RDS file ------------------------------------------------

write_rds(statcast_data_combined, glue("data-nick/statcast_data_clean.rds"), compress = "gz")
