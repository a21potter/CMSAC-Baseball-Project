# PURPOSE: Explore data


# Load libraries and dataset ----------------------------------------------

library(baseballr)
library(tidyverse)
statcast_data_all <- read_rds("data-nick/statcast_data_clean.rds")


# TEMP: Filter for 2020-22 ------------------------------------------------

statcast_data_2020_to_22 <- statcast_data_all %>%
  filter(game_date >= "2020-04-01")

# Counts and percentages of variables by shift type and pitcher/batter matchups
statcast_data_2020_to_22 %>%
  filter(!if_fielding_alignment == "", !of_fielding_alignment == "") %>%
  #select(stand, p_throws, if_fielding_alignment, of_fielding_alignment) %>%
  count(stand, p_throws, if_fielding_alignment, of_fielding_alignment) %>%
  mutate(prob = n/sum(n))

# Counts and percentages of variables by combination of shift type and pitcher/batter matchups
statcast_data_2020_to_22 %>%
  filter(!if_fielding_alignment == "", !of_fielding_alignment == "") %>%
  mutate(if_fielding_alignment = case_when(if_fielding_alignment == "Infield shift" ~ "Shift",
                                           if_fielding_alignment == "Strategic" ~ "Shift",
                                           if_fielding_alignment == "Standard" ~ "Standard")) %>%
  mutate(of_fielding_alignment = case_when(of_fielding_alignment == "4th outfielder" ~ "Shift",
                                           of_fielding_alignment == "Strategic" ~ "Shift",
                                           of_fielding_alignment == "Standard" ~ "Standard")) %>%
  #select(stand, p_throws, if_fielding_alignment, of_fielding_alignment) %>%
  count(stand, p_throws, if_fielding_alignment, of_fielding_alignment) %>%
  mutate(prob = n/sum(n))

# Table by pitcher/batter matchups (regardless of shift type)
statcast_data_2020_to_22 %>%
  #select(stand, p_throws, if_fielding_alignment, of_fielding_alignment) %>%
  count(stand, p_throws) %>%
  mutate(prob = n/sum(n))

# Prelim. spray charts
statcast_data_2020_to_22 %>%
  filter(stand == "R", p_throws == "L") %>%
  ggspraychart("hc_x", "-hc_y", bin_size = 100, point_alpha = 0.25, frame = TRUE) +
  facet_wrap(~game_year)
