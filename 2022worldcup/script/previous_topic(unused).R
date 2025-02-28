# library -----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(dplyr)

# import data -------------------------------------------------------------
soccer <- read.csv('data/soccer.csv')
df_Rank <- read_excel('data/2018rank.xlsx')
df_Value <- read_excel('data/squad_market_value.xlsx')

df_Rank <- df_Rank %>%
  mutate(country = club)
df_Value <- df_Value %>%
  mutate(country = Club)

test1 <- soccer %>%
  filter(WorldCup == 'Men') %>%
  group_by(country) %>%
  summarise(club,
            n_club = n_distinct(club),
            n=n())
  select(country, club) %>%
  mutate(n_club = n_distinct(club))
left_join(soccer, df_Rank, by = 'club')

 
