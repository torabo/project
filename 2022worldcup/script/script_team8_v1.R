# library -----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(showtext)
font_add_google("Poor Story", "poorstory")
# showtext_auto()

# Import data -------------------------------------------------------------
soccer <- read.csv('data/soccer.csv')

# Missing data ------------------------------------------------------------
table(is.na(soccer))
sum(is.na(soccer))
which(is.na(soccer))
colSums(is.na(soccer))

df_NA1 <- soccer[rowSums(is.na(soccer)) > 0,]

# Replace NA data ---------------------------------------------------------
# import external data
df_4NA <- read_excel('data/Womens Squads.xlsx')

df_4NA <- df_4NA %>%
  select(player, Caps, Goals) %>%
  rename(caps = Caps,
         goals = Goals)

soccer <- soccer %>% 
  left_join(df_4NA, by = "player") %>% 
  mutate(caps = coalesce(caps.x, caps.y),
         goals = coalesce(goals.x, goals.y)) %>% 
  select(-caps.x, -caps.y, -goals.x, -goals.y)

colSums(is.na(soccer))

df_NA2 <- soccer[rowSums(is.na(soccer)) > 0,]

# fill the remaining NA data with the mean by position
# for Thailand
df_mean_Thai <- soccer[rowSums(is.na(soccer)) == 0,] %>%
  filter(country == 'Thailand') %>%
  group_by(pos) %>%
  summarise(caps = round(mean(caps), digits = 0),
            goals = round(mean(goals), digits = 0))

df_Thai <- soccer %>%
  filter(country == 'Thailand',
         rowSums(is.na(soccer)) > 0)

df_Thai <- df_Thai %>%
  left_join(df_mean_Thai, by = "pos") %>%
  mutate(caps = coalesce(caps.x, caps.y),
         goals = coalesce(goals.x, goals.y)) %>% 
  select(-caps.x, -caps.y, -goals.x, -goals.y)

# for Nigeria
df_mean_Nig <- soccer[rowSums(is.na(soccer)) == 0,] %>%
  filter(country == 'Nigeria') %>%
  group_by(pos) %>%
  summarise(caps = round(mean(caps), digits = 0),
            goals = round(mean(goals), digits = 0))

df_Nig <- soccer %>%
  filter(country == 'Nigeria',
         rowSums(is.na(soccer)) > 0)

df_Nig <- df_Nig %>%
  left_join(df_mean_Nig, by = "pos") %>%
  mutate(caps = coalesce(caps.x, caps.y),
         goals = coalesce(goals.x, goals.y)) %>% 
  select(-caps.x, -caps.y, -goals.x, -goals.y)

# for Cameroon
df_mean_CMR <- soccer[rowSums(is.na(soccer)) == 0,] %>%
  filter(country == 'Cameroon') %>%
  group_by(pos) %>%
  summarise(caps = round(mean(caps), digits = 0),
            goals = round(mean(goals), digits = 0))

df_CMR <- soccer %>%
  filter(country == 'Cameroon',
         rowSums(is.na(soccer)) > 0)

df_CMR <- df_CMR %>%
  left_join(df_mean_CMR, by = "pos") %>%
  mutate(caps = coalesce(caps.x, caps.y),
         goals = coalesce(goals.x, goals.y)) %>% 
  select(-caps.x, -caps.y, -goals.x, -goals.y)

# merge data
df_not_NAcountry <- soccer[rowSums(is.na(soccer)) == 0,]

N_soccer <- rbind(df_not_NAcountry, df_Thai, df_CMR, df_Nig)

df_NA3 <- N_soccer[rowSums(is.na(N_soccer)) > 0,]

# remove junk for memory
rm(list=setdiff(ls(), "N_soccer"))

# Add rank data -----------------------------------------------------
soccer <- N_soccer %>%
  rename(sex = WorldCup)

# Men Rank
df_Rank1 <- read_excel('data/rank_2018.xlsx')
df_Rank1 <- df_Rank1 %>%
  rename(country = club)
M_soccer <- soccer %>%
  filter(sex == 'Men') %>%
  left_join(df_Rank1, by = 'country')

# Women Rank
df_Rank2 <- read_excel('data/rank_2019.xlsx')
df_Rank2 <- df_Rank2 %>%
  rename(country = club)
W_soccer <- soccer %>%
  filter(sex == 'Women') %>%
  left_join(df_Rank2, by = 'country')

# merge rank data
soccer <- rbind(M_soccer, W_soccer)

df_NA <- N_soccer[rowSums(is.na(soccer)) > 0,]

# clean memory
rm(list=setdiff(ls(), "soccer"))


# Divide data by sex ------------------------------------------------------
M_soccer <- soccer %>%
  filter(sex == 'Men')
W_soccer <- soccer %>%
  filter(sex == 'Women')
# check na
colSums(is.na(M_soccer))
colSums(is.na(W_soccer))

# Add team value data(only men) -------------------------------------------
M_team_value <- read_xlsx('data/squad_market_value.xlsx')

# Add new variable for team value without characters
M_team_value$Mean_team_value <- gsub("[^0-9.<>]","",M_team_value$Mean_Market_Value)
# Match units by killo
M_team_value$Mean_team_value <- dplyr::case_when(
  stringr::str_detect(M_team_value$Mean_Market_Value, 'm') ~ readr::parse_number(M_team_value$Mean_team_value) * 1e3,
  TRUE ~ parse_number(M_team_value$Mean_team_value))
M_soccer <- M_soccer %>%
  left_join(M_team_value %>%
              select(country = Club, Mean_team_value), by = 'country')

# Knowing about data ------------------------------------------------------------
# the number of country
n_distinct(soccer$country)   # Whole country : 45
n_distinct(M_soccer$country) # for men : 32
n_distinct(W_soccer$country) # for women : 24

# Summary
summary(M_soccer)
summary(W_soccer)
# about caps (Min, 1st Qu, Median, 3rd Qu, Max) (Mean)
### Men (0, 10, 25, 52, 158) (35.37)
### Women (0, 12, 28.50, 63, 282) (43.95)

# about goals (Min, 1st Qu, Median, 3rd Qu, Max) (Mean)
### Men (0, 0, 1, 5, 81) (4.52)
### Women (0, 0, 1, 8, 181) (7.31)
distinct(M_soccer, goals)
distinct(W_soccer, goals)

# about age (Min, 1st Qu, Median, 3rd Qu, Max) (Mean)
### Men (19, 25, 27, 30, 45) (27.42)
### Women (16, 23, 26, 29, 41) (26.03)

# Men+++++++++++++++++++++++++++++++++++++++++++++++++++++ ---------------------------------------------------------------------

# Topic 1 ---------------------------------------------------------
# How many veteran we need?

# Topic 1; What is veteran? -------------------------------------------------
# Define values
quantile(M_soccer$caps)
M_caps_25 = quantile(M_soccer$caps,0.25)

# Veteran
M_soccer <- M_soccer %>%
  mutate(level_1 = case_when(caps >= 100 ~ 'Veteran',
                             caps >= M_caps_25 ~ 'Intermediate',
                             TRUE ~ 'Novice'))

# the number of veterans
M_soccer %>%               # 40/522/174
  group_by(level_1) %>%
  summarise(n = n())

# the number of each level per country
t1_1M <- M_soccer %>%
  group_by(country, level_1) %>%
  summarise(n = n())

# the number of veterans per country
n_Vet_M <- M_soccer %>%
  filter(level_1 == 'Veteran') %>%
  group_by(country) %>%
  summarise(n_Vet = n())
n_Int_M <- M_soccer %>%
  filter(level_1 == 'Intermediate') %>%
  group_by(country) %>%
  summarise(n_Int = n())
n_Nov_M <- M_soccer %>%
  filter(level_1 == 'Novice') %>%
  group_by(country) %>%
  summarise(n_Nov = n())

# Add the number of Veteran without NA
M_soccer <- M_soccer %>%
  left_join(n_Vet_M, by = 'country') %>%
  left_join(n_Int_M, by = 'country') %>%
  left_join(n_Nov_M, by = 'country') %>%
  mutate_all( ~ replace(., is.na(.), 0))

# remove junk for memory
rm(list= ls()[!(ls() %in% c("soccer", "M_soccer", "W_soccer"))])

# Topic 1; Analysis -------------------------------------------------------
M_soccer %>%
  group_by(rank, country, n_Vet) %>%
  summarise(n = n())

M_soccer %>%
  group_by(rank, country, n_Vet) %>%
  summarise(group, n = n()) %>%
  ggplot(aes(x = country, y = n_Vet)) +
  geom_point() +
  facet_wrap(~ group, ncol = 3) +
  coord_flip()

ggsave(path = "figure",
       filename = "M_Vet_country_group.png",
       width = 4, height = 6)

# Topic 2 -----------------------------------------------------------------
# How many Rising star we need?

# Topic 2; What is Rising Star? -------------------------------------------------
# New variable(Goals per cap) without na
M_soccer <- M_soccer %>%
  mutate(GpC = goals/caps,
         GpC = coalesce(GpC, 0))
M_soccer <- M_soccer %>%
  group_by(country) %>%
  mutate(sum_GpC = sum(GpC))

M_FW <- M_soccer %>%
  filter(pos == 'FW')
quantile(M_FW$GpC)
M_FW_20 = quantile(M_FW$GpC, 0.80)

# Rising star
M_soccer <- M_soccer %>%
  mutate(level_2 = case_when(age_yrs <= 23 & goals >= M_FW_20  ~ 'Rising star',
                             TRUE ~ 'Not star'))

# the number of Rising Stars
M_soccer %>%               # 54/732
  group_by(level_2) %>%
  summarise(n = n())

# the number of each level per country
t2_1M <- M_soccer %>%
  group_by(country, level_2) %>%
  summarise(n = n())

# the number of Rising stars per country
n_RS_M <- M_soccer %>%
  filter(level_2 == 'Rising star') %>%
  group_by(country) %>%
  summarise(n_RS = n())

# Add the number of Rising Star without NA
M_soccer <- M_soccer %>%
  left_join(n_RS_M, by = 'country') %>%
  mutate_all( ~ replace(., is.na(.), 0))

# remove junk for memory
rm(list= ls()[!(ls() %in% c("soccer", "M_soccer", "W_soccer"))])

# Topic 2; Analysis -------------------------------------------------------
M_soccer %>%
  group_by(rank, country, n_RS) %>%
  summarise(n = n())

M_soccer %>%
  group_by(rank, country, n_RS) %>%
  summarise(group, n = n()) %>%
  ggplot(aes(x = country, y = n_RS)) +
  geom_point() +
  coord_flip() 

ggsave(path = "figure",
       filename = "M_RS_country_group.png",
       width = 6, height = 4)

# Topic 3 -----------------------------------------------------------------
# n_club, max_same_club
t3_1M <- M_soccer %>%
  group_by(country, club) %>%
  summarise(n = n()) %>%
  group_by(country) %>%
  summarise(n_club = n_distinct(club),
            max_same_club = max(n))

M_soccer <- M_soccer %>%
  left_join(t3_1M, by = 'country')

# Topic 3; Analysis -------------------------------------------------------
M_soccer %>%
  group_by(rank, country) %>%
  arrange(rank) %>%
  ggplot(aes(x = rank, y = Mean_team_value)) +
  geom_point()


p1 <- M_soccer %>%
  ggplot(aes(x = rank, y = Mean_team_value)) +
  geom_point(alpha = 0.1, size = 3) +
  stat_smooth(method = "lm") +
  theme(text=element_text(family = "poorstory")) +
  labs(title = '팀가치와 월드컵 최종순위의 관계', x = '최종순위', y = '팀가치(*e3)(EUR)')

p2 <- M_soccer %>%
  ggplot(aes(x = Mean_team_value, y = sum_GpC, colour = 'rank')) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ group)
  
M_soccer %>%
  ggplot(aes(x = n_RS, y = sum_GpC)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ group)




# Women+++++++++++++++++++++++++++++++++++++++++++++++++++ -------------------------------------------------------------------

# Topic 1; Define veteran -------------------------------------------------
# Define values
quantile(W_soccer$caps)
W_caps_25 = quantile(W_soccer$caps,0.25)


W_soccer <- W_soccer %>%
  mutate(level_1 = case_when(caps >= 100 ~ 'Veteran',
                           caps >= W_caps_25 ~ 'Intermediate',
                           TRUE ~ 'Novice'))

# the number of veterans
W_soccer %>%               # 68/353/131
  group_by(level_1) %>%
  summarise(n = n())

# the number of each level per country
t1_1W <- W_soccer %>%
  group_by(rank, group, country, level_1) %>%
  summarise(n = n())

# the number of veterans per country
n_Vet_W <- W_soccer %>%
  filter(level_1 == 'Veteran') %>%
  group_by(country) %>%
  summarise(n_Vet = n())

# Add the number of Veteran without NA
W_soccer <- W_soccer %>%
  left_join(n_Vet_W, by = 'country') %>%
  mutate_all( ~ replace(., is.na(.), 0))

# remove junk for memory
rm(list= ls()[!(ls() %in% c("soccer", "M_soccer", "W_soccer"))])

# Topic 1; Analysis ----------------------------------------------------------------
W_soccer %>%
  group_by(rank, country, n_Vet) %>%
  summarise(n = n())

W_soccer %>%
  group_by(rank, country, n_Vet) %>%
  summarise(group, n = n()) %>%
  ggplot(aes(x = country, y = n_Vet)) +
  geom_point() +
  facet_wrap(~ group, ncol = 3) +
  coord_flip()

ggsave(path = "figure",
       filename = "W_Vet_country_group.png",
       width = 4, height = 6)

# Topic 2 -----------------------------------------------------------------
# How many Rising star we need?

# Topic 2; Define Rising star -------------------------------------------------
W_soccer <- W_soccer %>%
  mutate(level_2 = case_when(age_yrs <= 25 & goals >= 20 ~ 'Rising star',
                           TRUE ~ 'Not star'))
# the number of Rising Stars
W_soccer %>%               # 8/544
  group_by(level_2) %>%
  summarise(n = n())

# the number of each level per country
t2_1W <- W_soccer %>%
  group_by(rank, group, country, level_2) %>%
  summarise(n = n())

# the number of Rising stars per country
n_RS_W <- W_soccer %>%
  filter(level_2 == 'Rising star') %>%
  group_by(country) %>%
  summarise(n_RS = n())

# Add the number of Rising Star without NA
W_soccer <- W_soccer %>%
  left_join(n_RS_W, by = 'country') %>%
  mutate_all( ~ replace(., is.na(.), 0))

# remove junk for memory
rm(list= ls()[!(ls() %in% c("soccer", "M_soccer", "W_soccer"))])

# Topic 2; Analysis --------------------------------------------------------
W_soccer %>%
  group_by(rank, country, n_RS) %>%
  summarise(n = n())

W_soccer %>%
  group_by(rank, country, n_RS) %>%
  summarise(group, n = n()) %>%
  ggplot(aes(x = country, y = n_RS)) +
  geom_point() +
  facet_wrap(~ group, ncol = 3)

ggsave(path = "figure",
       filename = "W_RS_country_group.png",
       width = 6, height = 4)

# Topic 3 -----------------------------------------------------------------
# 상대팀에 같은 클럽 선수가 있을 때
# 성씨로 뭉쳐 있을 때때

# extra -------------------------------------------------------------------
# # export dataframe
# rank_2019 <- W_soccer %>%
#   group_by(country) %>%
#   summarise(n = n())

# library('readr')
# write.csv(soccer, "data/New_soccer.csv")
# write.csv(M_soccer, "data/New_M_soccer.csv")
# write.csv(W_soccer, "data/New_W_soccer.csv")
# 
# # export dataset
# install.packages('xlsx')
# library(xlsx)
# write.xlsx(n_Veteran, "n_Veteran.xlsx")
# 