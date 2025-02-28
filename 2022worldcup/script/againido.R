# library -----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)

# import data -------------------------------------------------------------
soccer <- read.csv('data/soccer.csv')

dim(soccer)

soccer %>% filter(WorldCup=='Men') %>% summarise( NA_ = sum(is.na(goals)))
  # Missing data ------------------------------------------------------------
table(is.na(soccer))
sum(is.na(soccer))
which(is.na(soccer))
colSums(is.na(soccer))

thai <- soccer %>% filter(country == 'Thailand') 

df_4NA %>% filter(Country == 'Thailand') %>% View()


df_NA1 <- soccer[rowSums(is.na(soccer)) > 0,]

# Replace NA data ---------------------------------------------------------
# import external data
df_4NA <- read_excel('data/Womens Squads.xlsx')

dim(df_4NA)

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

colSums(is.na(N_soccer))

# remove junk -------------------------------------------------------------
rm(list=setdiff(ls(), "N_soccer"))

# Analysis Topics ---------------------------------------------------------
# How many veteran we need?

# Check & Modify data -----------------------------------------------------
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

# merge data
soccer <- rbind(M_soccer, W_soccer)

df_NA <- N_soccer[rowSums(is.na(soccer)) > 0,]

# remove junk
rm(list=setdiff(ls(), "soccer"))

# divide by sex
M_soccer <- soccer %>%
  filter(sex == 'Men')
W_soccer <- soccer %>%
  filter(sex == 'Women')

colSums(is.na(M_soccer))
colSums(is.na(W_soccer))

# the number of country
n_distinct(soccer$country)   # 45
n_distinct(M_soccer$country) # 32
n_distinct(W_soccer$country) # 24

# Define veteran ----------------------------------------------------------
# about caps (Min, 1st Qu, Median, 3rd Qu, Max)
summary(M_soccer) # (0, 10, 25, 52, 158)
summary(W_soccer) # (0, 12, 28.50, 63, 282)
# M_caps_Min = 0;      W_caps_Min = 0


quantile(M_soccer$caps)
quantile(M_soccer$caps,0.3)
quantile(W_soccer$caps)

M_caps_90 = quantile(M_soccer$caps,0.9)
W_caps_90 = quantile(W_soccer$caps,0.9)

M_caps_30 = quantile(M_soccer$caps,0.3)
W_caps_30 = quantile(W_soccer$caps,0.3)
# M_caps_2Q = 25;      W_caps_2Q = 28.50
# M_caps_3Q = 52;      W_caps_3Q = 63
# M_caps_Max = 158;    W_caps_Max = 282
# M_caps_Mean = 35.37; M_caps_Mean = 43.95

M_soccer <- M_soccer %>%
  mutate(level = case_when(caps >= 100  ~ 'Veteran',
                           caps >= M_caps_30 ~ 'Intermediate',
                           TRUE ~ 'Novice'))
W_soccer <- W_soccer %>%
  mutate(level = case_when(caps >= 100 ~ 'Veteran',
                           caps >= W_caps_30 ~ 'Intermediate',
                           TRUE ~ 'Novice'))
# the number of veterans
M_soccer %>%               # 40/522/174
  group_by(level) %>%
  summarise(n = n())
W_soccer %>%               # 68/353/131
  group_by(level) %>%
  summarise(n = n())

# the number of each level per country
t_1 <- M_soccer %>%
  group_by(country, level) %>%
  summarise(n = n())

t_2 <- W_soccer %>%
  group_by(rank, group, country, level) %>%
  summarise(n = n())

# the number of veterans per country
n_Vet_M <- M_soccer %>%
  filter(level == 'Veteran') %>%
  group_by(country) %>%
  summarise(n_Vet = n())
n_Vet_W <- W_soccer %>%
  filter(level == 'Veteran') %>%
  group_by(country) %>%
  summarise(n_Vet = n())

#the number of novice
n_Novi_M <- M_soccer %>%
  filter(level == 'Novice') %>%
  group_by(country) %>%
  summarise(n_Novice = n())

n_Novi_W <- W_soccer %>%
  filter(level == 'Novice') %>%
  group_by(country) %>%
  summarise(n_Novice = n())

#the number of inter
n_inter_M <- M_soccer %>%
  filter(level == 'Intermediate') %>%
  group_by(country) %>%
  summarise(n_inter = n())

n_inter_W <- W_soccer %>%
  filter(level == 'Intermediate') %>%
  group_by(country) %>%
  summarise(n_inter = n())




# add to dataset without NA
M_soccer <- M_soccer %>%
  left_join(n_Vet_M, by = 'country') %>%
  mutate_all( ~ replace(., is.na(.), 0))
W_soccer <- W_soccer %>%
  left_join(n_Vet_W, by = 'country') %>%
  mutate_all( ~ replace(., is.na(.), 0))
# novice

M_soccer <- M_soccer %>%
  left_join(n_Novi_M, by = 'country') %>%
  mutate_all( ~ replace(., is.na(.), 0))
W_soccer <- W_soccer %>%
  left_join(n_Novi_W, by = 'country') %>%
  mutate_all( ~ replace(., is.na(.), 0))

M_soccer <- M_soccer %>%
  left_join(n_inter_M, by = 'country') %>%
  mutate_all( ~ replace(., is.na(.), 0))
W_soccer <- W_soccer %>%
  left_join(n_inter_W, by = 'country') %>%
  mutate_all( ~ replace(., is.na(.), 0))


# analysis 1 ----------------------------------------------------------------
M_soccer %>%
  group_by(rank, country, n_Vet,n_Novice,n_inter) %>%
  summarise(n = n())

W_soccer %>%
  group_by(rank, country, n_Vet,n_Novice,n_inter) %>%
  summarise(n = n()) 

# 남자 베테랑선수들 초급 선수들과 랭킹 --------------------------------------------------------------
#rank-country color == n_Vet

M_soccer %>%
  group_by(rank, country,n_Vet) %>%
  ggplot(aes(x = rank, y = country,fill = n_Vet),) +
  xlab('FinalRank')+
  geom_bar(stat = 'identity') +
  scale_x_continuous(limits = c(0,32))+
  scale_fill_gradient(low = '#3e96ff' , high = '#ffa73e')

ggsave('M_rank-country by vet.png',width = 10, height = 10)
#rank-country color == n_Novice
M_soccer %>%
  group_by(rank, country,n_Novice) %>%
  ggplot(aes(x = rank, y = country,colour = n_Novice),) +
  xlab('FinalRank')+
  geom_point(size = 15) +
  geom_text(aes(label = n_Novice),color = 1)+
  scale_color_gradient(low = '#3e96ff' , high = '#ffa73e')
ggsave('M_rank-country by Nov.png',width = 10,height = 10)
#rank-country color == n_Inter
M_soccer %>%
  group_by(rank, country, n_inter) %>%
  ggplot(aes(x = rank, y = country,colour = n_inter)) +
  xlab('FinalRank')+
  geom_point(size = 15) +
  geom_text(aes(label = n_inter),color = 1)+
  scale_color_gradient(low = '#3e96ff' , high = '#ffa73e')
ggsave('M_rank-country by Inter .png',width = 10,height = 10)
library(gridExtra)

grid.arrange(M_Vet,M_Novice,ncol=1)

grid
ggsave(path = "figure",
       filename = "M_Novice_country_group.png",
       width = 6, height = 4)



# 여자 베테랑선수들 초급 선수들과 랭킹 ----------------------------------------------------

W_soccer %>%
  group_by(rank, country, n_Vet,group) %>%
  ggplot(aes(x = rank, y = country,colour = n_Vet),) +
  xlab('FinalRank')+
  geom_point(size = 15) +
  geom_text(aes(label = n_Vet),color = 1)+
  scale_color_gradient(low = '#3e96ff' , high = '#ffa73e')
ggsave('W_rank-country by vet.png',width = 10,height = 10)
#rank-country color == n_Novice
W_soccer %>%
  group_by(rank, country,n_Novice) %>%
  ggplot(aes(x = rank, y = country,colour = n_Novice),) +
  xlab('FinalRank')+
  geom_point(size = 15) +
  geom_text(aes(label = n_Novice),color = 1)+
  scale_color_gradient(low = '#3e96ff' , high = '#ffa73e')
ggsave('W_rank-country by Nov.png',width = 10,height = 10)
#rank-country color == n_Inter
W_soccer %>%
  group_by(rank, country, n_inter) %>%
  ggplot(aes(x = rank, y = country,colour = n_inter)) +
  xlab('FinalRank')+
  geom_point(size = 15) +
  geom_text(aes(label = n_inter),color = 1)+
  scale_color_gradient(low = '#3e96ff' , high = '#ffa73e')
ggsave('W_rank-country by Inter .png',width = 10,height = 10)



# rising star -------------------------------------------------------------
# restore data
M_soccer1 <- soccer %>%
  filter(sex == 'Men')
W_soccer1 <- soccer %>%
  filter(sex == 'Women')

# Check goals      # (Min, 1st Qu, Median, 3rd Qu, Max) (Mean)
summary(M_soccer)  # (0, 0, 1, 5, 81) (4.52)
summary(W_soccer)  # (0, 0, 1, 8, 181) (7.31)
distinct(M_soccer, goals)
distinct(W_soccer, goals)

# Check age        # (Min, 1st Qu, Median, 3rd Qu, Max) (Mean)
summary(M_soccer)  # (19, 25, 27, 30, 45) (27.42)
summary(W_soccer)  # (16, 23, 26, 29, 41) (26.03)

M_soccer1 <- M_soccer1 %>%
  mutate(level = case_when(age_yrs <= 25 & goals >= 20  ~ 'Rising star',
                           TRUE ~ 'Not star'))
W_soccer1 <- W_soccer1 %>%
  mutate(level = case_when(age_yrs <= 25 & goals >= 20 ~ 'Rising star',
                           TRUE ~ 'Not star'))
# the number of Rising Stars
M_soccer1 %>%               # 4/732
  group_by(level) %>%
  summarise(n = n())
W_soccer1 %>%               # 8/544
  group_by(level) %>%
  summarise(n = n())

# the number of each level per country
t_11 <- M_soccer1 %>%
  group_by(country, level) %>%
  summarise(n = n())

t_21 <- W_soccer1 %>%
  group_by(rank, group, country, level) %>%
  summarise(n = n())

# the number of Rising stars per country
n_RS_M <- M_soccer1 %>%
  filter(level == 'Rising star') %>%
  group_by(country) %>%
  summarise(n_RS = n())
n_RS_W <- W_soccer1 %>%
  filter(level == 'Rising star') %>%
  group_by(country) %>%
  summarise(n_RS = n())

# add to dataset without NA
M_soccer1 <- M_soccer1 %>%
  left_join(n_RS_M, by = 'country') %>%
  mutate_all( ~ replace(., is.na(.), 0))
W_soccer1 <- W_soccer1 %>%
  left_join(n_RS_W, by = 'country') %>%
  mutate_all( ~ replace(., is.na(.), 0))

# analysis 2 --------------------------------------------------------------
M_soccer1 %>%
  group_by(rank, country, n_RS) %>%
  summarise(n = n())

W_soccer1 %>%
  group_by(rank, country, n_RS) %>%
  summarise(n = n())

M_soccer1 %>%
  group_by(rank, country, n_RS) %>%
  summarise(group, n = n()) %>%
  ggplot(aes(x = country, y = n_RS)) +
  geom_point() +
  coord_flip()+
  facet_wrap(~ group, ncol = 3)

ggsave(path = "figure",
       filename = "M_RS_country_group.png",
       width = 6, height = 4)
W_soccer %>%
  group_by(rank, country, n_RS) %>%
  summarise(group, n = n()) %>%
  ggplot(aes(x = country, y = n_RS)) +
  geom_point() +
  facet_wrap(~ group, ncol = 3)
ggsave(path = "figure",
       filename = "W_RS_country_group.png",
       width = 6, height = 4)

# extra -------------------------------------------------------------------
# # export dataframe
# rank_2019 <- W_soccer %>%
#   group_by(country) %>%
#   summarise(n = n())
# library('readr')
# write.csv(rank_2019, "rank_2019.csv")
# 
# # export dataset
# install.packages('xlsx')
# library(xlsx)
# write.xlsx(n_Veteran, "n_Veteran.xlsx")
# 
# 
# 
# 
# install.packages('showtext')
# library(showtext)
# font_add_google("Poor Story", "poorstory")
# showtext_auto()
# 
# 
# N_soccer %>%
#   filter(country == 'Thailand',
#          country == 'Germany') %>%
#   ggplot(aes(x = caps)) +
#   geom_point(aes(fill = country),
#              alpha = 0.5,
#              position = "identity") +
#   theme(text = element_text(family = "poorstory"))
# 
# 
