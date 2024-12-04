library('dplyr')
library(stringr)    
library(tidytext)
library(KoNLP)

df<-readxl::read_excel("News.xlsx")
df1<-readxl::read_excel("pastnews.xlsx")
df$년도 <- substr(df$일자, 1, 4)
df1$년도 <- substr(df1$일자, 1, 4)
result <- rbind(df1, df)

News <- result[c('뉴스 식별자','년도','제목','본문','키워드')]
keywords <- c('이선균|1388|109|이재명')

filtered_News <- News %>%
  filter(!str_detect(키워드, keywords)) %>%  #keywords안들어가는 내용       
  distinct(본문, .keep_all = T) %>% 
  distinct(제목, .keep_all =T)  #중복제거

a <- filtered_News %>%
  mutate(제목 = str_replace_all(제목, "[^가-힣]", " "),
         제목 = str_squish(제목),
         본문 = str_replace_all(본문, "[^가-힣]", " "),
         본문 = str_squish(본문)) %>% 
  as_tibble()

title <- a %>%
  unnest_tokens(input = 제목,
                output = title_word,
                token = extractNoun,
                drop = F)

frequency <- title %>%
  count(title_word , sort = T) %>%   
  filter(str_count(title_word) > 1)

# 불용어,유의어처리 -------------------------------------------------------------------
sameword <- c('대전' = '지방',
              '부산' = '지방',
              '광주' = '지방',
              '청주' = '지방',
              '대구' = '지방',
              '울산' = '지방',
              '제주' = '지방',
              '군산' = '지방',
              '김포' = '지방',
              '강릉' = '지방',
              '포항' = '지방',
              '부천' = '지방',
              '원주' = '지방',
              '수원' = '수도권',
              '서울시' = '수도권',
              '용인' = '수도권',
              '서울' = '수도권',
              '인천' = '수도권',
              '강남' = '수도권',
              '박지선' = '유명인',
              '이재명' = '유명인',
              '최성봉' = '유명인',
              '김문기' = '유명인',
              '김부영' = '유명인',
              '김용호' = '유명인',
              '개그맨' = '유명인', 
              '개그우먼' = '유명인',
              '극단' = '자살',
              '추락사' = '자살',
              '추락' = '자살',
              '시도' = '자살',
              '교사' = '공무원',
              '민원' = '공무원',
              '소방관'='공무원',
              '부부' = '가족',
              '아들' = '가족',
              '엄마' = '가족',
              '아빠' = '가족',
              '모녀' = '가족',
              '일가족' = '가족',
              '모친' = '가족',
              '아내' = '가족',
              '남편' = '가족',
              '자녀' = '가족',
              '유족' = '가족',
              '자살예방' = '예방',
              '프로그램' = '예방',
              '쉼터' = '예방',
              '상담' = '예방',
              '센터' = '예방',
              '청소년상담' = '예방',
              '학부모' = '부모',
              '사건' = '사고',
              '우울증' = '정신건강',
              '번방' = 'N번방',
              '박사' = 'N번방'
              
              
              
)




stopword <- c('가거', '하다','이태','대장동','변희','성남','유한','창녕군수','지방','수도권','유명인','공무원','예방','복지센터','의혹','경비원','년간','속보','개월','발견','아파트','가족' ,
              '자살','선택','청소년','사망','경찰','살해','여성','남성','흉기','마음','검사','지원','자택','직장','하사','유서','네이버','총기','전세','초등교사','직원','사고','피해자',
              '심리','혐의','조사','참사','추정','만원','실종','부검','노조','단독','간호사','타살','공군','잼미','분당','신고','갑질','차량','수사','비극','야산','소장','남녀','항공')

# 년도별 빈도 계산 ---------------------------------------------------------------
Y2023 <- title %>%
  mutate(title_word = recode(title_word, !!!sameword)) %>% 
  filter(!title_word %in% stopword) %>% 
  group_by(년도) %>% 
  filter(년도 == '2023') %>% 
  count(title_word, sort = TRUE) %>% 
  filter(str_count(title_word) > 1)

Y2022 <- title %>%
  mutate(title_word = recode(title_word, !!!sameword)) %>% 
  filter(!title_word %in% stopword) %>% 
  group_by(년도) %>% 
  filter(년도 == '2022') %>% 
  count(title_word, sort = TRUE) %>% 
  filter(str_count(title_word) > 1)

Y2021 <- title %>%
  mutate(title_word = recode(title_word, !!!sameword)) %>% 
  filter(!title_word %in% stopword) %>% 
  group_by(년도) %>% 
  filter(년도 == '2021') %>% 
  count(title_word, sort = TRUE) %>% 
  filter(str_count(title_word) > 1)

Y2020 <- title %>%
  mutate(title_word = recode(title_word, !!!sameword)) %>% 
  filter(!title_word %in% stopword) %>% 
  group_by(년도) %>% 
  filter(년도 == '2020') %>% 
  count(title_word, sort = TRUE) %>% 
  filter(str_count(title_word) > 1)

Y2019 <- title %>%
  mutate(title_word = recode(title_word, !!!sameword)) %>% 
  filter(!title_word %in% stopword) %>% 
  group_by(년도) %>% 
  filter(년도 == '2019') %>% 
  count(title_word, sort = TRUE) %>% 
  filter(str_count(title_word) > 1)

Y2018 <- title %>%
  mutate(title_word = recode(title_word, !!!sameword)) %>% 
  filter(!title_word %in% stopword) %>% 
  group_by(년도) %>% 
  filter(년도 == '2018') %>% 
  count(title_word, sort = TRUE) %>% 
  filter(str_count(title_word) > 1)

Y2017 <- title %>%
  mutate(title_word = recode(title_word, !!!sameword)) %>% 
  filter(!title_word %in% stopword) %>% 
  group_by(년도) %>% 
  filter(년도 == '2017') %>% 
  count(title_word, sort = TRUE) %>% 
  filter(str_count(title_word) > 1)

Y2016 <- title %>%
  mutate(title_word = recode(title_word, !!!sameword)) %>% 
  filter(!title_word %in% stopword) %>% 
  group_by(년도) %>% 
  filter(년도 == '2016') %>% 
  count(title_word, sort = TRUE) %>% 
  filter(str_count(title_word) > 1)

Y2015 <- title %>%
  mutate(title_word = recode(title_word, !!!sameword)) %>% 
  filter(!title_word %in% stopword) %>% 
  group_by(년도) %>% 
  filter(년도 == '2015') %>% 
  count(title_word, sort = TRUE) %>% 
  filter(str_count(title_word) > 1)

Y2014 <- title %>%
  mutate(title_word = recode(title_word, !!!sameword)) %>% 
  filter(!title_word %in% stopword) %>% 
  group_by(년도) %>% 
  filter(년도 == '2014') %>% 
  count(title_word, sort = TRUE) %>% 
  filter(str_count(title_word) > 1)

Y2013 <- title %>%
  mutate(title_word = recode(title_word, !!!sameword)) %>% 
  filter(!title_word %in% stopword) %>% 
  group_by(년도) %>% 
  filter(년도 == '2013') %>% 
  count(title_word, sort = TRUE) %>% 
  filter(str_count(title_word) > 1)
# 시각화 ---------------------------------------------------------------------
library(ggplot2)
library(patchwork)

# 2023년도 그래프
plot_2023 <- ggplot(head(Y2023, 20), aes(x = reorder(title_word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL, title = "2023년도 단어 빈도") +
  theme(text = element_text(family = "AppleGothic"))

# 2022년도 그래프
plot_2022 <- ggplot(head(Y2022, 20), aes(x = reorder(title_word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL, title = "2022년도 단어 빈도") +
  theme(text = element_text(family = "AppleGothic"))

# 2021년도 그래프
plot_2021 <- ggplot(head(Y2021, 20), aes(x = reorder(title_word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL, title = "2021년도 단어 빈도") +
  theme(text = element_text(family = "AppleGothic"))

# 2020년도 그래프
plot_2020 <- ggplot(head(Y2020, 20), aes(x = reorder(title_word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL, title = "2020년도 단어 빈도") +
  theme(text = element_text(family = "AppleGothic"))

# 2018년도 그래프
plot_2019 <- ggplot(head(Y2019, 20), aes(x = reorder(title_word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL, title = "2019년도 단어 빈도") +
  theme(text = element_text(family = "AppleGothic"))

# 2018년도 그래프
plot_2018 <- ggplot(head(Y2018, 20), aes(x = reorder(title_word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL, title = "2018년도 단어 빈도") +
  theme(text = element_text(family = "AppleGothic"))

# 2017년도 그래프
plot_2017 <- ggplot(head(Y2017, 20), aes(x = reorder(title_word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL, title = "2017년도 단어 빈도") +
  theme(text = element_text(family = "AppleGothic"))

# 2016년도 그래프
plot_2016 <- ggplot(head(Y2016, 20), aes(x = reorder(title_word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL, title = "2016년도 단어 빈도") +
  theme(text = element_text(family = "AppleGothic"))

# 2015년도 그래프
plot_2015 <- ggplot(head(Y2015, 20), aes(x = reorder(title_word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL, title = "2015년도 단어 빈도") +
  theme(text = element_text(family = "AppleGothic"))


# 2014년도 그래프
plot_2014 <- ggplot(head(Y2014, 20), aes(x = reorder(title_word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL, title = "2014년도 단어 빈도") +
  theme(text = element_text(family = "AppleGothic"))


# 2013년도 그래프
plot_2013 <- ggplot(head(Y2013, 20), aes(x = reorder(title_word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL, title = "2013년도 단어 빈도") +
  theme(text = element_text(family = "AppleGothic"))




# 그래프 병렬 배치
plot_2023 + plot_2022 + plot_2021 + plot_2020 + plot_layout(ncol = 2)

plot_2019 + plot_2018 + plot_2017 + plot_2016 + plot_layout(ncol = 2)

plot_2015 + plot_2014 + plot_2013 + plot_layout(ncol = 2)

plot_2023 + plot_2022 + plot_2021 + plot_2020 + plot_2019 + plot_2018 + plot_2017 + plot_2016 + plot_2015 + plot_2014 + plot_2013 + plot_layout(ncol = 3)
# test --------------------------------------------------------------------

View(title[title['title_word'] == '후기',])

result <- a %>%
  filter(grepl("청소년", 키워드))

# 결과 출력
View(result)
