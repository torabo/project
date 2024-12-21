library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(KoNLP)

# 데이터 불러오기 ---------------------------------------------------------------
df <- read_excel("News.xlsx")
df1 <- read_excel("pastnews.xlsx")

# 연도 추출 ---------------------------------------------------------------------
df$년도 <- substr(df$일자, 1, 4)
df1$년도 <- substr(df1$일자, 1, 4)

# 데이터 병합 --------------------------------------------------------------------
result <- rbind(df, df1)

# 필요한 열 선택 -----------------------------------------------------------------
News <- result %>%
  select(년도, 제목, 본문, 키워드)

# 특정 키워드 제외 및 중복 제거 ---------------------------------------------------
keywords <- c('이선균|1388|109|이재명')

filtered_News <- News %>%
  filter(!str_detect(키워드, keywords)) %>%  # 특정 키워드 제외
  distinct(본문, .keep_all = TRUE) %>%       # 본문 중복 제거
  distinct(제목, .keep_all = TRUE)           # 제목 중복 제거

# 제목과 본문 정리 ----------------------------------------------------------------
a <- filtered_News %>%
  mutate(
    제목 = str_replace_all(제목, "[^가-힣snsSNS]", " "),  # 한글과 'sns' 외 제거
    제목 = str_squish(제목),                            # 공백 정리
    본문 = str_replace_all(본문, "[^가-힣snsSNS]", " "),  # 한글과 'sns' 외 제거
    본문 = str_squish(본문)                             # 공백 정리
  ) %>%
  as_tibble()

# 제목에서 명사 추출 --------------------------------------------------------------
title <- a %>%
  unnest_tokens(input = 제목,
                output = title_word,
                token = extractNoun,  # 명사 추출
                drop = FALSE)

# 단어 빈도 계산 ------------------------------------------------------------------
frequency <- title %>%
  count(title_word, sort = TRUE) %>%  # 단어 빈도 계산
  filter(str_count(title_word) > 1)  # 두 글자 이상 필터링

# 년도별 빈도 계산 ---------------------------------------------------------------
# --- sameword와 stopword 불러오기 ---
sameword_df <- read.csv("sameword.csv", fileEncoding = "UTF-8")
sameword <- setNames(as.character(sameword_df$replacement), sameword_df$word)
stopword <- readLines("stopword.txt", encoding = "UTF-8")

years <- 2024:2013

# 연도별 데이터 리스트 생성
yearly_data <- list()

for (year in years) {
  yearly_data[[as.character(year)]] <- title %>%
    mutate(title_word = recode(title_word, !!!sameword)) %>%  # sameword 적용
    group_by(년도) %>%
    filter(년도 == as.character(year)) %>%
    count(title_word, sort = TRUE) %>%
    filter(str_count(title_word) > 1)
}



# 2020~2024 vs 나머지 가중치 오즈비 -------------------------------------------------------
library(ggplot2)
library(tidylo)
library(tidyr)

frequency_long <- title %>%
  mutate(title_word = recode(title_word, !!!sameword)) %>% 
  filter(!title_word %in% stopword) %>% 
  group_by(년도) %>%
  mutate(년도 = case_when(
    년도 %in% c("2020", "2021", "2022", "2023", "2024") ~ "after2020",  
    년도 %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019") ~ "before2020")) %>% 
  count(title_word, sort = TRUE) %>% 
  filter(str_count(title_word) > 1)

# 로그오즈비
frequency_wide <- frequency_long %>% 
  pivot_wider(names_from = 년도,
              values_from = n,
              values_fill = list(n=0)) %>% 
  mutate(odds_before2020 = ((before2020+1)/sum(before2020+1)),
         odds_after2020 = ((after2020+1)/sum(after2020+1))) %>% 
  mutate(log_odds_ratio = log(odds_after2020/odds_before2020))

rate_log_df <- frequency_wide %>% 
  group_by(label = ifelse(log_odds_ratio > 0, "after2020", "before2020")) %>% 
  slice_max(abs(log_odds_ratio), n = 10) %>% 
  ggplot(aes(x = log_odds_ratio,
             y = reorder(title_word, log_odds_ratio),
             fill = label)) +
  geom_col() 

rate_log_df

# 가중로그오즈비
two_group_weighted <- frequency_long %>% 
  bind_log_odds(set = 년도,
                feature = title_word,
                n = n) %>% 
  arrange(-log_odds_weighted)

a <- two_group_weighted %>%
  filter(년도 == 'after2020')

d <- a %>% slice_max(abs(log_odds_weighted), n = 15) %>% 
  ggplot(aes(x = log_odds_weighted,
             y = reorder(title_word, log_odds_weighted))) +
  geom_col(show.legend = F) +
  geom_col(aes(fill = ifelse(title_word %in% c("인터넷", "인터넷방송", "N번방", "우울증갤러리"), "red", "gray"))) +
  scale_fill_identity() +
  facet_wrap(~년도, scale = "free_y")

d

two_group_weighted_top20 <- two_group_weighted %>%
  group_by(년도) %>%
  slice_max(log_odds_weighted, n = 7 , with_ties = F) %>%
  mutate(log_odds_weighted = ifelse(년도 == "before2020", -log_odds_weighted, log_odds_weighted))

two_group_weighted_top20 <- two_group_weighted_top20 %>% 
  ggplot(aes(x = log_odds_weighted,
             y = reorder(title_word, log_odds_weighted))) +
  # geom_col()에서 title_word가 특정 값일 때 빨간색으로 색 지정
  geom_col(aes(fill = 
                 ifelse(title_word %in% c("인터넷", "인터넷방송", "N번방", "우울증갤러리"),
                        "red",
                        ifelse(년도 == "after2020", "#66aaf2", "#004EA1")))) +
  labs(x = "Log Odds Weighted", y= '') +
  geom_text(aes(label = round(log_odds_weighted, 2), 
                hjust = ifelse(log_odds_weighted > 0, -0.3, 1.3)),  # 조건에 따른 hjust 설정
            size = 6) +
  scale_fill_manual(values = c("red" = "red", "#66aaf2" = "#66aaf2", "#004EA1" = "#004EA1"),
                    labels = c("red" = "인터넷 관련 단어", "#66aaf2" = "After 2020","#004EA1" = "Before 2020"),
                    name = "") +  # 범례 추가
  theme_minimal() +
  theme(
    legend.title = element_text(size = 12),        # 범례 제목 크기
    legend.text = element_text(size = 13),         # 범례 텍스트 크기
    axis.text = element_text(size = 20),           # x, y축 텍스트 크기
    axis.title = element_text(size = 20),          # x, y축 제목 크기
    plot.title = element_text(size = 16, face = "bold") # 그래프 제목 크기
  )


two_group_weighted_top20


# 빈도 분석 ---------------------------------------------------------------------
library(patchwork)
library(ggplot2)
library(showtext)

# Google 폰트 설정
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()

# 연도별 강조 단어 설정
highlight_words <- c("인터넷", "인터넷방송", "우울증갤러리", "sns", "N번방")
des <- c("코로나", "메르스")
mental <-c('정신건강','치매')
fam <-c('학교폭력','장애')
se<-c('성범죄')
# 그래프 저장 리스트
plots <- list()

for (year in names(yearly_data)) {
  # 연도별 데이터 정리
  data <- yearly_data[[year]] %>%
    head(7) %>%
    ungroup() %>%
    mutate(
      total = sum(n),                   # 연도별 총 단어 빈도 계산
      proportion = n / total * 100,     # 비율 계산
      label_position = n / 2,           # 막대 중앙 위치
      percent_position = n + max(n) * 0.05  # 퍼센트 위치
    )
  
  # 그래프 생성
  plots[[year]] <- ggplot(data, aes(x = reorder(title_word, n), y = n)) +
    geom_col(aes(fill = case_when(
      title_word %in% highlight_words ~ "highlight",  # 빨간색 강조
      title_word %in% des ~ "special",      # 파란색 강조
      title_word %in% mental ~ 'ment',
      title_word %in% fam ~ "fam",      # 파란색 강조
      title_word %in% se ~ 'se',
      TRUE ~ "normal"     # 일반 단어 (회색)                           
    ))) +
    coord_flip() +
    # 빈도(n)는 막대 중앙에, 퍼센트(%)는 막대 끝에 표시
    geom_text(aes(y = label_position, label = n), color = "white", fontface = "bold") +  # 빈도(n): 중앙
    geom_text(aes(y = percent_position, label = paste0(round(proportion, 1), "%")), hjust = 0) +  # 퍼센트(%): 막대 끝
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +  # x축 여백 추가
    labs(x = NULL, title = paste(year, "년도 단어 빈도")) +
    scale_fill_manual(
      values = c("highlight" = "red", 
                 "special" = "blue", 
                 "ment" ="#9370DB" ,
                 'fam' = '#ADFF2F',
                 'se' = '#4B0082',
                 "normal" = "gray"),  # 색상 매핑
      guide = "none"  # 범례 제거
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "nanumgothic"),
      plot.title = element_text(size = 20, face = "bold"),
      axis.text = element_text(size = 14)
    )
}
# --- 그래프 병렬 배치 ---
# 2013년부터 2018년 그래프 병렬 배치
combined_plot_2013_2018 <- wrap_plots(plots[names(plots)[names(plots) %in% c("2013", "2014", "2015", "2016", "2017", "2018")]]) +
  plot_layout(ncol = 3)  # 한 줄에 3개의 그래프

# 2019년부터 2024년 그래프 병렬 배치
combined_plot_2019_2024 <- wrap_plots(plots[names(plots)[names(plots) %in% c("2019", "2020", "2021", "2022", "2023", "2024")]]) +
  plot_layout(ncol = 3)  # 한 줄에 3개의 그래프
# 2013년부터 2024년까지 그래프 병렬 배치
combined_plot_2013_2024 <- wrap_plots(plots[names(plots)[names(plots) %in% as.character(2013:2024)]]) +
  plot_layout(ncol = 4)  # 한 줄에 3개의 그래프

# 그래프 출력
combined_plot_2013_2024
# 그래프 출력
combined_plot_2013_2018
combined_plot_2019_2024



# 년도별 워드 클라우드 -------------------------------------------------------------
library(wordcloud2)
library(showtext)
library(dplyr)

# 폰트 및 텍스트 설정
showtext_auto()
font_add_google(name = "Nanum Gothic", family = "nanumgothic")

# 사용자 설정: seed와 색상
set.seed(1234)  # 워드클라우드 결과 고정

max_color <- "#FF4500"  # 강렬한 빨강 계열
min_color <- "#1E90FF"  # 선명한 파랑 계열

# 워드클라우드 저장할 리스트
wordclouds <- list()

# 연도별 워드클라우드 생성
for (year in names(yearly_data)) {
  data <- yearly_data[[year]] %>%
    head(13) %>%  # 상위 15개 단어 선택
    ungroup() %>%
    select(title_word, n) %>%
    arrange(desc(n))  # 빈도 기준 정렬
  
  # 빈도 기반 색상 설정
  gradient_colors <- scales::col_numeric(
    palette = c(min_color, max_color),  # 사용자 설정 색상
    domain = range(data$n)  # 빈도 범위
  )(data$n)  # 빈도에 따른 색상 매핑
  
  # 워드클라우드 생성
  wordclouds[[year]] <- wordcloud2(
    data, 
    size = 1,  # 워드클라우드 단어 크기
    minSize = 0.1,  # 최소 크기
    color = gradient_colors,  # 빈도 기반 색상 적용
    fontFamily = "nanumgothic",  # 폰트 설정
    backgroundColor = "white",  
    shape = "circle"
  )
}

(wordclouds[['2024']])
(wordclouds[['2023']])
(wordclouds[['2022']])
(wordclouds[['2021']])
(wordclouds[['2020']])
(wordclouds[['2019']])
(wordclouds[['2018']])
(wordclouds[['2017']])
(wordclouds[['2016']])
(wordclouds[['2015']])
(wordclouds[['2014']])
(wordclouds[['2013']])
# 2020전후 ------------------------------------------------------------------
library(wordcloud2)
library(dplyr)
library(showtext)

# 폰트 및 텍스트 설정
showtext_auto()
font_add_google(name = "Nanum Gothic", family = "nanumgothic")

# 사용자 설정: 색상
set.seed(1234)
min_color <- "#1E90FF"  # 선명한 파랑 계열
max_color <- "#FF4500"  # 강렬한 빨강 계열

# 2020년 이전 데이터 (before2020)
before_2020 <- frequency_long %>%
  filter(년도 == "before2020") %>%
  ungroup() %>%  # 그룹화 해제
  select(title_word, n) %>%  # 'title_word'와 'n' 열 선택
  arrange(desc(n)) %>%
  head(20) %>%
  mutate(n = as.numeric(n))  # n 열을 숫자형으로 변환

# 2020년 이후 데이터 (after2020)
after_2020 <- frequency_long %>%
  filter(년도 == "after2020") %>%
  ungroup() %>%  # 그룹화 해제
  select(title_word, n) %>%  # 'title_word'와 'n' 열 선택
  arrange(desc(n)) %>%
  head(20) %>%
  mutate(n = as.numeric(n))  # n 열을 숫자형으로 변환

# 빈도 기반 색상 설정
gradient_colors_before <- scales::col_numeric(
  palette = c(min_color, max_color),
  domain = range(before_2020$n)
)(before_2020$n)

gradient_colors_after <- scales::col_numeric(
  palette = c(min_color, max_color),
  domain = range(after_2020$n)
)(after_2020$n)

# 워드클라우드 생성 (Before 2020)
beforeplot <- wordcloud2(
  before_2020, 
  size = 1.5,  # 워드클라우드 단어 크기
  minSize = 0.1,  # 최소 크기
  color = gradient_colors_before,  # 빈도 기반 색상 적용
  fontFamily = "nanumgothic",
  backgroundColor = "white",
  shape = "circle"
)

# 워드클라우드 생성 (After 2020)
afterplot <- wordcloud2(
  after_2020, 
  size = 1.5,  # 워드클라우드 단어 크기
  minSize = 0.1,  # 최소 크기
  color = gradient_colors_after,  # 빈도 기반 색상 적용
  fontFamily = "nanumgothic",
  backgroundColor = "white",
  shape = "circle"
)

beforeplot
afterplot
