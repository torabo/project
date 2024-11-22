# Read Data ---------------------------------------------------------------
d2020=read.csv("../../teen_suicide/csv/kyrbs2020.csv")
d2021=read.csv("../../teen_suicide/csv/kyrbs2021.csv")

# Check Column Variable ---------------------------------------------------
# 열 이름 비교
unique_to_df1 <- setdiff(colnames(d2020), colnames(d2021))
unique_to_df2 <- setdiff(colnames(d2021), colnames(d2020))
mismatched_columns <- union(unique_to_df1, unique_to_df2)

# 맞지 않는 열 추출
mismatched_in_d2020 <- d2020[, unique_to_df1, drop = FALSE]
mismatched_in_d2021 <- d2021[, unique_to_df2, drop = FALSE]

# 결과 출력
print("d2020에만 있는 열:")
print(colnames(mismatched_in_d2020))

print("d2021에만 있는 열:")
print(colnames(mismatched_in_d2021))

# Combining Data ----------------------------------------------------------
library(dplyr)
df1 <- bind_rows(d2020, d2021)

# Population --------------------------------------------------------------
library('survey')
design_df1 = svydesign(ids = ~CLUSTER,  # 집락변수
                       strata = ~STRATA,      # 층화변수
                       weights = ~W,           # 가중치변수
                       data = df1,
                       nest = TRUE)      # 집락이 층화변수 내에 중첩되도록 설정

dim(df1)
(sum(weights(design_df1)))/2

# Preprocessing -----------------------------------------------------------

# Load a Variable ---------------------------------------------------------
#id
id = 1:109796
#자살생각
y_idea <- df1$M_SUI_CON 
#자살계획
y_plan <- df1$M_SUI_PLN 
#자살시도
y_attempt <- df1$M_SUI_ATT 
#코로나19로 인해 가정의 경제적 상태가 이전보다 어려워졌다고 생각하는가?
x_economy_covid19 <- df1$E_COVID19
#최근 7일 동안 스마트폰 사용 여부 및 시간간
#주중 스마트폰 사용 여부
smartphone_use_weekdays <- df1$INT_SPWD
#주중 스마트폰 사용 시간 (분)
smartphone_use_time_weekdays <- df1$INT_SPWD_TM
#주말 스마트폰 사용 여부 
smartphone_use_weekend <- df1$INT_SPWK
#주말 스마트폰 사용 시간 (분)
smartphone_use_time_weekend <- df1$INT_SPWK_TM
#우울감 경험
sad <- df1$M_SAD
#성별
x_sex <- df1$SEX 
#학년
x_school_year <- df1$GRADE 
#학업성적
x_academic_grade <- df1$E_S_RCRD 
#경제상태
x_economic_status <- df1$E_SES 
#거주형태
x_living_with_family <- df1$E_RES 
#가정형편으로 인한 경제적 도움
x_financial_help <- df1$E_AID 
#주관적 건강인지
x_health_status <- df1$PR_HT 
#주관적 수면 충족
x_sleep_status <- df1$M_SLP_EN 
#스트레스
x_perceived_stress <- df1$M_STR 
#범불안장애 불러오기
GAD1 <- df1$M_GAD_1 
GAD2 <- df1$M_GAD_2
GAD3 <- df1$M_GAD_3
GAD4 <- df1$M_GAD_4
GAD5 <- df1$M_GAD_5
GAD6 <- df1$M_GAD_6
GAD7 <- df1$M_GAD_7
#범불안장애 점수화
GAD1 <- case_when(GAD1 == 1 ~ 0,
                  GAD1 == 2 ~ 1,
                  GAD1 == 3 ~ 2,
                  GAD1 == 4 ~ 3)
GAD2 <- case_when(GAD2 == 1 ~ 0,
                  GAD2 == 2 ~ 1,
                  GAD2 == 3 ~ 2,
                  GAD2 == 4 ~ 3)
GAD3 <- case_when(GAD3 == 1 ~ 0,
                  GAD3 == 2 ~ 1,
                  GAD3 == 3 ~ 2,
                  GAD3 == 4 ~ 3)
GAD4 <- case_when(GAD4 == 1 ~ 0,
                  GAD4 == 2 ~ 1,
                  GAD4 == 3 ~ 2,
                  GAD4 == 4 ~ 3)
GAD5 <- case_when(GAD5 == 1 ~ 0,
                  GAD5 == 2 ~ 1,
                  GAD5 == 3 ~ 2,
                  GAD5 == 4 ~ 3)
GAD6 <- case_when(GAD6 == 1 ~ 0,
                  GAD6 == 2 ~ 1,
                  GAD6 == 3 ~ 2,
                  GAD6 == 4 ~ 3)
GAD7 <- case_when(GAD7 == 1 ~ 0,
                  GAD7 == 2 ~ 1,
                  GAD7 == 3 ~ 2,
                  GAD7 == 4 ~ 3)
#범불안장애 점수 합
x_GAD <- GAD1+GAD2+GAD3+GAD4+GAD5+GAD6+GAD7 
#범불안장애 점수 확인(혹시 모르니 직접 더해보기)
head(GAD1)
head(GAD2)
head(GAD3)
head(GAD4)
head(GAD5)
head(GAD6)
head(GAD7)
head(x_GAD) 
#외로움 경험
x_loneliness <- df1$M_LON 
#폭력 경험
x_violence <- df1$V_TRT 
#음주 여부
x_drinking <- df1$AC_DAYS 
#약물사용경험
x_drug_use <- df1$DR_HAB 
#흡연여부
x_smoking <- df1$TC_DAYS 
# Categorizing the Variables ----------------------------------------------

#자살 생각 있다=2, 없다=1 -> 있다=1, 없다=0
y_idea <- case_when(y_idea == 2 ~ 1,
                    y_idea == 1 ~ 0)
#자살 계획 있다=2, 없다=1 -> 있다=1, 없다=0
y_plan <- case_when(y_plan == 2 ~ 1,
                    y_plan == 1 ~ 0)
#자살 시도 있다=2, 없다=1 -> 있다=1, 없다=0
y_attempt <- case_when(y_attempt == 2 ~ 1,
                       y_attempt == 1 ~ 0)
#코로나19로 인해 가정의 경제적 상태가 이전보다 어려워졌다고 생각하는가?
#매우 그렇다=1, 그렇다=2 -> 그렇다= 1
#그렇지 않은 편이다=3, 그렇지 않다=4 -> 그렇지 않다=0
x_economy_covid19 <- case_when(x_economy_covid19 == 1 ~ 1,
                               x_economy_covid19 == 2 ~ 1,
                               x_economy_covid19 == 3 ~ 0,
                               x_economy_covid19 == 4 ~ 0)
#우울감 경험 있다=2, 없다=1 -> 있다=1, 없다=0
sad <- case_when(sad == 2 ~ 1,
                 sad == 1 ~ 0)
#성별 남자=1, 여자=2 -> 남자=0, 여자=1
x_sex <- case_when(x_sex == 1 ~ 0,
                   x_sex == 2 ~ 1)
#학년 중1=1, 중2=2, 중3=3, 고1=4, 고2=5, 고3=6 -> 고3=0
x_school_year <- case_when(x_school_year == 6 ~0 ,
                           x_school_year == 1 ~1,
                           x_school_year == 2 ~2,
                           x_school_year == 3 ~3,
                           x_school_year == 4 ~4,
                           x_school_year == 5 ~5)


#학업성적 
#상=1, 중상=2 -> 상=0
#중=3 -> 중=1
#중하=4, 하=5 -> 하=2
x_academic_grade <- case_when(x_academic_grade == 1 ~ 0,
                              x_academic_grade == 2 ~ 0,
                              x_academic_grade == 3 ~ 1,
                              x_academic_grade == 4 ~ 2,
                              x_academic_grade == 5 ~ 2)
#경제상태
#상=1,  상=0
#중상2 , 중3 ,중하4 => 중=1
#하=5 -> 하=2
x_economic_status <- case_when(x_economic_status == 1 ~ 0,
                               x_economic_status == 2 ~ 0,
                               x_economic_status == 3 ~ 1,
                               x_economic_status == 4 ~ 2,
                               x_economic_status == 5 ~ 2)

#거주형태
#가족=1, 친척집=2, 하숙및자취=3, 기숙사=4, 보육시설=5
#-> 가족=0, 친척집=1, 하숙및자취=2, 기숙사=3, 보육시설=4
x_living_with_family <- case_when(x_living_with_family ==1 ~ 0,
                                  x_living_with_family == 2 ~ 1,
                                  x_living_with_family == 3 ~ 2,
                                  x_living_with_family == 4 ~ 3,
                                  x_living_with_family == 5 ~ 4)

#가정형편으로 인한 경제적 도움 있다=2, 없다=1 -> 있다=1, 없다=0
x_financial_help <- case_when(x_financial_help == 2 ~ 1,
                              x_financial_help == 1 ~ 0)
#주관적 건강인지
#매우건강한편=1, 건강한편=2 -> 건강함=1
#보통=3 -> 보통=2
#건강하지못한편=4, 매우건강하지못한편=5 -> 건강하지않음=3
x_health_status <- case_when(x_health_status == 1 ~ 1,
                             x_health_status == 2 ~ 1,
                             x_health_status == 3 ~ 2,
                             x_health_status == 4 ~ 3,
                             x_health_status == 5 ~ 3)
#주관적 수면 충족
#매우충분=1, 충분=2 -> 충분=1
#보통=3 -> 보통=2
#충분하지않다=4, 전혀충분하지않다=5 -> 불충분=3
x_sleep_status <- case_when(x_sleep_status == 1 ~ 1,
                            x_sleep_status == 2 ~ 1,
                            x_sleep_status == 3 ~ 2,
                            x_sleep_status == 4 ~ 3,
                            x_sleep_status == 5 ~ 3)
#스트레스
#대단히많이느낀다=1, 많이느낀다=2 -> 높음=1
#조금느낀다=3 -> 중간=2
#별로느끼지않는다=4, 전혀느끼지않는다=5 -> 낮음=0
x_perceived_stress <- case_when(x_perceived_stress == 1 ~ 1,
                                x_perceived_stress == 2 ~ 1,
                                x_perceived_stress == 3 ~ 2,
                                x_perceived_stress == 4 ~ 0,
                                x_perceived_stress == 5 ~ 0)
#범불안장애
#범불안장애 점수
#0~4 = 0, 5~9 = 1, 10~14 = 2, 15~21 = 3
x_GAD <- case_when(x_GAD >= 0 & x_GAD <= 4 ~ 0,
                   x_GAD >= 5 & x_GAD <= 9 ~ 1, 
                   x_GAD >= 10 & x_GAD <= 14 ~ 2, 
                   x_GAD > 14 ~ 3)
#외로움 경험
#항상느꼈다=5, 자주느꼈다=4 -> 높음=2
#가끔느꼈다=3 -> 중간=1
#거의느끼지않았다=2, 전혀느끼지않았다=1 -> 낮음=0
x_loneliness <- case_when(x_loneliness == 5 ~ 2,
                          x_loneliness == 4 ~ 2,
                          x_loneliness == 3 ~ 1,
                          x_loneliness == 2 ~ 0,
                          x_loneliness == 1 ~ 0)

#폭력 경험 1~6번(있다)=2-7, 0번(없다)=1 -> 있다=1, 없다=0
x_violence <- case_when(x_violence == 1 ~ 0,
                        x_violence %in% 2:7 ~ 1)
#음주 여부 1번~6번(월 1~29일)=2-7, 0번(없다)=1, 비해당=9999 
# -> 있다=1, 없다=0, 비해당= 0
x_drinking <- case_when(x_drinking == 1 ~ 0,
                        x_drinking == 9999 ~ 0,
                        x_drinking %in% 2:7 ~ 1)
#약물 사용 경험 있다=2, 없다=1 -> 있다=1, 없다=0
x_drug_use <- case_when(x_drug_use == 2 ~ 1,
                        x_drug_use == 1 ~ 0)
#흡연 여부 1번~6번(월 1~29일)=2-7, 0번(최근 30일동안없다)=1, 비해당=9999 
# -> 있다=1, 없다=0, 비해당= 0
x_smoking <- case_when(x_smoking == 1 ~ 0,
                       x_smoking == 9999 ~ 0,
                       x_smoking %in% 2:7 ~ 1)

#CLUSTER ,STRATA ,W
CLUSTER <- df1$CLUSTER
STRATA <- df1$STRATA
W <- df1$W

#데이터 프레임 결합
df2 = data.frame(id, y_idea, y_plan, y_attempt, x_economy_covid19, sad, x_sex, 
                 x_school_year, x_academic_grade, x_economic_status, 
                 x_living_with_family, x_financial_help, x_health_status, 
                 x_sleep_status, x_perceived_stress, x_GAD, x_loneliness, 
                 x_violence, x_drinking, x_drug_use, x_smoking, CLUSTER, 
                 STRATA, W)

# Table1 ---------------------------------------------------------------------

# 가중치를 2로 나눈 설계 객체 생성
df2$W_half <- df2$W / 2

design_df2_half <- svydesign(
  ids = ~CLUSTER,
  strata = ~STRATA,
  weights = ~W_half,
  data = df2,
  nest = TRUE
)

# Table1 ---------------------------------------------------------------------

svytable(~sad+x_economy_covid19, design = design_df2_half)

# suicide idea, plan, attempt
round(svytable(~y_idea+x_economy_covid19+sad, design = design_df2_half))
round(svytable(~y_plan+x_economy_covid19+sad, design = design_df2_half))
round(svytable(~y_attempt+x_economy_covid19+sad, design = design_df2_half))

# sex
round(svytable(~x_sex+x_economy_covid19+sad, design = design_df2_half))

# school year
round(svytable(~x_school_year+x_economy_covid19+sad, design = design_df2_half))

# academic grade
# sad=1 값이 이상함.
round(svytable(~x_academic_grade+x_economy_covid19+sad, design = design_df2_half))

# economic_status
# sad=1의 값이 x_academic_grade sad=1의 값임.
round(svytable(~x_economic_status+x_economy_covid19+sad, design = design_df2_half))

# living with family
round(svytable(~x_living_with_family+x_economy_covid19+sad, design = design_df2_half))

# financial help
round(svytable(~x_financial_help+x_economy_covid19+sad, design = design_df2_half))

# subjective health status
round(svytable(~x_health_status+x_economy_covid19+sad, design = design_df2_half))

# subjective sleep sufficiency status
round(svytable(~x_sleep_status+x_economy_covid19+sad, design = design_df2_half))

# perceived stress
round(svytable(~x_perceived_stress+x_economy_covid19+sad, design = design_df2_half))

# general anxiety disorder
round(svytable(~x_GAD+x_economy_covid19+sad, design = design_df2_half))

# loneliness
round(svytable(~x_loneliness+x_economy_covid19+sad, design = design_df2_half))

# violence experience
round(svytable(~x_violence+x_economy_covid19+sad, design = design_df2_half))

# drinking
round(svytable(~x_drinking+x_economy_covid19+sad, design = design_df2_half))

# drug use experience
round(svytable(~x_drug_use+x_economy_covid19+sad, design = design_df2_half))

# smoking
round(svytable(~x_smoking+x_economy_covid19+sad, design = design_df2_half))

# chi2 --------------------------------------------------------------------

chisq.test(svytable(~y_idea+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~y_idea+x_economy_covid19+sad, design = design_df2_half)[,,1])

chisq.test(svytable(~y_plan+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~y_plan+x_economy_covid19+sad, design = design_df2_half)[,,1])

chisq.test(svytable(~y_attempt+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~y_attempt+x_economy_covid19+sad, design = design_df2_half)[,,1])

chisq.test(svytable(~x_sex+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~x_sex+x_economy_covid19+sad, design = design_df2_half)[,,1])

chisq.test(svytable(~x_school_year+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~x_school_year+x_economy_covid19+sad, design = design_df2_half)[,,1])

chisq.test(svytable(~x_academic_grade+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~x_academic_grade+x_economy_covid19+sad, design = design_df2_half)[,,1])

chisq.test(svytable(~x_economic_status+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~x_economic_status+x_economy_covid19+sad, design = design_df2_half)[,,1])

chisq.test(svytable(~x_living_with_family+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~x_living_with_family+x_economy_covid19+sad, design = design_df2_half)[,,1])

chisq.test(svytable(~x_financial_help+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~x_financial_help+x_economy_covid19+sad, design = design_df2_half)[,,1])

chisq.test(svytable(~x_health_status+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~x_health_status+x_economy_covid19+sad, design = design_df2_half)[,,1])

chisq.test(svytable(~x_sleep_status+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~x_sleep_status+x_economy_covid19+sad, design = design_df2_half)[,,1])

chisq.test(svytable(~x_perceived_stress+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~x_perceived_stress+x_economy_covid19+sad, design = design_df2_half)[,,1])

chisq.test(svytable(~x_GAD+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~x_GAD+x_economy_covid19+sad, design = design_df2_half)[,,1])

chisq.test(svytable(~x_loneliness+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~x_loneliness+x_economy_covid19+sad, design = design_df2_half)[,,1])

chisq.test(svytable(~x_violence+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~x_violence+x_economy_covid19+sad, design = design_df2_half)[,,1])

chisq.test(svytable(~x_drinking+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~x_drinking+x_economy_covid19+sad, design = design_df2_half)[,,1])

chisq.test(svytable(~x_drug_use+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~x_drug_use+x_economy_covid19+sad, design = design_df2_half)[,,1])

chisq.test(svytable(~x_smoking+x_economy_covid19+sad, design = design_df2_half)[,,2])
chisq.test(svytable(~x_smoking+x_economy_covid19+sad, design = design_df2_half)[,,1])

# OR ----------------------------------------------------------------------
# `sad == 1`인 데이터만 필터링한 설계 객체 생성
design_sad_1 <- subset(design_df2_half, sad == 1)
# `sad == 0`인 데이터만 필터링한 설계 객체 생성
design_sad_0 <- subset(design_df2_half, sad == 0)
#제대로 되는지 확인

sum(weights(design_sad_1))
sum(weights(design_sad_0))

# Losgistic sad1 group (depression) ------------------------------------------------------

#자살생각과 독립변수들간 로지스틱회귀분석
logit1 <- svyglm(
  formula = y_idea ~ factor(x_economy_covid19)+factor(x_sex)+factor(x_school_year)+factor(x_academic_grade)+factor(x_economic_status)+factor(x_living_with_family)+factor(x_financial_help)+factor(x_health_status)+factor(x_sleep_status)+factor(x_perceived_stress)+factor(x_GAD)+factor(x_loneliness)+factor(x_violence)+factor(x_drinking)+factor(x_drug_use)+factor(x_smoking),
  design = design_sad_1,
  family = quasibinomial()
)

sad1_idea <- round(exp(cbind(OR = coef(logit1), confint(logit1))),2)
df_sad1_idea <- as.data.frame(sad1_idea)
df_sad1_idea

#자살계획과 독립변수들간 로지스틱회귀분석
logit2 <- svyglm(
  formula = y_plan ~ factor(x_economy_covid19)+factor(x_sex)+factor(x_school_year)+factor(x_academic_grade)+factor(x_economic_status)+factor(x_living_with_family)+factor(x_financial_help)+factor(x_health_status)+factor(x_sleep_status)+factor(x_perceived_stress)+factor(x_GAD)+factor(x_loneliness)+factor(x_violence)+factor(x_drinking)+factor(x_drug_use)+factor(x_smoking),
  design = design_sad_1,
  family = quasibinomial()
)
sad1_plan <- round(exp(cbind(OR = coef(logit2), confint(logit2))),2)
df_sad1_plan <- as.data.frame(sad1_plan)
df_sad1_plan

#자살시도 독립변수들간 로지스틱회귀분석
logit3 <- svyglm(
  formula = y_attempt ~ factor(x_economy_covid19)+factor(x_sex)+factor(x_school_year)+factor(x_academic_grade)+factor(x_economic_status)+factor(x_living_with_family)+factor(x_financial_help)+factor(x_health_status)+factor(x_sleep_status)+factor(x_perceived_stress)+factor(x_GAD)+factor(x_loneliness)+factor(x_violence)+factor(x_drinking)+factor(x_drug_use)+factor(x_smoking),
  design = design_sad_1,
  family = quasibinomial()
)
sad1_attempt <- round(exp(cbind(OR = coef(logit3), confint(logit3))),2)
df_sad1_attempt <- as.data.frame(sad1_attempt)
df_sad1_attempt


# Losgistic sad0 group (Nodepression) ---------------------------------------

#자살생각과 독립변수들간 로지스틱회귀분석

logit4 <- svyglm(
  formula = y_idea ~ factor(x_economy_covid19)+factor(x_sex)+factor(x_school_year)+factor(x_academic_grade)+factor(x_economic_status)+factor(x_living_with_family)+factor(x_financial_help)+factor(x_health_status)+factor(x_sleep_status)+factor(x_perceived_stress)+factor(x_GAD)+factor(x_loneliness)+factor(x_violence)+factor(x_drinking)+factor(x_drug_use)+factor(x_smoking),
  design = design_sad_0,
  family = quasibinomial()
)
sad0_idea <- round(exp(cbind(OR = coef(logit4), confint(logit4))),2)
df_sad0_idea <- as.data.frame(sad0_idea)
df_sad0_idea

#자살계획 독립변수들간 로지스틱회귀분석

logit5 <- svyglm(
  formula = y_plan ~ factor(x_economy_covid19)+factor(x_sex)+factor(x_school_year)+factor(x_academic_grade)+factor(x_economic_status)+factor(x_living_with_family)+factor(x_financial_help)+factor(x_health_status)+factor(x_sleep_status)+factor(x_perceived_stress)+factor(x_GAD)+factor(x_loneliness)+factor(x_violence)+factor(x_drinking)+factor(x_drug_use)+factor(x_smoking),
  design = design_sad_0,
  family = quasibinomial()
)
sad0_plan <- round(exp(cbind(OR = coef(logit5), confint(logit5))),2)
df_sad0_plan <- as.data.frame(sad0_plan)
df_sad0_plan

#자살시도 독립변수들간 로지스틱회귀분석

logit6 <- svyglm(
  formula = y_attempt ~ factor(x_economy_covid19)+factor(x_sex)+factor(x_school_year)+factor(x_academic_grade)+factor(x_economic_status)+factor(x_living_with_family)+factor(x_financial_help)+factor(x_health_status)+factor(x_sleep_status)+factor(x_perceived_stress)+factor(x_GAD)+factor(x_loneliness)+factor(x_violence)+factor(x_drinking)+factor(x_drug_use)+factor(x_smoking),
  design = design_sad_0,
  family = quasibinomial()
)
sad0_attempt <- round(exp(cbind(OR = coef(logit6), confint(logit6))),2)
df_sad0_attempt <- as.data.frame(sad0_attempt)
df_sad0_attempt

#모두합치기

table2 <- cbind(df_sad1_idea, df_sad1_plan, df_sad1_attempt, df_sad0_idea, df_sad0_plan, df_sad0_attempt)

#이름지정
rownames(table2) <- c("Intercept", "Economy_COVID19_agree", "Female", "Middle_school_1st", "Middle_school_2nd", "Middle_school_3rd", 
                      "High_school_1st", "High_school_2nd", "Academic_grade_middle", "Academic_grade_lower", "Economic_status_middle", 
                      "Economic_status_low", "Living_with_relatives", "Living_boarding_house", "Living_dormitory", "Living_orphanage", 
                      "Financial_help_yes", "Health_status_normal", "Health_status_bad", "Sleep_status_normal", "Sleep_status_bad", 
                      "Perceived_stress_much", "Perceived_stress_normal", "GAD_mild", "GAD_moderate", "GAD_severe", "Loneliness_much", 
                      "Loneliness_normal", "Violence_yes", "Drinking_yes", "Drug_use_yes", "Smoking_yes")

colnames(table2) <- c("Suicidal ideation_Dep" ,"95%CI(LOW)","95%CI(HIGH)", "Suicide plan_Dep" ,"95%CI(LOW)","95%CI(HIGH)", "Suicide attempt_Dep","95%CI(LOW)","95%CI(HIGH)", "Suicidal ideation_Nodep" , "95%CI(LOW)","95%CI(HIGH)","Suicide plan_Nodep" ,"95%CI(LOW)","95%CI(HIGH)", "Suicide attempt_Nodep","95%CI(LOW)","95%CI(HIGH)")


write.csv(table2, "Table2bymara.csv")

