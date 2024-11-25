# library 세팅 --------------------------------------------------------------
library('survey')
library('dplyr')
# Read Data ---------------------------------------------------------------
d2020=read.csv("../../teen_suicide/csv/kyrbs2020.csv")
d2021=read.csv("../../teen_suicide/csv/kyrbs2021.csv")
d2022=read.csv("../../teen_suicide/csv/kyrbs2022.csv")
d2023=read.csv("../../teen_suicide/csv/kyrbs2023.csv")

# 데이터프레임 합치기
df <- dplyr::bind_rows(d2020, d2021, d2022, d2023)
# make variable names -----------------------------------------------------
id = 1:length(df$OBS)
#자살생각
y_idea <- df$M_SUI_CON 
#자살계획
y_plan <- df$M_SUI_PLN 
#자살시도
y_attempt <- df$M_SUI_ATT 

#주중 스마트폰 사용 시간 (분)
smartphone_use_time_weekdays <- df$INT_SPWD_TM
#주말 스마트폰 사용 시간 (분)
smartphone_use_time_weekend <- df$INT_SPWK_TM

#우울감 경험
sad <- df$M_SAD
#성별
x_sex <- df$SEX 
#학년
x_school_year <- df$GRADE 
#학업성적
x_academic_grade <- df$E_S_RCRD 
#경제상태
x_economic_status <- df$E_SES 
#거주형태
x_living_with_family <- df$E_RES 
#가정형편으로 인한 경제적 도움
x_financial_help <- df$E_AID 
#주관적 건강인지
x_health_status <- df$PR_HT 
#주관적 수면 충족
x_sleep_status <- df$M_SLP_EN 
#스트레스
x_perceived_stress <- df$M_STR 
#범불안장애 불러오기
GAD1 <- df$M_GAD_1 
GAD2 <- df$M_GAD_2
GAD3 <- df$M_GAD_3
GAD4 <- df$M_GAD_4
GAD5 <- df$M_GAD_5
GAD6 <- df$M_GAD_6
GAD7 <- df$M_GAD_7
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
#외로움 경험
x_loneliness <- df$M_LON 
#폭력 경험
x_violence <- df$V_TRT 
#음주 여부
x_drinking <- df$AC_DAYS 
#약물사용경험
x_drug_use <- df$DR_HAB 
#흡연여부
x_smoking <- df$TC_DAYS 

# Categorizing the Variables ----------------------------------------------
#자살생각 있다=1, 없다=0
y_idea <- case_when(y_idea == 2 ~ 1,
                    y_idea == 1 ~ 0)
#자살계획 있다=1, 없다=0
y_plan <- case_when(y_plan == 2 ~ 1,
                    y_plan == 1 ~ 0)
#자살시도 있다=1, 없다=0
y_attempt <- case_when(y_attempt == 2 ~ 1,
                       y_attempt == 1 ~ 0)


#우울감 경험 있다=1, 없다=0
sad <- case_when(sad == 2 ~ 1,
                 sad == 1 ~ 0)
#성별 남자=0, 여자=1
x_sex <- case_when(x_sex == 1 ~ 0,
                   x_sex == 2 ~ 1)
#학년 고3=0, 중1=1, 중2=2, 중3=3, 고1=4, 고2=5
x_school_year <- case_when(x_school_year == 6 ~0 ,
                           x_school_year == 1 ~1,
                           x_school_year == 2 ~2,
                           x_school_year == 3 ~3,
                           x_school_year == 4 ~4,
                           x_school_year == 5 ~5)

#학업성적 
#상 (상,중상) = 0
#중 (중) = 1
#하 (중하, 하) = 2
x_academic_grade <- case_when(x_academic_grade == 1 ~ 0,
                              x_academic_grade == 2 ~ 0,
                              x_academic_grade == 3 ~ 1,
                              x_academic_grade == 4 ~ 2,
                              x_academic_grade == 5 ~ 2)
#경제상태
#상 (상,중상) = 0
#중 (중) = 1
#하 (중하, 하) = 2
x_economic_status <- case_when(x_economic_status == 1 ~ 0,
                               x_economic_status == 2 ~ 0,
                               x_economic_status == 3 ~ 1,
                               x_economic_status == 4 ~ 2,
                               x_economic_status == 5 ~ 2)

#거주형태
#가족=0, 친척집=1, 하숙및자취=2, 기숙사=3, 보육시설=4
x_living_with_family <- case_when(x_living_with_family ==1 ~ 0,
                                  x_living_with_family == 2 ~ 1,
                                  x_living_with_family == 3 ~ 2,
                                  x_living_with_family == 4 ~ 3,
                                  x_living_with_family == 5 ~ 4)

#가정형편으로 인한 경제적 도움 받은적 있다 = 1, 없다 = 0
x_financial_help <- case_when(x_financial_help == 2 ~ 1,
                              x_financial_help == 1 ~ 0)
#주관적 건강인지
#건강 (매우건강, 건강) = 0
#보통 (보통) = 1
#건강x (매우건강x, 건강x) = 2
x_health_status <- case_when(x_health_status == 1 ~ 0,
                             x_health_status == 2 ~ 0,
                             x_health_status == 3 ~ 1,
                             x_health_status == 4 ~ 2,
                             x_health_status == 5 ~ 2)
#주관적 수면 충족
#충분 (매우충분, 충분) = 0
#보통 (보통) = 1
#불충분 (전혀충분x, 불충분) = 2
x_sleep_status <- case_when(x_sleep_status == 1 ~ 0,
                            x_sleep_status == 2 ~ 0,
                            x_sleep_status == 3 ~ 1,
                            x_sleep_status == 4 ~ 2,
                            x_sleep_status == 5 ~ 2)
#스트레스
#낮음 (전혀x, 느낌x) = 0
#높음 (대단히많이, 많이) = 1
#중간 (조금) = 2
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
#낮음 (전혀x, 거의 느낌x) = 0
#중간 (가끔) = 1
#높음 (항상느낌, 자주느낌) = 2
x_loneliness <- case_when(x_loneliness == 5 ~ 2,
                          x_loneliness == 4 ~ 2,
                          x_loneliness == 3 ~ 1,
                          x_loneliness == 2 ~ 0,
                          x_loneliness == 1 ~ 0)

#폭력 경험 있다=1, 없다=0
x_violence <- case_when(x_violence == 1 ~ 0,
                        x_violence %in% 2:7 ~ 1)

#음주 여부 있다=1, 없다=0, 비해당= 0
x_drinking <- case_when(x_drinking == 1 ~ 0,
                        x_drinking == 9999 ~ 0,
                        x_drinking %in% 2:7 ~ 1)

#약물 사용 경험 있다=1, 없다=0
x_drug_use <- case_when(x_drug_use == 2 ~ 1,
                        x_drug_use == 1 ~ 0)

#흡연 여부 있다=1, 없다=0, 비해당= 0
x_smoking <- case_when(x_smoking == 1 ~ 0,
                       x_smoking == 9999 ~ 0,
                       x_smoking %in% 2:7 ~ 1)

#CLUSTER ,STRATA ,W
CLUSTER <- df$CLUSTER
STRATA <- df$STRATA
W <- df$W

#주중스마트폰 사용시간 NA값 처리(0분으로)
smartphone_use_time_weekdays<- replace(smartphone_use_time_weekdays, 
                                       is.na(smartphone_use_time_weekdays), 0)



# -------------------------------------------------------------------------
df_merge = data.frame(id, y_idea, y_plan, y_attempt, sad, x_sex, 
                      x_school_year, x_academic_grade, x_economic_status, 
                      x_living_with_family, x_financial_help, x_health_status, 
                      x_sleep_status, x_perceived_stress, x_GAD, x_loneliness, 
                      x_violence, x_drinking, x_drug_use, x_smoking, CLUSTER, 
                      STRATA, W, smartphone_use_time_weekdays)
# 년도별 dataframe 나누기 -------------------------------------------------------

Y2020 <- subset(df_merge, substr(STRATA, 1, 4) == "2020") 
#결측치 x
Y2021 <- subset(df_merge, substr(STRATA, 1, 4) == "2021") 
#결측치 x
Y2022 <- subset(df_merge, substr(STRATA, 1, 4) == "2022")
# 마약 관련 x ,재정적 도움 x
Y2023 <- subset(df_merge, substr(STRATA, 1, 4) == "2023")
# 마약 관련 x

Y2022 <- subset(Y2022, select = -c(x_drug_use,x_financial_help))
Y2023 <- subset(Y2023, select = -c(x_drug_use))
# 스마트폰중독 점수 ---------------------------------------------------------------

#주중 스마트폰 사용 시간 평균 산출

Y2020mean <- mean(Y2020$smartphone_use_time_weekdays)
Y2021mean <- mean(Y2021$smartphone_use_time_weekdays)
Y2022mean <- mean(Y2022$smartphone_use_time_weekdays)
Y2023mean <- mean(Y2023$smartphone_use_time_weekdays)

#주중 스마트폰 사용 시간 평균보다크다=1, 평균보다 작거나 같다=0
Y2020$smartphone_use_time_weekdays<- case_when(
  Y2020$smartphone_use_time_weekdays > Y2020mean ~ 1,
  Y2020$smartphone_use_time_weekdays <= Y2020mean ~ 0)

Y2021$smartphone_use_time_weekdays<- case_when(
  Y2021$smartphone_use_time_weekdays > Y2021mean ~ 1,
  Y2021$smartphone_use_time_weekdays <= Y2021mean ~ 0)

Y2022$smartphone_use_time_weekdays<- case_when(
  Y2022$smartphone_use_time_weekdays > Y2022mean ~ 1,
  Y2022$smartphone_use_time_weekdays <= Y2022mean ~ 0)

Y2023$smartphone_use_time_weekdays<- case_when(
  Y2023$smartphone_use_time_weekdays > Y2023mean ~ 1,
  Y2023$smartphone_use_time_weekdays <= Y2023mean ~ 0)




# design ------------------------------------------------------------------
# 연도별 데이터셋 이름 정의
years <- 2020:2023
data_names <- paste0("Y", years)

# svydesign 객체를 리스트로 생성
design_list <- lapply(data_names, function(data_name) {
  svydesign(
    ids = ~CLUSTER,
    strata = ~STRATA,
    weights = ~W,
    data = get(data_name), # 데이터프레임 동적 호출
    nest = TRUE
  )
})

# 리스트의 각 요소에 이름 지정
names(design_list) <- paste0("design", years)
# 개별 객체로 저장
list2env(design_list, envir = .GlobalEnv)

# 우울 비우울 그룹 별 design ------------------------------------------------------
design_names <- paste0("design", years)
# sad 조건별로 분리
sad_design_list <- lapply(design_names, function(design_name) {
  design <- get(design_name)
  list(
    sad = subset(design, sad == 1),
    NOsad = subset(design, sad == 0)
  )
})

# 리스트의 이름 지정
names(sad_design_list) <- design_names

# 개별 객체로 저장
for (year in years) {
  assign(paste0("design", year, "_sad"), sad_design_list[[paste0("design", year)]]$sad)
  assign(paste0("design", year, "_NOsad"), sad_design_list[[paste0("design", year)]]$NOsad)
}




# 로지스틱회귀분석 ----------------------------------------------------------------
# 독립 변수 목록 정의
independent_vars <- c(
  "smartphone_use_time_weekdays", "x_sex", "x_school_year", 
  "x_academic_grade", "x_economic_status", "x_living_with_family", 
  "x_financial_help", "x_health_status", "x_sleep_status", 
  "x_perceived_stress", "x_GAD", "x_loneliness", "x_violence", 
  "x_drinking", "x_drug_use", "x_smoking")

# 공식 생성
formula_idea <- paste("y_idea ~", paste0("factor(", independent_vars, ")", collapse = "+"))
formula_plan <- paste("y_plan ~", paste0("factor(", independent_vars, ")", collapse = "+"))
formula_attemt <- paste("y_attempt ~", paste0("factor(", independent_vars, ")", collapse = "+"))



# 2020year ----------------------------------------------------------------
idea_sad_2020 <- svyglm(
  formula = as.formula(formula_idea),
  design = design2020_sad,
  family = quasibinomial()
)

idea_nosad_2020 <- svyglm(
  formula = as.formula(formula_idea),
  design = design2020_NOsad,
  family = quasibinomial()
)

plan_sad_2020 <- svyglm(
  formula = as.formula(formula_plan),
  design = design2020_sad,
  family = quasibinomial()
)

plan_nosad_2020 <- svyglm(
  formula = as.formula(formula_plan),
  design = design2020_NOsad,
  family = quasibinomial()
)

attempt_sad_2020 <- svyglm(
  formula = as.formula(formula_attemt),
  design = design2020_sad,
  family = quasibinomial()
)

attempt_nosad_2020 <- svyglm(
  formula = as.formula(formula_attemt),
  design = design2020_NOsad,
  family = quasibinomial()
)

LR_sad_idea_2020 <- round(exp(cbind(OR = coef(idea_sad_2020), confint(idea_sad_2020))),2)
LR_nosad_idea_2020 <- round(exp(cbind(OR = coef(idea_nosad_2020), confint(idea_nosad_2020))),2)

LR_sad_plan_2020 <- round(exp(cbind(OR = coef(plan_sad_2020), confint(plan_sad_2020))),2)
LR_nosad_plan_2020 <- round(exp(cbind(OR = coef(plan_nosad_2020), confint(plan_nosad_2020))),2)

LR_sad_attempt_2020 <- round(exp(cbind(OR = coef(attempt_sad_2020), confint(attempt_sad_2020))),2)
LR_nosad_attempt_2020 <- round(exp(cbind(OR = coef(attempt_nosad_2020), confint(attempt_nosad_2020))),2)



# 2021year ----------------------------------------------------------------

idea_sad_2021 <- svyglm(
  formula = as.formula(formula_idea),
  design = design2021_sad,
  family = quasibinomial()
)

idea_nosad_2021 <- svyglm(
  formula = as.formula(formula_idea),
  design = design2021_NOsad,
  family = quasibinomial()
)

plan_sad_2021 <- svyglm(
  formula = as.formula(formula_plan),
  design = design2021_sad,
  family = quasibinomial()
)

plan_nosad_2021 <- svyglm(
  formula = as.formula(formula_plan),
  design = design2021_NOsad,
  family = quasibinomial()
)


attempt_sad_2021 <- svyglm(
  formula = as.formula(formula_attemt),
  design = design2021_sad,
  family = quasibinomial()
)

attempt_nosad_2021 <- svyglm(
  formula = as.formula(formula_attemt),
  design = design2021_NOsad,
  family = quasibinomial()
)

LR_sad_idea_2021 <- round(exp(cbind(OR = coef(idea_sad_2021), confint(idea_sad_2021))),2)
LR_nosad_idea_2021 <- round(exp(cbind(OR = coef(idea_nosad_2021), confint(idea_nosad_2021))),2)

LR_sad_plan_2021 <- round(exp(cbind(OR = coef(plan_sad_2021), confint(plan_sad_2021))),2)
LR_nosad_plan_2021 <- round(exp(cbind(OR = coef(plan_nosad_2021), confint(plan_nosad_2021))),2)

LR_sad_attempt_2021 <- round(exp(cbind(OR = coef(attempt_sad_2021), confint(attempt_sad_2021))),2)
LR_nosad_attempt_2021 <- round(exp(cbind(OR = coef(attempt_nosad_2021), confint(attempt_nosad_2021))),2)


# 2022year ----------------------------------------------------------------

independent_vars_2022 <- c(
  "smartphone_use_time_weekdays", "x_sex", "x_school_year", 
  "x_academic_grade", "x_economic_status", "x_living_with_family", 
   "x_health_status", "x_sleep_status", 
  "x_perceived_stress", "x_GAD", "x_loneliness", "x_violence", 
  "x_drinking", "x_smoking")

# 공식 생성
formula_idea_2022 <- paste("y_idea ~", paste0("factor(", independent_vars_2022, ")", collapse = "+"))
formula_plan_2022 <- paste("y_plan ~", paste0("factor(", independent_vars_2022, ")", collapse = "+"))
formula_attemt_2022 <- paste("y_attempt ~", paste0("factor(", independent_vars_2022, ")", collapse = "+"))



idea_sad_2022 <- svyglm(
  formula = as.formula(formula_idea_2022),
  design = design2022_sad,
  family = quasibinomial()
)

idea_nosad_2022 <- svyglm(
  formula = as.formula(formula_idea_2022),
  design = design2022_NOsad,
  family = quasibinomial()
)

plan_sad_2022 <- svyglm(
  formula = as.formula(formula_plan_2022),
  design = design2022_sad,
  family = quasibinomial()
)

plan_nosad_2022 <- svyglm(
  formula = as.formula(formula_plan_2022),
  design = design2022_NOsad,
  family = quasibinomial()
)


attempt_sad_2022 <- svyglm(
  formula = as.formula(formula_attemt_2022),
  design = design2022_sad,
  family = quasibinomial()
)

attempt_nosad_2022 <- svyglm(
  formula = as.formula(formula_attemt_2022),
  design = design2022_NOsad,
  family = quasibinomial()
)

LR_sad_idea_2022 <- round(exp(cbind(OR = coef(idea_sad_2022), confint(idea_sad_2022))),2)
LR_nosad_idea_2022 <- round(exp(cbind(OR = coef(idea_nosad_2022), confint(idea_nosad_2022))),2)

LR_sad_plan_2022 <- round(exp(cbind(OR = coef(plan_sad_2022), confint(plan_sad_2022))),2)
LR_nosad_plan_2022 <- round(exp(cbind(OR = coef(plan_nosad_2022), confint(plan_nosad_2022))),2)

LR_sad_attempt_2022 <- round(exp(cbind(OR = coef(attempt_sad_2022), confint(attempt_sad_2022))),2)
LR_nosad_attempt_2022 <- round(exp(cbind(OR = coef(attempt_nosad_2022), confint(attempt_nosad_2022))),2)



# 2023year ----------------------------------------------------------------
independent_vars_2023 <- c(
  "smartphone_use_time_weekdays", "x_sex", "x_school_year", 
  "x_academic_grade", "x_economic_status", "x_living_with_family", 
  "x_financial_help", "x_health_status", "x_sleep_status", 
  "x_perceived_stress", "x_GAD", "x_loneliness", "x_violence", 
  "x_drinking", "x_smoking")

# 공식 생성
formula_idea_2023 <- paste("y_idea ~", paste0("factor(", independent_vars_2023, ")", collapse = "+"))
formula_plan_2023 <- paste("y_plan ~", paste0("factor(", independent_vars_2023, ")", collapse = "+"))
formula_attemt_2023 <- paste("y_attempt ~", paste0("factor(", independent_vars_2023, ")", collapse = "+"))

idea_sad_2023 <- svyglm(
  formula = as.formula(formula_idea_2023),
  design = design2023_sad,
  family = quasibinomial()
)

idea_nosad_2023 <- svyglm(
  formula = as.formula(formula_idea_2023),
  design = design2023_NOsad,
  family = quasibinomial()
)


plan_sad_2023 <- svyglm(
  formula = as.formula(formula_plan_2023),
  design = design2023_sad,
  family = quasibinomial()
)

plan_nosad_2023 <- svyglm(
  formula = as.formula(formula_plan_2023),
  design = design2023_NOsad,
  family = quasibinomial()
)


attempt_sad_2023 <- svyglm(
  formula = as.formula(formula_attemt_2023),
  design = design2023_sad,
  family = quasibinomial()
)

attempt_nosad_2023 <- svyglm(
  formula = as.formula(formula_attemt_2023),
  design = design2023_NOsad,
  family = quasibinomial()
)

LR_sad_idea_2023 <- round(exp(cbind(OR = coef(idea_sad_2023), confint(idea_sad_2023))),2)
LR_nosad_idea_2023 <- round(exp(cbind(OR = coef(idea_nosad_2023), confint(idea_nosad_2023))),2)

LR_sad_plan_2023 <- round(exp(cbind(OR = coef(plan_sad_2023), confint(plan_sad_2023))),2)
LR_nosad_plan_2023 <- round(exp(cbind(OR = coef(plan_nosad_2023), confint(plan_nosad_2023))),2)

LR_sad_attempt_2023 <- round(exp(cbind(OR = coef(attempt_sad_2023), confint(attempt_sad_2023))),2)
LR_nosad_attempt_2023 <- round(exp(cbind(OR = coef(attempt_nosad_2023), confint(attempt_nosad_2023))),2)

# make table --------------------------------------------------------------
# 공통 컬럼 이름 정의
common_colnames <- c(
  "Suicidal ideation_Dep", "95%CI(LOW)", "95%CI(HIGH)",
  "Suicide plan_Dep", "95%CI(LOW)", "95%CI(HIGH)",
  "Suicide attempt_Dep", "95%CI(LOW)", "95%CI(HIGH)",
  "Suicidal ideation_Nodep", "95%CI(LOW)", "95%CI(HIGH)",
  "Suicide plan_Nodep", "95%CI(LOW)", "95%CI(HIGH)",
  "Suicide attempt_Nodep", "95%CI(LOW)", "95%CI(HIGH)"
)

# 테이블 생성 및 컬럼 이름 적용
table_list <- lapply(years, function(year) {
  table <- cbind(
    get(paste0("LR_sad_idea_", year)),
    get(paste0("LR_sad_plan_", year)),
    get(paste0("LR_sad_attempt_", year)),
    get(paste0("LR_nosad_idea_", year)),
    get(paste0("LR_nosad_plan_", year)),
    get(paste0("LR_nosad_attempt_", year))
  )
  colnames(table) <- common_colnames
  return(table)
})

# 리스트를 연도별 이름으로 저장
names(table_list) <- paste0("table2_", years)

# 개별 테이블로 저장
list2env(table_list, envir = .GlobalEnv)

write.csv(table2_2020, "Table2_2020.csv")
write.csv(table2_2021, "Table2_2021.csv")
write.csv(table2_2022, "Table2_2022.csv")
write.csv(table2_2023, "Table2_2023.csv")

#전부 유의하지 않음

