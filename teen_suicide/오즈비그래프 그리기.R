# 패키지 로드
library(forestplot)

# 데이터 입력 (새로운 숫자 적용)
data <- data.frame(
  Group = c("Depression", "Suicide idea", "Suicide plan", "Suicide attempt",
            "No-depression", "Suicide idea", "Suicide plan", "Suicide attempt"),
  OR = c(NA, 1.31, 1.47, 1.38, NA, 1.49, 1.41, 1.48),       # Odds Ratio
  Lower = c(NA, 1.26, 1.4, 1.3, NA, 1.36, 1.32, 1.32),      # Lower CI
  Upper = c(NA, 1.36, 1.55, 1.45, NA, 1.62, 1.51, 1.67) )    # Upper CI

# 신뢰구간과 OR 값을 문자열로 병합
data$CI <- ifelse(is.na(data$OR), "",
                  paste0(format(data$OR, nsmall = 2), " (", 
                         format(data$Lower, nsmall = 2), "-", 
                         format(data$Upper, nsmall = 2), ")"))

# 포레스트 플롯 그리기
forestplot(
  labeltext = cbind(data$Group, data$CI),   # 그룹과 OR 값 표시
  mean = data$OR,
  lower = data$Lower,
  upper = data$Upper,
  xlab = "Odds Ratio (95% CI)",              # X축 라벨
  boxsize = 0.3,                             # 박스 크기 키우기
  col = fpColors(box = "darkorange",         # 박스 색상
                 line = "darkorange",        # 선 색상
                 zero = "gray50"),           # 기준선 색상
  zero = 1,                                  # 기준선
  xticks = seq(1, 2, 0.1),                   # X축 눈금 범위 및 간격 설정
  lineheight = unit(1.2, "cm"),              # 라인 간격 조정
  graph.pos = 2,                             # 그래프 위치 조정 (텍스트와 간격)
  txt_gp = fpTxtGp(
    label = gpar(fontsize = 15, family = "AppleGothic"), # 텍스트 스타일
    xlab = gpar(fontsize = 20, fontface = "bold", family = "AppleGothic"),
    ticks = gpar(fontsize = 25, family = "AppleGothic"), # 눈금 스타일
    title = gpar(fontsize = 1, fontface = "bold", family = "AppleGothic") # 제목 스타일
  ),
  hrzl_lines = list(                         # 수평선 추가
    "1" = gpar(lwd = 2.5, col = "gray50"),   # Depression 그룹 위 수평선
    "5" = gpar(lwd = 2.5, col = "gray50")    # No-depression 그룹 위 수평선
  ),
  clip = c(1, 2),                            # 그래프 범위 제한
  new_page = TRUE                            # 새 페이지에 출력
)
