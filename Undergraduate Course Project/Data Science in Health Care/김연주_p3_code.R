#Problem Set3
#김연주
#2021250461

library(foreign)
library(devtools)
library(tidyverse)
library(readr)
library(readxl)
library(haven)
library(survey)
library(dplyr)
library(devtools)
install_github("e-mitchell/meps_r_pkg/MEPS")
library(MEPS)

options(survey.lonely.psu='adjust');

######################################
#9
#(a)
#데이터 로드

d2021 = read_MEPS(year = 2021, type = "FYC")

#변수 선택
d_21=d2021 %>%
  select(TOTEXP21,PHQ242,K6SUM42,AGE21X,SEX,RACETHX,INSCOV21,POVCAT21,MIDX,STRKDX,HIBPDX,DIABDX_M18,ASTHDX,CANCERDX,CHOLDX,PERWT21F) %>%
  mutate(PHQ242 = ifelse(PHQ242 %in% c(-1, -15), NA, PHQ242),
         K6SUM42 = ifelse(K6SUM42 %in% c(-1, -15), NA, K6SUM42)) %>%
  mutate(MTILL=ifelse(PHQ242>=3|K6SUM42>=13,1,0)) %>%
  mutate(
    MIDX = ifelse(MIDX %in% c(1, 2), ifelse(MIDX == 2, 0, 1), NA),
    STRKDX = ifelse(STRKDX %in% c(1, 2), ifelse(STRKDX == 2, 0, 1), NA),
    HIBPDX = ifelse(HIBPDX %in% c(1, 2), ifelse(HIBPDX == 2, 0, 1), NA),
    DIABDX_M18 = ifelse(DIABDX_M18 %in% c(1, 2), ifelse(DIABDX_M18 == 2, 0, 1), NA),
    ASTHDX = ifelse(ASTHDX %in% c(1, 2), ifelse(ASTHDX == 2, 0, 1), NA),
    CANCERDX = ifelse(CANCERDX %in% c(1, 2), ifelse(CANCERDX == 2, 0, 1), NA),
    CHOLDX = ifelse(CHOLDX %in% c(1, 2), ifelse(CHOLDX == 2, 0, 1), NA)
  ) %>%
  filter(AGE21X >= 18) %>%
  drop_na()

# 데이터 확인
summary(d_21)
head(d_21)




#(b)
library(dplyr)
library(broom)

#종속변수의 0을 매우 작은 양수로 대체
d_21=d_21 %>%
  mutate(TOTEXP21 = ifelse(TOTEXP21 == 0, 0.0001, TOTEXP21))

fit1=glm(TOTEXP21 ~ MTILL, family = Gamma(link = "log"), weights = PERWT21F, data = d_21)

# 결과 요약 출력
summary(fit1)




#(c)
fit2=glm(MTILL ~ AGE21X + SEX + RACETHX + INSCOV21 + POVCAT21 + MIDX + STRKDX + HIBPDX + DIABDX_M18 + ASTHDX + CANCERDX + CHOLDX, family = binomial(link = "logit"), data = d_21)

summary(fit2)

# Add propensity scores to the dataset
d_21=d_21 %>%
  mutate(prop_score = predict(fit2, type = "response"))

# Compare propensity score distributions
library(ggplot2)


# Density plot of propensity scores
ggplot(d_21, aes(x = prop_score, fill = factor(MTILL))) + geom_density(alpha = 0.5) + labs(title = "Propensity Score Distribution by Mental Illness Status", x = "Propensity Score", fill = "Mental Illness") + theme_minimal()




#(d)
library(MatchIt)

beta_glm = coef(summary(fit1))["MTILL", "Estimate"]

fit4=matchit(MTILL ~ AGE21X + SEX + RACETHX + INSCOV21 + POVCAT21 + MIDX + STRKDX + HIBPDX + DIABDX_M18 + ASTHDX + CANCERDX + CHOLDX, data = d_21, method = "nearest", distance = "logit")
matched=match.data(fit4)

summary(fit4)
summary(matched)

# 매칭된 데이터에서 GLM 적합
fit_psm = glm(TOTEXP21 ~ MTILL, family = Gamma(link = "log"), weights = PERWT21F, data = matched)

summary(fit_psm)

# 기존 GLM 결과와 매칭 기반 GLM 결과 비교
beta_glm = coef(summary(fit1))["MTILL", "Estimate"]
beta_psm = coef(summary(fit_psm))["MTILL", "Estimate"]

# 절대 및 상대 차이 계산
absolute_diff = abs(beta_psm - beta_glm)
relative_diff = (absolute_diff / abs(beta_glm)) * 100

cat("Absolute Difference:", absolute_diff, "\n")
cat("Relative Difference (%):", relative_diff, "\n")

library(ggplot2)

plot(fit4, type = "jitter", interactive = FALSE)
plot(fit4, type = "hist")



#10
#(a)
library(haven)
data=read_dta("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\보건의료데이터과학\\CK1994.dta",encoding='UTF-8')

# 데이터 확인
head(data)
summary(data)

library(dplyr)

data_1=data %>%
  select(store, chain, co_owned, state, time, empft, emppt) %>% 
  filter(!is.na(empft), !is.na(emppt))

# 데이터 요약 확인
summary(data_1)
head(data_1)


#(c)
library(dplyr)
library(ggplot2)

wage_data = data %>%
  select(state, time, wage_st) %>%
  filter(!is.na(wage_st)) 

ggplot(wage_data, aes(x = factor(time), y = wage_st, fill = factor(state))) + geom_boxplot(alpha = 0.7, outlier.shape = NA) + geom_jitter(width = 0.2, alpha = 0.4, aes(color = factor(state))) + labs(title = "Hourly Wage Before and After Policy Implementation", x = "Time (0 = Before, 1 = After)", y = "Hourly Wage", fill = "State (0 = PA, 1 = NJ)") + theme_minimal()



#(d)
#평행추세가정: 불가능. 정책 시행 이전에 대한 시간 변수가 부재함. 
#외생적 개입 없음: 데이터가 최저임금 정책 외 다른 정책적, 경제적 변화 포함하지 않아서 직접 검증 불가. 경제적 지표, 사회적 요인 등이 필요.
#처리 효과의 동일성: 정책이 처리군의 모든 지점에 동일한 영향 미치는지. 이건 chain, co_owned 등으로로 검증 가능. 각 지점에서 실제 임금 준수 여부를 나타내는 변수가 있다면 더욱 구체적인 검증 가능. 
# 브랜드(chain) 및 소유(co_owned)별 처리 효과 차이 확인
did_model = lm(empft ~ time * state + chain + co_owned, data = data)
summary(did_model)


#(e)

data_1 = data %>%
  select(state, time, empft, emppt) %>%
  filter(!is.na(empft), !is.na(emppt))

# 정책 전후 그룹별 평균 값 계산
group_summary = data_1 %>%
  group_by(state, time) %>%
  summarise(mean_empft = mean(empft, na.rm = TRUE), mean_emppt = mean(emppt, na.rm = TRUE))

print(group_summary)

# 시각화: 정책 전후 풀타임 고용 평균
ggplot(data_1, aes(x = factor(time), y = empft, fill = factor(state))) + geom_bar(stat = "summary", fun = "mean", position = "dodge") + labs(title = "Full-Time Employment Before and After Policy", x = "Time (0 = Before, 1 = After)", y = "Mean Full-Time Employment", fill = "State (0 = PA, 1 = NJ)") + theme_minimal()

# 시각화: 정책 전후 파트타임 고용 평균
ggplot(data_1, aes(x = factor(time), y = emppt, fill = factor(state))) + geom_bar(stat = "summary", fun = "mean", position = "dodge") + labs(title = "Part-Time Employment Before and After Policy", x = "Time (0 = Before, 1 = After)", y = "Mean Part-Time Employment", fill = "State (0 = PA, 1 = NJ)") + theme_minimal()

# DiD 회귀 분석: 풀타임 고용
did_ft = lm(empft ~ time * state, data = data_1)
summary(did_ft)

# DiD 회귀 분석: 파트타임 고용
did_pt = lm(emppt ~ time * state, data = data_1)
summary(did_pt)


