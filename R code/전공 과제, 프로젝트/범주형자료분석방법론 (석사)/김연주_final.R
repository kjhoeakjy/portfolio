#Final exam
#김연주 2021250461
#1-a
# 데이터 입력
data1 = data.frame(
  Therapy = rep(c("A", "S"), each = 8),
  Gender = rep(c("Male", "Female"), each = 4, times = 2),
  Response = rep(c(1, 2, 3, 4), times = 4), # 1=pd, 2=nc, 3=pr, 4=cr
  Count = c(41, 44, 20, 20, 12, 7, 3, 1, 28, 45, 29, 26, 4, 12, 5, 2)
)
data1

data1$Response <- factor(data1$Response, levels = c(1, 2, 3, 4))
data1$Response <- relevel(data1$Response, ref = "2") # 기준 범주 설정


#Baseline-Category Logit 모델 적합
library(VGAM)

fit1 <- vglm(Response ~ Therapy + Gender, family = multinomial(refLevel = "2"), weights = Count, data = data1)


summary(fit1)

#1-c
summary_fit1 = summary(fit1)

# 계수와 표준 오차 추출
coefficients = coef(summary_fit1)
standard_errors = sqrt(diag(vcov(fit1)))

# z-값 계산
z_values =coefficients / standard_errors

# p-값 계산
p_values = 2 * (1 - pnorm(abs(z_values)))

# z-값과 p-값 출력
z_values
p_values


# 범주 4와 관련된 계수 계산:다른 범주로부터 간접적으로 계산됨
vcov_matrix = vcov(fit1)
logit_4_coeff = -1 * (coef(fit1)["(Intercept):1"] + coef(fit1)["(Intercept):3"])

se_4 = sqrt(
  vcov_matrix["(Intercept):1", "(Intercept):1"] + vcov_matrix["(Intercept):3", "(Intercept):3"] + 2 * vcov_matrix["(Intercept):1", "(Intercept):3"])

z_4 = logit_4_coeff / se_4
p_4 = 2 * (1 - pnorm(abs(z_4)))

cat("범주 4 (Complete Remission)의 결과:\n")
cat("Estimate (Logit):", logit_4_coeff, "\n")
cat("SE:", se_4, "\n")
cat("z-value:", z_4, "\n")
cat("p-value:", p_4, "\n")

#1-d
# 모델 계수 추출
coefficients = coef(fit1)

# 범주 1 (Progressive Disease)의 로짓 값
logit_1 = coefficients["(Intercept):1"] + 
  coefficients["TherapyS:1"] * 1 + coefficients["GenderMale:1"] * 1 

# 범주 3 (Partial Remission)의 로짓 값
logit_3 = coefficients["(Intercept):3"] + 
  coefficients["TherapyS:3"] * 1 +
  coefficients["GenderMale:3"] * 1

# 기준 범주 (No Change)의 로짓 값은 0
logit_2 = 0

# 범주 4의 로짓 값
logit_4 = -log(1 + exp(logit_1) + exp(logit_3))

# 각 범주의 지수화된 값 계산
exp_logits = c(exp(logit_1), exp(logit_3), exp(logit_4), 1) 

# 각 범주의 확률 계산
exp_logits = c(exp(logit_1), 1, exp(logit_3), exp(logit_4))
probabilities = exp_logits / sum(exp_logits)

# 범주 4의 확률 출력
cat("범주 4 (Complete Remission)의 로짓 값:", logit_4, "\n")
cat("범주 4 (Complete Remission)의 확률:", probabilities[4], "\n")

#1-e
library(VGAM)

# Response를 순서형 변수로 변환
data1$Response = ordered(data1$Response, levels = c(1, 2, 3, 4))  # 1=pd, 2=nc, 3=pr, 4=cr

# Cumulative Logit Model 적합
fit1_cum = vglm(Response ~ Therapy + Gender, family = cumulative(parallel = TRUE),  weights = Count, data = data1)

summary(fit1_cum)

#1-g
# TherapyS 계수 추출
therapy_effect <- coefficients[grep("TherapyS", names(coefficients))]
therapy_effect

# 계수와 표준 오차 추출
coefficients = coef(summary_fit_cum)
z_values = coefficients[, "Estimate"] / coefficients[, "Std. Error"] 
p_values = 2 * (1 - pnorm(abs(z_values))) 

results = data.frame(
  Estimate = coefficients[, "Estimate"],
  Std.Error = coefficients[, "Std. Error"],
  z.value = z_values,
  p.value = p_values
)
results

#1-h
coefficients = coef(fit1_cum)

# 누적 로짓 계산
logit_1 = coefficients["(Intercept):1"] + coefficients["TherapyS"] * 1 + coefficients["GenderMale"] * 1
logit_2 = coefficients["(Intercept):2"] + coefficients["TherapyS"] * 1 + coefficients["GenderMale"] * 1
logit_3 = coefficients["(Intercept):3"] + coefficients["TherapyS"] * 1 + coefficients["GenderMale"] * 1

# 누적 확률 계산
cum_prob_1 = exp(logit_1) / (1 + exp(logit_1))
cum_prob_2 = exp(logit_2) / (1 + exp(logit_2))
cum_prob_3 = exp(logit_3) / (1 + exp(logit_3))

# 각 범주의 확률 계산
prob_1 = cum_prob_1 
prob_2 = cum_prob_2 - cum_prob_1 
prob_3 = cum_prob_3 - cum_prob_2
prob_4 = 1 - cum_prob_3 

cat("Complete Remission (범주 4) 확률:", prob_4, "\n")

########################################
#2-a
# 데이터 입력
data2 = array(c(59, 109, 78, 205, 103, 87, 32, 42),
              dim = c(2, 2, 2),
              dimnames = list(
                Worker = c("Low", "High"),
                Supervisor = c("Low", "High"),
                Quality = c("Good", "Bad")
              ))
data2

library(MASS)
# Full Model
full_model <- loglm(~ Quality * Supervisor * Worker, data = data2)

# 모델 결과 출력
summary(full_model)

# Reduced Model (No three-factor interaction)
reduced_model <- loglm(~ (Quality + Supervisor + Worker)^2, data = data2)

# 모델 결과 출력
summary(reduced_model)

# LRT
anova(reduced_model, full_model, test = "Chisq")


#2.b

# (QS, QW) 모델 적합
fit2.2 = loglm(~ Quality + Supervisor + Worker + Quality:Supervisor + Quality:Worker, data2)

# (QW, SW) 모델 적합
fit2.3 = loglm(~ Quality + Supervisor + Worker + Quality:Worker + Supervisor:Worker, data2)

# (SW, QS) 모델 적합
fit2.4 = loglm(~ Quality + Supervisor + Worker + Supervisor:Worker + Quality:Supervisor, data2)

# (QS, QW, SW) 모델 적합
fit2.5 = loglm(~ Quality + Supervisor + Worker + Quality:Supervisor + Quality:Worker + Supervisor:Worker, data2)

# 각 모델 적합도 출력
summary(fit2.2)  
summary(fit2.3)  
summary(fit2.4)  
summary(fit2.5)  

# 모델 비교 (Likelihood Ratio Test)
anova(fit2.2, fit2.5) 
anova(fit2.3, fit2.5) 
anova(fit2.4, fit2.5)  

#2.c
# 예상값 입력
expected <- fitted(fit2.5)
expected

# Odds Ratio 계산 함수
compute_odds_ratio <- function(expected, quality) {
  # Supervisor = Low
  odds_low <- expected["Low", "Low", quality] / expected["High", "Low", quality]
  
  # Supervisor = High
  odds_high <- expected["Low", "High", quality] / expected["High", "High", quality]
  
  # Odds Ratio
  odds_ratio <- odds_high / odds_low
  return(list(odds_low = odds_low, odds_high = odds_high, odds_ratio = odds_ratio))
}

# Quality = Good
result_good <- compute_odds_ratio(expected, "Good")
cat("Quality = Good:\n")
cat("  Odds (Supervisor = Low):", result_good$odds_low, "\n")
cat("  Odds (Supervisor = High):", result_good$odds_high, "\n")
cat("  Odds Ratio:", result_good$odds_ratio, "\n\n")

# Quality = Bad
result_bad <- compute_odds_ratio(expected, "Bad")
cat("Quality = Bad:\n")
cat("  Odds (Supervisor = Low):", result_bad$odds_low, "\n")
cat("  Odds (Supervisor = High):", result_bad$odds_high, "\n")
cat("  Odds Ratio:", result_bad$odds_ratio, "\n")


#2.d
# 데이터 입력
data2 <- array(c(59, 78, 103, 32, 109, 205, 87, 42),
               dim = c(2, 2, 2),
               dimnames = list(
                 Worker = c("Low", "High"),
                 Supervisor = c("Low", "High"),
                 Quality = c("Good", "Bad")
               ))

# 로그선형 모델 적합
fit_loglinear <- loglm(~ Quality + Supervisor + Worker + 
                         Quality:Supervisor + Quality:Worker + Supervisor:Worker, data2)

# 예상값 추출
expected <- fitted(fit_loglinear)

# 로그 오즈 계산 (Good vs Bad)
log_odds_loglinear <- log(expected[,, "Good"] / expected[,, "Bad"])
cat("Log Odds (Loglinear Model):\n")
print(log_odds_loglinear)

# 데이터를 데이터프레임으로 변환
data_frame <- as.data.frame(as.table(data2))
colnames(data_frame) <- c("Worker", "Supervisor", "Quality", "Freq")

# Quality Good (1)과 Bad (0)으로 변환
data_frame$Quality <- ifelse(data_frame$Quality == "Good", 1, 0)

# 데이터를 Good과 Bad로 나누어 병합
good <- subset(data_frame, Quality == 1)
bad <- subset(data_frame, Quality == 0)
data_logit <- merge(good, bad, by = c("Supervisor", "Worker"), suffixes = c("_Good", "_Bad"))

# 로짓 모델 적합
fit_logit <- glm(cbind(Freq_Good, Freq_Bad) ~ Supervisor * Worker, 
                 family = binomial(link = "logit"), 
                 data = data_logit)

# 로짓 모델 계수 기반 로그 오즈 계산
logit_coeff <- coef(fit_logit)
log_odds_logit <- matrix(NA, nrow = 2, ncol = 2, dimnames = list(c("Low", "High"), c("Low", "High")))

for (w in c("Low", "High")) {
  for (s in c("Low", "High")) {
    intercept <- logit_coeff["(Intercept)"]
    sup_effect <- ifelse(s == "High", logit_coeff["SupervisorHigh"], 0)
    work_effect <- ifelse(w == "High", logit_coeff["WorkerHigh"], 0)
    interaction_effect <- ifelse(s == "High" & w == "High", logit_coeff["SupervisorHigh:WorkerHigh"], 0)
    
    log_odds_logit[w, s] <- intercept + sup_effect + work_effect + interaction_effect
  }
}

cat("Log Odds (Logit Model):\n")
print(log_odds_logit)





#3.a
# 데이터 입력
data3= matrix(c(9,16,37,82), nrow = 2, byrow = TRUE, dimnames = list("MI Controls" = c("Diabetes","No Diabetes"), "MI Cases" = c("Diabetes", "No Diabetes")))
data3

# McNemar's Test
mcnemar.test(data3)

#3.b
n=9+16+37+82
d=(16-37)/n
se=sqrt(16+37)/n
d+1.96*se
d-1.96*se


#4.a
# 데이터 입력
data4 = array(c(
  11, 7, 8, 3, 11, 4, 11, 16,  # Female Junior
  29, 8, 7, 9, 23, 2, 7, 14,   # Female Senior
  33, 19, 45, 43, 13, 39, 29, 19,  # Male Junior
  11, 23, 67, 12, 6, 16, 17, 19  # Male Senior
), dim = c(2, 2, 2, 2, 2),
dimnames = list(
  First = c("Not Obese", "Obese"),
  Second = c("Not Obese", "Obese"),
  Third = c("Not Obese", "Obese"),
  Gender = c("Female", "Male"),
  AgeGroup = c("Junior", "Senior")
))
data4

# 데이터 프레임 형태로 변환
data4 = as.data.frame.table(data)
colnames(data4) = c("First", "Second", "Third", "Gender", "AgeGroup", "Count")

# 데이터 확인
head(data4)

# GEE 모델 적합
library(geepack)
data4$First = ifelse(data4$First == "Not Obese", 0, 1)
gee_model <- geeglm(First ~ Second + Third + Gender + AgeGroup, 
                    family = binomial(link = "logit"), 
                    data = data4, 
                    id = interaction(Gender, AgeGroup), 
                    weights = Count)  # Count를 가중치로 설정

summary(gee_model)


