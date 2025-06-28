#Problem Set1
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
#5
#(a)
#데이터 로드

d2019 = read_MEPS(year = 2019, type = "FYC")
d2020 = read_MEPS(year = 2020, type = "FYC")
d2021 = read_MEPS(year = 2021, type = "FYC")
d2022 = read_MEPS(year = 2022, type = "FYC")

#변수 선택

names(d2019)

d_19=d2019 %>%
  select(DUPERSID, VARSTR, VARPSU,IPTEXP19, OPTEXP19, OBVEXP19, ERTEXP19, RXEXP19, IPTSLF19, OPTSLF19, OBVSLF19, ERTSLF19, RXSLF19,PERWT19F) %>%
  mutate(year=2019)%>%
  mutate(total_exp=IPTEXP19 + OPTEXP19 + OBVEXP19 + ERTEXP19 + RXEXP19)%>%
  mutate(pocket_exp=IPTSLF19 + OPTSLF19 + OBVSLF19 + ERTSLF19 + RXSLF19)%>%
  mutate(perwt=PERWT19F)

head(d_19)

d_20=d2020 %>%
  select(DUPERSID, VARSTR, VARPSU,IPTEXP20, OPTEXP20, OBVEXP20, ERTEXP20, RXEXP20, IPTSLF20, OPTSLF20, OBVSLF20, ERTSLF20, RXSLF20,PERWT20F) %>%
  mutate(year=2020)%>%
  mutate(total_exp=IPTEXP20 + OPTEXP20 + OBVEXP20 + ERTEXP20 + RXEXP20)%>%
  mutate(pocket_exp=IPTSLF20 + OPTSLF20 + OBVSLF20 + ERTSLF20 + RXSLF20)%>%
  mutate(perwt=PERWT20F)

d_21=d2021 %>%
  select(DUPERSID, VARSTR, VARPSU,IPTEXP21, OPTEXP21, OBVEXP21, ERTEXP21, RXEXP21, IPTSLF21, OPTSLF21, OBVSLF21, ERTSLF21, RXSLF21,PERWT21F) %>%
  mutate(year=2021)%>%
  mutate(total_exp=IPTEXP21 + OPTEXP21 + OBVEXP21 + ERTEXP21 + RXEXP21)%>%
  mutate(pocket_exp=IPTSLF21 + OPTSLF21 + OBVSLF21 + ERTSLF21 + RXSLF21)%>%
  mutate(perwt=PERWT21F)

d_22=d2022 %>%
  select(DUPERSID, VARSTR, VARPSU,IPTEXP22, OPTEXP22, OBVEXP22, ERTEXP22, RXEXP22, IPTSLF22, OPTSLF22, OBVSLF22, ERTSLF22, RXSLF22,PERWT22F) %>%
  mutate(year=2022)%>%
  mutate(total_exp=IPTEXP22 + OPTEXP22 + OBVEXP22 + ERTEXP22 + RXEXP22)%>%
  mutate(pocket_exp=IPTSLF22 + OPTSLF22 + OBVSLF22 + ERTSLF22 + RXSLF22)%>%
  mutate(perwt=PERWT22F)

#merge

pooled=bind_rows(d_19, d_20, d_21, d_22)

pooled=pooled %>%
  mutate(poolwt = perwt / 4)

head(pooled, 20)
summary(pooled)

#(b)
#inflation 고려
inflation_factors = c('2019' = 255.657, '2020' = 258.811, '2021' = 270.970, '2022' = 292.655)

# 2022년 기준 물가 조정 비율 계산
base_year = as.numeric(inflation_factors["2022"])
inflation_adjustment = as.numeric(inflation_factors) / base_year
inflation_adjustment
inflation_adjustment=c('2019' = 0.8735781, '2020' = 0.8843553, '2021' = 0.9259025, '2022' = 1)


pooled = pooled %>%
  mutate(inflation_factor = case_when(
    as.numeric(year) == 2019 ~ inflation_adjustment['2019'],
    as.numeric(year) == 2020 ~ inflation_adjustment['2020'],
    as.numeric(year) == 2021 ~ inflation_adjustment['2021'],
    as.numeric(year) == 2022 ~ inflation_adjustment['2022'],
    TRUE ~ NA_real_ 
  ),
  total_exp_adj = total_exp / inflation_factor,
  pocket_exp_adj = pocket_exp / inflation_factor
  )
head(pooled, 20)
summary(pooled)

# Define the survey design
dsgn = svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~poolwt,
  data = pooled,
  nest = TRUE)
summary(dsgn)

#average
svymean(~total_exp_adj, dsgn)
svymean(~pocket_exp_adj, dsgn)

#(c)
# 25th, 50th, 75th percentile
svyquantile(~total_exp_adj, dsgn, c(0.25, 0.5, 0.75))
svyquantile(~pocket_exp_adj, dsgn, c(0.25, 0.5, 0.75))

#(d)
# histogram
library(ggplot2)

# total_exp_adj 히스토그램
ggplot(pooled, aes(x = total_exp_adj)) + geom_histogram(binwidth = 5000, fill = "blue") +
labs(x = "Total Expense") +theme_minimal()

# 로그 변환 histogram
ggplot(pooled, aes(x = log(0.01+total_exp_adj))) + geom_histogram(binwidth = 0.5, fill = "blue") +
  labs(x = "Log of Total Expense") +theme_minimal()  

# pocket_exp_adj 히스토그램
ggplot(pooled, aes(x = pocket_exp_adj)) + geom_histogram(binwidth = 1000, fill = "red") +
  labs(x = "Pocket Expense") +theme_minimal() 

# 로그 변환 histogram
ggplot(pooled, aes(x = log(1+pocket_exp_adj))) + geom_histogram(binwidth = 0.5, fill = "red") +
  labs(x = "Log of Pocket Expense") +theme_minimal()  

########################################
#6.
#(a)
#데이터로드
d2020 = read_MEPS(year = 2020, type = "FYC")
d2022 = read_MEPS(year = 2022, type = "FYC")
d_2020=d2020 %>%
  select(TOTEXP20, AGE20X, SEX, RACETHX, CANCERDX, STRKDX, DIABDX_M18)

d_2022=d2022 %>%
  select(TOTEXP22, AGE22X, SEX, RACETHX, CANCERDX, STRKDX, DIABDX_M18)



#결측치 제거
sum(is.na(d_2020))
sum(is.na(d_2022))

table(d_2020$SEX)
table(d_2020$RACETHX)
table(d_2020$CANCERDX)
table(d_2020$STRKDX)
table(d_2020$DIABDX_M18)

d_2020=d_2020 %>%
  mutate(CANCERDX = ifelse(CANCERDX %in% c(-15, -8, -7, -1), NA, CANCERDX),
         STRKDX=ifelse(STRKDX %in% c(-8, -7, -1), NA, STRKDX),
         DIABDX_M18=ifelse(DIABDX_M18 %in% c(-15, -8, -7, -1), NA, DIABDX_M18))
d_2022=d_2022 %>%
  mutate(CANCERDX = ifelse(CANCERDX %in% c(-15, -8, -7, -1), NA, CANCERDX),
         STRKDX=ifelse(STRKDX %in% c(-8, -7, -1), NA, STRKDX),
         DIABDX_M18=ifelse(DIABDX_M18 %in% c(-15, -8, -7, -1), NA, DIABDX_M18))

d_2020= na.omit(d_2020)
d_2022= na.omit(d_2022)


#(b)
#선형회귀
model20_1 = lm(TOTEXP20 ~ AGE20X + factor(SEX) + factor(RACETHX)+factor(CANCERDX)+factor(STRKDX)+factor(DIABDX_M18), data = d_2020) 
summary(model20_1)
#
model22_1 = lm(TOTEXP22 ~ AGE22X + factor(SEX) + factor(RACETHX)+factor(CANCERDX)+factor(STRKDX)+factor(DIABDX_M18), data = d_2022) 
summary(model22_1)

#(c)
#goodness-of-fit test
library(Metrics)
model20_1_predict=predict(model20_1, se.fit = TRUE)
summary(model20_1_predict$fit)
summary(model20_1)$r.squared 

mae(d_2020$TOTEXP20, model20_1_predict$fit)
mse(d_2020$TOTEXP20, model20_1_predict$fit)
rmse(d_2020$TOTEXP20, model20_1_predict$fit)

#
model22_1_predict=predict(model22_1, se.fit = TRUE)
summary(model22_1_predict$fit)
summary(model22_1)$r.squared 

mae(d_2022$TOTEXP22, model22_1_predict$fit)
mse(d_2022$TOTEXP22, model22_1_predict$fit)
rmse(d_2022$TOTEXP22, model22_1_predict$fit)


#(d)
#log transformed model

d_2020$log_TOTEXP20_1=log(d_2020$TOTEXP20 + 0.00001)
model20_2 = lm(log_TOTEXP20_1 ~ AGE20X + factor(SEX) + factor(RACETHX)+factor(CANCERDX)+factor(STRKDX)+factor(DIABDX_M18), data = d_2020) 
summary(model20_2)
#
d_2022$log_TOTEXP22_1=log(d_2022$TOTEXP22 + 0.00001)
model22_2 = lm(log_TOTEXP22_1 ~ AGE22X + factor(SEX) + factor(RACETHX)+factor(CANCERDX)+factor(STRKDX)+factor(DIABDX_M18), data = d_2022) 
summary(model22_2)

#glm
model20_3 = glm(TOTEXP20 ~ AGE20X + factor(SEX) + factor(RACETHX)+factor(CANCERDX)+factor(STRKDX)+factor(DIABDX_M18), data = d_2020, subset = TOTEXP20 > 0) 
summary(model20_3)

model20_4=glm(TOTEXP20 ~ AGE20X + factor(SEX) + factor(RACETHX)+factor(CANCERDX)+factor(STRKDX)+factor(DIABDX_M18), data = d_2020, family = Gamma(link = "log"), subset = TOTEXP20 > 0) 
summary(model20_4)


#

model22_3 = glm(TOTEXP22 ~ AGE22X + factor(SEX) + factor(RACETHX)+factor(CANCERDX)+factor(STRKDX)+factor(DIABDX_M18), data = d_2022, subset = TOTEXP22 > 0) 
summary(model22_3)



model22_4 = glm(TOTEXP22 ~ AGE22X + factor(SEX) + factor(RACETHX)+factor(CANCERDX)+factor(STRKDX)+factor(DIABDX_M18), data = d_2022, family = Gamma(link = "log"), subset = TOTEXP22 > 0) 
summary(model22_4)

# two-part model
library("twopartm")

model20_5 = tpm(TOTEXP20 ~ AGE20X + factor(SEX) + factor(RACETHX)+factor(CANCERDX)+factor(STRKDX)+factor(DIABDX_M18), data = d_2020, link_part1 = "logit", family_part2 = Gamma(link = "log"))
summary(model20_5)

model22_5 = tpm(TOTEXP22 ~ AGE22X + factor(SEX) + factor(RACETHX)+factor(CANCERDX)+factor(STRKDX)+factor(DIABDX_M18), data = d_2022, link_part1 = "logit", family_part2 = Gamma(link = "log"))
summary(model22_5)


#(e)
#goodness-of-fit test using deviance

library(Metrics)
model20_2_predict=predict(model20_2, se.fit = TRUE)
summary(model20_2_predict$fit)
summary(model20_2)$r.squared 

mae(d_2020$TOTEXP20, model20_2_predict$fit)
mse(d_2020$TOTEXP20, model20_2_predict$fit)
rmse(d_2020$TOTEXP20, model20_2_predict$fit)

model22_2_predict=predict(model22_2, se.fit = TRUE)
summary(model22_2_predict$fit)
summary(model22_2)$r.squared 

mae(d_2022$TOTEXP22, model22_2_predict$fit)
mse(d_2022$TOTEXP22, model22_2_predict$fit)
rmse(d_2022$TOTEXP22, model22_2_predict$fit)

#

null_model1 = glm(TOTEXP20 ~ 1, data = d_2020, subset = TOTEXP20 > 0)
null_model2 = glm(TOTEXP22 ~ 1, data = d_2022, subset = TOTEXP22 > 0)

df_diff=df.residual(null_model1) - df.residual(model20_3)
pchisq(model20_3$null.deviance-model20_3$deviance, df = df_diff, lower.tail = FALSE)

df_diff=df.residual(null_model1) - df.residual(model20_4)
pchisq(model20_4$null.deviance-model20_4$deviance, df = df_diff, lower.tail = FALSE)

df_diff=df.residual(null_model1) - df.residual(model22_3)
pchisq(model22_3$null.deviance-model22_3$deviance, df = df_diff, lower.tail = FALSE)

df_diff=df.residual(null_model1) - df.residual(model22_4)
pchisq(model22_4$null.deviance-model22_4$deviance, df = df_diff, lower.tail = FALSE)


#(g)
#데이터 로드
d_20=d2020 %>%
  select(TOTEXP20, COGLIM31, DFCOG42)
d_22=d2022 %>%
  select(TOTEXP22, COGLIM31, DFCOG42)
d_20=d_20 %>%
  mutate(DFCOG42 = ifelse(DFCOG42 %in% c(-8, -7, -1), NA, DFCOG42), COGLIM31 = ifelse(COGLIM31 %in% c(-15, -8, -1), NA, COGLIM31))
d_22=d_22 %>%
  mutate(DFCOG42 = ifelse(DFCOG42 %in% c(-8, -7, -1), NA, DFCOG42), COGLIM31 = ifelse(COGLIM31 %in% c(-8, -1), NA, COGLIM31))        
d_20= na.omit(d_20)
d_22= na.omit(d_22)

#상관관계
cor(d_20$TOTEXP20, d_20$COGLIM31)
cor(d_20$TOTEXP20, d_20$DFCOG42)
cor(d_22$TOTEXP22, d_22$COGLIM31)
cor(d_22$TOTEXP22, d_22$DFCOG42)

#회귀 분석
model1 = lm(TOTEXP20 ~ factor(COGLIM31) + factor(DFCOG42), data = d_20)
summary(model1)

model2 = lm(TOTEXP22 ~ factor(COGLIM31) + factor(DFCOG42), data = d_22)
summary(model2)

