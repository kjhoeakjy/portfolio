#Problem Set2
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

######################################
#6
#(a)
#데이터 로드
d2018 = read_MEPS(year = 2018, type = "FYC")
d2019 = read_MEPS(year = 2019, type = "FYC")
d_2018=d2018 %>%
  select(OBDRV18,OBTOTV18,AGE18X,SEX,RACETHX,MARRY18X,INS18X,POVCAT18,REGION18)%>%
  mutate(OFFVST=OBDRV18+OBTOTV18)
d_2019=d2019 %>%
  select(OBDRV19,OBTOTV19,AGE19X,SEX,RACETHX,MARRY19X,INS19X,POVCAT19,REGION19)%>%
  mutate(OFFVST=OBDRV19+OBTOTV19)

#(b)

#결측값 제거
table(d_2018$SEX)
table(d_2018$RACETHX)
table(d_2018$MARRY18X)
table(d_2018$INS18X)
table(d_2018$POVCAT18)
table(d_2018$REGION18)
table(d_2019$SEX)
table(d_2019$RACETHX)
table(d_2019$MARRY19X)
table(d_2019$INS19X)
table(d_2019$POVCAT19)
table(d_2019$REGION19)

d_2018=d_2018 %>%
  mutate(MARRY18X = ifelse(MARRY18X==-7, NA, MARRY18X),
         INS18X=ifelse(INS18X==-1, NA, INS18X),
         REGION18=ifelse(REGION18==-1, NA, REGION18))%>%
  rename(OBDRV=OBDRV18,
         OBTOTV=OBTOTV18,
         AGE=AGE18X,
         MARRY = MARRY18X, 
         INS = INS18X, 
         REGION = REGION18,
         POVCAT=POVCAT18)
d_2019=d_2019 %>%
  mutate(MARRY19X = ifelse(MARRY19X %in% c(-8, -7), NA, MARRY19X),
         INS19X=ifelse(INS19X==-1, NA, INS19X),
         REGION19=ifelse(REGION19==-1, NA, REGION19))%>%
  rename(OBDRV=OBDRV19,
         OBTOTV=OBTOTV19,
         AGE=AGE19X,
         MARRY = MARRY19X, 
         INS = INS19X, 
         REGION = REGION19,
         POVCAT=POVCAT19)

d_2018= na.omit(d_2018)
d_2019= na.omit(d_2019)

#merge
pooled=rbind(d_2018, d_2019)
head(pooled)


#선형회귀
model18_lin = lm(OFFVST ~ AGE+factor(SEX)+factor(RACETHX)+factor(MARRY)+factor(INS)+factor(POVCAT)+factor(REGION), data = d_2018) 
summary(model18_lin)
model19_lin = lm(OFFVST ~ AGE+factor(SEX)+factor(RACETHX)+factor(MARRY)+factor(INS)+factor(POVCAT)+factor(REGION), data = d_2019) 
summary(model19_lin)

model_lin=lm(OFFVST ~ AGE+factor(SEX)+factor(RACETHX)+factor(MARRY)+factor(INS)+factor(POVCAT)+factor(REGION), data = pooled) 
summary(model_lin)


#(C)
model18_p = glm(OFFVST ~ AGE+factor(SEX)+factor(RACETHX)+factor(MARRY)+factor(INS)+factor(POVCAT)+factor(REGION), family="poisson", data = d_2018) 
summary(model18_p)
model19_p = glm(OFFVST ~ AGE+factor(SEX)+factor(RACETHX)+factor(MARRY)+factor(INS)+factor(POVCAT)+factor(REGION), family="poisson", data = d_2019) 
summary(model19_p)

model_p = glm(OFFVST ~ AGE+factor(SEX)+factor(RACETHX)+factor(MARRY)+factor(INS)+factor(POVCAT)+factor(REGION), family="poisson", data = pooled) 
summary(model_p)



#(d)
library(MASS)
model18_nb = glm.nb(OFFVST ~ AGE+factor(SEX)+factor(RACETHX)+factor(MARRY)+factor(INS)+factor(POVCAT)+factor(REGION), data = d_2018) 
summary(model18_nb)
model19_nb = glm.nb(OFFVST ~ AGE+factor(SEX)+factor(RACETHX)+factor(MARRY)+factor(INS)+factor(POVCAT)+factor(REGION), data = d_2019) 
summary(model19_nb)

model_nb = glm.nb(OFFVST ~ AGE+factor(SEX)+factor(RACETHX)+factor(MARRY)+factor(INS)+factor(POVCAT)+factor(REGION), data = pooled) 
summary(model_nb)

#(e)
library(Metrics)
model18_lin_predict=predict(model18_lin, se.fit = TRUE)
summary(model18_lin_predict$fit)
mae(d_2018$OFFVST, model18_lin_predict$fit)
mse(d_2018$OFFVST, model18_lin_predict$fit)
rmse(d_2018$OFFVST, model18_lin_predict$fit)

model18_p_predict=predict(model18_p, se.fit = TRUE)
summary(model18_p_predict$fit)
mae(d_2018$OFFVST, model18_p_predict$fit)
mse(d_2018$OFFVST, model18_p_predict$fit)
rmse(d_2018$OFFVST, model18_p_predict$fit)

model18_nb_predict=predict(model18_nb, se.fit = TRUE)
summary(model18_nb_predict$fit)
mae(d_2018$OFFVST, model18_nb_predict$fit)
mse(d_2018$OFFVST, model18_nb_predict$fit)
rmse(d_2018$OFFVST, model18_nb_predict$fit)


model19_lin_predict=predict(model19_lin, se.fit = TRUE)
summary(model19_lin_predict$fit)
mae(d_2019$OFFVST, model19_lin_predict$fit)
mse(d_2019$OFFVST, model19_lin_predict$fit)
rmse(d_2019$OFFVST, model19_lin_predict$fit)

model19_p_predict=predict(model19_p, se.fit = TRUE)
summary(model19_p_predict$fit)
mae(d_2019$OFFVST, model19_p_predict$fit)
mse(d_2019$OFFVST, model19_p_predict$fit)
rmse(d_2019$OFFVST, model19_p_predict$fit)

model19_nb_predict=predict(model19_nb, se.fit = TRUE)
summary(model19_nb_predict$fit)
mae(d_2019$OFFVST, model19_nb_predict$fit)
mse(d_2019$OFFVST, model19_nb_predict$fit)
rmse(d_2019$OFFVST, model19_nb_predict$fit)


model_lin_predict=predict(model_lin, se.fit = TRUE)
summary(model_lin_predict$fit)
mae(pooled$OFFVST, model_lin_predict$fit)
mse(pooled$OFFVST, model_lin_predict$fit)
rmse(pooled$OFFVST, model_lin_predict$fit)

model_p_predict=predict(model_p, se.fit = TRUE)
summary(model_p_predict$fit)
mae(pooled$OFFVST, model_p_predict$fit)
mse(pooled$OFFVST, model_p_predict$fit)
rmse(pooled$OFFVST, model_p_predict$fit)

model_nb_predict=predict(model_nb, se.fit = TRUE)
summary(model_nb_predict$fit)
mae(pooled$OFFVST, model_nb_predict$fit)
mse(pooled$OFFVST, model_nb_predict$fit)
rmse(pooled$OFFVST, model_nb_predict$fit)

#(f)
#nb model


#7
#(a)
library(dplyr)
d2002 = read_MEPS(year = 2002, type = "FYC")
d_2002=d2002 %>%
  dplyr::select(ADHECR42, EQU42, ADDAYA42, ADPLMT42, ADMACC42, ADMLMT42, ADPAIN42, ADPEP42, ADBLUE42, ADSOCA42, PCS42, MCS42,ADLHLP31, ADLHLP42, ADLHLP53,ADL3MO31,ADL3MO42,ADL3MO53,IADLHP31, IADLHP42, IADLHP53,IADL3M31, IADL3M42,IADL3M53)%>%
  mutate(VAS = ADHECR42,
         EQ5D = EQU42,
         SF12_p = PCS42,
         SF12_m = MCS42)

#SF6D 계산에 쓰이는 7가지 항목
#PF: ADDAYA42, RL:ADPLMT42, ADMACC42, SF:ADSOCA42,PAIN:ADPAIN42, MH: ADBLUE42, VIT:ADPEP42

#변수별 결측값 확인, 제거
table(d_2002$ADHECR42)
table(d_2002$ADDAYA42)
table(d_2002$ADPLMT42)
table(d_2002$ADMACC42)
table(d_2002$ADSOCA42)
table(d_2002$ADPAIN42)
table(d_2002$ADBLUE42)
table(d_2002$ADPEP42)
table(d_2002$ADLHLP31)
table(d_2002$ADLHLP42)
table(d_2002$ADLHLP53)
table(d_2002$ADL3MO31)
table(d_2002$ADL3MO42)
table(d_2002$ADL3MO53)
table(d_2002$IADLHLP31)
table(d_2002$IADLHLP42)
table(d_2002$IADLHLP53)
table(d_2002$IADL3M31)
table(d_2002$IADL3M42)
table(d_2002$IADL3M53)

d_2002 = d_2002 %>%
  mutate(across(c(ADHECR42, EQU42, ADDAYA42, ADPLMT42, ADMACC42, ADSOCA42, ADPAIN42, ADBLUE42, ADPEP42,ADLHLP31, ADLHLP42, ADLHLP53,ADL3MO31,ADL3MO42,ADL3MO53,IADLHP31, IADLHP42, IADLHP53,IADL3M31, IADL3M42,IADL3M53,PCS42, MCS42), 
                ~ replace(., . %in% c(-9, -8, -7, -1), NA)))


library(dplyr)

#1) SF-6D
# 필요한 항목들만 선택하고, SF-6D 관련 차원 매핑
sf6d_data = d_2002 %>%
  mutate(
    PF = case_when(ADDAYA42 == 1 ~ 1, ADDAYA42 == 2 ~ 2, ADDAYA42 == 3 ~ 3, TRUE ~ NA_integer_),
    RL = case_when(ADPLMT42 == 1 & ADMACC42 == 1 ~ 1, ADPLMT42 == 2 & ADMACC42 == 1 ~ 2,
                   ADPLMT42 == 1 & ADMACC42 == 2 ~ 3, ADPLMT42 == 2 & ADMACC42 == 2 ~ 4, TRUE ~ NA_integer_),
    SF = case_when(ADSOCA42 == 1 ~ 1, ADSOCA42 == 2 ~ 2, ADSOCA42 == 3 ~ 3, ADSOCA42 == 4 ~ 4, ADSOCA42 == 5 ~ 5, TRUE ~ NA_integer_),
    PAIN = case_when(ADPAIN42 == 1 ~ 1, ADPAIN42 == 2 ~ 2, ADPAIN42 == 3 ~ 3, ADPAIN42 == 4 ~ 4, ADPAIN42 == 5 ~ 5, TRUE ~ NA_integer_),
    MH = case_when(ADBLUE42 == 1 ~ 1, ADBLUE42 == 2 ~ 2, ADBLUE42 == 3 ~ 3, ADBLUE42 == 4 ~ 4, ADBLUE42 == 5 ~ 5, TRUE ~ NA_integer_),
    VT = case_when(ADPEP42 == 1 ~ 1, ADPEP42 == 2 ~ 2, ADPEP42 == 3 ~ 3, ADPEP42 == 4 ~ 4, ADPEP42 == 5 ~ 5, TRUE ~ NA_integer_)
  )


# 모델의 가중치를 사용하여 SF-6D 유틸리티 점수 계산
sf6d_data = sf6d_data %>%
  mutate(
    sf6d_score = if_else(
      !is.na(PF) & !is.na(RL) & !is.na(SF) & !is.na(PAIN) & !is.na(MH) & !is.na(VT),
      1.000 +  
        (PF == 2) * 0.012 + (PF == 3) * -0.045 +
        (RL == 2 | RL == 3 | RL == 4) * -0.063 +
        (SF == 2) * -0.063 + (SF == 3) * -0.066 + (SF == 4) * -0.081 + (SF == 5) * -0.093 +
        (PAIN == 2) * -0.004 + (PAIN == 3) * -0.042 + (PAIN == 4) * -0.077 + (PAIN == 5) * -0.137 +
        (MH == 2 | MH == 3) * -0.059 + (MH == 4) * -0.113 + (MH == 5) * -0.134 +
        (VT == 2) * -0.097 + (VT == 3 | VT == 4) * -0.078 + (VT == 5) * -0.106 +
        if_else(PF == 3 | RL == 4 | SF == 5 | PAIN == 5 | MH == 5 | VT == 5, -0.077, 0),
      NA_real_
    )
  )


# 결과 확인
head(sf6d_data)

#2) ADL, IADL 계산
#3개월 이상 지속 여부 변수들에 결측값이 매우 많아 이 경우 NA를 0으로 처리하여 더함.
#ADL, IADL 변수의 최솟값은 0, 최댓값은 6.

final_data = sf6d_data %>%
  mutate(
    # ADL3MO 및 IADL3M의 결측값을 0으로 변환
    across(c(ADL3MO31, ADL3MO42, ADL3MO53, IADL3M31, IADL3M42, IADL3M53), ~ replace_na(., 0)),
    
    # 2를 0으로 변환. 2가 no이기 때문.
    across(c(ADLHLP31, ADLHLP42, ADLHLP53, IADLHP31, IADLHP42, IADLHP53,ADL3MO31, ADL3MO42, ADL3MO53, IADL3M31, IADL3M42, IADL3M53), 
           ~ if_else(. == 2, 0, .)),
    
    # ADL 점수 계산 (ADL 관련 6개 변수의 합, 하나라도 NA가 있으면 NA)
    adl_score = case_when(
      if_any(c(ADLHLP31, ADLHLP42, ADLHLP53, ADL3MO31, ADL3MO42, ADL3MO53), is.na) ~ NA_real_,
      TRUE ~ rowSums(across(c(ADLHLP31, ADLHLP42, ADLHLP53, ADL3MO31, ADL3MO42, ADL3MO53)))
    ),
    
    # IADL 점수 계산 (IADL 관련 6개 변수의 합, 하나라도 NA가 있으면 NA)
    iadl_score = case_when(
      if_any(c(IADLHP31, IADLHP42, IADLHP53, IADL3M31, IADL3M42, IADL3M53), is.na) ~ NA_real_,
      TRUE ~ rowSums(across(c(IADLHP31, IADLHP42, IADLHP53, IADL3M31, IADL3M42, IADL3M53)))
    )
  )

# 결과 확인
head(final_data)

# 결측값 개수 확인
total_rows = nrow(final_data)
missing_sf6d=sum(is.na(final_data$sf6d_score))
missing_adl=sum(is.na(final_data$adl_score))
missing_iadl=sum(is.na(final_data$iadl_score))
missing_VAS=sum(is.na(final_data$VAS))
missing_EQ5D=sum(is.na(final_data$EQ5D))
missing_PCS=sum(is.na(final_data$PCS42))
missing_MCS=sum(is.na(final_data$MCS42))

total_rows
missing_sf6d
missing_adl
missing_iadl
missing_VAS
missing_EQ5D
missing_PCS
missing_MCS

#최종 결과
final=final_data %>%
  dplyr::select(VAS,EQ5D,sf6d_score,SF12_p,SF12_m,adl_score,iadl_score)
head(final)


#(b)

# 각 변수의 평균과 표준편차 계산
summary_stats=final %>%
  summarise(
    mean_VAS = mean(VAS, na.rm = TRUE),
    sd_VAS = sd(VAS, na.rm = TRUE),
    mean_EQ5D = mean(EQ5D, na.rm = TRUE),
    sd_EQ5D = sd(EQ5D, na.rm = TRUE),
    mean_sf6d = mean(sf6d_score, na.rm = TRUE),
    sd_sf6d = sd(sf6d_score, na.rm = TRUE),
    mean_SF12_p = mean(SF12_p, na.rm = TRUE),
    sd_SF12_p = sd(SF12_p, na.rm = TRUE),
    mean_SF12_m = mean(SF12_m, na.rm = TRUE),
    sd_SF12_m = sd(SF12_m, na.rm = TRUE),
    mean_adl = mean(adl_score, na.rm = TRUE),
    sd_adl = sd(adl_score, na.rm = TRUE),
    mean_iadl = mean(iadl_score, na.rm = TRUE),
    sd_iadl = sd(iadl_score, na.rm = TRUE)
  )

# 결과 출력
print(summary_stats)


#(c)
cor_matrix=final %>%
  dplyr::select(VAS, EQ5D, sf6d_score, SF12_p, SF12_m, adl_score, iadl_score) %>% 
  cor(use = "complete.obs")

# 결과 출력
print(cor_matrix)


#(d)
data= d2002 %>%
  dplyr::select(AGE02X, SEX, RACETHNX, MARRY02X, POVCAT02, REGION02, ADHECR42, EQU42)

data = data %>%
  mutate(across(c(AGE02X, SEX, RACETHNX, MARRY02X, POVCAT02, REGION02, ADHECR42, EQU42), 
                ~ replace(., . %in% c(-9, -8, -7, -1), NA)))

vas_model=glm(ADHECR42 ~ AGE02X+factor(SEX)+factor(RACETHNX)+factor(MARRY02X)+factor(REGION02), family="poisson", data = data)
eq5d_model=lm(EQU42 ~ AGE02X + factor(SEX) + factor(RACETHNX) + factor(MARRY02X) + factor(POVCAT02) + factor(REGION02), data = data)

# 결과 요약
summary(vas_model)
summary(eq5d_model)

#확인
model_data_vas=na.omit(data[c("AGE02X", "SEX", "RACETHNX", "MARRY02X", "REGION02", "ADHECR42")])
model_data_eq5d=na.omit(data[c("AGE02X", "SEX", "RACETHNX", "MARRY02X", "POVCAT02", "REGION02", "EQU42")])

predicted_vas=vas_model$fitted.values
predicted_eq5d = eq5d_model$fitted.values

actual_vas=model_data_vas$ADHECR42
actual_eq5d=model_data_eq5d$EQU42

mae_vas=mean(abs(actual_vas - predicted_vas))
mse_vas=mean((actual_vas - predicted_vas)^2)
rmse_vas=sqrt(mse_vas)

mae_eq5d=mean(abs(actual_eq5d - predicted_eq5d))
mse_eq5d=mean((actual_eq5d - predicted_eq5d)^2)
rmse_eq5d=sqrt(mse_eq5d)

cat("VAS 모델 - MAE:", mae_vas, "MSE:", mse_vas, "RMSE:", rmse_vas, "\n")
cat("EQ5D 모델 - MAE:", mae_eq5d, "MSE:", mse_eq5d, "RMSE:", rmse_eq5d, "\n")

