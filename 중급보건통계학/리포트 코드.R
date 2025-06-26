knhanes_20_=subset(knhanes,age>=20)
knhanes_2040=subset(knhanes_20_,age<50)


knhanes_0=subset(knhanes_2040,select=c('sex','age','ainc','npins','BH9_11','BH1','BH2_61','MH1_yr','MO1_wk','BD1_11','BD2_31','BA2_12','BA1_3','BA1_5','BA2_2_5','BS1_1','BS3_1','EQ5D','HE_sbp','HE_dbp','HE_glu'))


#sex=성별
#age=나이. 우리는 20~49세만 다룸 (연속형)
#ainx=가구 월평균 총소득 (연속형)
#npins=민간의료보험 가입 여부
#BH9_11=독감 접종 여부
#BH1=2년간 건강검진 수진 여부
#BH2_61=2년간 암검진 여부
#MH1_yr=1년간 입원 이용 여부
#MO1_wk=2주간 외래 이용 여부
#BA2_12=운전시 안전벨트 착용 여부
#BA1_3=자전거 헬멧 이용 여부
#BA1_5=오토바이 헬멧 이용 여부
#BA2_2_5=1년간 자동차 음주운전 여부
#BD1_11=1년간 음주 빈도
#BD2_31=폭음빈도
#BS1_1=평생흡연여부
#BS3_1=현재 흡연 여부
#EQ5D (연속형)
#HE_sbp=최종 수축기 혈압 (연속형)
#HE_dbp=최종 이완기 혈압 (연속형)
#HE_glu=공복혈당 (연속형)

#고혈압: 수축기 140이상 또는 이완기 90이상 
#고혈압 전단계: 수축기 120이상, 이완기 80이상
#정상: 수축기 120미만, 이완기 80미만
#당뇨병: 공복혈당 126 이상
#공복혈당장애: 공복혈당 100이상
#정상: 공복혈당 100미만



#결측값 처리
knhanes_0$npins=as.factor(ifelse(knhanes_0$npins<=2,knhanes_0$npins,NA))
knhanes_0$BH9_11=as.factor(ifelse(knhanes_0$BH9_11<=2,knhanes_0$BH9_11,NA))
knhanes_0$BH1=as.factor(ifelse(knhanes_0$BH1<=2,knhanes_0$BH1,NA))
knhanes_0$BH2_61=as.factor(ifelse(knhanes_0$BH2_61<=2,knhanes_0$BH2_61,NA))
knhanes_0$MH1_yr=as.factor(ifelse(knhanes_0$MH1_yr<=2,knhanes_0$MH1_yr,NA))
knhanes_0$MO1_wk=as.factor(ifelse(knhanes_0$MO1_wk<=2,knhanes_0$MO1_wk,NA))
knhanes_0$BA2_12=as.factor(ifelse(knhanes_0$BA2_12<=5,knhanes_0$BA2_12,NA))
knhanes_0$BA1_3=as.factor(ifelse(knhanes_0$BA1_3<=5,knhanes_0$BA1_3,NA))
knhanes_0$BA1_5=as.factor(ifelse(knhanes_0$BA1_5<=5,knhanes_0$BA1_5,NA))
knhanes_0$BA2_2_5=as.factor(ifelse(knhanes_0$BA2_2_5<=2,knhanes_0$BA2_2_5,NA))
knhanes_0$BD1_11=as.factor(ifelse(knhanes_0$BD1_11<=6,knhanes_0$BD1_11,NA))
knhanes_0$BD2_31=as.factor(ifelse(knhanes_0$BD2_31<=5,knhanes_0$BD2_31,NA))
knhanes_0$BS1_1=as.factor(ifelse(knhanes_0$BS1_1<=3,knhanes_0$BS1_1,NA))
knhanes_0$BS3_1=as.factor(ifelse(knhanes_0$BS3_1<=3,knhanes_0$BS3_1,NA))

knhanes_1=na.omit(subset(knhanes_0,select=c(age,npins,EQ5D,ainc,HE_sbp,HE_dbp,HE_glu)))




#민간의료보험 여부에 따른 연속형 변수 기술통계
library(psych)
describeBy(knhanes_1$age,knhanes_1$npins)
describeBy(knhanes_1$EQ5D,knhanes_1$npins)
describeBy(knhanes_1$ainc,knhanes_1$npins)
describeBy(knhanes_1$HE_sbp,knhanes_1$npins)
describeBy(knhanes_1$HE_dbp,knhanes_1$npins)
describeBy(knhanes_1$HE_glu,knhanes_1$npins)


#보험 여부에 따른 연속형 변수 등분산 검정 (모두 기각됨)
var.test(age~npins,data=knhanes_1,alternative=c("two.sided"))
var.test(EQ5D~npins,data=knhanes_1,alternative=c("two.sided"))
var.test(ainc~npins,data=knhanes_1,alternative=c("two.sided"))
var.test(HE_sbp~npins,data=knhanes_1,alternative=c("two.sided"))
var.test(HE_dbp~npins,data=knhanes_1,alternative=c("two.sided"))
var.test(HE_glu~npins,data=knhanes_1,alternative=c("two.sided"))

#이분산 T검정 (모두 기각됨)
t.test(age~npins,data=knhanes_1,var.equal=F)
t.test(EQ5D~npins,data=knhanes_1,var.equal=F)
t.test(ainc~npins,data=knhanes_1,var.equal=F)
t.test(HE_sbp~npins,data=knhanes_1,var.equal=F)
t.test(HE_dbp~npins,data=knhanes_1,var.equal=F)
t.test(HE_glu~npins,data=knhanes_1,var.equal=F)

#정규성 검정 (모두 기각됨)
shapiro.test(knhanes_1$age[knhanes_1$npins==1])
shapiro.test(knhanes_1$age[knhanes_1$npins==2])
shapiro.test(knhanes_1$ainc[knhanes_1$npins==1])
shapiro.test(knhanes_1$ainc[knhanes_1$npins==2])
shapiro.test(knhanes_1$EQ5D[knhanes_1$npins==1])
shapiro.test(knhanes_1$EQ5D[knhanes_1$npins==2])
shapiro.test(knhanes_1$HE_sbp[knhanes_1$npins==1])
shapiro.test(knhanes_1$HE_sbp[knhanes_1$npins==2])
shapiro.test(knhanes_1$HE_dbp[knhanes_1$npins==1])
shapiro.test(knhanes_1$HE_dbp[knhanes_1$npins==2])
shapiro.test(knhanes_1$HE_glu[knhanes_1$npins==1])
shapiro.test(knhanes_1$HE_glu[knhanes_1$npins==2])

#민간의료보험 여부에 따른 범주형 변수 빈도 (이때는 결측값 제거 안 한 상태로 해야됨. 그래서 knhanes_0이용함)
contin_sex=xtabs(data=knhanes_0,~npins+sex)
contin_BH9_11=xtabs(data=knhanes_0,~npins+BH9_11)
contin_BH1=xtabs(data=knhanes_0,~npins+BH1)
contin_BH2_61=xtabs(data=knhanes_0,~npins+BH2_61)
contin_MH1_yr=xtabs(data=knhanes_0,~npins+MH1_yr)
contin_MO1_wk=xtabs(data=knhanes_0,~npins+MO1_wk)
contin_BA2_12=xtabs(data=knhanes_0,~npins+BA2_12)
contin_BA1_3=xtabs(data=knhanes_0,~npins+BA1_3)
contin_BA1_5=xtabs(data=knhanes_0,~npins+BA1_5)
contin_BA2_2_5=xtabs(data=knhanes_0,~npins+BA2_2_5)
contin_BD1_11=xtabs(data=knhanes_0,~npins+BD1_11)
contin_BD2_31=xtabs(data=knhanes_0,~npins+BD2_31)
contin_BS1_1=xtabs(data=knhanes_0,~npins+BS1_1)
contin_BS3_1=xtabs(data=knhanes_0,~npins+BS3_1)

contin_sex;contin_BH9_11;contin_BH1;contin_BH2_61;contin_MH1_yr;contin_MO1_wk;contin_BA2_12;contin_BA1_3;contin_BA1_5;contin_BA2_2_5;contin_BD1_11;contin_BD2_31;contin_BS1_1;contin_BS3_1

#범주형 변수 연관성 F검정 (sex,MH1_yr,BA1_5 만 수용)
X_0<-chisq.test(contin_sex)
X_1<-chisq.test(contin_BH9_11)
X_2<-chisq.test(contin_BH1)
X_3<-chisq.test(contin_BH2_61)
X_4<-chisq.test(contin_MH1_yr)
X_5<-chisq.test(contin_MO1_wk)
X_6<-chisq.test(contin_BA2_12)
X_7<-chisq.test(contin_BA1_3)
X_8<-chisq.test(contin_BA1_5)
X_9<-chisq.test(contin_BA2_2_5)
X_10<-chisq.test(contin_BD1_11)
X_11<-chisq.test(contin_BD2_31)
X_12<-chisq.test(contin_BS1_1)
X_13<-chisq.test(contin_BS3_1)

X_0;X_1;X_2;X_3;X_4;X_5;X_6;X_7;X_8;X_9;X_10;X_11;X_12;X_13


#상관성
#망함. dbp는 상관계수가 0임...
cor(knhanes_npins[c('HE_sbp','HE_dbp','HE_glu','npins')],use='complete.obs',method='pearson')

cor.test(~EQ5D+npins,data=knhanes_npins,method=c('pearson'))

cor.test(~BH9_11+npins,data=knhanes_npins,method=c('spearman'))

#정규성 검정
shapiro.test(edu_eq5d$EQ5D[edu_eq5d$edu==1])
shapiro.test(edu_eq5d$EQ5D[edu_eq5d$edu==2])
