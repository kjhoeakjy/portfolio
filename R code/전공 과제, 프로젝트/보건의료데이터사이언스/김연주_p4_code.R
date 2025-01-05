#Problem Set4
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

########################################################
#1
# 데이터 로드
d2022 <- read_MEPS(year = 2022, type = "FYC")

d2022_1 <- d2022 %>%
  select(ADBMI42, AGE22X, SEX, RACETHX, EDUCYR, MNHLTH31, WLKLIM31, 
         REGION22, PRIVAT22, OPTOTV22, DIABDX_M18, STRKDX, CANCERDX, TTLP22X) %>%
  
  # 모든 변수에서 음수값을 NA로 변환
  mutate(across(everything(), ~ ifelse(. < 0, NA, .))) %>%
  
  # 변수 변환 및 재코딩
  #binomial 변수 중 2 (no)는 0으로로 변경
  mutate(
    bmi = ADBMI42,
    age = AGE22X,
    gender = ifelse(SEX == 1, 1, 0),
    race = RACETHX,
    education = EDUCYR,
    health = case_when(
      MNHLTH31 == 1 ~ 5,
      MNHLTH31 == 2 ~ 6,
      MNHLTH31 == 3 ~ 7,
      MNHLTH31 == 4 ~ 8,
      MNHLTH31 == 5 ~ 9
    ),
    limitation = ifelse(WLKLIM31 == 1, 1, 0),
    region = case_when(
      REGION22 == 1 ~ 2,
      REGION22 == 2 ~ 3,
      REGION22 == 3 ~ 4,
      REGION22 == 4 ~ 5
    ),
    private = ifelse(PRIVAT22 == 1, 1, ifelse(PRIVAT22 == 2, 0, NA)),
    visits_hosp = ifelse(OPTOTV22 >= 1, 1, ifelse(OPTOTV22 == 0, 0, NA)),
    diabetes = ifelse(DIABDX_M18 == 1, 1, ifelse(DIABDX_M18 == 2, 0, NA)),
    stroke = ifelse(STRKDX == 1, 1, ifelse(STRKDX == 2, 0, NA)),
    cancer = ifelse(CANCERDX == 1, 1, ifelse(CANCERDX == 2, 0, NA)),
    income = TTLP22X/1000
  ) %>%
  
  # 새로 생성된 변수만 선택
  select(
    bmi, age, gender, race, education, health, limitation, region, 
    private, visits_hosp, diabetes, stroke, cancer, income
  ) %>%
  drop_na()

# health, race, limitation, region을 범주형 변수로 변환
d2022_1 <- d2022_1 %>%
  mutate(
    health = as.factor(health),
    race = as.factor(race),
    limitation = as.factor(limitation),
    region = as.factor(region)
  )


d2022_1


###############################################################
#2.
#(a)
# 로지스틱 회귀 모델 개발
fit1 <- glm(visits_hosp ~ bmi + age + gender + race + education + health + limitation + region + private + diabetes + stroke + cancer + income, data = d2022_1, family = binomial(link = "logit"))

# 모델 결과 요약
summary(fit1)

# 모델 해석을 위한 오즈비 계산
exp(coef(fit1))  # 각 계수의 오즈비

#(b)
library(glmnet)
library(ggplot2)

# 종속 변수와 독립 변수 설정
x <- model.matrix(visits_hosp ~ bmi + age + gender + race + education + health + limitation + region + private + diabetes + stroke + cancer + income, data = d2022_1)[, -1]  # 모델 행렬 생성 (Intercept 제거)
y <- d2022_1$visits_hosp


#lasso
set.seed(123)
lasso_cv <- cv.glmnet(x, y, alpha = 1, nfolds = 20, family = "gaussian")
lasso_lambda_min <- lasso_cv$lambda.min
lasso_coef <- coef(lasso_cv, s = "lambda.min")
lasso_num_vars <- sum(lasso_coef != 0) - 1  # Intercept 제외


cat("Lasso 최적 lambda:", lasso_lambda_min, "\n")
cat("Lasso 선택된 변수 수:", sum(lasso_coef != 0) - 1, "\n") # Intercept 제외


#ridge
set.seed(123)
ridge_cv <- cv.glmnet(x, y, alpha = 0, nfolds = 20, family = "gaussian")
ridge_lambda_min <- ridge_cv$lambda.min
ridge_coef <- coef(ridge_cv, s = "lambda.min")
ridge_num_vars <- sum(ridge_coef != 0) - 1


cat("Ridge 최적 lambda:", ridge_lambda_min, "\n")
cat("Ridge 선택된 변수 수:", sum(ridge_coef != 0) - 1, "\n")


#elastic net
set.seed(123)
elastic_cv <- cv.glmnet(x, y, alpha = 0.5, nfolds = 20, family = "gaussian")
elastic_lambda_min <- elastic_cv$lambda.min
elastic_coef <- coef(elastic_cv, s = "lambda.min")
elastic_num_vars <- sum(elastic_coef != 0) - 1


cat("Elastic Net 최적 lambda:", elastic_lambda_min, "\n")
cat("Elastic Net 선택된 변수 수:", sum(elastic_coef != 0) - 1, "\n")


#MSE 비교
cat("Lasso 최소 MSE:", min(lasso_cv$cvm), "\n")
cat("Ridge 최소 MSE:", min(ridge_cv$cvm), "\n")
cat("Elastic Net 최소 MSE:", min(elastic_cv$cvm), "\n")


#(c)
cat("Lasso 최적 lambda:", lasso_lambda_min, "선택된 변수 수:", lasso_num_vars, "\n")
cat("Ridge 최적 lambda:", ridge_lambda_min, "선택된 변수 수:", ridge_num_vars, "\n")
cat("Elastic Net 최적 lambda:", elastic_lambda_min, "선택된 변수 수:", elastic_num_vars, "\n")

plot(lasso_cv)
title("Lasso Regression: Cross-Validation MSE", line = 2.5)

plot(ridge_cv)
title("Ridge Regression: Cross-Validation MSE", line = 2.5)

plot(elastic_cv)
title("Elastic Net Regression: Cross-Validation MSE", line = 2.5)



###################################
#3
#(a)
# 패키지 불러오기
library(caret)

# 데이터 준비
# 필요한 변수만 선택
data <- d2022_1 %>%
  select(diabetes, bmi, age, gender, education, visits_hosp, income,stroke, cancer) %>%
  drop_na()

# 종속변수와 독립변수 분리
x <- data %>% select(-diabetes)  # 독립 변수
y <- as.factor(data$diabetes)    # 종속 변수 (당뇨병 여부를 범주형으로 설정)

# 데이터 분할: 80% 훈련, 20% 테스트
set.seed(123)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
train_x <- x[trainIndex, ]
train_y <- y[trainIndex]
test_x <- x[-trainIndex, ]
test_y <- y[-trainIndex]

# 로지스틱 회귀 모델 학습
logistic_model <- glm(train_y ~ ., data = train_x, family = binomial)

# 모델 요약
summary(logistic_model)

# 테스트 데이터에 대한 예측 확률
pred_prob <- predict(logistic_model, newdata = test_x, type = "response")

# 예측 결과 (0.5를 기준으로 분류)
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

# 혼동행렬 생성
conf_matrix <- confusionMatrix(as.factor(pred_class), as.factor(test_y))

# 오분류율(Misclassification Rate) 계산
misclassification_rate <- 1 - conf_matrix$overall["Accuracy"]

# 결과 출력
print(conf_matrix)
cat("Out-of-Sample Misclassification Rate:", misclassification_rate, "\n")


#(b)
library(class)
library(caret)

# k = 1
set.seed(123)
pred_knn1 <- knn(train = train_x, test = test_x, cl = train_y, k = 1)
conf_matrix1 <- confusionMatrix(pred_knn1, test_y)
misclassification_rate1 <- 1 - conf_matrix1$overall["Accuracy"]

# k = 5
set.seed(123)
pred_knn5 <- knn(train = train_x, test = test_x, cl = train_y, k = 5)
conf_matrix5 <- confusionMatrix(pred_knn5, test_y)
misclassification_rate5 <- 1 - conf_matrix5$overall["Accuracy"]

# k = 50
set.seed(123)
pred_knn50 <- knn(train = train_x, test = test_x, cl = train_y, k = 50)
conf_matrix50 <- confusionMatrix(pred_knn50, test_y)
misclassification_rate50 <- 1 - conf_matrix50$overall["Accuracy"]

# 결과 출력
cat("KNN (k = 1) Misclassification Rate:", misclassification_rate1, "\n")
cat("KNN (k = 5) Misclassification Rate:", misclassification_rate5, "\n")
cat("KNN (k = 50) Misclassification Rate:", misclassification_rate50, "\n")



#(c)
library(rpart)
library(rpart.plot)
library(caret)

# 데이터 분할: 80% 훈련, 20% 테스트
set.seed(123)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Classification Tree 모델 훈련
tree_model <- rpart(diabetes ~ ., data = train_data, method = "class")

# 트리 시각화
rpart.plot(tree_model, main = "Classification Tree for Diabetes")

# 테스트 데이터에 대한 예측
pred_tree <- predict(tree_model, newdata = test_data, type = "class")

# 혼동행렬 생성
conf_matrix <- confusionMatrix(pred_tree, as.factor(test_data$diabetes))

# 오분류율(Misclassification Rate) 계산
misclassification_rate <- 1 - conf_matrix$overall["Accuracy"]

# 결과 출력
print(conf_matrix)
cat("Out-of-Sample Misclassification Rate:", misclassification_rate, "\n")

#(d)
# 모델별 결과 데이터프레임 생성
results <- data.frame(
  Model = c("Logistic Regression", "KNN (k=1)", "KNN (k=5)", "KNN (k=50)", "Classification Tree"),
  Accuracy = c(0.8441, 0.7784, 0.8284, 0.8456, 0.8470),
  Misclassification_Rate = c(0.1559, 0.2216, 0.1716, 0.1544, 0.1530)
)

# Accuracy 비교 시각화
library(ggplot2)
ggplot(results, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Comparison: Accuracy", y = "Accuracy", x = "Model") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Misclassification Rate 비교 시각화
ggplot(results, aes(x = Model, y = Misclassification_Rate, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Comparison: Misclassification Rate", y = "Misclassification Rate", x = "Model") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#######################################
#4
#(b)
# 데이터 표준화 (평균=0, 표준편차=1)
pca_data_scaled <- scale(data)

# PCA 실행
pca_result <- prcomp(pca_data_scaled, center = TRUE, scale. = TRUE)

# PCA 결과 요약
summary_pca <- summary(pca_result)
print(summary_pca)

# 첫 번째와 두 번째 주성분의 설명된 분산 비율
explained_variance <- summary_pca$importance[2, 1:2]
cat("첫 번째 주성분이 설명하는 변동 비율:", explained_variance[1], "\n")
cat("두 번째 주성분이 설명하는 변동 비율:", explained_variance[2], "\n")

# 누적 설명 비율
cumulative_variance <- summary_pca$importance[3, 1:2]
cat("첫 두 주성분의 누적 설명 비율:", cumulative_variance[2], "\n")

# Scree Plot 생성
scree_data <- data.frame(
  Principal_Component = paste0("PC", 1:length(pca_result$sdev)),
  Variance_Explained = summary_pca$importance[2, ]  # 변동 비율
)

ggplot(scree_data, aes(x = Principal_Component, y = Variance_Explained)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Scree Plot", x = "Principal Components", y = "Proportion of Variance Explained") +
  theme_minimal()

#(c)
pca_result

#(d)
# PCA 로딩 행렬 확인
pca_loadings <- pca_result$rotation

# 첫 번째와 두 번째 주성분에 대한 기여도 추출
pc1_contribution <- abs(pca_loadings[, 1]) / sum(abs(pca_loadings[, 1])) * 100
pc2_contribution <- abs(pca_loadings[, 2]) / sum(abs(pca_loadings[, 2])) * 100

# 데이터 프레임으로 변환
contribution_df <- data.frame(
  Variable = rownames(pca_loadings),
  PC1 = pc1_contribution,
  PC2 = pc2_contribution
)

# 결과 확인
print(contribution_df)

# 데이터 시각화: 변수별 기여도 비교
library(ggplot2)

# PC1에 대한 변수 기여도 시각화
ggplot(contribution_df, aes(x = reorder(Variable, PC1), y = PC1, fill = Variable)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Variable Contributions to PC1",
       x = "Variables", y = "Contribution (%)") +
  theme_minimal()

# PC2에 대한 변수 기여도 시각화
ggplot(contribution_df, aes(x = reorder(Variable, PC2), y = PC2, fill = Variable)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Variable Contributions to PC2",
       x = "Variables", y = "Contribution (%)") +
  theme_minimal()

