##########################
#load necessary libraries, options, and data
##########################
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(dslabs)
library(knitr)
library(gridExtra)
library(caret)
library(randomForest)

#set options
options(timeout = 120, digits = 3, scipen = 999)

#dataset - import from my github site, OG dataset: https://www.kaggle.com/datasets/lainguyn123/student-performance-factors
data <- read.csv("https://raw.githubusercontent.com/saramdar/HarvardX-Capstone-Choose-Your-Own/refs/heads/main/student_data.csv", sep = ",")

str(data) # initial view
data <- replace(data, data == '', NA) # replace blanks with NA
###########################
# Data Cleaning
###########################

any(is.na(data)) # check for NAs
sum(is.na(data)) # count NAs
sum(!complete.cases(data)) # count rows with NAs
colnames(data)[colSums(is.na(data))>0] #identify columns containing NAs

# preprocessing character data into factors, reordering levels (low = 1, high = 3)
data_clean <- data %>%
  mutate(Parental_Involvement = factor(Parental_Involvement, levels=c("Low", "Medium","High")),
         Access_to_Resources = factor(Access_to_Resources, levels=c("Low", "Medium","High")),
         Extracurricular_Activities = factor(Extracurricular_Activities),
         Motivation_Level = factor(Motivation_Level, levels=c("Low", "Medium","High")),
         Internet_Access = factor(Internet_Access),
         Family_Income = factor(Family_Income, levels=c("Low", "Medium","High")),
         School_Type = factor(School_Type),
         Peer_Influence = factor(Peer_Influence),
         Learning_Disabilities = factor(Learning_Disabilities),
         Gender = factor(Gender)) %>%
  select(-one_of(c("Teacher_Quality", 
                   "Parental_Education_Level", 
                   "Distance_from_Home")))%>% # exclude predictors with NAs
  mutate(Pass = case_when( # add "Pass" outcome
    Exam_Score >= 65 ~"1",
    Exam_Score < 65 ~ "0"))%>%
  mutate(Pass = factor(Pass)) 

#view final data set
head(data_clean)
dim(data_clean)
str(data_clean)

#######################################################
### DATA EXPLORATION and VISUALIZATION
#######################################################
#Student Success Variables: exam_score and pass
min(data_clean$Exam_Score)
max(data_clean$Exam_Score)
mean(data_clean$Exam_Score)

data_clean %>% # distribution of exam_score
  ggplot(aes(Exam_Score)) +
  geom_histogram(bins = 50, color = "black") 

data_clean %>% count(Pass) %>% ggplot(aes(x=Pass, y=n)) + geom_col() + ylab("count") #distribution of pass/fail

data_clean %>% count(Pass) %>% 
  reframe(Pass=Pass, proportion = n/nrow(data_clean)) %>% kable(caption="Proportion of Pass/Fail")

#Explore Predictors
#about the student:
#Hours Studied
study_g <- data_clean %>%
  ggplot(aes(Hours_Studied, Exam_Score)) +
  geom_point()+geom_jitter(width = 0.2, alpha = 0.1)
study_t <- data_clean %>% group_by(Hours_Studied, Pass) %>% 
  tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Pass, values_from = proportion)

#Attendance
attend_g <- data_clean %>%
  ggplot(aes(Attendance, Exam_Score)) +
  geom_point()+ geom_jitter(width = 0.2, alpha = 0.1)
attend_t <- data_clean %>% 
  group_by(Attendance, Pass) %>% 
  tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Pass, values_from = proportion)

#Previous Scores
prev_g <- data_clean %>%
  ggplot(aes(Previous_Scores, Exam_Score)) +
  geom_point()+ geom_jitter(width = 0.2, alpha = 0.1)
prev_t <- data_clean %>% 
  group_by(Previous_Scores, Pass) %>% 
  tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Pass, values_from = proportion)

#Extra Curricular Activities
xtra_g <- data_clean %>%
  ggplot(aes(Extracurricular_Activities, Exam_Score)) + 
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot()
xtra_t <- data_clean %>% group_by(Extracurricular_Activities, Pass) %>% 
  tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Pass, values_from = proportion)

#sleep hours
sleep_g <- data_clean %>%
  ggplot(aes(Sleep_Hours, Exam_Score, group = Sleep_Hours)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot()
sleep_t <- data_clean %>% group_by(Sleep_Hours, Pass) %>% 
  tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Pass, values_from = proportion)

#Motivation Level
mot_g <- data_clean %>%
  ggplot(aes(Motivation_Level, Exam_Score)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot()
mot_t <- data_clean %>% group_by(Motivation_Level, Pass) %>% 
  tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Pass, values_from = proportion)

#Physical Activity
phys_g <- data_clean %>%
  ggplot(aes(Physical_Activity, Exam_Score, group= Physical_Activity)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot()
phys_t <- data_clean %>% group_by(Physical_Activity, Pass) %>% 
  tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Pass, values_from = proportion)

#Learning Disability
dis_g <- data_clean %>%
  ggplot(aes(Learning_Disabilities, Exam_Score)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot()
dis_t <- data_clean %>% group_by(Learning_Disabilities, Pass) %>% 
  tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Pass, values_from = proportion)

#Gender
gender_g <- data_clean %>%
  ggplot(aes(Gender, Exam_Score)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot()
gender_t <- data_clean %>% group_by(Gender, Pass) %>% 
  tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Pass, values_from = proportion)

#Home environment:
#Parental_Involvement
parent_g <- data_clean %>%
  ggplot(aes(Parental_Involvement, Exam_Score)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot()
parent_t <- data_clean %>% group_by(Parental_Involvement, Pass) %>% 
  tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Parental_Involvement, values_from = proportion)

#Access to Resources
reso_g <- data_clean %>%
  ggplot(aes(Access_to_Resources, Exam_Score)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot()
reso_t <- data_clean %>% group_by(Access_to_Resources, Pass) %>% 
  tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Access_to_Resources, values_from = proportion)

#Internet_Access
int_g <- data_clean %>%
  ggplot(aes(Internet_Access, Exam_Score)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot()
int_t <- data_clean %>% group_by(Internet_Access, Pass) %>% 
  tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Pass, values_from = proportion)

#Tutoring Sessions
tut_g <- data_clean %>%
  ggplot(aes(Tutoring_Sessions, Exam_Score, group = Tutoring_Sessions)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot()
tut_t <- data_clean %>% group_by(Tutoring_Sessions, Pass) %>% 
  tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Pass, values_from = proportion)

#Family Income
inc_g <- data_clean %>%
  ggplot(aes(Family_Income, Exam_Score)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot()
inc_t <- data_clean %>% group_by(Family_Income, Pass) %>% 
  tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Pass, values_from = proportion)

#School Environment:
#school type
school_g <- data_clean %>%
  ggplot(aes(School_Type, Exam_Score)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot()
school_t <- data_clean %>% group_by(School_Type, Pass) %>% 
  tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Pass, values_from = proportion)

#Peer Influence
peer_g <- data_clean %>%
  ggplot(aes(Peer_Influence, Exam_Score)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot()
peer_t <- data_clean %>% group_by(Peer_Influence, Pass) %>% 
  tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Pass, values_from = proportion)

#visualizations of predictors
grid.arrange(study_g, attend_g, prev_g, xtra_g, sleep_g, mot_g, phys_g, dis_g, gender_g, ncol=3) # student behaviors and attitudes
grid.arrange(parent_g, reso_g, int_g, tut_g, inc_g, ncol = 3) # home environment
grid.arrange(school_g, peer_g, ncol = 2) # school environment

#####################
# Modeling
####################
#create test and train sets
set.seed(40, sample.kind = "Rounding") #if using R 3.6 or later
index <- createDataPartition(y = data_clean$Exam_Score, times = 1, p = 0.2, list = FALSE)
train <- data_clean[-index, ] %>% select(-Pass) # remove Pass for reg models
test <- data_clean[index,] %>% select(-Pass) # remove Pass for reg models
train_pass <- data_clean[-index, ] %>% select(-Exam_Score) # remove Exam_Score for class models
test_pass <- data_clean[index,] %>% select(-Exam_Score) # remove Exam_Score for class models

#confirm even split of data
tibble("Train" = mean(train$Exam_Score), "Test" = mean(test$Exam_Score))

tibble("Train_Pass" = mean(train_pass == 1), 
       "Test_Pass" = mean(test_pass == 1))

################
# Regression
################
#GLM
set.seed(41, sample.kind = "Rounding")#if using R 3.6 or later
train_glm <- train(Exam_Score~., data = train, method = "glm")
preds_glm <- predict(train_glm, test)
rmse_glm <- RMSE(preds_glm, test$Exam_Score)
results <- tibble(Model="GLM", RMSE = rmse_glm)

#KNN with cross validation
tc <- trainControl("cv", number=2, repeats = 2)
set.seed(42, sample.kind = "Rounding")#if using R 3.6 or later
train_knn <- train(Exam_Score~., 
                   data = train, 
                   method = "knn", 
                   trControl = tc,
                   preProcess = c("center", "scale"),
                   tuneLength = 15) # start with default k=5 and increment by 2 15 times
preds_knn <- predict(train_knn, test)
rmse_knn <- RMSE(preds_knn, test$Exam_Score)
results <- bind_rows(results, 
                          data.frame(Model="KNN", RMSE = rmse_knn))

#random forest with cross validation and hyperparameter tuning
#this code can take some time depending on processing power
#default
set.seed(43, sample.kind = "Rounding")#if using R 3.6 or later
rf_default <- train(Exam_Score~., 
                    data=train,
                    method = "rf",
                    trControl=tc) ###samples mtry = (2,11,12)

#tune mtry
#this code can take some time depending on processing power
tg <- expand.grid(.mtry=c(6:13)) 
set.seed(44, sample.kind = "Rounding")#if using R 3.6 or later
rf_mtry <- train(Exam_Score~.,
                 data=train,
                 method = "rf",
                 trControl=tc,
                 tuneGrid=tg)
best_mtry <- rf_mtry$bestTune$mtry

#final rf model using best_mtry from previous tuning
#this code can take some time depending on processing power
set.seed(45, sample.kind = "Rounding")#if using R 3.6 or later
train_rf <- train(Exam_Score~.,
                  data=train,
                  method = "rf",
                  trControl=tc,
                  tuneGrid= expand.grid(.mtry=best_mtry))
preds_rf <- predict(train_rf, test)
rmse_rf <- RMSE(preds_rf, test$Exam_Score)
results <- bind_rows(results, 
                     data.frame(Model="Random Forest", RMSE = rmse_rf))

######################
# Classification
######################
#Logistic GLM
train_class_glm <- train(Pass ~ .,
                         data = train_pass,
                         method = "glm")
preds_class_glm <- predict(train_class_glm, test_pass)
acc_glm <- mean(preds_class_glm == test_pass$Pass)
results_acc <- tibble(Model="GLM", Accuracy = acc_glm)

#KNN with cross validation
set.seed(47, sample.kind = "Rounding")
train_class_knn <- train(Pass~., 
                         data = train_pass, 
                         method = "knn", 
                         trControl = tc,
                         preProcess = c("center", "scale"),
                         tuneLength = 15)
preds_class_knn <- predict(train_class_knn, test_pass)
acc_knn <- confusionMatrix(preds_class_knn, test_pass$Pass)$overall[["Accuracy"]]
results_acc <- bind_rows(results_acc, 
                         data.frame(Model="KNN", Accuracy = acc_knn))

#random forest with cross validation and hyperparameter tuning
#default
set.seed(48, sample.kind = "Rounding")#if using R 3.6 or later
rf_class_default <- train(Pass~., 
                          data=train_pass,
                          method = "rf",
                          trControl=tc) ###samples mtry = (2,11,12)

#tune mtry
# tg same as reg,  #+/- 5 from previous model best mtry 11
set.seed(49, sample.kind = "Rounding")#if using R 3.6 or later
rf_class_mtry <- train(Pass~.,
                       data=train_pass,
                       method = "rf",
                       trControl=tc,
                       tuneGrid=tg)
best_mtry <- rf_class_mtry$bestTune$mtry

#final rf model using best_mtry from previous tuning
set.seed(50, sample.kind = "Rounding")#if using R 3.6 or later
train_class_rf <- train(Pass~.,
                        data=train_pass,
                        method = "rf",
                        trControl=tc,
                        tuneGrid= expand.grid(.mtry=best_mtry))
preds_class_rf <- predict(train_class_rf, test)
acc_rf <- confusionMatrix(preds_class_rf, test_pass$Pass)$overall[["Accuracy"]]
results_acc <- bind_rows(results_acc, 
                         data.frame(Model="Random Forest", Accuracy = acc_rf))

#########################
# Results
########################

results %>% kable() # Regression
results_acc %>% kable() # Classification

#comparing best models: 
#Best Regression: GLM
compare_glm <- data.frame(Exam_Score = test$Exam_Score, Preds = preds_glm) # create dataframe
compare_glm %>% ggplot(aes(Exam_Score, Preds)) + geom_point() + #plot test exam score vs predicted score
  geom_jitter(alpha = 2) +
  geom_vline(aes(xintercept = 65), color = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = 65), color = "red", linetype = "dashed") +
  ylab("Prediction")
glm_falseneg <- compare_glm %>% filter(Preds >= 65 & Exam_Score < 65) %>% nrow()

#Best Classification: GLM
confusionMatrix(preds_class_glm, test_pass$Pass)

#Compare sensitivity of reg vs class
glm_truepos <- compare_glm %>% filter(Preds < 65 & Exam_Score < 65) %>% nrow()
glm_sens <- glm_truepos/(glm_truepos+glm_falseneg)
glm_class_sens <- confusionMatrix(preds_class_glm, test_pass$Pass)$byClass[c("Sensitivity")] 

tibble("GLM Regression" = glm_sens, 
       "GLM Classification" = glm_class_sens) %>% kable(caption="Model Sensitivity")

# Important variables in best model
plot(varImp(train_glm), top=5, xlim=c(0,100), main = "GLM")

