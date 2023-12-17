#import data set
alc.train <- read.csv("~/Desktop/UCLA Courses/Stats 101C/HW 4/TrainSAData2.csv")
alc.test <- read.csv("~/Desktop/UCLA Courses/Stats 101C/HW 4/TestSAData2NoY.csv")

dim(alc.train) #70000 28
dim(alc.test) #30000 27

summary(alc.train)

#-----------------------------------------------------------------------------------------
#find the number of numerical predictors
num_predictors <- sapply(alc.train, is.numeric)
sum(num_predictors) #there are 21 predictors
names(alc.train)[num_predictors]

#find the number of categorical predictors
categ_predictors <- sapply(alc.train, is.character)
sum(categ_predictors) #there are 7 predictors
names(alc.train)[categ_predictors]

#missing values percentage
#training
miss_val_train <- sum(is.na(alc.train))
percentage_missing_train <- (miss_val_train / prod(dim(alc.train))) * 100
percentage_missing_train

#testing
miss_val_test <- sum(is.na(alc.test))
percentage_missing_test <- (miss_val_test / prod(dim(alc.test))) * 100
percentage_missing_test

#frequency in response variable -- alcoholic status
freq_train <- table(alc.train$Alcoholic.Status)
prop_train <- prop.table(freq_train)

#check for missing data patterns
install.packages("VIM")
library(VIM)
alc.train_plot <- aggr(alc.train, col = c("lightblue", "pink"), numbers = TRUE, sortVars = TRUE,
                       labels = names(alc.train), cex.axis = 7, gap = 3, 
                       ylab = c("Missing data", "Pattern"))
alc.train_plot 
#it seems like age category has the most missing values ~8300
#all the other categories have about 4800-4900 missing values

#-----------------------------------------------------------------------------------------
#handle missing values using MICE
#mice
install.packages("mice")
library(mice)

#understand pattern for missing values
md.pattern(alc.train)

#impute data using mice
impute_method <- make.method(alc.train)

#categorical variables
#[1] "sex"              "hear_left"        "hear_right"       "BMI.Category"    
#[5] "AGE.Category"     "Smoking.Status"   "Alcoholic.Status"
#ensure they are as.factor()
alc.train$sex <- as.factor(alc.train$sex)
alc.train$hear_left <- as.factor(alc.train$hear_left)
alc.train$hear_right <- as.factor(alc.train$hear_right)
alc.train$BMI.Category <- as.factor(alc.train$BMI.Category)
alc.train$AGE.Category <- as.factor(alc.train$AGE.Category)
alc.train$Smoking.Status <- as.factor(alc.train$Smoking.Status)
alc.train$Alcoholic.Status <- as.factor(alc.train$Alcoholic.Status)

#updatte impute methods
impute_method["sex"] <- "logreg"
impute_method["hear_left"] <- "logreg"
impute_method["hear_right"] <- "logreg"
impute_method["BMI.Category"] <- "polyreg"
impute_method["AGE.Category"] <- "polyreg"
impute_method["Smoking.Status"] <- "polyreg"
impute_method["Alcoholic.Status"] <- "logreg"


impute_alc.train <- mice(alc.train, m = 5, method = impute_method, seed = 500)
summary(impute_alc.train)

sum(is.na(alc.train$Alcoholic.Status))

#data sets
dataset_1 <- complete(impute_alc.train, 1)
dataset_2 <- complete(impute_alc.train, 2)
dataset_3 <- complete(impute_alc.train, 3)
dataset_4 <- complete(impute_alc.train, 4)
dataset_5 <- complete(impute_alc.train, 5)

# Saving dataset_1 as a CSV file
write.csv(dataset_1, "mice_imputedData1.csv", row.names = FALSE)
write.csv(dataset_2, "mice_imputedData2.csv", row.names = FALSE)
write.csv(dataset_3, "mice_imputedData3.csv", row.names = FALSE)
write.csv(dataset_4, "mice_imputedData4.csv", row.names = FALSE)
write.csv(dataset_5, "mice_imputedData5.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------
#we select data set 2
#set seed
set.seed(1)

#full model
#convert response variable to as factor
dataset_2$Alcoholic.Status <- as.factor(dataset_2$Alcoholic.Status)  

#full model
rf_m1 <- randomForest(Alcoholic.Status ~ ., data = dataset_2)
print(rf_m1)

##assess the importance of each variable
importance(rf_m1)
varImpPlot(rf_m1)

#reduced model
#create model
rf_m2 <- randomForest(Alcoholic.Status ~ gamma_GTP + hemoglobin + HDL_chole +
                        age + sex + Smoking.Status + triglyceride, data = dataset_2)
print(rf_m2)

##assess the importance of each variable
importance(rf_m2)
varImpPlot(rf_m2)

#------------------------------------------------------------------------------------
#logistic regression for stepwise regression (feature selection)

#recode response variable into binary
dataset_2$Alcoholic.Status <- ifelse(dataset_2$Alcoholic.Status == "Y", 1, 0)

#create logistic regression model
lr.m1 <- glm(Alcoholic.Status ~ ., data = dataset_2, family = binomial())
summary(lr.m1)

#stepwise regression
#forward 
forward_selection <- step(lr.m1, direction = "forward")
summary(forward_selection)

#backward
backward_selection <- step(lr.m1, direction = "backward")
summary(backward_selection)

#------------------------------------------------------------------------------------
#lasso regression
library(glmnet)

#set lambda
j = seq(10, -2, length = 100)
lambda.v = 10^j
length(lambda.v)
lambda.v[c(1, 100)]

#set x and y
x <- model.matrix(Alcoholic.Status ~ ., data = dataset_2)
y <- dataset_2$Alcoholic.Status

#fit the lasso regression model
lasso_m1 <- glmnet(x, y, alpha = 1, lambda = lambda.v)
coeffs.lasso = coef(lasso_m1)

cv.output.lasso = cv.glmnet(x, y, alpha = 1)
plot(cv.output.lasso)

bestlamb.cv.lasso = cv.output.lasso$lambda.min
bestlamb.cv.lasso

predict(lasso_m1, s = bestlamb.cv.lasso, type = "coefficients")









