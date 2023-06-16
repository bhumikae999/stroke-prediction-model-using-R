# Importing packages
library(tidyverse)
library(naniar)
library(caTools) 
library(ggplot2) 
library(superheat) 
library(scatterplot3d) 
library(ROCR)

data = read.csv("stroke_data.csv")
str(data)
glimpse(data)

# Data Pre-processing
# Checking dataset values
unique(data $ gender)
unique(data $ ever_married) 
unique(data $ Residence_type)
unique(data $ smoking_status)

# Converting character values to numeric values
# As seen in the above values, the character values can be converted into 
# numeric values.
clean_data <- data %>% mutate(gender = if_else(gender == "Female", 0, if_else(gender == "Male", 1, 2)), ever_married = if_else(ever_married == "Yes", 1, 0), Residence_type = if_else(Residence_type == "Rural", 0, 1), smoking_status = if_else(smoking_status == "never smoked", 0, if_else(smoking_status == "formerly smoked", 1, if_else(smoking_status == "smokes", 2, 3))))
summary(clean_data)

# Handling missing values
miss_scan_count(data = data, search = list("N/A", "Unknown"))
# There are 201 "N/A" values in the bmi column that likely caused this column 
# to be parsed as character, although it should be numerical. Let's take care 
# of that by replacing those values with actual NAs. Moreover, there are a 
# lot of "Unknown" values in smoking_status which we have to take care of 
# too. We see that we have 1544 unknown values for smoking status and 
# therefore are missing a lot of information in a potentially informative 
# predictor. We will have to deal with this. Lets replace those values with 
# NAs.
clean_data <- replace_with_na(data = clean_data, replace = list(bmi = c("N/A"), smoking_status = c(3))) %>% mutate(bmi = as.numeric(bmi))
summary(clean_data)

# Visualizing the input
# Heatmap
superheat(subset(clean_data, select = -c(stroke)), scale = TRUE, bottom.label.size = 0.5, bottom.label.text.angle = 90, bottom.label.text.size = 3)

# BMI Distribution
# We see that the missingness doesn't show clear association with other 
# variables and therefore we can assume this missingness is MCAR (missing 
# completely at random). The distribution is right skewed (long tail to the 
# right) as this is the only variable with missing data (at least of the 
# numerical variables). 
ggplot(clean_data, aes(x = bmi)) + geom_density(color = "black", fill = "lightblue") + labs(title = "Distribution of BMI") 

# Gender Distribution
ggplot(data, aes(x = factor(gender), fill = factor(gender))) + geom_bar() + theme_classic()

# Age and BMI wrt Stroke
ggplot(clean_data, aes(x = age, y = bmi, color = stroke)) + geom_point() + scale_color_gradient(low = "lightblue", high = "red")

# Avg Glucose Level with stroke
ggplot(clean_data, aes(x = stroke, y = avg_glucose_level, group = stroke, fill = stroke)) + geom_boxplot()

# Logistic Regression
set.seed(99)  # Set a seed for reproducible results
split = sample.split(clean_data $ stroke, SplitRatio = 0.7)
train = subset(clean_data, split == TRUE)
test = subset(clean_data, split == FALSE)
logistic_regression_1 = glm(stroke~., data = train, family = 'binomial')
summary(logistic_regression_1)

# A lot of variables are not significant. Hence we will be removing Variables 
# based on significance level. The least significant variable as seen is 
# Residence_type with a Pr-value of 0.78326. Hence we will remove 
# Residence_type.
logistic_regression_2 = glm(stroke ~ gender + age + hypertension + heart_disease + ever_married + avg_glucose_level + bmi + smoking_status, data = train, family = 'binomial')
summary(logistic_regression_2)

# The least significant variable as seen is gender with a Pr-value of 
# 0.33212. Hence we will remove gender.
logistic_regression_2 = glm(stroke ~ age + hypertension + heart_disease + ever_married + avg_glucose_level + bmi + smoking_status, data = train, family = 'binomial')
summary(logistic_regression_2)

# The least significant variable as seen is bmi with a Pr-value of 0.29928. 
# Hence we will remove bmi.
logistic_regression_2 = glm(stroke ~ age + hypertension + heart_disease + ever_married + avg_glucose_level + smoking_status, data = train, family = 'binomial')
summary(logistic_regression_2)

# The least significant variable as seen is heart_disease with a Pr-value of 
# 0.44624. Hence we will remove heart_disease.
logistic_regression_2 = glm(stroke ~ age + hypertension + ever_married + avg_glucose_level + smoking_status, data = train, family = 'binomial')
summary(logistic_regression_2)

# The least significant variable as seen is ever_married with a Pr-value of 
# 0.14813. Hence we will remove ever_married.

logistic_regression_2 = glm(stroke ~ age + hypertension + avg_glucose_level + smoking_status, data = train, family = 'binomial')
summary(logistic_regression_2)

# The least significant variable as seen is smoking_status with a Pr-value of 
# 0.12771. Hence we will remove smoking_status.
logistic_regression_2 = glm(stroke ~ age + hypertension + avg_glucose_level, data = train, family = 'binomial')
summary(logistic_regression_2)
# Hence we get the three most significant variables having Pr-values less 
# than 0.05.

# Predictions on training set and confusion matrix
predict_train = predict(logistic_regression_2, type = 'response')
table(train $ stroke, predict_train>0.2)

# Accuracy on training set
(3274 + 38) / nrow(train)

# Predictions on test set
predict_test = predict(logistic_regression_2, newdata = test, type = 'response')
table(test $ stroke, predict_test>0.2)

# Accuracy on testing set
(1414 + 18) / nrow(test)

# Plotting results
# ROCR Curve and area under the curve
rocr_prediction = prediction(predict_test, test $ stroke)
auc = as.numeric(performance(rocr_prediction, 'auc') @ y.values)
auc
rocr_performance = performance(rocr_prediction, 'tpr','fpr')
plot(rocr_performance, colorize = TRUE, main = 'ROCR Curve')

# 3D Scatterplot
with(clean_data, {scatterplot3d(x = age,  y = hypertension, z = avg_glucose_level, main = "Stroke Prediction Scatterplot", xlab = "Age", ylab = "Hypertension", zlab = "Average Glucose Level")})
