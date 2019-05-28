#knitr::opts_chunk$set(echo = TRUE)

#load_libraries
library(knitr)
library(lubridate)
library(VIM)
library(stringr)
library(psych)
library(pROC)
library(dplyr)

# read data
indian_diabetes <- read.csv("data/diabetes.csv", header=TRUE)
n.var <- names(indian_diabetes)
#remove pedigree function
indian_diabetes$DiabetesPedigreeFunction <- NULL
#impute glucose median
glucose_median <- median(indian_diabetes$Glucose[indian_diabetes$Glucose!=0])
indian_diabetes$Glucose <- ifelse(indian_diabetes$Glucose==0, glucose_median, indian_diabetes$Glucose)
#impute blood pressure typical value
indian_diabetes$BloodPressure <- ifelse(indian_diabetes$BloodPressure==0, 105, indian_diabetes$BloodPressure)
#impute skin thick mean
skin_mean <- as.integer(mean(indian_diabetes$SkinThickness[indian_diabetes$SkinThickness!=0]))
indian_diabetes$SkinThickness <- ifelse(indian_diabetes$SkinThickness==0, skin_mean, indian_diabetes$SkinThickness)
#impute BMI mean
bmi_mean <- lapply(mean(indian_diabetes$BMI[indian_diabetes$BMI!=0]), round, 1)[[1]]
indian_diabetes$BMI <- ifelse(indian_diabetes$BMI==0, bmi_mean, indian_diabetes$BMI)

#correlacion
cor_matrix <- cor(indian_diabetes[,1:7])

#analyze outliers
outlier_values <- boxplot.stats(indian_diabetes$Glucose)$out  # outlier values.
boxplot(indian_diabetes$Glucose, main="Glucose", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

