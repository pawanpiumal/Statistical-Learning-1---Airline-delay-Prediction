#libraries
library(dplyr)
library(ggplot2)
library(glmnet)
library(caret)
library(vip)
library(randomForest)
library(xgboost)
library(leaps)
library(unbalanced)
library(pROC)
library(tibble)
library(ROSE)
library(beepr)
library(plumber)
setwd("C:\\Users\\pawan\\Downloads\\3rd Year 2nd Semester - UOC\\ST3082 - Statistical Learning I\\Project1\\DataSet")

carriers = readRDS('Carriers.rds')
model = readRDS('RandomForestWithKnown.rds')
train = readRDS('trainForDataProject.RDS')





pr("plumber.R") %>%
  pr_run(port=8000)

