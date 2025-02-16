library(tidyverse)
library(rpart)
library(rpart.plot)
df <- read.csv("BMI/bmi.csv")

# BmiCat - for everyone that are obese
df$BmiCat <- as.numeric(df$Bmi>30)

df$Agecat <- case_when(
  df$Age <= 0 ~ "baby",
  df$Age <= 13 ~ "barn",
  df$Age <= 19 ~ "teen",
  df$Age <= 30 ~ "Ung",
  df$Age <= 60 ~ "voksen",
  df$Age <= 203 ~ "Senior",
  TRUE ~ "så der fejl i koden")

df <- df[,c(2,3,6,7)]

# Fit decision tree model
tree_model <- rpart(BmiCat ~ ., data = df, 
                    method = "class")

# extra 104 shows Gini & Procent
rpart.plot(tree_model, type = 2, extra = 104, box.palette = "BuGn")



