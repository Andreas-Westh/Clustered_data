# Load required libraries
library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)
library(mongolite)
library(ggplot2)
library(factoextra)
#### Data Retrieval ####
# Connect to MongoDB
cong <- mongo(collection = "games", db = "soccer", url = "mongodb://localhost")
conm <- mongo(collection = "matches", db = "soccer", url = "mongodb://localhost")
conp <- mongo(collection = "players", db = "wyscout", url = "mongodb://localhost")

# Retrieve all matches
allmatches <- conm$find(query = '{}', fields = '{}')

# Filter matches for the selected team
allDutch <- allmatches %>% filter(competitionId == 635)

# Extract match IDs
idv <- allDutch[,'_id']

# Query MongoDB for game events
query <- jsonlite::toJSON(list(`_id` = list(`$in` = idv)), auto_unbox = TRUE)
result <- cong$find(query = query, fields = '{}')
testl <- result$events
testdf <- bind_rows(testl)

# Convert to dataframe and filter for the selected team
resdf <- fromJSON(toJSON(testdf), flatten = TRUE)

# Filter only passes
df_passes <- resdf %>% filter(type.primary == "pass")


##### 1. Gennemsnit på 50.000 afleveringer med % successfulde #####
true_count <- table(df_passes$pass.accurate)["TRUE"]
cat(true_count,"\n")

pass_acc <- round((true_count / nrow(df_passes)) * 100,2)
cat(pass_acc,"%\n")

##### 2. Forklaringer på om aflevering er successfuld eller ej #####
df_passes$pass.accurate <- as.numeric(df_passes$pass.accurate)
cor(df_passes$pass.accurate, df_passes$pass.length)
  # -0.17
cor(df_passes$pass.accurate, df_passes$pass.angle)
  # 0.02


##### 3. Hvor stor del af afleveringer er en assist til et skud #####
shotpass <- df_passes[sapply(df_passes$type.secondary, function(x) "shot_assist" %in% x), ]
assist_acc <- round((nrow(shotpass) / nrow(df_passes)) * 100,2)
cat(assist_acc,"%\n")


##### 4. Opstil et beslutningstræ til vurdering af præcision i en given aflevering i jeres data #####
library(rpart)
library(rpart.plot)
df_passes$pass.accurate <- as.factor(df_passes$pass.accurate)
tree_model <- rpart(pass.accurate ~ pass.length, data = df_passes, method = "class")
rpart.plot(tree_model, type = 3, extra = 101, fallen.leaves = TRUE)




