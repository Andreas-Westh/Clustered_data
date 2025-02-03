library(mongolite)
library(jsonlite)
library(dplyr)



conm<- mongo(
  url="mongodb://localhost",
  db="soccer",
  collection="matches",
)

allmatch <- conm$find(query='{}',fields='{}')

all_matches <- as.data.frame(allmatch)
all_matches$home_team <- sub("-.*", "", all_matches$label) #første del af string
all_matches$away_team <- sub("^[^-]+(-[^0-9]+).*", "\\1",all_matches$label) #anden del af string
all_matches$away_team <- gsub("- ", "",all_matches$away) #anden del af string fjern bindestreg 
all_matches$away_team <- gsub(",", "",all_matches$away) #anden del af string fjerdf
all_matches$home_GOAL <- gsub(".* ([0-9]+)-[0-9]+.*", "\\1",all_matches$label) #Home goals
all_matches$away_GOAL <- gsub(".* [0-9]+-([0-9]+).*", "\\1",all_matches$label) #df

Ajax <- all_matches[grepl("Ajax", all_matches$label, ignore.case = TRUE), ]

cong<- mongo(
  url="mongodb://localhost",
  db="soccer",
  collection="games",
)

idv=Ajax[,'_id']
# allgames <- cong$find(query='{}', fields = '{}')
idv <- as.character(idv)
AaM <- allgames[allgames$`_id` %in% idv,]

# Ensure 'events' is a list and flatten it
events_list <- lapply(AaM$events, as.data.frame)

# Bind all event data frames together
events_df <- flatten(bind_rows(events_list))

# Filter for pass events where `type$primary == "pass"`
pass_events <- events_df %>%
  filter(type.primary == "pass")

hist(pass_events$pass.length)



# To Do
# The muscles:
#   group_by
#   mutate
#   1. Lenght
#   2. 


                     