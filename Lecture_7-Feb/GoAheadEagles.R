library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)
library(mongolite)
library(factoextra)

#connect to mongo
cong=mongo(
  collection = "games",
  db = "soccer",
  url= "mongodb://localhost"
)

conm=mongo(
  collection = "matches",
  db = "soccer",
  url= "mongodb://localhost"
)
conp=mongo(
  collection = "players",
  db = "soccer",
  url= "mongodb://localhost"
)

# hent kampene
allmatches = conm$find(query = '{}',fields = '{}')
allDutch=allmatches %>% filter(competitionId==635)
allEagles=allDutch %>% filter(str_detect(label,"Go Ahead Eagles"))
#allAjax$res=gsub(".*,","",allAjax$label)
#allAjax$teams=gsub(",.*","",allAjax$label)

#AJAX STATS
# hent events for Ajax' kampe
# find _id
idv=allEagles[,'_id']
# testkampe
idvt=idv[1:3]
idvt=idv
query = jsonlite::toJSON(list(`_id` = list(`$in` = idvt)), auto_unbox = TRUE)
result=cong$find(query=query, fields = '{}')
testl=result$events
testdf=bind_rows(testl)
resdf=fromJSON(toJSON(testdf),flatten=T)

EaglesEvents=resdf %>% filter(team.name=="Go Ahead Eagles")

# only relevent events
EaglesEventspassduel <- EaglesEvents %>%
  group_by(matchId) %>%
  filter(type.primary %in% c("pass", "duel", "interception") | 
           type.secondary %in% c("foul", "sliding_tacles")) %>%
  ungroup()







