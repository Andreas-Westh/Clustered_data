# pca

# kemans?

# hlc

# interesting things
#   scaled vs unscaled




library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)
library(mongolite)



#### Data Retrieval ####
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
  db = "wyscout",
  url= "mongodb://localhost"
)

# hent kampene
allmatches = conm$find(query = '{}',fields = '{}')
allDutch=allmatches %>% filter(competitionId==635)
allAjax=allDutch %>% filter(str_detect(label,"Ajax"))
#allAjax$res=gsub(".*,","",allAjax$label)
#allAjax$teams=gsub(",.*","",allAjax$label)

#AJAX STATS
# hent events for Ajax' kampe
# find _id
idv=allAjax[,'_id']
# testkampe
idvt=idv[1:3]
idvt=idv
query = jsonlite::toJSON(list(`_id` = list(`$in` = idvt)), auto_unbox = TRUE)
result=cong$find(query=query, fields = '{}')
testl=result$events
testdf=bind_rows(testl)
##### Tjek er om eget alternativ er bedre #####
resdf=fromJSON(toJSON(testdf),flatten=T)

ajaxEvents=resdf %>% filter(team.name=="Ajax")
# only passes
ajaxEventsPasses=ajaxEvents %>% filter(type.primary=="pass")
# remove columns
av=colnames(ajaxEventsPasses)
av
avsub=av[1:30]
avsub2=avsub[-c(3:9)]
ajaxEventsPassesSub=ajaxEventsPasses[,avsub2]
ajaxEventsPassesSub$cat=unlist(lapply(ajaxEventsPassesSub$type.secondary, function(x) x[1]))


# find relevant pass-types
ddf=as.data.frame(table(ajaxEventsPassesSub$cat)) 
ddf=ddf %>% filter(Freq>410)
nv=unique(as.character(ddf$Var1))

pstat=ajaxEventsPassesSub %>% group_by(matchId, cat) %>% select(matchId,cat) %>% 
  filter(cat %in% nv) %>% 
  summarise(value=n(),.groups = "drop") %>% 
  ungroup()


# collect stats
ajstat=ajaxEventsPassesSub %>% group_by(matchId) %>% mutate(
  passl=mean(pass.length),
  passvar=sd(pass.length),
  totpasses=n(),
  accratio=round((totpasses - sum(pass.accurate))/totpasses,3),
) %>% select(accratio,totpasses,matchId,passl,passvar) %>% unique() %>% ungroup()
colnames(ajstat)
ajstatLong=ajstat %>% pivot_longer(c(totpasses,passl,passvar,accratio),names_to = "cat",values_to = "value")

totstat=bind_rows(pstat,ajstatLong)

# now wide
totstat_wide=totstat %>% pivot_wider(names_from = "cat", values_from = "value")
totstat_wide_scaled=as.data.frame(scale(totstat_wide))
totstat_wide_scaled <- totstat_wide_scaled[,-1]

kmod=kmeans(totstat_wide, nstart = 10, centers =3)
fviz_cluster(kmod, data = totstat_wide[,-1])
totstat_wide$cluster <- as.factor(kmod$cluster)
totstat_wide <- totstat_wide %>%
  relocate(cluster, .before = everything())

library(dplyr)
library(stringr)
library(tidyr)

# Split 'label' into home team, away team, and score
allmatches <- allmatches %>%
  mutate(
    home_team = str_extract(label, "^[^-]+"),  # Extract first team name
    away_team = str_extract(label, "(?<=– ).*?(?=,)"),  # Extract second team name
    home_goals = as.numeric(str_extract(label, "(?<=, )\\d+(?=-)")),  # First digit after comma
    away_goals = as.numeric(str_extract(label, "(?<=-)\\d+"))  # Number after hyphen
  )

ajax_matches <- allmatches %>%
  filter(home_team == "Ajax" | away_team == "Ajax")

# Reorder columns for clarity
allmatches <- allmatches %>% select(label, home_team, away_team, home_goals, away_goals, everything())

ajax_matches <- allmatches %>%
  filter(home_team == "Ajax" | away_team == "Ajax")






#### Do the elbow test ####
    ##### Wulf's elbow method #####
    dftwss=data.frame(k=1:20,twss=0)
    for (i in (1:20)) {
      tmod=kmeans(totstat_wide_scaled,centers = i,nstart = 10)
      dftwss[i,'twss']=tmod$tot.withinss
    }
      plot(dftwss)


# Compute WCSS (total within-cluster sum of squares) for K = 1 to 20
dftwss <- data.frame(k = 1:20, twss = 0)
for (i in 1:20) {
  tmod <- kmeans(totstat_wide_scaled, centers = i, nstart = 10)
  dftwss[i, 'twss'] <- tmod$tot.withinss
}

# Plot the Elbow Method graph
ggplot(dftwss, aes(x = k, y = twss)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Elbow Method for Optimal K",
       x = "Number of Clusters (K)",
       y = "Total Within-Cluster Sum of Squares") +
  theme_minimal()



#### kmeans cluster ####
kmod=kmeans(totstat_wide_scaled, nstart = 10, centers =3)
kmod=kmeans(totstat_wide_scaled, nstart = 10, centers =3)
library(factoextra)
fviz_cluster(kmod, data = totstat_wide_scaled)

#### pca ####
data.pca <- princomp(totstat_wide_scaled)
summary(data.pca)
data.pca$loadings[, 1:3]
fviz_pca_var(data.pca, col.var = "black")

#### 3D Visual with 3 most influential variables ####
colnames(allmatches)[colnames(allmatches) == "_id"] <- "matchId"
totstat_wide_scaled <- merge(allmatches,totstat_wide, "matchId")
##### Noget galt med away team, skal fixes #####

# Plotly 3D Cluster Plot
plot_ly(totstat_wide, 
        x = ~passl, 
        y = ~totpasses, 
        z = ~accratio, 
        color = ~cluster,
        colors = c("red", "blue", "green"), 
        type = "scatter3d", 
        mode = "markers",
        text = ~paste("Match ID:", matchId, "<br>",
                      "Pass Length:", round(passl, 2), "<br>",
                      "Total Passes:", totpasses, "<br>",
                      "Accuracy Ratio:", accratio, "<br>",
                      "Kampen endte:", label),
        hoverinfo = "text") %>%
  layout(title = "3D Interactive Cluster Plot for Ajax Pass Statistics",
         scene = list(
           xaxis = list(title = "Pass Length"),
           yaxis = list(title = "Total Passes"),
           zaxis = list(title = "Accuracy Ratio")
         ))

#### hcl ####
distm=dist(totstat_wide_scaled)
hi=hclust(distm,method = "complete")
plot(hi)
cut_avg <- cutree(hi, k = 3)
rect.hclust(hi , k = 3, border = 2:6)





