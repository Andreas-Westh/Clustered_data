library(dkstat)
library(dplyr)
library(tidyverse)
library(factoextra)
library(plotly)
library(mapDK)

#### Data Retrieval Straf44 ####
strafmeta <- dst_meta("straf44")
  strafmeta$variables
  strafmeta$values$OVERTRÆD

straf_query <- list(
    OMRÅDE = "*",
    OVERTRÆD = c("Voldsforbrydelser i alt","Voldtægt mv.","Manddrab"),
    TID = c("2023","2022","2021","2020")
  )

dfStraf <- dst_get_data(table = "straf44", query = straf_query, lang = "da")

##### Datacleaning #####
dfStraf <- dfStraf %>% group_by(OMRÅDE,OVERTRÆD) %>% 
    summarise(nval=sum(value))

dfStrafWide <- dfStraf %>% pivot_wider(names_from = OVERTRÆD, values_from = nval)



#### Data Retrieval by2 ####
bymeta <- dst_meta("by2")
bymeta$variables
by2_query <- list(
  KOMK = "*",
  BYST = "*",
  Tid = "2023")
dfBy2 <- dst_get_data(table = "by2", query = by2_query, lang = "da")

##### Data cleaning #####
dfBy2 <- dfBy2 %>% select(-TID) #dfBy2 <- dfBy2[,-3]
dfBy2 <- dfBy2 %>% filter(BYST !="Uden fast bopæl")
# Sum for value, for hver kommune
dfBy2 <- dfBy2 %>% group_by(KOMK) %>% mutate(
  total=sum(value),
  urbprct=if_else(BYST=="Landdistrikter",(value/total)*100,0) # Beregne urbaniceringsprocent
  ) %>% ungroup()

dfBy_urb <- dfBy2 %>% filter(BYST=="Landdistrikter") %>% select(KOMK,urbprct)

colnames(dfBy_urb) <- c("OMRÅDE","urbproct")


##### Merging #####
dkArrest <- inner_join(dfStrafWide, dfBy_urb, by="OMRÅDE")
dkArrest$urbproct <- round(dkArrest$urbproct,1)
dkArrest_Scaled <- as.data.frame(scale(dkArrest[,-1]))


##### Outliers and correlation #####
# Outliers
hist(dkArrest_Scaled$Manddrab)
hist(dkArrest_Scaled$`Voldsforbrydelser i alt`)
hist(dkArrest_Scaled$`Voldtægt mv.`)

## Remove outlier
# Found via checking row number in non-scaled df
dkArrest_Scaled <- dkArrest_Scaled[-52,] # Removes copenhagen
dkArrest <- dkArrest[-52,]

# Correlations
dkArrest_Corr <- cor(dkArrest_Scaled)
corrplot::corrplot(dkArrest_Corr,addCoef.col = "black",method = "square",type = "lower")


#### Clustering ####
##### kmeans #####
colldf <- as.data.frame(matrix(nrow=0,ncol=2))
for (i in (1:8)) { # Max 8 clusters
  tmp_cluster <- kmeans(dkArrest_Scaled, centers = i, nstart = 10) # nstart creates multiple (10) configurations of centroids, picks the best
  colldf[i,1]=i
  colldf[i,2]=tmp_cluster$tot.withinss # Total within-cluster sum of squares
}

##### Elbow plot #####
plot(colldf)

# Optimal k
dk_cluster <- kmeans(dkArrest_Scaled, centers = 3,nstart=10)

# Quick viz check
fviz_cluster(dk_cluster,data = dkArrest_Scaled)

# Clusterinfo addes to dkArrest
dkArrest$Cluster <- dk_cluster$cluster



#### Analyse ####
dkArrast_Analyse <- dkArrest %>% group_by(Cluster) %>% 
  summarise(across(2:5,mean))

# Make long for plot
dkArrast_AnalyseLong <- dkArrast_Analyse %>% 
  pivot_longer(c(2,3,4,5),names_to = "Crimetype",values_to = "value")

# Barplot
ggplot(dkArrast_AnalyseLong,aes(x=Cluster,y=log(value),fill = as.factor(Crimetype)))+
  geom_bar(stat = "identity",position = "dodge")

##### PCA #####
dkPCA <- princomp(dkArrest_Scaled)
dkPCA$loadings

##### 3d plot #####
p <- plot_ly(
  data = dkArrest,
  x=~Manddrab,y=~`Voldtægt mv.`,z=~urbproct,
  type = "scatter3d",
  mode = "markers",
  color = ~as.factor(Cluster),
  text = ~paste(
    "Kommune: ",OMRÅDE,"<br>",
    "Drab: ",Manddrab,"<br>",
    "Pop: ",urbproct,"<br>",
    "Voldtægt",`Voldtægt mv.`,"<br>"
    ),
  hoverinfo="text"
)
p

##### Denmark Map plot #####
dkArrest_Map <- dkArrest %>% select(OMRÅDE,Cluster) %>% mutate(Cluster=Cluster*1000)
colnames(dkArrest_Map) <- c("Kommune","Cluster")
mapDK(values = "Cluster", id = "Kommune", data = dkArrest_Map)

