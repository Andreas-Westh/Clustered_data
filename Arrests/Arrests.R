library(plotly)
library(dplyr)
library(dkstat)
library(dplyr)

#### US Arrests ####
dfUS=USArrests

dfUSSc = as.data.frame(scale(dfUS))
uscl <- kmeans(dfUSSc, centers = 3, nstart = 10)

# Elbow method
dftwss=data.frame(k=1:20,twss=0)
for (i in (1:20)) {
  tmod=kmeans(dfUSSc,centers = i,nstart = 10)
  dftwss[i,'twss']=tmod$tot.withinss
}
plot(dftwss)

kmres=kmeans(dfUSSc, centers = 3, nstart = 10)
fviz_cluster(uscl,dfUSSc)

#analyse af de 3 cluster
dfUS$cluster <- uscl$cluster
dfUS$state <- rownames(dfUS)
dfUS_summary <- dfUS %>%
  group_by(cluster, state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

dfUS_summary <- dfUS %>%
  group_by(cluster, state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>% 
  mutate(total = dfUS_summary$Assault + dfUS_summary$UrbanPop + dfUS_summary$Rape + dfUS_summary$Murder) %>%
  arrange(desc(total)) 

dfUS$cluster <- uscl$cluster

pca.res <- princomp(dfUSSc)
pca.res$loadings

library(plotly)
plot_ly(dfUS_summary, 
        x = ~Assault, 
        y = ~Rape, 
        z = ~Murder, 
        color = ~cluster,
        colors = c("#ed6a5a", "#f4f1bb", "#9bc1bc"), 
        type = "scatter3d", 
        mode = "markers",
        text = ~paste("State:", state, "<br>",
                      "Total Crime Stats:", total, "<br>",
                      "Assault:", Assault, "<br>",
                      "Rape:", Rape, "<br>",
                      "Murder:", Murder, "<br>",
                      "Cluster:", cluster, "<br>"),
        hoverinfo = "text") %>%
  layout(title = "US Assults Clusters",
         scene = list(
           xaxis = list(title = "Assault"),
           yaxis = list(title = "Rape"),
           zaxis = list(title = "Murder")
         ))

dfUS_cluster <- dfUS %>% 
  group_by(cluster) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

dfUS_long <- dfUS_cluster %>%
  pivot_longer(cols = c(Murder, Assault, UrbanPop, Rape), 
               names_to = "Crime_Type", 
               values_to = "Value")


ggplot(dfUS_long, aes(x = factor(cluster), y = Value, fill = Crime_Type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Crime Statistics by Cluster",
       x = "Cluster",
       y = "Crime Rate",
       fill = "Crime Type") +
  theme_minimal() +
  scale_fill_manual(values = c("Murder" = "#ed6a5a", 
                               "Assault" = "#f4f1bb", 
                               "UrbanPop" = "#9bc1bc", 
                               "Rape" = "#5D576B"))  

##### Plot clusters on states #####
library(plotly)

plot_geo(dfUS_summary, locationmode = "USA-states") %>%
  add_trace(
    locations = ~state,
    z = ~cluster,
    text = ~state,
    colorscale = "Viridis"
  ) %>%
  layout(title = "US States Clustered by Crime Statistics", geo = list(scope = "usa"))








#### DK Arrests ####
dkstraf <- dst_meta(table = "STRAF44", lang = "da")

straf_filter <- list(
  Tid=c("2023"),
  ALDER="Alder i alt",
  OMRÅDE="*",
  OVERTRÆD=c("Voldsforbrydelser i alt","Voldtægt mv.","Manddrab"),
  KØN="I alt"
)
DanskStraf <- dst_get_data(table = "STRAF44", query = straf_filter, lang = "da")

DanskStraf_clean <- DanskStraf[,-c(1,2,5)]
DS_wide <- DanskStraf_clean %>%
  pivot_wider(names_from = OVERTRÆD, values_from = value, values_fill = list(value = 0))
colnames(DS_wide)[colnames(DS_wide) == "Voldtægt mv."] <- "Rape"
colnames(DS_wide)[colnames(DS_wide) == "Voldsforbrydelser i alt"] <- "Assault"
colnames(DS_wide)[colnames(DS_wide) == "Manddrab"] <- "Murder"

DS_summary <- DS_wide %>%
  mutate(total = as.numeric(Rape + Assault + Murder)) %>%  
  arrange(desc(total)) 

# Outliars
DS_summary <- DS_summary[DS_summary$OMRÅDE != "Hele landet", ]
DS_summary <- DS_summary[!grepl("^Region ", DS_summary$OMRÅDE), ]
DS_summary <- DS_summary[DS_summary$OMRÅDE != "København", ]


DS_scale = as.data.frame(scale(DS_summary[2:4]))
DS_cluster <- kmeans(DS_scale, centers = 3, nstart = 10)

DS_elbow=data.frame(k=1:20,twss=0)
for (i in (1:20)) {
  tmod=kmeans(DS_scale,centers = i,nstart = 10)
  DS_elbow[i,'twss']=tmod$tot.withinss
}
plot(DS_elbow)

DK_kmres=kmeans(DS_scale, centers = 3, nstart = 10)
fviz_cluster(DS_cluster,DS_scale)

DS_summary$cluster <- DS_cluster$cluster

##### Kort #####




 dkby2 <- dst_meta(table = "BY2", lang = "da")
 by2_filter <- list(
   Tid=c("2023"),
   KOMK="*",
   BYST="*")
 BY2 <- dst_get_data(table = "BY2", query = by2_filter, lang = "da")

#BY2 <- read.csv("Arrests/BY2.csv", sep = ";")
BY2 <- BY2[,-1]
BY2 <- BY2 %>% 
  filter(as.numeric(value) > 0)

BY2s <- BY2 %>%
  group_by(KOMK) %>%
  summarise(
    total_value = sum(value),  # Total
    land_value = sum(value[BYST == "Landdistrikter"]),  # antallet under Landdistrikter
    land_share = round((land_value / total_value) * 100,2)  # beregn landdistrikter
  ) %>%
  arrange(desc(total_value))











BY_Vold <- merge(BY2, DanskStraf, "KOMK")
BY_Vold <- BY_Vold %>% 
  filter(as.numeric(value) > 0)

BY_Vold$ALDER.x <- as.numeric(gsub(" år", "", BY_Vold$ALDER.x))
BY_Vold_Clean <- BY_Vold[,-c(7,8,10)]

