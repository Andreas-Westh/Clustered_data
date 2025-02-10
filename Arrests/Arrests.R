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


# dkby2 <- dst_meta(table = "BY2", lang = "da")
# by2_filter <- list(
#   Tid=c("2023", "2022", "2021", "2020"),
#   KOMK="*",
#   BYST="*",
#   ALDER="*",
#   KØN="*")
# DanskBY2 <- dst_get_data(table = "BY2", query = by2_filter, lang = "da")

BY2 <- read.csv("BY2.csv", sep = ";")
BY2 <- BY2 %>% 
  filter(as.numeric(INDHOLD) > 0)
colnames(DanskStraf)[colnames(DanskStraf) == "OMRÅDE"] <- "KOMK"

BY_Vold <- merge(BY2, DanskStraf, "KOMK")
BY_Vold <- BY_Vold %>% 
  filter(as.numeric(value) > 0)

BY_Vold$ALDER.x <- as.numeric(gsub(" år", "", BY_Vold$ALDER.x))
BY_Vold_Clean <- BY_Vold[,-c(7,8,10)]

