# https://www.youtube.com/watch?v=WUjgcBCLIQY&t=24s

library(dkstat)
library(dplyr)
library(tidyverse)

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


#### Clustering ####
