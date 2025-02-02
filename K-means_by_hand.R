library(dplyr)
library(ggplot2)

#### By hand ####

# load data
df <- read.csv("Clustered_Data.csv")
plot(df)

#1. Randomly assign a number, from 1 to K, to each of the observations.
#These serve as initial cluster assignments for the observations.

df$cluster <- round(runif(20,1,k),0)

ggplot(df, aes(x=X,y=Y,color=as.factor(cluster)))+
  geom_point()

#2. Iterate until the cluster assignments stop changing:
#  (a) For each of the K clusters, compute the cluster centroid. The
#kth cluster centroid is the vector of the p feature means for the
#observations in the kth cluster.

centroids <- df %>% group_by(cluster) %>% mutate(X=mean(X),Y=mean(Y)) %>%
  unique() %>% 
  ungroup()

ggplot(df, aes(x=X,y=Y,color=as.factor(cluster)))+
  geom_point()+
  geom_point(data=centroids, aes(x=X,y=Y, size = 4, shape = as.factor(cluster)))

#(b) Assign each observation to the cluster whose centroid is closest
# Close = Euclidian distance pdist = ((cx-px)^2+(cy-py)^2)

for (i in (1:nrow(df))) {
  colv=vector()
  for (j in (1:k)){
  tmpdist = sqrt(((centroids[[j, 'X']] - df[[i, 'X']])^2) 
                 + ((centroids[[j, 'Y']] - df[[i, 'Y']])^2))
      colv[j]=tmpdist
  }
ncl=which.min(colv)
df[i, 'cluster']=ncl
}

ggplot(df, aes(x=X,y=Y,color=as.factor(cluster)))+
  geom_point()+
  geom_point(data=centroids, aes(x=X,y=Y, size = 4, shape = as.factor(cluster)))

centroids <- df %>% group_by(cluster) %>% mutate(X=mean(X),Y=mean(Y)) %>%
  unique() %>% 
  ungroup()

#(where closest is defined using Euclidean distance).


#### While Loop ####



