library(dplyr)
library(plotly)

# Set seed for reproducability
set.seed(210802)

dffull <- read.csv("bmi.csv")
colnames(dffull)
df <- dffull[,-c(4,5)]
head(df)
colnames(df) <- c("X","Y","Z")

# If scaling was relevant for the dataset, this could be used:
  # dfsc <- scale(df)
  # head(dfsc)
  # df <- as.data.frame(dfsc)

# 1. Randomly assign a number, from 1 to K, to each observationm
k <- 3 # Number of clusters
df$cluster <- sample(1:k, nrow(df), replace = T)

  # 3D scatter plot of the first random initilation
  #plot_ly(df, x = ~X, y = ~Y, z = ~Z, 
  #        color = ~cluster, 
  #        type = "scatter3d", mode = "markers")

plot_ly() %>%
  add_trace(data = df, x = ~X, y = ~Y, z = ~Z, 
            type = "scatter3d", mode = "markers",
            color = ~as.factor(cluster), marker = list(size = 5),
            text = ~paste("Age:", X, "<br>",
                          "Height:", Y, "<br>",
                          "Weight:",Z,"<br>"),  # Fixed text formatting
            hoverinfo = "text"  # Ensures only the text is shown in hover
  ) %>%
  add_trace(data = centroids, x = ~X, y = ~Y, z = ~Z, 
            type = "scatter3d", mode = "markers",
            marker = list(size = 10, symbol = "diamond", color = "black"),
            name = "Centroids"
  ) %>%
  layout(title = "Pure Random Cluster",
         scene = list(xaxis = list(title = "Age"),
                      yaxis = list(title = "Height (cm)"),
                      zaxis = list(title = "Weight (kg)")))



# Loop until cluster assignments stop changing
isChanged <- TRUE
roundcounter <- 0

# Start K-means loop
while (isChanged) {
  roundcounter <- roundcounter + 1
  cat("\nIteration:", roundcounter, "\n")
  
  # Step 1: Compute new centroids (average X, Y, Z per cluster)
  centroids <- df %>%
    group_by(cluster) %>%
    summarise(X = mean(X), Y = mean(Y), Z = mean(Z)) %>%
    ungroup()
  
  # Step 2: Visualize current clusters and centroids
  p <- plot_ly() %>%
    add_trace(data = df, x = ~X, y = ~Y, z = ~Z, 
              type = "scatter3d", mode = "markers",
              color = ~as.factor(cluster), marker = list(size = 5),
              text = ~paste("Age:", X, "<br>",
                            "Height:", Y, "<br>",
                            "Weight:",Z,"<br>"),  # Fixed text formatting
              hoverinfo = "text"  # Ensures only the text is shown in hover
    ) %>%
    add_trace(data = centroids, x = ~X, y = ~Y, z = ~Z, 
              type = "scatter3d", mode = "markers",
              marker = list(size = 10, symbol = "diamond", color = "black"),
              name = "Centroids") %>%
    layout(title = paste("Iteration:", roundcounter),
           scene = list(xaxis = list(title = "Age"),
                        yaxis = list(title = "Height (cm)"),
                        zaxis = list(title = "Weight (kg)")))
  
  print(p)  # Display plot
  Sys.sleep(2)  # Pause for visualization
  
  # Step 3: Assign each point to the nearest centroid
  changecounter <- 0
  for (i in 1:nrow(df)) {
    distances <- sapply(1:k, function(j) {
      sqrt((centroids$X[j] - df$X[i])^2 + (centroids$Y[j] - df$Y[i])^2 + (centroids$Z[j] - df$Z[i])^2)
    })
    new_cluster <- which.min(distances)  # Find the closest centroid
    
    # Update cluster if it has changed
    if (df$cluster[i] != new_cluster) {
      df$cluster[i] <- new_cluster
      changecounter <- changecounter + 1
    }
  }
  
  # Step 4: Check if any clusters changed
  isChanged <- changecounter > 0
  cat("Changes in this iteration:", changecounter, "\n")
  
  # Print a message when no more changes occur
  if (!isChanged) {
    cat("\n The K-mean clustering is finished at ", roundcounter," iterations!\n")
  }
}

df_bind <- cbind(dffull, df)
aggregated <- as.data.frame(table(df_bind$cluster, df_bind$BmiClass)) %>%
  arrange(Var1)
