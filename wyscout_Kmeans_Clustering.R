#                       _,aaadP""""""Ybaaaa,,_
#                   ,adP,__,,,aaaadP"""""Y888888a,_
#                ,a8888888P"''             "Y8888888b,
#             _a888888888"                   `Y88888888b,
#           ,d888888888P'                       "888888888b,
#         ,88888888P"Y8,                       ,P'   `""Y888b,
#       ,d8888P"'     "Ya,                    ,P'         `Ya`b,
#      ,P88"'           `Ya,                 ,P'            `b`Yi
#     d",P                `"Y,              ,P'              `Y "i
#   ,P' P'                   "888888888888888b                `b "i
#  ,P' d'                    d8888888888888888b                `b `b
#  d' d'                    ,888888888888888888b                I, Y,
# ,f ,f                    ,88888888888888888888b               `b, b
# d' d'                    d888888888888888888888b              ,88,I
#,P  8                    ,88888888888888888888888b,_          ,d8888
#d'  8,                   d8888888888888888888888P'`"Ya,_     ,d88888
#8  d88b,             ,adP""Y888888888888888888P'      `""Ya, d88888P
#8 ,88888b,       ,adP"'     `"Y8888888888888"'             `"888888I
#Y,88888888b, ,adP"'             ""Y888888P"                  888888'
#`888888888888P'                     ""YP"                    888888
# I88888888888                          8                     88888I
# `Y8888888888                          8                     88888'
#  `Y888888888    Run the file          8                     8888I
#   `Y88888888    soccer_stats.R        8                     8P"8'
#    `Y8888888,   Before this!!!!!      8                   ,d',d'
#     `b""""Y8b                         8                 ,d" ,d'
#       "b,   "Y,                       8               ,P" ,d"
#         "b,   "Ya,_                 ,d88ba,,___   _,aP" ,P"
#           "Ya_   ""Ya,_       _,,ad88888888888888P"' _,d"
#             `"Ya_    ""Yaaad88888888888888888888P _,d"'
#                 `"Ya,_     "Y888888888888888888P",d"'
#                    `""Ya,__`Y888888888888888P"""
#                         ``"""""""""""""''


library(dplyr)
library(plotly)

# Set seed for reproducibility
set.seed(123)

# Select relevant columns (Pass Length, Pass Angle, Minute)
df_full <- pass_events_Ajax %>%
  select(pass.length, pass.angle, minute)

# Rename for consistency with structure
colnames(df_full) <- c("X", "Y", "Z")

# Scale data if needed
df_scaled <- scale(df_full)
df <- as.data.frame(df_scaled)
colnames(df) <- c("X", "Y", "Z")

# Number of clusters
k <- 3
df$cluster <- sample(1:k, nrow(df), replace = TRUE)

# Initial 3D scatter plot of random cluster assignment
plot_ly(df, x = ~X, y = ~Y, z = ~Z, 
        color = ~cluster, 
        type = "scatter3d", mode = "markers")

# Loop until cluster assignments stop changing
isChanged <- TRUE
roundcounter <- 0

#### Kmeans Cluster Loop ####
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
              
              color = ~as.factor(cluster), marker = list(size = 5)) %>%
    add_trace(data = centroids, x = ~X, y = ~Y, z = ~Z, 
              type = "scatter3d", mode = "markers",
              marker = list(size = 10, symbol = "diamond", color = "black"),
              name = "Centroids") %>%
    layout(title = paste("Iteration:", roundcounter),
           scene = list(xaxis = list(title = "Pass Length (Scaled)"),
                        yaxis = list(title = "Pass Angle (Scaled)"),
                        zaxis = list(title = "Match Minute (Scaled)")))
  
  print(p)  # Display plot
  Sys.sleep(2)  # Pause for visualization
  
  # Step 3: Assign each point to the nearest centroid
  changecounter <- 0
  for (i in 1:nrow(df)) {
    distances <- sapply(1:k, function(j) {
      sqrt((centroids$X[j] - df$X[i])^2 + 
             (centroids$Y[j] - df$Y[i])^2 + 
             (centroids$Z[j] - df$Z[i])^2)
    })
    new_cluster <- which.min(distances)  # Find the closest centroid
    
    # Update cluster if changed
    if (df$cluster[i] != new_cluster) {
      df$cluster[i] <- new_cluster
      changecounter <- changecounter + 1
    }
  }
  
  # Step 4: Check if clusters changed
  isChanged <- changecounter > 0
  cat("Changes in this iteration:", changecounter, "\n")
  
  if (!isChanged) {
    cat("\n The K-means clustering is finished at ", roundcounter, " iterations!\n")
  }
}



#### Data exploration ####
##### Unscale the values #####
original_means <- colMeans(pass_events_Ajax %>% select(pass.length, pass.angle, minute), na.rm = TRUE)
original_sds <- apply(pass_events_Ajax %>% select(pass.length, pass.angle, minute), 2, sd)

##### Cluster summary #####
cluster_summary <- df %>%
  group_by(cluster) %>%
  summarise(
    avg_pass_length = mean(X, na.rm = TRUE) * original_sds[1] + original_means[1],
    avg_pass_angle = mean(Y, na.rm = TRUE) * original_sds[2] + original_means[2],
    avg_minute = mean(Z, na.rm = TRUE) * original_sds[3] + original_means[3],
    count = n()
  ) %>% as.data.frame()

##### Player position per Cluster #####
df_with_positions <- pass_events_Ajax %>%
  select(player.position) %>%
  mutate(cluster = df$cluster)

position_counts <- df_with_positions %>%
  group_by(cluster, player.position) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>% as.data.frame()

