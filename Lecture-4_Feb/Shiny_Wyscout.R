library(shiny)
library(dplyr)
library(ggplot2)
library(factoextra)
library(cluster)

library(mongolite)
con_test <- mongo(collection = "matches", db = "soccer", url = "mongodb://localhost")
con_test$count()


# UI
ui <- fluidPage(
  titlePanel("Soccer Team Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Select a Team:", choices = NULL),
      uiOutput("tab_selector")
    ),
    mainPanel(
      uiOutput("dynamic_tabs")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Load data
  all_matches <- reactive({
    # Retrieve all matches from MongoDB
    conm$find(query = '{}', fields = '{}')
  })
  
  # Update team choices based on available data
  observe({
    teams <- all_matches() %>%
      select(home_team = "home_team_name", away_team = "away_team_name") %>%
      distinct() %>%
      unlist() %>%
      unique()
    updateSelectInput(session, "team", choices = teams)
  })
  
  # Generate dynamic tabs based on user selection
  output$tab_selector <- renderUI({
    req(input$team)
    selectInput("tab", "Select Analysis Type:", choices = c("Generic Stats", "PCA & Clustering"))
  })
  
  output$dynamic_tabs <- renderUI({
    req(input$tab)
    if (input$tab == "Generic Stats") {
      plotOutput("generic_stats")
    } else if (input$tab == "PCA & Clustering") {
      plotOutput("pca_plot")
    }
  })
  
  # Generic Stats Plot
  output$generic_stats <- renderPlot({
    req(input$team)
    # Filter data for the selected team
    team_data <- all_matches() %>%
      filter(home_team == input$team | away_team == input$team)
    
    # Example: Plot number of matches played per season
    ggplot(team_data, aes(x = season)) +
      geom_bar() +
      labs(title = paste("Number of Matches per Season for", input$team),
           x = "Season", y = "Number of Matches")
  })
  
  # PCA & Clustering Plot
  output$pca_plot <- renderPlot({
    req(input$team)
    # Filter and prepare data for PCA
    team_data <- all_matches() %>%
      filter(home_team == input$team | away_team == input$team) %>%
      select_if(is.numeric) %>%
      na.omit()
    
    # Perform PCA
    pca_result <- prcomp(team_data, scale. = TRUE)
    
    # Determine optimal number of clusters
    wss <- (nrow(team_data) - 1) * sum(apply(team_data, 2, var))
    for (i in 2:15) wss[i] <- sum(kmeans(team_data, centers = i)$withinss)
    optimal_clusters <- which.min(wss)
    
    # K-means clustering
    kmeans_result <- kmeans(team_data, centers = optimal_clusters)
    
    # Plot PCA with clusters
    fviz_cluster(list(data = team_data, cluster = kmeans_result$cluster),
                 geom = "point", ellipse.type = "convex", ggtheme = theme_minimal())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
