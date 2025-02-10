# To do 
# 1. Add soccerfield visual
#     ggsoccer




library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Ajax Passing Statistics"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Filters"),
      selectInput("formation", "Select Formation", choices = unique(pass_events_Ajax$team.formation), selected = NULL, multiple = TRUE),
      selectInput("player", "Select Player", choices = unique(pass_events_Ajax$player.name), selected = NULL, multiple = TRUE),
      selectInput("position", "Select Position", choices = unique(pass_events_Ajax$player.position), selected = NULL, multiple = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Key Stats",
                 h4("High Pass Rate"), verbatimTextOutput("highpass_rate"),
                 h4("Total Success Rate"), verbatimTextOutput("total_successrate"),
                 h4("Ajax Success Rate"), verbatimTextOutput("ajax_successrate")
        ),
        tabPanel("Pass Length Distribution", plotOutput("pass_length_hist")),
        tabPanel("Pass Length Summary", verbatimTextOutput("pass_length_summary")),
        tabPanel("Pass Success Rate", tableOutput("pass_success_table")),
        tabPanel("Pass Type Distribution", tableOutput("pass_type_table")),
        tabPanel("Passes by Player", tableOutput("pass_player_table")),
        tabPanel("Passes by Formation", tableOutput("pass_formation_table")),
        tabPanel("Passes by Position", tableOutput("pass_position_table"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive filtering based on user selection
  filtered_data <- reactive({
    data <- pass_events_Ajax
    if (!is.null(input$formation) && length(input$formation) > 0) {
      data <- data %>% filter(team.formation %in% input$formation)
    }
    if (!is.null(input$player) && length(input$player) > 0) {
      data <- data %>% filter(player.name %in% input$player)
    }
    if (!is.null(input$position) && length(input$position) > 0) {
      data <- data %>% filter(player.position %in% input$position)
    }
    return(data)
  })
  
  ##### High Pass Rate #####
  output$highpass_rate <- renderText({
    data <- filtered_data()
    total_passes_Ajax <- nrow(data)
    pass_height_Ajax <- as.data.frame(table(data$pass.height))
    high_pass_rate <- round((sum(pass_height_Ajax$Freq[pass_height_Ajax$Var1 == "high"]) / total_passes_Ajax) * 100, 2)
    paste("High Pass Rate:", high_pass_rate, "%")
  })
  
  ##### Total Success Rate #####
  output$total_successrate <- renderText({
    data <- filtered_data()
    pass_successrate_total <- as.data.frame(table(data$pass.accurate))
    success_rate <- round((pass_successrate_total$Freq[pass_successrate_total$Var1 == "TRUE"] / sum(pass_successrate_total$Freq)) * 100, 2)
    paste("Total Success Rate:", success_rate, "%")
  })
  
  ##### Ajax-Specific Success Rate #####
  output$ajax_successrate <- renderText({
    data <- filtered_data()
    pass_successrate_ajax <- as.data.frame(table(data$pass.accurate))
    ajax_success_rate <- round((pass_successrate_ajax$Freq[pass_successrate_ajax$Var1 == "TRUE"] / sum(pass_successrate_ajax$Freq)) * 100, 2)
    paste("Ajax Success Rate:", ajax_success_rate, "%")
  })
  
  ##### Pass Length Histogram #####
  output$pass_length_hist <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = pass.length)) +
      geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
      labs(title = "Pass Length Distribution", x = "Pass Length (meters)", y = "Frequency") +
      theme_minimal()
  })
  
  ##### Pass Length Summary #####
  output$pass_length_summary <- renderPrint({
    data <- filtered_data()
    summary(data$pass.length)
  })
  
  ##### Pass Success Table #####
  output$pass_success_table <- renderTable({
    data <- filtered_data()
    pass_successrate_total <- as.data.frame(table(data$pass.accurate))
    pass_successrate_total$percentage <- round((pass_successrate_total$Freq / sum(pass_successrate_total$Freq)) * 100, 2)
    pass_successrate_total
  })
  
  ##### Front/Back Pass Table #####
  output$pass_type_table <- renderTable({
    data <- filtered_data()
    pass_type_counts <- data %>%
      mutate(pass_type = case_when(
        grepl("forward_pass", type.secondary) ~ "Forward Pass",
        grepl("back_pass", type.secondary) ~ "Back Pass",
        TRUE ~ "Other"
      )) %>%
      group_by(pass_type) %>%
      summarise(count = n()) %>%
      mutate(percentage = round((count / sum(count)) * 100, 2)) %>%
      as.data.frame()
    pass_type_counts
  })
  
  ##### Passes by Player Table #####
  output$pass_player_table <- renderTable({
    data <- filtered_data()
    pass_player_avg <- data %>%
      group_by(player.name) %>%
      summarise(
        total_passes = n(),
        avg_pass_length = mean(pass.length, na.rm = TRUE)
      ) %>%
      arrange(desc(avg_pass_length)) %>% 
      as.data.frame()
    pass_player_avg
  })
  
  ##### Passes by Formation Table #####
  output$pass_formation_table <- renderTable({
    data <- filtered_data()
    pass_formation <- data %>%
      group_by(team.formation) %>%
      summarise(
        total_passes = n(),
        avg_pass_length = mean(pass.length, na.rm = TRUE),
        long_passes = sum(pass.length > 23),
        long_passes_procent = round((long_passes / total_passes) * 100, 2)
      ) %>% 
      as.data.frame()
    pass_formation
  })
  
  ##### Passes by Position Table #####
  output$pass_position_table <- renderTable({
    data <- filtered_data()
    pass_position <- data %>%
      group_by(player.position) %>%
      summarise(
        total_passes = n(),
        avg_pass_length = mean(pass.length, na.rm = TRUE),
        long_passes = sum(pass.length > 23),
        long_passes_procent = round((long_passes / total_passes) * 100, 2),
        pass_success_rate = round((sum(pass.accurate, na.rm = TRUE) / total_passes) * 100, 2)
      ) %>% 
      as.data.frame()
    pass_position
  })
}

# Run the App
shinyApp(ui = ui, server = server)
