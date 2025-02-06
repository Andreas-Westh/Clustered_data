library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)
library(mongolite)
library(plotly)

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

colnames(allmatches)[colnames(allmatches) == "_id"] <- "matchId"
totstat_wide <- merge(allmatches,totstat_wide, "matchId")
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


library(shiny)
library(plotly)

# UI layout
ui <- fluidPage(
  titlePanel("3D Interactive Cluster Plot"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for selecting hover text labels
      selectInput("hover_labels", "Select labels for hover text:",
                  choices = c("Match ID" = "matchId",
                              "Pass Length" = "passl",
                              "Total Passes" = "totpasses",
                              "Accuracy Ratio" = "accratio",
                              "Kampen endte" = "label"),
                  selected = c("matchId", "passl", "totpasses", "accratio", "label"),
                  multiple = TRUE)
    ),
    
    mainPanel(
      plotlyOutput("plot") # Output area for the 3D plot
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlotly({
    req(totstat_wide)  # Ensure data exists
    
    # Debug: Print user-selected hover labels
    print("User selected hover labels:")
    print(input$hover_labels)
    
    # ✅ Ensure UI labels match exactly
    label_mapping <- list(
      "Match ID" = "matchId",
      "Pass Length" = "passl",
      "Total Passes" = "totpasses",
      "Accuracy Ratio" = "accratio",
      "Kampen endte" = "label"
    )
    
    # ✅ Ensure input selections are characters and mapped correctly
    selected_columns <- unname(label_mapping[as.character(input$hover_labels)])
    
    # Debug: Print mapped column names
    print("Mapped column names for hover text:")
    print(selected_columns)
    
    # Remove NULL values
    selected_columns <- selected_columns[!is.na(selected_columns)]
    
    # Debug: Print after removing NAs
    print("Filtered selected columns:")
    print(selected_columns)
    
    # Check if all selected columns exist in the dataset
    missing_cols <- setdiff(selected_columns, colnames(totstat_wide))
    if (length(missing_cols) > 0) {
      stop(paste("Missing columns in dataset:", paste(missing_cols, collapse = ", ")))
    }
    
    # Construct hover text dynamically
    hover_text <- apply(totstat_wide, 1, function(row) {
      paste(sapply(selected_columns, function(var) {
        value <- row[[var]]
        if (is.numeric(value)) value <- round(as.numeric(value), 2)
        paste0(var, ": ", value)
      }), collapse = "<br>")
    })
    
    # ✅ Ensure cluster is not a list
    if ("cluster" %in% colnames(totstat_wide)) {
      if (is.list(totstat_wide$cluster)) {
        totstat_wide$cluster <- sapply(totstat_wide$cluster, function(x) ifelse(is.null(x), NA, x))
      }
      totstat_wide$cluster <- as.factor(totstat_wide$cluster)  # Convert properly
    }
    
    # Generate the 3D scatter plot
    plot_ly(totstat_wide, 
            x = ~passl, 
            y = ~totpasses, 
            z = ~accratio, 
            color = ~cluster,  # Ensure this is a factor
            colors = c("#1f77b4", "#ff7f0e", "#2ca02c"),  
            type = "scatter3d", 
            mode = "markers",
            text = hover_text,
            hoverinfo = "text") %>%
      layout(title = list(text = "3D Interactive Cluster Plot for Ajax Pass Statistics"),
             scene = list(
               xaxis = list(title = "Pass Length"),
               yaxis = list(title = "Total Passes"),
               zaxis = list(title = "Accuracy Ratio")
             ))
  })
}






shinyApp(ui, server)