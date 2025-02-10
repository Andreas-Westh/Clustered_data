library(mongolite)
library(jsonlite)
library(dplyr)

#### To Do ####
#   1. Highpass rate
#       13.01%
#   2. Total successrate 
#       83.86%
#          For only Ajax = 87.04%
#   3. length(arg?)
#       Done, har lavet alt muligt sjovt
#   4. Back/forward rate
#       front 32%, back 15%, other 53%
#   5. Passes per defensive action
#       idk, kunne ikke få det til at virke


conm<- mongo(
  url="mongodb://localhost",
  db="soccer",
  collection="matches",
)

allmatch <- conm$find(query='{}',fields='{}')

all_matches <- as.data.frame(allmatch)
all_matches$home_team <- sub("-.*", "", all_matches$label) #første del af string
all_matches$away_team <- sub("^[^-]+(-[^0-9]+).*", "\\1",all_matches$label) #anden del af string
all_matches$away_team <- gsub("- ", "",all_matches$away) #anden del af string fjern bindestreg 
all_matches$away_team <- gsub(",", "",all_matches$away) #anden del af string fjerdf
all_matches$home_GOAL <- gsub(".* ([0-9]+)-[0-9]+.*", "\\1",all_matches$label) #Home goals
all_matches$away_GOAL <- gsub(".* [0-9]+-([0-9]+).*", "\\1",all_matches$label) #df

Ajax <- all_matches[grepl("Ajax", all_matches$label, ignore.case = TRUE), ]

cong<- mongo(
  url="mongodb://localhost",
  db="soccer",
  collection="games",
)

idv=Ajax[,'_id']
# allgames <- cong$find(query='{}', fields = '{}')
idv <- as.character(idv)
AaM <- allgames[allgames$`_id` %in% idv,]

# Ensure 'events' is a list and flatten it
events_list <- lapply(AaM$events, as.data.frame)

# Bind all event data frames together
events_df <- flatten(bind_rows(events_list))

# Filter for pass events where `type$primary == "pass"`
pass_events <- events_df %>%
  filter(type.primary == "pass")
hist(pass_events$pass.length)
pass_height_df <- as.data.frame(table(pass_events$pass.height))
total_passes <- as.numeric(nrow(pass_events))
pass_height_df <- as.data.frame(table(pass_events$pass.height))

pass_height_df$procent <- round((pass_height_df$Freq / total_passes) * 100, 2)


pass_events_Ajax <- pass_events %>% filter(pass_events$team.name == "Ajax")
hist(pass_events_Ajax$pass.length)

##### Highpass rate #####
pass_height_Ajax <- as.data.frame(table(pass_events_Ajax$pass.height))
total_passes_Ajax <- as.numeric(nrow(pass_events_Ajax))
pass_height_Ajax <- as.data.frame(table(pass_events_Ajax$pass.height))

pass_height_Ajax$procent <- round((pass_height_Ajax$Freq / total_passes_Ajax) * 100, 2)



##### Total successrate #####
pass_successrate_total <- as.data.frame(table(pass_events$pass.accurate))
pass_successrate_total$procent <- round((pass_successrate_total$Freq / sum(pass_successrate_total$Freq)) * 100, 2)
  # For just Ajax
    pass_successrate_ajax <- as.data.frame(table(pass_events_Ajax$pass.accurate))
    pass_successrate_ajax$procent <- round((pass_successrate_ajax$Freq / sum(pass_successrate_ajax$Freq)) * 100, 2)


    
##### Lenght #####
    library(ggplot2)
    
    ggplot(pass_events_Ajax, aes(x = pass.length)) +
      geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
      labs(
        title = "Distribution of Pass Lengths (Ajax)",
        x = "Pass Length (meters)",
        y = "Frequency"
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = pretty(pass_events_Ajax$pass.length, n = 10))
    
summary(pass_events_Ajax$pass.length)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0.00   11.00   16.00   18.59   23.00  118.00 
sd(pass_events_Ajax$pass.length)
#   11.6

# Pass length by accuracy
ggplot(pass_events_Ajax, aes(x = pass.length, fill = pass.accurate)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
  labs(title = "Pass Length Distribution by Accuracy (Ajax)", x = "Pass Length (meters)", y = "Frequency") +
  theme_minimal()

# Average pass lenght per player
pass_player_avg <- pass_events_Ajax %>%
  group_by(player.name) %>%
  summarise(
    total_passes = n(), # Counts total passes
    avg_pass_length = mean(pass.length, na.rm = TRUE)
    ) %>%
  arrange(desc(avg_pass_length)) %>% as.data.frame()

# Passes for formation
pass_formation <- pass_events_Ajax %>%
  group_by(team.formation) %>%
  summarise(
    total_passes = n(),
    avg_pass_length = mean(pass.length, na.rm = TRUE),
    long_passes = sum(pass.length > 23),
    long_passes_procent = round((long_passes / total_passes) * 100, 2)
    ) %>% 
  as.data.frame()


##### Front or back #####
pass_type_counts <- pass_events_Ajax %>%
  mutate(pass_type = case_when(
    grepl("forward_pass", type.secondary) ~ "Forward Pass",
    grepl("back_pass", type.secondary) ~ "Back Pass",
    TRUE ~ "Other"
  )) %>%
  group_by(pass_type) %>%
  summarise(count = n()) %>%
  mutate(percentage = round((count / sum(count)) * 100, 2)) %>%
  as.data.frame()


##### PPDA #####
# https://dataglossary.wyscout.com/ppda/

# The muscles:
#   group_by
#   mutate



                     