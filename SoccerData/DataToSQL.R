library(RMariaDB)
library(dplyr)
library(readr)



#### Get SQL password ####
readRenviron("SoccerData/.Renviron")
password <- Sys.getenv("password")
password <- paste0(password,'"')

#### SQL Connection ####
con <- dbConnect(
  RMariaDB::MariaDB(),
  user = "root",
  password = password,
  dbname = "Superliga",
  host = "localhost",  # Change if using a remote DB
  port = 3306          # Default MySQL port
)

#### Data import loop ####
folder_path <- "SoccerData/Data/bif2" # Path to the bif2 folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

for (file in csv_files) {
  table_name <- tools::file_path_sans_ext(basename(file))  # Extract filename as table name
  df <- read_csv(file)  # Read CSV
  dbWriteTable(con, table_name, df, overwrite = TRUE, row.names = FALSE)  # Upload to MySQL
  message(paste("Uploaded:", table_name))
}


