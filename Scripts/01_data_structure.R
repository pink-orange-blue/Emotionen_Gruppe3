##############################
#
#
#
#   Script to clean WhatsApp Chatlog Data
#
#
#
##############################


# Install packages if required!

#install.packages("tidyverse")
#install.packages("WhatsR")

library(tidyverse)
library(WhatsR)

source(file.path("Scripts", "00_Helpers.R"))


data_dir <- "Data"
save_dir <- "Data_cleaned"

exclude_files <- c("example_chat.rds")

## read all .rds files under data



raw_files <- list.files(data_dir, pattern="\\.rds$")
#raw_files <- setdiff(raw_files, exclude_files)


dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)


 ## save all cleaned files in new folder 
for (file in raw_files) {
  chat <- readRDS(file.path(data_dir, file))
  chat <- chat %>% restore_chatlog_structure()
  
  saveRDS(chat, file = file.path(save_dir, file))
  
}



