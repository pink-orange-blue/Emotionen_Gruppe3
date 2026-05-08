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

source(file.path("Scripts", "00_Helpers.R")) ## this reads the helpers functions. See the respective file for details!


data_dir <- "Data"
save_dir <- "Data_cleaned"

dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)


exclude_files <- c("example_chat.rds")



## read all .rds files under data

raw_files <- list.files(data_dir, pattern="\\.rds$")

#raw_files <- setdiff(raw_files, exclude_files) ##uncomment to exclude the example chat!



 ## save all cleaned files in new folder in the proper format
for (file in raw_files) {
  chat <- readRDS(file.path(data_dir, file))
  chat <- chat %>% restore_chatlog_structure() ## I wrote this transformation function to bridge the gap to WhatsR
  
  saveRDS(chat, file = file.path(save_dir, file))
  
}


## right now, we don't have much more to do with the data in terms of transformation. That will come later!
## However, if you want to exclude certain files or do other transformations, add in the code yourself in the loop above!


