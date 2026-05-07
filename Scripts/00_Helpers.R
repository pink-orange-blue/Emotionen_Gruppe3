

library(tidyverse)
library(WhatsR)

source(file.path("Scripts", "99_plot_emoji_fix.R"))

restore_chatlog_structure <- function(chatlog) {
  
  fixed_chatlog <- chatlog %>% 
    mutate(
      DateTime = Timestamp,
      Sender = Sender_anonymized,
      Sender_anon = Sender_anonymized,
      Message = NA,
      Flat = NA,
      TokVec = NA,
      URL = Links_anonymized,
      URL_anon = Links_anonymized,
      Media = NA,
      Media_anon = NA,
      Location = NA,
      Location_anon = NA,
      Emoji = Emoji,
      EmojiDescriptions = Emoji_description,
      Smilies = Smilies,
      SystemMessage = NA,
      TokCount = NA,
      TimeOrder = Time_order,
      DisplayOrder = Display_order,
      .keep = "none"
    ) %>%
    relocate(Emoji, .before = EmojiDescriptions) %>% 
    relocate(Smilies, .before = SystemMessage)
  
  return(fixed_chatlog)
}


compare_chatlogs_to_template<- function(chatlog) {
  
  proper_format <- WhatsR::parse_chat("Data/Simulated_WhatsR_chatlog.txt")
  
  example_short <- head(example)
  proper_short <- head(proper_format)
  fix_short <- head(chatlog)
  
  num_identical <- 0
  num_total <- 0
  non_identical <- c()
  
  
  for(name in names(proper_short)) {
    
    num_total <- num_total + 1
    
    cat("Current column is ", name, "\n")
    cat("Proper format: \n")
    cat(str(proper_short[name]))
    cat("Actual format:\n")
    cat(str(fix_short[name]))
    
    cat("class of proper_short[[", name,"]] = ", class(proper_short[[name]]), "\n")
    cat("class of fix_short[[", name,"]] = ", class(fix_short[[name]]), "\n")
    
    is_identical_format <- class(proper_short[[name]]) == class(fix_short[[name]])
    cat("is_identical_format = ", is_identical_format, "\n")
    if(all(is_identical_format == TRUE)) {
      num_identical <- num_identical + 1
      
    } else {
      non_identical <- c(non_identical, name)
    }
    cat(".... are they equal? -- ", is_identical_format,"\n")
    cat("\n#########")
    cat("\n-------------------------------------------------------------------------------------------\n")
    
    
    
  }
  
  cat("Total entries: ", num_total)
  cat("\nIdentical entries: ", num_identical)
  cat("\nList of non-identical cols:") 
  print(non_identical)
  
  
}

# 
# example <- readRDS("Data/example_chat.rds")
# 
# 
# 
# fix <- example %>% restore_chatlog_structure()
# 
# compare_chatlogs_to_template(fix)
# 
# 
# #WhatsR::plot_emoji(fix)
# 
# #WhatsR::plot_emoji(proper_format)
# 
# 
# plot_emoji(fix,
#            plot = "bar",
#            min_occur = 15,
#            font_family = "sans")
