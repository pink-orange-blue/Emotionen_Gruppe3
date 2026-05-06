library(tidyverse)

restore_chatlog_structure <- function(chatlog) {
  
  fixed_chatlog <- chatlog %>% 
    mutate(
      DateTime = Timestamp,
      Sender = NA,
      Sender_anon = Sender_anonymized,
      Message = NA,
      Flat = NA,
      TokVec = NA,
      URL = NA,
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
    relocate(Emoji, .before = EmojiDescriptions)
  
  return(fixed_chatlog)
}

fix <- example %>% restore_chatlog_structure()
WhatsR::plot_emoji(fix)
