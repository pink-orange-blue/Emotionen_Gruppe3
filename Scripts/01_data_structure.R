##############################
#
#
#
#   Script to clean WhatsApp Chatlog Data
#
#
#
##############################




library(tidyverse)
library(WhatsR)

example_chat_01 <- readRDS("example_chat.rds")
testuser <- readRDS("TestUser.rds")
parsed_chat_example <- WhatsR::parse_chat("Simulated_WhatsR_chatlog.txt")

pce <- parsed_chat_example

ec1 <- example_chat_01


ec1[6,6]#

step1 <- sapply(ec1$Emoji, is.na)

step2 <- !sapply(sapply(ec1$Emoji, is.na),sum)

View(step1)

View(step2)



step1ideal <- sapply(pce$Emoji, is.na)
step2ideal <- !sapply(step1ideal, sum)

View(step1ideal)
View(step2ideal)

good_links <- example_chat_01 %>% select(Links_anonymized) %>% filter(!is.na(Links_anonymized))

plot_emoji(ec1)



good_links



str(example_chat_01)

WhatsR::plot_emoji(example_chat_01)

ex <- WhatsR::create_chatlog()


emojis <- WhatsR::download_emoji()
emvec <- emojis$R.native
WhatsR::plot_emoji(example_chat_01, emoji_vec = "all")

WhatsR::plot_smilies(example_chat_01)

WhatsR::plot_links(example_chat_02)

str(example_chat_01)
str(parsed_chat_example)

example_chat_02 <- example_chat_01 %>% mutate(URL_anon = Links_anonymized)
