################################################
##
##
##    Script to visualize WhatsApp-Chatlogs
##
##
#################################################

# install packages if necessary!
#install.packages("ggplot2")
#...

library(ggplot2)
library(ragg)
library(lubridate)

library(tidyverse)
library(WhatsR)

source(file.path("Scripts", "00_Helpers.R"))

save_dir <- "Data_cleaned"
plot_dir <- "Plots"
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

simulated_chat <- WhatsR::parse_chat(file.path("Data", "Simulated_WhatsR_chatlog.txt"))


## load all cleaned chats
files <- list.files(save_dir, pattern="\\.rds$")

chatlogs <- list()

for (file in files) {
  chat <- readRDS(file.path(save_dir, file))
  name <- tools::file_path_sans_ext(file)
  
  chatlogs[[name]] <- chat
}

#View(chatlogs)


## All chatlogs are contained in the list "chatlogs"
# extract them using [[filename_without_extension]]
# mylog <- chatlogs[["example_chat"]]



#### Visualizations

# First, let's start with the in-built plots from WhatsR

example_chat <- chatlogs[["example_chat"]]

## to plot emojis, I implemented my own fix since the package has some issues with emoji rendering. Do NOT use WhatsR::plot_emoji, use the function in the repo instead. By default, it is already masked
## whatsR supports several plot types, we will iterate through them because lazy code is good code

plot_types <- c("heatmap", "cumsum", "bar", "splitbar")

for(plot_type in plot_types) {
  
  myplot <- plot_emoji(example_chat,
                       min_occur = 10,
                       plot = plot_type
  )
  
  ## whatsR uses ggplot2's system, therefore we can save the output the usual way
  print(myplot)
  ggsave(file.path(plot_dir, paste0("emoji_plot_",plot_type,".png")))
  
}


## there are many plots available in whatsR, explore them using autocomplete with WhatsR::
## However, keep in mind that much of the data is not available in our exports (location, media, messages/tokens...)
## you can directly read your own whatsapp chat exports in R if you want to experience the full functionality, or use the simulated chat

WhatsR::plot_links(example_chat)

WhatsR::plot_tokens(simulated_chat, exclude_sm = TRUE)


##### now we will look into creating our own visualizations!

## Firstly, all data needs to be tabular to be understood by ggplot. 
## Our chat-exports may seem tabular, but many entries are in fact *NESTED*, 
## that is, the data frame contains lists inside its elements, e.g. for emojis.
## We will need to deal with this on a case-by-case-basis

## one handy feature from tidyverse is "unnest_longer()"

## emojis


emoji_unnested <- example_chat %>%
  select(Emoji) %>%
  unnest_longer(Emoji) %>%
  filter(!is.na(Emoji))


emoji_counts <- example_chat %>%
  select(Emoji) %>%
  unnest_longer(Emoji) %>%
  filter(!is.na(Emoji)) %>%
  count(Emoji, sort = TRUE)

emoji_counts



### with tabular (and in this case aggregated) data, we can now move on to visualize!



#### GGPlot2 is the definitive package for R visualization. For further info see https://rstudio.github.io/cheatsheets/html/data-visualization.html 

# Every GGPlot is created in the following way:

# ggplot(data = <Data>) +
#   <Geom_Function>(mapping = aes(<Mappings>),
#                   stat = <Stat>,
#                   position = <Position>) +
#   <Coordinate_Function> +
#   <Facet_Function> +
#   <Scale_Function> +
#   <Theme_Function>

# The core functionality is using "+" to add features/visual details/labels etc. to a graph


emoji_counts %>%
  slice_max(n, n = 20) %>%   # Top 20 Emojis
  ggplot(aes(x = reorder(Emoji, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Häufigste Emojis",
    x = NULL,
    y = "Anzahl"
  ) +
  theme_minimal(base_size = 14, base_family = "Segoe UI Emoji")


## uh oh, the emoji display is broken! But there's a workaround (don't ask)



agg_png(
  file.path(plot_dir, "emoji_plot_bar_fixed.png"),
  width = 1200,
  height = 800,
  res = 144
)

emoji_counts %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(Emoji, n), y = n)) +
  geom_col() +
  labs(
    title = "Häufigste Emojis",
    x = "Emoji",
    y = "N"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "emoji")
  )

dev.off()



### lets do the same for links now, as an exercise!

links_unnested <- example_chat %>%
  select(DateTime, URL) %>%
  unnest_longer(URL) %>%
  filter(!is.na(URL))

## now we have one row for each link in the dataset. We can now move on to cumulate them or visualize them otherwise

## basic barplot 

# Count how often each domain appears
domain_counts <- links_unnested %>%
  count(URL, sort = TRUE)

# Plot the 10 most common domains
domain_counts %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(URL, n), y = n)) +
  
  # Create bars
  geom_col() +
  
  # Flip axes so labels are easier to read
  coord_flip() +
  
  # Add labels
  labs(
    title = "Most Shared Domains",
    x = "Domain",
    y = "Number of Shared Links"
  ) +
  
  # Use a clean theme
  theme_minimal()


#### links over time

# Group timestamps into calendar weeks
links_per_week <- links_unnested %>%
  mutate(week = floor_date(DateTime, unit = "week")) %>%
  count(week)

# Plot activity over time
ggplot(links_per_week, aes(x = week, y = n)) +
  
  # Draw a line
  geom_line() +
  
  # Add labels
  labs(
    title = "Shared Links Over Time",
    x = "Week",
    y = "Number of Shared Links"
  ) +
  
  # Clean visual style
  theme_minimal()



#### 


