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
library(slider)
library(ragg)
library(tidyverse)
library(WhatsR)

source(file.path("Scripts", "00_Helpers.R")) ## lets again load our helpers, we might need them!

save_dir <- "Data_cleaned"
plot_dir <- "Plots"
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

simulated_chat <- WhatsR::parse_chat(file.path("Data", "Simulated_WhatsR_chatlog.txt")) ## lets load a simulated chat example.


## load all cleaned chats
files <- list.files(save_dir, pattern="\\.rds$")

chatlogs <- list()

for (file in files) {
  chat <- readRDS(file.path(save_dir, file))
  name <- tools::file_path_sans_ext(file)
  
  chatlogs[[name]] <- chat ## now we have a list of all our chatlogs!
}

#View(chatlogs)


## All chatlogs are contained in the list "chatlogs"
# extract them using [[filename_without_extension]]
# mylog <- chatlogs[["example_chat"]]



#### Visualizations

# First, let's start with the in-built plots from WhatsR

example_chat <- chatlogs[["example_chat"]]

## to plot emojis, I implemented my own fix since the package has some issues with emoji rendering. 
## Do NOT use WhatsR::plot_emoji, use the function in the repo instead. By default, it is already masked.
## Just run "plot_emoji", but not WhatsR::plot_emoji
## The function supports several plot types, we will iterate through them because lazy code is good code

plot_types <- c("heatmap", "cumsum", "bar", "splitbar") # always check the documentation! Press F1 while the cursor is within a function name to open it! or write ?function in the CLI.

for(plot_type in plot_types) {
  
  myplot <- plot_emoji(example_chat,
                       min_occur = 10,
                       plot = plot_type
  )
  
  ## whatsR uses ggplot2's system, therefore we can save the output the usual way
  print(myplot)
  ggsave(file.path(plot_dir, paste0("emoji_plot_",plot_type,".png")),
         device = ragg::agg_png,
         bg = "white"
         ) ## we use agg rendering because emoji can be tricky.
  
}


## there are many plots available in whatsR, explore them using autocomplete with WhatsR::
## However, keep in mind that much of the data is not available in our exports (location, media, messages/tokens...)
## you can directly read your own whatsapp chat exports in R if you want to experience the full functionality, or use the simulated chat

WhatsR::plot_links(example_chat)

WhatsR::plot_tokens(simulated_chat, exclude_sm = TRUE) ## we can only plot tokens for the simulated chat since our exports do not contain the whole text.


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

#### A lot of things are possible with ggplot. Do not underestimate the power of a good data viz!
## See this advanced example for instance
## EMOJI X TIME rolling average


plot_emoji_x_time <- function(chatlog, num_emoji = 5, plotname = "placeholder", start_date = as.POSIXct("2010-01-01")) {
  chatlog <- chatlog %>%
    filter(DateTime >= start_date)
  
  
  
  # Unnest emoji column
  emoji_time <- chatlog %>%
    select(DateTime, Emoji) %>%
    unnest_longer(Emoji) %>%
    filter(!is.na(Emoji))
  
  # Find top 10 emojis overall
  top_emojis <- emoji_time %>%
    count(Emoji, sort = TRUE) %>%
    slice_head(n = num_emoji) %>%
    pull(Emoji)
  
  # Count emoji usage per month
  emoji_monthly <- emoji_time %>%
    
    # Keep only top emojis
    filter(Emoji %in% top_emojis) %>%
    
    # Convert timestamps to months
    mutate(month = floor_date(DateTime, "month")) %>%
    
    # Count uses per emoji per month
    count(month, Emoji)
  
  # Compute rolling 3-month average
  emoji_rolling <- emoji_monthly %>%
    
    arrange(Emoji, month) %>%
    
    group_by(Emoji) %>%
    
    mutate(
      rolling_avg = slide_dbl(
        n,
        mean,
        .before = 2,
        .complete = FALSE
      )
    )
  
  # Plot rolling averages
  agg_png(
    file.path(plot_dir, paste0(plotname, ".png")),
    width = 2000,
    height = 1400,
    res = 144
  )
  
  
  ggplot(
    emoji_rolling,
    aes(
      x = month,
      y = rolling_avg,
      color = Emoji
    )
  ) +
    
    # Draw lines
    geom_line(linewidth = 1.2) +
    
    # Labels
    labs(
      title = "Emoji Usage Over Time",
      subtitle = "3-Month Rolling Average",
      x = "Time",
      y = "Average Uses per Month",
      color = "Emoji"
    ) +
    
    # Minimal theme
    theme_minimal(base_size = 14) +
    
    # Larger emoji legend
    theme(
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 14)
    )
  ggsave(
    file.path(plot_dir, paste0(plotname, ".png")),
    device = ragg::agg_png,
    width = 2000,
    height = 1400,
    dpi = 144,
    units = "px",
    bg = "white"
  )
  
}

plot_emoji_x_time(example_chat, 5, "emoji_x_time_plot", as.POSIXct("2020-01-01"))



##################################





### lets write it as a function so that we can easily re-use it!

plot_relative_emoji <- function(chatlog, num_emoji = 5, plotname = "placeholder", start_date = as.POSIXct("2010-01-01")) {
  

  chatlog <- chatlog %>%
    filter(DateTime >= start_date)
  
  # ----------------------------
  # Prepare emoji time data
  # ----------------------------
  
  # Convert list-column into one row per emoji
  emoji_time <- chatlog %>%
    select(DateTime, Emoji) %>%
    unnest_longer(Emoji) %>%
    filter(!is.na(Emoji))
  
  # Find most frequently used emojis overall
  top_emojis <- emoji_time %>%
    count(Emoji, sort = TRUE) %>%
    slice_head(n = num_emoji) %>%
    pull(Emoji)
  
  # ----------------------------
  # Count ALL emoji usage per month
  # ----------------------------
  
  all_emoji_per_month <- emoji_time %>%
    mutate(month = floor_date(DateTime, "month")) %>%
    count(month, name = "total_emoji")
  
  # ----------------------------
  # Count top emoji usage per month
  # and normalize by ALL emoji activity
  # ----------------------------
  
  emoji_monthly <- emoji_time %>%
    
    # Keep only top emojis
    filter(Emoji %in% top_emojis) %>%
    
    # Convert timestamps to months
    mutate(month = floor_date(DateTime, "month")) %>%
    
    # Count emoji usage
    count(month, Emoji, name = "emoji_count") %>%
    
    # Join total emoji activity
    left_join(all_emoji_per_month, by = "month") %>%
    
    # Relative share of all emojis
    mutate(
      emoji_share = emoji_count / total_emoji
    )
  
  # ----------------------------
  # Compute rolling average
  # ----------------------------
  
  emoji_rolling <- emoji_monthly %>%
    
    arrange(Emoji, month) %>%
    
    group_by(Emoji) %>%
    
    mutate(
      rolling_avg = slide_dbl(
        emoji_share,
        mean,
        .before = 2,
        .complete = FALSE
      )
    )
  

  # ----------------------------
  # Plot
  # ----------------------------
  
  ggplot(
    emoji_rolling,
    aes(
      x = month,
      y = rolling_avg,
      color = Emoji
    )
  ) +
    
    # Draw lines
    geom_line(linewidth = 1.4) +
    
    # Labels
    labs(
      title = "Relative Emoji Popularity Over Time",
      subtitle = "3-Month Rolling Average",
      x = "Time",
      y = "Share of All Emoji Usage",
      color = "Emoji"
    ) +
    
    # Clean theme
    theme_minimal(base_size = 16) +
    
    # White background + larger legend text
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 16)
    )
  
  ggsave(
    file.path(plot_dir, paste0(plotname, ".png")),
    device = ragg::agg_png,
    width = 2000,
    height = 1400,
    dpi = 144,
    units = "px",
    bg = "white"
  )
  
}

plot_relative_emoji(example_chat, 5, "rel_emoji", as.POSIXct("2020-01-01"))
#
