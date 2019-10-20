library(data.table)
library(tidyverse)
library(RColorBrewer)
library(ggrepel)

# This is a historical dataset on the modern Olympic Games, including all the Games from Athens 1896 to Rio 2016. 
# basic bio data on athletes and medal results from Athens 1896 to Rio 2016
data <- fread('../various_data/120-years-of-olympic-history-athletes-and-results/athlete_events.csv')

# My color palette
my_col <- c("#C3D7A4", "#52854C", "#4E84C4", "#293352", 
            "#24576D", "#099DD7", "#28AADC", "#248E84",
            "#FFDB6D", "#C4961A", "#F4EDCA", "#D16103",
            "#F2583F", "#96503F", "#000000", "#1B9E77",
            "#D95F02", "#7570B3", "#E7298A", "#66A61E",
            "#E6AB02", "#A6761D", "#666666")

my_col2 <- c('#9c5221', '#FEB303', '#BFC3C2')

# Algerian Participation
data_dz <- fread('data/DZ_Olympic_Games.csv')

# The dataset display the participation of Algeria to Olympic games from 1964 to 2016.
# How many times Algeria participated to the Olympic games?
nrow(data_dz[, .N, Year]) # [1] 15 times
data_dz[, .N, by = .(Year, Sport, Games, Name)
        ][order(Year)
          ][, participant := .N, by = .(Year, Sport, Games, Name)] %>%  ggplot() +
  aes(Sport, participant, fill = Sport) +
  geom_bar(stat = 'identity') +
  labs(
    title = 'Algerian participation in Olympic Games from Tokyo 1964 to Rio 2016',
    x = '',
    y = 'Number of participant',
    caption = '\nSource : https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results'
  ) +
  scale_fill_manual(values = my_col) +
  coord_flip() +
  theme_minimal() + # start with a minimal theme and add what we need
  theme(text = element_text(color = "gray20"),
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank(),
        legend.position = 'none') +
  facet_wrap(~Games, ncol = 8)
ggsave('figs/DZ_part_olympic_games_64_16.png', width = 14, height = 6, dpi = 320)

# Number of medals
data_dz_medal <- data_dz[!is.na(Medal), .(Games, Name, Sex, Age, Sport, Medal)][
  order(Games)
  ][, .N, by = .(Games, Name, Sex, Age, Sport, Medal)] 

data_dz_medal[, name_sport := str_c(Name, Sport, sep = '_')][] %>%  ggplot() +
  aes(name_sport, N, fill = Medal) +
  geom_bar(stat = 'identity') +
  labs(
    title = 'Algerian Olympic medalists from Tokyo 1964 to Rio 2016',
    x = '',
    y = 'Number of Medals',
    caption = '\nSource : https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results'
  ) +
  scale_fill_manual(values = my_col2) +
  scale_y_continuous(breaks = seq(0, 4, 1)) +
  # geom_text(aes(label=Games), vjust=.5, color="black", size=3.5)+
  coord_flip() +
  theme_minimal() + # start with a minimal theme and add what we need
  theme(text = element_text(color = "gray20"),
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank())
ggsave('figs/data_dz_medal_64_16.png', width = 14, height = 6, dpi = 320)
