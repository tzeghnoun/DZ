library(data.table)
library(tidyverse)
library(rvest)
library(lubridate)

my_col <- c("#C3D7A4", "#28AADC", "#24576D", "#248E84",
            "#FFDB6D", "#C4961A", "#F4EDCA", "#D16103",
            "#F2583F", "#96503F", "#000000", "#1B9E77",
            "#C3D7A4", "#52854C", "#4E84C4", "#293352",
            "#D95F02", "#7570B3", "#E7298A", "#66A61E",
            "#E6AB02", "#A6761D", "#666666")

# Algeria national football team 2010–19 results

# Scraping from wikipedia page
url <- 'https://en.wikipedia.org/wiki/Algeria_national_football_team_2010%E2%80%9319_results'

dz_foot_results_10_19 <- url %>% read_html() %>% 
  html_nodes('table') %>% 
  html_table(fill = TRUE)

x <- lapply(3:109, function(x) {dz_foot_results_10_19[[x]][1, ]})
df <- do.call(rbind, x)

fwrite(df, 'data/dz_foot_results_10_19.csv')

# Reading the csv file
data <- fread('data/dz_foot_results_10_19.csv')
data <- data[, X3 := gsub('\\D+', ' ', X3)]
data <- data[, c('home_goals', 'away_goals', 'home_penalty', 'away_penalty') := tstrsplit(X3, ' ')]
data <- data[, X1 := gsub('(\\d+-\\d+-\\d+)(.+)', '\\1', X1)]
names(data) <- c('date', 'home_team', 'result', 'away_team', 'location', 
                 'home_goals', 'away_goals', 'home_penalty', 'away_penalty')
data <- data[, date := dmy(date)
             ][, lapply(.SD, as.integer), .SDcols = 6:9, by = .(date, home_team, away_team, location)
               ][, c('goal_diff', 'penalty_diff') := .(home_goals - away_goals, home_penalty - away_penalty)]
data <- data[, result := (ifelse(is.na(penalty_diff), ifelse(goal_diff > 0, paste0(home_team, '_', 'wins'),
                                                            ifelse(goal_diff < 0, paste0(away_team, '_', 'wins'), 'Tie')),
                          ifelse(penalty_diff > 0, paste0(home_team, '_', 'wins'), paste0(away_team, '_', 'wins'))))]

data <- data[, 'Algeria_result' := ifelse(result =='Tie', 'Draw', ifelse(result %like% 'Algeria', 'Win', 'Loss'))]

data_plot  <- data[, .N, by = .(year(date), Algeria_result)][order(year, -Algeria_result)] 

data_plot$lab_ypos <- ave(data_plot$N, data_plot$year, FUN=cumsum) 
data_plot <- data_plot[, lab_ypos := lab_ypos - (0.5 * N)] 


data_plot %>% 
  ggplot() +
  aes(year, N, fill = Algeria_result) +
  geom_bar(stat = 'identity') +
  scale_x_continuous(breaks = seq(2010, 2019, 1)) +
  scale_fill_manual(values = my_col, name = "Result") +
  geom_text(aes(y = lab_ypos, label = N),  color = "white", size = 4.5, 
            fontface=2) +
  labs(
    title = 'Algeria national football team 2010–19 results',
    subtitle = 'All kind of competitions',
    x = '',
    y = 'Number of matches played\n',
    caption = '\nSource: https://en.wikipedia.org/wiki/Algeria_national_football_team_2010%E2%80%9319_results#cite_note-Algeria_v_Nigeria-7'
  ) +
  theme(text = element_text(color = "gray20"),
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(hjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = "white", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect('white'))

ggsave('figs/dz_foot_results_10_19.png', width = 10, height = 6, dpi = 320)
