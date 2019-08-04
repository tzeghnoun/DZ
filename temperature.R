library(vroom)
library(data.table)
library(tidyverse)
library(lubridate)
library(ggeconodist)
library(patchwork)
library(hrbrthemes)

data <- vroom("data/tas_1901_2016_DZA.csv")
data <- as.data.table(data)
data <- data[, mois := str_extract(tolower(Statistics), "[a-z]{3}")]
data <- data[, date := paste0(mois, "-", Year)]
x <- rep(1:12, 116)
data <- data[, c("mois", "jour") := .(rep(1L:12L, 116), rep(1L, 1392))]
data <- data[, date := make_date(year = Year, month = mois, day = jour)]
data <- data[, c("Temperature - (Celsius)", "date")]

names(data) <- c("temperature", "date")
data <- data[, mois := month(date)]
data$mois <- factor(data$mois, levels = c(1:12), labels = c("January", "February", "March", "April", "May", "June", "July",
                                   "August", "September", "October", "November", "December"), ordered=TRUE)

gg <- data %>% 
  ggplot() +
  aes(factor(mois), temperature) +
  geom_econodist(width = 0.25) +
  scale_y_continuous(expand = c(0,0), position = "left", limits = range(0,35), breaks = seq(0, 35, 5)) +
  labs(
    x = NULL, y = NULL,
    title = "Historical monthly temperature for Algeria." ,
    subtitle = "Time period : 1901 - 2016",
    caption = "Source : worldbank.org | @D.Malko"
  ) +
  theme_econodist()

grid.newpage()
left_align(gg, c("subtitle", "title", "caption")) %>% 
  add_econodist_legend(econodist_legend_grob(), below = "subtitle") %>% 
  grid.draw()
ggsave("dz_historical_monthly_temperature.png", width = 12, height = 10, dpi = 300)

data_plot <- data[, lapply(.SD, mean), .SDcols = 1, by = year(date)] 


g <- data_plot %>% 
  ggplot() +
  aes(year, temperature) +
  geom_line(size = 0.8, color = "darkgrey", alpha = 0.5, show.legend = TRUE) +
  geom_ribbon(aes(ymin = -Inf, ymax = temperature), fill = "grey", alpha = 0.3) +
  geom_smooth(method = 'loess', se = FALSE, show.legend = TRUE) +
  geom_point(size = 1.25, color = "blue") +
  geom_point(data = filter(data_plot, year == 2016), aes(year, temperature), 
             color = "red", alpha = 0.2, size = 15) +
  geom_point(data = filter(data_plot, year == 2016), aes(year, temperature), 
             color = "red", alpha = 0.5, size = 5) +
  geom_point(data = filter(data_plot, year == 2016), aes(year, temperature), 
             color = "red", size = 2) +
  scale_x_continuous(limits = range(1901,2016), breaks = seq(1901, 2016, 5)) +
  labs(
    x = NULL, y = NULL,
    title = "Annual average temperature for Algérie" ,
    subtitle = "Time period : 1901 - 2016",
    caption = "Source : worldbank.org | @D.Malko"
  ) +
  theme_econodist() +
grid.newpage()
left_align(g, c("subtitle", "title", "caption")) %>% 
  grid.draw()

ggsave("dz_annual_average_temperature.png", width = 12, height = 10, dpi = 300)
