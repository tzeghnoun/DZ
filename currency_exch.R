library(data.table)
library(tidyverse)
library(ggrepel)
library(lubridate)

data <- fread("data/currency_rate.csv")
str(data)

data[!LOCATION %in% c('USA') & TIME > 1980] %>% ggplot() +
  aes(TIME, Value, color = LOCATION) +
  geom_line() +
  # scale_y_continuous(limits = c(0, 30)) +
  geom_text_repel(data = data[TIME == '2018' & Value > 500 & !LOCATION %in% c('USA')], mapping = aes(TIME, Value, label = LOCATION)) +
  theme(panel.background = element_rect(fill = 'white'),
        legend.position = 'none')

data[LOCATION %in% c('CHN', 'JPN', 'KOR', 'IND')] %>% ggplot() +
  aes(TIME, Value, color = LOCATION) +
  geom_line() +
  geom_text_repel(data = data[TIME == '2018' & LOCATION %in% c('CHN', 'JPN', 'KOR', 'IND')], mapping = aes(TIME, Value, label = LOCATION)) +
  # geom_ribbon(data = data[LOCATION == 'CHN' & TIME > 2006], mapping = aes(ymin = 6, ymax = 7, fill = 'red'), alpha = .3) +
  # geom_vline(xintercept = 2008, linetype = 'dashed') +
  scale_x_continuous(c(1950, 2018), breaks = seq(1950, 2018, 5)) +
  theme(panel.background = element_rect(fill = 'white'),
        legend.position = 'none')

# China / U.S. Foreign Exchange Rate
chnusd <- fread('data/EXCHUS.csv')
str(chnusd)
chnusd <- chnusd[, c('date', 'value') := .(ymd(DATE), EXCHUS)]
chnusd %>% ggplot() +
  aes(date, value) +
  geom_line() +
  geom_ribbon(data = chnusd[year(date) > 2007], mapping = aes(ymin = 6, ymax = 7, fill = 'red'), 
              alpha = .2) +
  labs(
    title = 'China / U.S. Foreign Exchange Rate',
    x = '',
    y = ''
  ) +
  theme(
    panel.background = element_rect(fill = 'white'),
    legend.position = 'none')




