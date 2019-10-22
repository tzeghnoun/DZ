library(data.table)
library(tidyverse)
library(lubridate)
library(janitor)
library(ggrepel)


# GDP per Capita
GdpPerCap <- fread('data/GdpPerCap.csv', header = TRUE)
GdpPerCap <- melt.data.table(GdpPerCap, id.vars = 1:4, measure.vars = 5:ncol(GdpPerCap), 
                             variable.name = 'year', value.name = 'gdp_per_cap')
GdpPerCap <- GdpPerCap[, date := year(as.Date(year, '%Y'))]
names(GdpPerCap) <- janitor::make_clean_names(names(GdpPerCap))
U
data_plot <- GdpPerCap[country_code %in% c('DZA', 'RUS', 'CHN', 'KOR')] 

data_plot[date > 1989] %>% ggplot() +
  aes(date, gdp_per_cap, color = country_name) +
  geom_line(size = 1) +
  geom_text_repel(data = data_plot[date == 2017], 
                  mapping = aes(x = 2018, y = gdp_per_cap, label = country_name), vjust = 1) +
  scale_x_continuous(breaks = seq(1990, 2019, 3)) +
  scale_y_continuous(limits = c(0, 30000), breaks = seq(0, 30000, 5000)) +
  labs(
    title = "GDP per Capita",
    x = '',
    y = 'GDP per capita, PPP (current international $)',
    caption = 'Source : World Bank Databank'
  ) +
  theme(panel.border = element_blank(),  
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major.y= element_line(size=0.1,linetype="dotted", color="#6D7C83"),
        panel.grid.major.x= element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 10, face = "bold"), 
        axis.ticks = element_blank(), 
        legend.background = element_blank(), 
        legend.key = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_text(size = 13, face = "bold"),
        legend.position = 'none'
  ) 

