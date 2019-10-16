library(data.table)
library(tidyverse)
library(janitor)
library(ggrepel)

data <- fread('../various_data/gdppercapita_const_2011.csv')
noms <- as.character(data[1])
names(data) <- noms 
data <- data[2:nrow(data)]
data <- data[, !c(5:34)]
data <- melt(data, id.vars = 1:4, measure.vars = 5:ncol(data), variable.name = 'year')
data <- data[, c('year', 'value') := .(as.integer(as.character(year)), round(value, 2))]
names(data) <- names(data) %>% make_clean_names()

data_plot <- data[country_code %in% c('DZA', 'CHN', 'UMC', 'LMY')]

# Plotting
data_plot[country_code %in% c('DZA', 'CHN')] %>% ggplot() +
  aes(year, value, color = country_name) +
  geom_line(size = 1) +
  geom_point(size = 4, alpha = .1) +
  # scale_color_brewer(palette = 'Dark2') +
  geom_line(data = data_plot[country_code %in% c('UMC', 'LMY')], 
            mapping = aes(year, value), linetype = 'dashed', size = 1) +
  scale_color_brewer(palette = 'Dark2') +
  geom_text_repel(data = data_plot[year == 2017], 
                  mapping = aes(x = 2019, y = value, label = country_name), hjust = 1) +
  scale_x_continuous(breaks = seq(1990, 2019, 2)) +
  # scale_y_continuous(limits = c(0, 30000), breaks = seq(0, 30000, 5000)) +
  
  labs(
    title = "GDP per capita, PPP (constant 2011 international $)",
    subtitle = 'Selected Countries: China & Algeria',
    x = '',
    y = 'GDP per capita, PPP (constant 2011 international $)',
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

RColorBrewer::display.brewer.all()
