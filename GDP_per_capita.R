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
  geom_point(size = 4, alpha = .2) +
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

ggsave('figs/DZ_CHN_GDP_per_capita.png', width = 10, height = 6, dpi = 300)


# World Economic Outlook, ©IMF (October 2019)
# GDP per capita, current prices\n (U.S. dollars per capita)
data <- import('../various_data/imf-GDPpercapita-20191017.xls')
setDT(data)
data <- melt.data.table(data, id.vars = 1, measure.vars = 2:ncol(data), variable.name = 'year')
names(data) <- c('country', 'year', 'gdp_per_cap')

# Custome palette 
my_col <- c("#E69F00", "#0072B2", "#56B4E9", "#D55E00")

# Plot
data <- data[!is.na(country)
             ][, c('year', 'gdp_per_cap') := .(as.integer(as.character(year)),
                                               as.numeric(gdp_per_cap))]
data[country %in% c('Algeria', "China, People's Republic of", 'North Africa', 'Emerging and Developing Asia') & year < 2020] %>% ggplot() +
  aes(year, gdp_per_cap, color = country) +
  geom_line(size = 1) +
  geom_point(size = 2, alpha = .3) +
  geom_text_repel(data = data[year == 2020 & country %in% c('Algeria', "China, People's Republic of", 'North Africa', 'Emerging and Developing Asia')],
                  mapping = aes(x = year, y = gdp_per_cap, label = country), 
                  hjust = 1) +
  scale_color_manual(values = my_col) +
  scale_x_continuous(c(1980, 2020), breaks = seq(1980, 2020, 5)) +
  labs(
    title = 'GDP per capita, current prices',
    subtitle = 'Selected countries/regions : Algeria, China, North Africa, Emerging and Developing Asia',
    x = '',
    y = 'U.S. dollars per capita',
    caption = 'Source: World Economic Outlook, ©IMF (October 2019)'
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

ggsave('figs/DZ_CHN_IMF_GDP_per_capita.png', width = 10, height = 6, dpi = 300)
