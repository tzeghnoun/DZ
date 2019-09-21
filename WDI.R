library(data.table)
library(tidyverse)
library(lubridate)
library(janitor)
library(ggrepel)

data <- fread('../various_data/WDIData.csv', header = TRUE)
data <- data[, V64 := NULL]
data <- melt.data.table(data, id.vars = 1:4, measure.vars = 5:ncol(data), variable.name = 'year')
data <- data[, 'year' := as.integer(as.character(year))]
names(data) <- make_clean_names(names(data))
data_18 <- data[year == '2018']
dz <- data[country_name %like% 'Algeria' & !is.na(value)]
# Labor force, female (% of total labor force)		
dz_plot <- dz[indicator_code == 'SL.TLF.TOTL.FE.ZS'] 
dz_plot %>% 
  ggplot() +
  aes(year, value, color = indicator_code) +
  geom_line() +
  labs(
    title = "Labor force, female (% of total labor force)",
    x = '',
    y = '',
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


# Adolescent fertility rate (births per 1,000 women ages 15-19)	
dz[indicator_code == 'SP.ADO.TFRT'] %>% 
  ggplot() +
  aes(year, value) +
  geom_line() +
  labs(
    title = "Adolescent fertility rate (births per 1,000 women ages 15-19)",
    x = '',
    y = '',
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
