library(data.table)
library(tidyverse)
library(lubridate)
library(janitor)
library(ggrepel)

dz_plot <- fread('data/wdi_dz.csv', header = TRUE)

# Labor force, female (% of total labor force)		
dz_plot[indicator_code == 'SL.TLF.TOTL.FE.ZS'] %>% 
  ggplot() +
  aes(year, value) +
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
ggsave("labor_force_fe.png", path = "figs/", width = 12, height = 8)

# Adolescent fertility rate (births per 1,000 women ages 15-19)	
dz_plot[indicator_code == 'SP.ADO.TFRT'] %>% 
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
ggsave("adolescent_fertility_rate.png", path = "figs/", width = 12, height = 8)

# CO2 emissions (kg per 2010 US$ of GDP)	
dz_plot[indicator_code %like% 'EN.ATM.CO2E.KD.GD'] %>% 
  ggplot() +
  aes(year, value) +
  geom_line() +
  labs(
    title = "CO2 emissions (kg per 2010 US$ of GDP)",
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
ggsave("co2_emissions.png", path = "figs/", width = 12, height = 8)

# Final consumption expenditure (% of GDP)	
dz_plot[indicator_code %like% 'NE.CON.TOTL.ZS'] %>% 
  ggplot() +
  aes(year, value) +
  geom_line() +
  labs(
    title = "Final consumption expenditure (% of GDP)",
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
ggsave("final_consumption_expend.png", path = "figs/", width = 12, height = 8)

# GDP per capita (constant 2010 US$)
dz_plot[indicator_code == 'NY.GDP.PCAP.KD'] %>% 
  ggplot() +
  aes(year, value) +
  geom_line() +
  geom_vline(xintercept = 1985, linetype = 'dashed') +
  geom_vline(xintercept = 1994, linetype = 'dashed') +
  scale_x_continuous(limits = c(1960, 2018), breaks = seq(1960, 2018, 2)) +
  labs(
    title = "GDP per capita (constant 2010 US$)",
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
        axis.text.x = element_text(angle = 90, vjust = 1),
        axis.ticks = element_blank(), 
        legend.background = element_blank(), 
        legend.key = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_text(size = 13, face = "bold"),
        legend.position = 'none'
  ) 
ggsave("gdp_per_capita.png", path = "figs/", width = 12, height = 8)

# Imports of goods and services (% of GDP)
dz_plot[indicator_code == 'NE.IMP.GNFS.ZS'] %>% 
  ggplot() +
  aes(year, value) +
  geom_line() +
  scale_x_continuous(limits = c(1960, 2018), breaks = seq(1960, 2018, 2)) +
  labs(
    title = "Imports of goods and services (% of GDP)",
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
        axis.text.x = element_text(angle = 90, vjust = 1),
        axis.ticks = element_blank(), 
        legend.background = element_blank(), 
        legend.key = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_text(size = 13, face = "bold"),
        legend.position = 'none'
  ) 
ggsave("import_goods_services.png", path = "figs/", width = 12, height = 8)

# Mobile cellular subscriptions (per 100 people)
dz_plot[indicator_code == 'IT.CEL.SETS.P2' & year > 2000] %>% 
  ggplot() +
  aes(year, value) +
  geom_line() +
  scale_x_continuous(limits = c(2000, 2018), breaks = seq(2000, 2018, 2)) +
  labs(
    title = "Mobile cellular subscriptions (per 100 people)",
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
        axis.text.x = element_text(angle = 90, vjust = 1),
        axis.ticks = element_blank(), 
        legend.background = element_blank(), 
        legend.key = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_text(size = 13, face = "bold"),
        legend.position = 'none'
  )
ggsave("mobile_cellular_subsc.png", path = "figs/", width = 12, height = 8)

# Population growth (annual %)
dz_plot[indicator_code == 'SP.POP.GROW'] %>% 
  ggplot() +
  aes(year, value) +
  geom_line() +
  scale_x_continuous(limits = c(1960, 2018), breaks = seq(1960, 2018, 2)) +
  labs(
    title = "Population growth (annual %)",
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
        axis.text.x = element_text(angle = 90, vjust = 1),
        axis.ticks = element_blank(), 
        legend.background = element_blank(), 
        legend.key = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_text(size = 13, face = "bold"),
        legend.position = 'none'
  )
ggsave("population_growth.png", path = "figs/", width = 12, height = 8)
