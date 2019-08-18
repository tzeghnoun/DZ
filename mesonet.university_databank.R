library(data.table)
library(rio)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(rvest)
library(janitor)
library(ggforce)
library(ggthemes)

nb.cols <- 50
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)
# DZ Temperature from https://mesonet.agron.iastate.edu/request/download.phtml?network=DZ__ASOS
data <- fread("data/asos_dz.txt")
str(data)
data <- data[, c('date', 'temp_c', "valid", "tmpc") := 
               .(lubridate::ymd_hm(data$valid), as.numeric(tmpc), NULL, NULL)]

setcolorder(data, c('date', 'station', 'temp_c', 'lon',  'lat'))
str(data)

data <- data[, day_night := ifelse(hour(date) %between% c(6, 20), 
                                   "Daytime", "Night")] 

# Scraping the OACI code from wikipedia
url <- 'https://fr.wikipedia.org/wiki/Liste_des_a%C3%A9rodromes_en_Alg%C3%A9rie'
table <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE)
code_oaci <- table[[3]] %>% 
  janitor::clean_names()
setDT(code_oaci)
code_oaci <- code_oaci[, .("commune" = commune_de_laerodrome, "station" = code_oaci)]

data <- merge(data, code_oaci, all.x = TRUE)

data[commune %in% c('Adrar', 'Ouargla', 'Tiaret', 
                    'Batna')] %>% 
  ggplot() +
  aes(date, temp_c, color = day_night) +
  geom_jitter(alpha = .1) +
  geom_smooth(se = TRUE) +
  scale_colour_brewer(palette = "Set1", name = "Time of observation") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%B") +
  labs(
    title = "Observed temperature over the current year",
    subtitle = "Red dots are observations made during daytime, blue during the night",
    x = '',
    y = '',
    caption = "© Iowa State University of Science and Technology"
  ) +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major.y= element_line(size=0.1,linetype="dotted", color="#6D7C83"),
        panel.grid.major.x= element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        axis.line.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 13, face = "bold"),
        line = element_line(colour = "black"),
        text = element_text(colour = "black"), 
        axis.line = element_line(size = rel(0.8)), 
        axis.text = element_text(size = rel(1)), 
        axis.text.x = element_text(vjust = 0), 
        axis.text.y = element_text(hjust = 0), 
        axis.ticks = element_line(), 
        axis.ticks.y = element_blank(), 
        axis.title = element_text(size = rel(1)), 
        axis.title.x = element_text(), 
        axis.title.y = element_text(angle = 90)) +
  facet_wrap(~commune, ncol = 2)

data[commune %in% c("Dar El Beïda", "Djanet", "Illizi", "Chlef"), max(temp_c), by = .(date(date), day_night, commune)] %>% 
  ggplot() +
  aes(date, V1, color = commune) +
  geom_line() +
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
        strip.text = element_text(size = 13, face = "bold")
  ) +
  facet_wrap(~day_night)


