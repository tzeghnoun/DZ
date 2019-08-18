library(data.table)
library(rio)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
nb.cols <- 50
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)
# DZ Temperature from https://mesonet.agron.iastate.edu/request/download.phtml?network=DZ__ASOS
data <- fread("coursera/asos_dz.txt")
str(data)
data <- data[, c('date', 'temp_c', "valid", "tmpc") := 
               .(lubridate::ymd_hm(data$valid), as.numeric(tmpc), NULL, NULL)]

setcolorder(data, c('date', 'station', 'temp_c', 'lon',  'lat'))
str(data)

Algiers <- data[station %like% 'DAAG'][, day_night := ifelse(hour(date) %between% c(6, 20), 
                                                             "day", "night")] 
Algiers %>% 
  ggplot() +
  aes(date, temp_c, color = day_night) +
  geom_jitter(alpha = .2) +
  geom_smooth(se = TRUE) +
  scale_colour_brewer(palette = "Set1") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%B") +
  labs(
    title = "Observed temperature over the current year",
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
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank())  
Algiers[, mean(temp_c, na.rm = TRUE), by = .(quarter(date), day_night)]


data[!is.na(temp_c)] %>% 
  ggplot() +
  aes(date, temp_c, color = station) +
  geom_jitter(alpha = .2) +
  scale_color_manual(values = mycolors) +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major.y= element_line(size=0.1,linetype="dotted", color="#6D7C83"),
        panel.grid.major.x= element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank()) 
