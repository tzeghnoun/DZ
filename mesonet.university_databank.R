if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(data.table, rio, tidyverse, lubridate, RColorBrewer, rvest,
               janitor, ggforce, ggthemes, ggtext, extrafont, ggupset, tibbletime,
               ggtext, ggrepel, glue, patchwork, cowplot, gtable, grid, magick, scales)

## loading fonts
loadfonts(device = "win", quiet = TRUE)


nb.cols <- 50
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)
# DZ Temperature in 2019 from https://mesonet.agron.iastate.edu/request/download.phtml?network=DZ__ASOS
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

# Adding the commune names to the datatable
data <- merge(data, code_oaci, all.x = TRUE)

data[commune %in% c('Chlef', 'Tamanrasset')] %>% 
  ggplot() +
  aes(date, temp_c, color = day_night) +
  geom_jitter(alpha = .1) +
  geom_smooth(se = TRUE) +
  scale_colour_brewer(palette = "Set1", name = "Time of observation") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "Observed temperature from January to August 2019",
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

ggsave("figs/chlef_Tamenrasset_temperature.png", width = 10, height = 6, dpi = 300)

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

################################################################"
# Trying Frollmean & frollapply
library(ggdark)
data <- fread("data/asos_2019.txt")
 
data <- data[, c('date', 'temp_c', "valid", "tmpc") := 
               .(lubridate::ymd_hm(data$valid), as.numeric(tmpc), NULL, NULL)]

setcolorder(data, c('date', 'station', 'temp_c'))

data_plot <- data[station == 'DAAG' & !is.na(temp_c), .(avg_temp = mean(temp_c)), keyby = date] 

data_plot[, {
  ggplot() +
  aes(date, avg_temp) +
  geom_point(alpha = .08, color = "green") +
  labs(
    title = 'Algiers weather',
    subtitle = "Dots : recorded temperature; Red-line : 6-hr mean & Blue-line : 6-hr median",
    x = '',
    y = 'Temperature (C°)'
  ) +
  geom_line(mapping = aes(date, frollapply(avg_temp, 360, median)), col = "#4E84C4", size = 1L) +
  geom_line(mapping = aes(date, frollmean(avg_temp, 360)), col = '#D16103', size = 1L) +
  scale_x_datetime(date_breaks = '1 month', labels = date_format("%m-%Y") ) +
  dark_mode() 
  }]

ggsave('figs/Algiers_temperature_2019.png', width = 12, height = 6, dpi = 300)

#######################################○""




# Load the dataset of temperature observed since january 2000
dz_temp_00_19 <- fread('data/asos-since_2000.txt')
# Check the structure of the dataset
str(dz_temp_00_19)
# Tranform the column valid to date.time formate using the lubridate package & the variable tmpc to integer
dz_temp_00_19[, c('date', 'tmpc', 'valid') := .(ymd_hm(valid), as.integer(tmpc), NULL)]
# Set an order to the variable
setcolorder(dz_temp_00_19, c('date', 'station', 'tmpc'))

# Scraping the OACI code from wikipedia to add the name of commune to our dataset
url <- 'https://fr.wikipedia.org/wiki/Liste_des_a%C3%A9rodromes_en_Alg%C3%A9rie'
table <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE)
code_oaci <- table[[3]] %>% 
  janitor::clean_names()
setDT(code_oaci)
code_oaci <- code_oaci[, .("commune" = commune_de_laerodrome, "station" = code_oaci)]
rm(table)
# Adding the commune names to the datatable
dz_temp_00_19 <- merge(dz_temp_00_19, code_oaci, all.x = TRUE)

# Perform a str & summary of the dataset
summary(dz_temp_00_19)

# The min record of -35 looks weird. Let's check where & when it have been recorded
index_min <- which.min(dz_temp_00_19$tmpc)
dz_temp_00_19$date[index_min] # It was recorded on "2017-06-24 10:50:00 UTC"
dz_temp_00_19$station[index_min] # recorded at "HME" station corresponding to Hassi Messaoud according to the https://mesonet.agron.iastate.edu
# Hassi Messaoud is also recorded as "DAUH" station with 155275 observations. 'HME' has 24707 observations
# Let's compare the daily average of the recording temperature of both stations
dz_temp_00_19[station %in% c('DAUH', 'HME'), .('avg' = mean(tmpc, na.rm = TRUE)), by = .(dat = as.Date(date), station)] %>% 
  ggplot() +
  aes(dat, avg, col = station) +
  geom_line()

# Station 'DAUH' looks more complete & consistent. I will remove 'HME' station
dz_temp_00_19 <- dz_temp_00_19[!station %like% 'HME']

# Generate a dataset from 2010 to 2019 with quantile (p25, p50 & p75) values by day
dz_temp_10_19 <- dz_temp_00_19[year(date) > 2009, .('min' = min(tmpc), 'average' = mean(tmpc, na.rm = TRUE), 
                                                    'max' = max(tmpc)), by = .(dat = as.Date(date), station, commune)]


dz_temp_10_19 <- melt.data.table(dz_temp_10_19, id.vars = c('dat', 'station', 'commune'), 
                                 measure.vars = 4:6, variable.name = 'dz_tmpc', value.name = 'tmpc')

windowsFonts(robotoc = windowsFont("Roboto Condensed"))

dz_temp_10_19[!is.na(commune) & !commune %in% c('El Bayadh', 'In Guezzam', 'Ghriss')] %>%
  ggplot() +
  aes(dat, tmpc, col = dz_tmpc) +
  geom_line() +
  labs(
    title = "<b style='color: #a50044'>Min, Average & Max </b><b style='color: black'>daily temperature recorded </b><br> <b style ='color:#a50044'>since 2010 </b>",
    x = '', 
    y = '',
    caption = "© Iowa State University of Science and Technology"
  ) +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y") +
  theme_economist(base_size = 12) +
  theme(plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown(lineheight = 1.1),
        legend.position = "none") +
  facet_wrap(~commune, ncol = 3)

ggsave("figs/min_avg_max_tempc.png", width = 12, height = 20, dpi = 300)


# Load data of temperature observed since january 2000 for DAAG only
# Load the dataset 
dz_temp_00_19 <- fread('grep -w DAAG data/asos-since_2000.txt')
setnames(dz_temp_00_19, c('V1', 'V2', 'V3'), c('station', 'date', 'value'))
dz_temp_00_19 <- dz_temp_00_19[, c('date', 'value') := .(ymd_hm(date), as.integer(value))]


dz_2000 <- dz_temp_00_19[year(date) == '2000'
                         ][, am_pm := am(date)
                         ][, lapply(.SD, mean, na.rm = TRUE), .SDcols = 'value', by = .(as_date(date), am_pm)]
dz_temp_10_19 <- dz_temp_00_19[year(date) > 2010
                               ][, am_pm := am(date)
                               ][, lapply(.SD, mean, na.rm = TRUE), .SDcols = 'value', by = .(as_date(date), am_pm)
                                 ][, p_value = value - dz_2000$value, by = .(as_date, am_pm)]

dz_temp_00_19[value < 50 & year(date) > 2010, ] %>% ggplot() +
  aes(date, value_p00) +
  geom_point() +
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
dz_temp_00_19[date %like% '01-01 00:00:00'][]

# Update of dataset of weather since 2000 & the plot
data <- fread('grep DAAG ../various_data/asos_dz_weather_since_2000.txt')
data <- data[, .('date' = ymd_hm(V2), 'temp_c' = as.integer(V3))
             ]
data_plot <- data[, .(mois = month(date), jour = day(date)), by = temp_c] %>% unique()
data_plot <- data_plot[, .(date = paste(mois, jour, sep = '/')), by = temp_c
                       ][, date := as.Date(date, '%m/%d')
                         ][, c('avg', 'mediane') := .(mean(temp_c, na.rm = TRUE),
                                                      median(temp_c, na.rm = TRUE)), by = date]

data_plot[temp_c < 50] %>% ggplot() +
  aes(x = date, y = temp_c) +
  geom_point(color = 'green', alpha = .5) +
  geom_line(mapping = aes(date, mediane), col = "#4E84C4", size = 1L) +
  geom_line(mapping = aes(date, avg), col = '#D16103', size = 1L) +
  scale_x_date(limits=c(as.Date("2019-01-01"), as.Date("2019-12-30")), date_breaks = '1 month', labels = date_format("%B")) +
  labs(
    title = 'Algiers weather since 2000',
    subtitle = 'Green dots: observed temperature; Red_line : mean & Blue_line : median',
    x = '',
    y = ''
  ) +
  dark_mode()

