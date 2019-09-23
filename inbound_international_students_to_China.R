library(data.table)
library(tidyverse)
library(janitor)

data <- fread('data/students_china_2011_2016.csv')

# ISO	ISO-a3 codes for countries
# Country	Home country of foreign students who studied in China
# Region	Home region of foreign students who studied in China
# Total	Total number of foreign students who studied in China (excluding Hong Kong and Macau)
# Short 	Number of foreign students who studied in China (excluding Hong Kong and Macau) for less than six months
# Long	Number of foreign students who studied in China (excluding Hong Kong and Macau) for more than six months
# Year	Year in which foreign students studied in China

data <- melt.data.table(data, id.vars = 1:3, measure.vars = 4:ncol(data), 
                        variable.name = 'year_type')
data <- data[, c('year', 'type') := tstrsplit(year_type, " ", fixed=TRUE)
             ][,  c('year', 'value', 'year_type') := 
                 .(as.integer(year), as.integer(value), NULL)]
setcolorder(data, c("ISO", "Country", "Region", "year", "type", "value")) 

data[Country == 'Algeria'] %>% ggplot() +
  aes(year, value, color = type) +
  geom_line() +
  labs(
    title = "Inbound International Students to China, 2011-2016",
    subtitle = '# of Algerian students',
    x = '',
    y = '',
    caption = 'Source : China’s Foreign Affairs (2012-2017)'
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
        strip.text = element_text(size = 13, face = "bold")
  ) 
