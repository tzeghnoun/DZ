library(tidyverse)
library(rvest)

# Algeria national football team 2010–19 results

# Scraping from wikipedia page
url <- 'https://en.wikipedia.org/wiki/Algeria_national_football_team_2010%E2%80%9319_results'

dz_foot_results_10_19 <- url %>% read_html() %>% 
  html_nodes('table') %>% 
  html_table(fill = TRUE)

x <- lapply(3:109, function(x) {dz_foot_results_10_19[[x]][1, ]})
df <- do.call(rbind, x)

fwrite(df, 'data/dz_foot_results_10_19.csv')
