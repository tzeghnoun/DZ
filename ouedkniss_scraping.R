library(data.table)
library(tidyverse)
library(rvest)
# library(robotstxt)


url <- 'https://www.ouedkniss.com'
robotstxt::paths_allowed(url)

# The objectif of this script is a web scrapping of "ouedkniss immobilier annonces"
url_base <- 'https://www.ouedkniss.com/annonces/index.php?c=immobilier&sc=location&prix=1&prix_unite=2&p='
urls <- map(1:100, function(x) {paste0(url_base, x)})

# Because of the fast updating of the ads we decided to scrap the links
# of the ads first then scrap the details of each ads to avoid any
# gaps.

links <- lapply(1:100, function(x) {
  tryCatch(
    urls[[x]] %>%
      read_html() %>%
      html_nodes('.button_details') %>%
      html_attr('href'),
    error = function(e){NA}    # a function that returns NA regardless of what it's passed
  )
})


# ADD THE TIME LINKS WERE SCRAPED : 30/10/2019 between 07:40 & 13h

links <- unlist(links)
links <- sapply(1:1000, function(x) {paste0('https://www.ouedkniss.com/', links[x])})

# Scraping

titre <- lapply(1:1000, function(x) {
  tryCatch( 
    links[x] %>%
      read_html() %>%
      html_nodes('#Title') %>%
      html_text(),
    error = function(e){NA},    # a function that returns NA regardless of what it's passed
    Sys.sleep(sample(seq(1, 3, by=0.001), 1))
  )
  
})

date <- lapply(1:1000, function(x) {
  tryCatch(
    links[x] %>%
      read_html() %>%
      html_nodes('p:nth-child(3) .description_span') %>%
      html_text(),
    error = function(e){NA},    # a function that returns NA regardless of what it's passed
    Sys.sleep(sample(seq(1, 3, by=0.001), 1))
  )    
})

quartier <- lapply(1:1000, function(x) {
  tryCatch(
    links[x] %>%
      read_html() %>%
      html_nodes('#Quartier .description_span') %>%
      html_text(),
    error = function(e){NA},    # a function that returns NA regardless of what it's passed
    Sys.sleep(sample(seq(1, 3, by=0.001), 1))
  )      
})

specs <- lapply(1:1000, function(x) {
  tryCatch(
    links[x] %>%
      read_html() %>%
      html_nodes('.description_label , .description_span') %>%
      html_text(),
    error = function(e){NA},    # a function that returns NA regardless of what it's passed
    Sys.sleep(sample(seq(1, 3, by=0.001), 1))
  )
})

prix <- lapply(1:1000, function(x) {
  tryCatch(
    links[x] %>%
      read_html() %>%
      html_nodes('#Prix span') %>%
      html_text(),
    error = function(e){NA},    # a function that returns NA regardless of what it's passed
    Sys.sleep(sample(seq(1, 3, by=0.001), 1))
  )
})

description <- lapply(1:1000, function(x) {
  tryCatch(
    links[x] %>%
      read_html() %>%
      html_nodes('#GetDescription') %>%
      html_text(),
    error = function(e){NA},    # a function that returns NA regardless of what it's passed
    Sys.sleep(sample(seq(1, 3, by=0.001), 1))
  )
})

# In order to save the lists generated from scraping we use the following command
save(prix, file="data/prix.RData")
# the same for the other lists : date, quartier, specs & titre


# *******************************************************************************
# Cleaning the scraped dataset on 30/10/2019 between 07:40 & 13h
# *******************************************************************************
# Load the lists saved in the data folder
load('data/date.RData')
load('data/prix.RData')
load('data/titre.RData')
load('data/specs.RData')

idx1 <- which(nchar(date) == 12)
idx2 <- which(nchar(prix) == 12)
idx3 <- which(nchar(titre) == 12)
idx4 <- which(nchar(specs) == 12)
date[idx1] <- NA
prix[idx2] <- NA
titre[idx3] <- NA
specs[idx4] <- NA

# Let's focus on the price, the title & specs. Title s specs have no empty entry.
# But as the price is important and has 47 empty entries (idx2) we will tranform
# the value of title & specs related to the idx2 to NA
titre[idx2] <- NA
specs[idx2] <- NA

quartier <- sapply(1:1000, function(x) {
  idx <- which(specs[[x]] %like% 'Quartier')
  idx <- idx + 1
  ifelse(idx > 0, specs[[x]][idx], NA)
  
})

Superficie <- sapply(1:1000, function(x) {
  idx <- which(specs[[x]] %like% 'Superficie')
  idx <- idx + 1
  ifelse(idx > 0, specs[[x]][idx], NA)
  
})

n_piece <- sapply(1:1000, function(x) {
  idx <- which(specs[[x]] %like% 'Nombre de pièces')
  idx <- idx + 1
  ifelse(idx > 0, specs[[x]][idx], NA)
  
})

n_etage <- sapply(1:1000, function(x) {
  idx <- which(specs[[x]] %like% 'étage')
  idx <- idx + 1
  ifelse(idx > 0, specs[[x]][idx], NA)
  
})

Specification <- sapply(1:1000, function(x) {
  idx <- which(specs[[x]] %like% 'Spécifications')
  idx <- idx + 1
  ifelse(idx > 0, specs[[x]][idx], NA)
  
})

dt <- data.table(
  x = 1:1000,
  date = date,
  titre = titre,
  quartier = quartier,
  etage = n_etage,
  piece = n_piece,
  prix = prix,
  superficie = Superficie,
  specification = Specification
)

dt <- dt[, lapply(.SD, as.character), .SDcols = 2:9, by = .(x)]

dt <- dt[, lapply(.SD, unlist), .SDcols = 2:9, by = .(x)]

dt <- dt[, prix := ifelse(prix == '1 DA Négociable', NA, prix)
         ][, c('superficie', 'prix') := .(str_extract(superficie, "\\d+"), str_extract(prix, "\\d+(\\.|,)?\\d+"))
           ][, lapply(.SD, as.numeric), .SDcols = 5:8, by = .(date, titre, quartier, specification)
             ][, prix := ifelse(prix < 1000, prix*10^4, prix)]

# Scraping the list of Wilaya d'Algérie
# url <- 'https://fr.wikipedia.org/wiki/Liste_des_wilayas_d%27Alg%C3%A9rie'
# wilaya <- url %>% read_html() %>% 
#   html_nodes('table') %>% 
#   html_table(fill = TRUE)
# wilaya <- wilaya[[1]][2]
# wilaya <- gsub("Wilaya de", "", wilaya[1:48,])
# wilaya <- gsub("Wilaya d'", "", wilaya)
# wilaya <- str_trim(wilaya, 'both')

dt <- dt[date != 'NA'][, titre := gsub('F\\d?', '', titre)
                       ][, titre := str_replace(titre, '^\\w+\\s\\w+\\s', '')
                         ][, titre := str_replace(titre, '\\w+$', '')
                           ][, titre := gsub('^De Villa\\s+', '', titre)
                             ][, titre := str_trim(titre, 'both')
                               ][, titre := str_extract(titre, '^\\w+')]

dt <- dt[, titre := ifelse(titre == 'Ain', 'Ain-Temouchent', 
                           ifelse(titre == 'Tizi', 'Tizi-Ouzou', 
                                  ifelse(titre == 'El', 'EL-Tarf', 
                                         ifelse(quartier == 'Avenue oujda', 'Oran', 
                                                ifelse(titre == 'Location', 'Alger', titre)))))]


dt[!is.na(prix) & !is.na(superficie) & !is.na(titre)][, .(prix, superficie, titre, piece)
                                                      ][titre == 'Alger'] %>% 
  ggplot() +
  aes(superficie, prix, color = factor(piece)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(limits = c(0, 500)) +
  scale_y_continuous(limits = c(0, 200000)) +
  theme_light()









# Generate the title list. It took almost an hour
# titre <- lapply(1:1000, function(x) {
#   tryCatch(
#     links[x] %>%
#       read_html() %>%
#       html_nodes('#Title') %>%
#       html_text(),
#     error = function(e){NA},    # a function that returns NA regardless of what it's passed
#     Sys.sleep(sample(seq(1, 3, by=0.001), 1))
#   )
# })
# 
# 
# # Description
# description <- lapply(1:1000, function(x) {
#   tryCatch(
#     links[x] %>%
#       read_html() %>%
#       html_nodes('#Description') %>%
#       html_text(),
#     error = function(e){NA},    # a function that returns NA regardless of what it's passed
#     Sys.sleep(sample(seq(1, 3, by=0.001), 1))
#   )
# })
# 
# 
# #Prix
# prix  <- lapply(1:1000, function(x) {
#   tryCatch(
#     links[x] %>%
#       read_html() %>%
#       html_nodes('#Prix span') %>%
#       html_text(),
#     error = function(e){NA},    # a function that returns NA regardless of what it's passed
#     Sys.sleep(sample(seq(1, 3, by=0.001), 1))
#   )
# })
# 

# Unlist title, description & prix lists to create a DT
# There are some entries with 0 character which is equal to 12 if
# we apply nchar(). We need to transform them to NA

idx <- which(nchar(titre) == 12)
titre[idx] <- NA
unlist_titre <- unlist(titre)

idx <- which(nchar(description) == 12)
description[idx] <- NA
unlist_description <- unlist(description)

idx <- which(nchar(prix) == 12)
prix[idx] <- NA
unlist_prix <- unlist(prix)

# Creating a DT with these three vectors

ouedkniss <- data.table(titre = unlist_titre,
                        description = unlist_description,
                        prix = unlist_prix, 
                        link = links)

write.csv('data/ouedkniss_28_10_19_08_03.csv', x = ouedkniss)

# Exploring the DT
test <- fread('data/ouedkniss_28_10_19_08_03.csv', header = TRUE)
idx <- which(is.na(test$titre) | is.na(test$description) | is.na(test$prix))
test <- test[-idx]
