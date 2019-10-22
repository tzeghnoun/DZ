library(data.table)
library(rvest)
library(robotstxt)
library(tidyverse)
library(stringi)
library(lubridate)

my_col <- c("#C3D7A4", "#28AADC", "#24576D", "#248E84",
            "#FFDB6D", "#C4961A", "#F4EDCA", "#D16103",
            "#F2583F", "#96503F", "#000000", "#1B9E77",
            "#C3D7A4", "#52854C", "#4E84C4", "#293352",
            "#D95F02", "#7570B3", "#E7298A", "#66A61E",
            "#E6AB02", "#A6761D", "#666666")

books <- fread('data/berzakh_catalogue.csv', header = TRUE,
               col.names = c('id', 'date', 'auteur', 'titre', 'resume'))
 
books <- books[, date := gsub('Aout', 'Août', date)
               ][, date := dmy(paste0('01 ', date), locale = 'French')]

data_plot <- books[, .N, by = .(year(date), months(date), auteur, titre)]
data_plot <- data_plot[year != 2019
                       ][, period := ifelse(year > 2011, '2012-2018', '2002-2011')]
data_plot$months <- factor(data_plot$months, 
                           levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

data_plot %>% ggplot() +
  aes(months, N, fill = period) +
  geom_bar(stat = 'identity') +
  scale_x_discrete( name = "Month" ) +
  # scale_x_date(date_breaks = 'month', date_labels = "%b") +
  scale_fill_manual(values = sample(my_col)) +
  labs(
    title = 'Number of book titles published by the Algerian book publisher Barzakh',
    subtitle = 'Time period : 2002 - 2019',
    x = '',
    y = 'Number of books published',
    caption = '\nhttp://www.editions-barzakh.com/catalogue'
  ) +
  coord_flip() +
  theme(text = element_text(color = "gray20"),
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        # axis.title.x = element_text(hjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_line(color = "gray40", size = 0.5),
        panel.grid.major = element_line(color = "white", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect('white'))

ggsave('figs/barzakh_published_books.png', width = 14, height = 6, dpi = 320)

data_plot[year(date) < 2019 & year(date) > 2011, .(.N/7), by = .(months(date))
          ][order(-V1)
            ][]
data_plot[year(date) < 2012 & year(date) > 2002, .(.N/10), by = .(months(date))
          ][order(-V1)
            ][]

# Find bellow as comments the scrapping steps

# robotstxt::paths_allowed(domain = "http://www.editions-barzakh.com/")
# 
# url <- 'http://www.editions-barzakh.com/catalogue?page='
# urls <- map2_chr(url, 1:20, function(x, y) {paste0(x, y)})
# 
# titre <- function(x) {
#   urls[x] %>% 
#   read_html() %>% 
#   html_nodes('h3') %>% 
#   html_text()
# }
# 
# resume <- function(x) {
#   tryCatch(
#     urls[x] %>% 
#       read_html() %>% 
#       html_nodes('p') %>% 
#       html_text(), 
#     error = function(e){NA}    # a function that returns NA regardless of what it's passed
#   )
# }
# 
# 
# titre_data <- lapply(1:20, titre)
# titre_data_unlist <- unlist(titre_data)
# 
# # Need to change the urls to get the resume & the date
# url <- 'http://www.editions-barzakh.com/catalogue/'
# # Remove the french accent in the title list
# titre_url <- stri_trans_general(str = titre_data_unlist, id = "Latin-ASCII")
# 
# titre_url <- gsub("\\s$", "", titre_url)
# titre_url <- gsub(" ", "-", titre_url)
# titre_url <- gsub(",", "", titre_url)
# titre_url <- gsub("'", "", titre_url)
# titre_url <- gsub("\\(", "", titre_url)
# titre_url <- gsub("\\)", "", titre_url)
# titre_url <- tolower(titre_url)
# 
# urls <- map2_chr(url, titre_url, function(x, y) {paste0(x, y)})
# 
# 
# resume_data <- lapply(1:198, resume)
# # Check the links that doesn't work
# # index <- is.na(resume_data)
# # which(index)
# 
# resume_data_unlist <- sapply(1:198, function(x) {do.call(paste, c(as.list(resume_data[[x]]), sep = "."))})
# 
# # Auteur
# auteur_data <- lapply(1:198, function(x) {
#   tryCatch(
#     urls[x] %>% 
#       read_html() %>% 
#       html_nodes('.author-link') %>% 
#       html_text(), 
#     error = function(e){NA}    # a function that returns NA regardless of what it's passed
#   )
# })
# 
# auteur_data <- lapply(1:198, function(x) { ifelse(length(auteur_data[[x]]) == 0, NA, auteur_data[[x]])})
# auteur_data_unlist <- unlist(auteur_data)
# 
# # Date
# date_data <- lapply(1:198, function(x) {
#   tryCatch(
#     urls[x] %>% 
#       read_html() %>% 
#       html_nodes('h4') %>% 
#       html_text(), 
#     error = function(e){NA}    # a function that returns NA regardless of what it's passed
#   )
# })
# 
# date_data_unlist <- unlist(date_data)
# 
# # Concatenate the vectors to create a DT
# berzakh_catalogue <- data.table(date_data_unlist, auteur_data_unlist, titre_data_unlist, resume_data_unlist)
# 
# write.csv(berzakh_catalogue, 'data/berzakh_catalogue.csv')

