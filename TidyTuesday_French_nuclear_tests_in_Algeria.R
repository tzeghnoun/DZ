library(data.table)
library(tidyverse)
library(lubridate)
library(maps)
library(RColorBrewer)
library(ggrepel)
library(ggdark)
library(gganimate)

# Upload the dataset from the TidyTuesday page on github
nuclear_explosions <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

# Check the dataset
str(nuclear_explosions)

# Transform the variable date_long to a date format with the lubridate package
nuclear_explosions[, my_date := ymd(date_long)]

# Create an animated map of the French nuclear tests in Algeria
dz_map <- nuclear_explosions[region %like% "ALG"] %>%
  ggplot(aes(longitude, latitude)) +
  borders("world", regions = "Algeria") +
  geom_point(size = 10, alpha = .5, color = "green") +
  geom_text_repel(mapping = aes(longitude, latitude, 
                                label = as.character(my_date)), nudge_x = 1, 
                  color = "#948f8f", size = 7) +
  scale_color_manual(values = mycolors) +
  annotate(geom="text", x=-1, y= 28, label="REGGANE", color="#6D7C83", 
           size=10, hjust=0.5,vjust=0, fontface="bold") +
  annotate(geom="text", x=7, y= 27, label="IN ECKER", color="#6D7C83", 
           size=10, hjust=0.5,vjust=0, fontface="bold") +
  transition_states(my_date,1,1) +
  shadow_mark(past=TRUE) +
  labs(
    title = "French nuclear tests in Algeria",
    subtitle = 'Time period : 1960 - 1966',
    caption = "Source : Stockholm International Peace Research Institute"
  ) +
  dark_mode(theme_void(base_size = 22))

options(gganimate.dev_args = list(width = 900, height = 900))
p_ani <- animate(dz_map, nframes = 90, fps=3, detail = 1)
p_ani

# Save the gif
anim_save("map_ani.gif")