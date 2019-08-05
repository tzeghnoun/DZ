library(data.table)
library(ggrepel)
library(RColorBrewer)
library(tidyverse)
library(ggthemes)

primary_energy_cons <- fread("data/primary_energy_cons.csv")

setDT(primary_energy_cons)

label_plot <- primary_energy_cons[region %like% "Africa" & year == 2018][order(-primary_energy_consumption)][]

nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(9, "Set1"))(nb.cols)

primary_energy_cons[region %like% "Africa"][, average_Africa := mean(primary_energy_consumption), by = year] %>% 
  ggplot() +
  aes(year, primary_energy_consumption, col = country) +
  geom_line(size = 1) +
  geom_line(aes(year, average_Africa), col = "black", size = 0.9, linetype ="dashed") +
  scale_color_manual(values = mycolors) +
  geom_text_repel(label_plot, mapping = aes(x = 2021, y = primary_energy_consumption, 
                                            label = country), size = 4) +
  annotate(geom = "text", x = 2007, y = 50, label = "Average",
           size = 5, vjust = 2) +
  geom_vline(xintercept = 2000, linetype = "dashed", color = "#8c8c8c", size = 1) +
  labs(
    title = "Primary energy consumption in Africa (mtoe)",
    subtitle = "Time period : 1965 - 2018",
    x = "",
    y = "Energy consumption - mtoe",
    caption = "Source : BP Statistical Review 2019"
  ) +
  scale_x_continuous(limits = c(1965, 2022), breaks = seq(1965, 2022, 5)) +
  theme_economist() +
  theme(legend.position = "none")

ggsave("primary_energy_cons.png", path = "figs/", width = 12, height = 8) 
