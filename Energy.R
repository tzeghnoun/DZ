library(data.table)
library(ggrepel)
library(RColorBrewer)
library(tidyverse)
library(ggthemes)

# Reading the dt
primary_energy_cons <- fread("data/primary_energy_consumption_mtoe.csv")
ng_cons <- fread("data/ng_cons_Bcm.csv")

# Loading and selecting the population figures from the consolidated dataset 
data <- fread("../energy_stat/data/BP/bp-stats-review-2019-consolidated-dataset-narrow-format.csv")
data <- data[Var %like% "pop", .(Country, Year, Value)]
names(data) <- c("country", "year", "pop")

# Expand the color palette
nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(9, "Set1"))(nb.cols)

# Plotting the Primary Energy consumption per capita
primary_energy_cons <- merge.data.frame(primary_energy_cons, data, by = c("country", "year"))
setDT(primary_energy_cons)[, primary_energy_consumption_pc := round(primary_energy_consumption/pop, 2)]
# Preparing for the customized labels
pec_label_plot <- primary_energy_cons[region %like% "Africa" & year == 2018][order(-primary_energy_consumption)]

primary_energy_cons[region %like% "Africa"][, average_Africa := mean(primary_energy_consumption), by = year] %>% 
  ggplot() +
  aes(year, primary_energy_consumption_pc, col = country) +
  geom_line(size = 1) +
  # geom_line(aes(year, average_Africa), col = "black", size = 0.9, linetype ="dashed") +
  scale_color_manual(values = mycolors) +
  geom_text_repel(pec_label_plot, mapping = aes(x = 2021, y = primary_energy_consumption_pc, 
                                            label = country), size = 4) +
  # annotate(geom = "text", x = 2007, y = 50, label = "Average",
  #          size = 5, vjust = 2) +
  geom_vline(xintercept = 2000, linetype = "dashed", color = "#8c8c8c", size = 1) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "orange", size = 1) +
  labs(
    title = "Primary energy consumption per capita (toe)",
    subtitle = "Time period : 1965 - 2018",
    x = "",
    y = "Energy consumption - toe",
    caption = "Source : BP Statistical Review 2019"
  ) +
  scale_x_continuous(limits = c(1965, 2022), breaks = seq(1965, 2022, 5)) +
  theme_economist() +
  theme(legend.position = "none")

ggsave("primary_energy_cons.png", path = "figs/", width = 12, height = 8) 




ng_cons <- merge.data.frame(ng_cons, data, by = c("country", "year"))
setDT(ng_cons)[, c("pop", "ng_cons_per_capita") := 
                     .(round(pop, 2), round(1000*gas_consumption_bcm/(pop), 2))]

data_plot <- ng_cons[region %like% "Africa" & year == 2018] 

ng_cons[region %like% "Africa"] %>% 
  ggplot() +
  aes(year, ng_cons_per_capita, color = country) +
  geom_line(size = 1) +
  geom_text_repel(data_plot, mapping = aes(x = 2021, y = ng_cons_per_capita, 
                                           label = country), size = 4) +
  labs(
    title = "Natural gas consumption per capita (cubic meters per person)",
    subtitle = "Time period : 1965 - 2018",
    x = "",
    y = "Natural gas consumption in Africa (cm/person)",
    caption = "Source : BP Statistical Review 2019"
  ) +
  scale_x_continuous(limits = c(1965, 2022), breaks = seq(1965, 2022, 5)) +
  theme_economist() +
  theme(legend.position = "none")

ggsave("natural_gas_cons.png", path = "figs/", width = 12, height = 8)
