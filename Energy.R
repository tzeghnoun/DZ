library(data.table)
library(ggrepel)
library(RColorBrewer)
library(tidyverse)
library(ggthemes)

# Reading the dt
primary_energy_cons <- fread("data/primary_energy_consumption_mtoe.csv")
ng_cons <- fread("data/ng_cons_Bcm.csv")
oil_cons <- fread("data/oil_consumption_kbbl.csv")
renewables_cons <- fread("data/renew_energy_consumption_mtoe.csv")

# Loading and selecting the population figures from the consolidated dataset 
data <- fread("../energy_stat/data/BP/bp-stats-review-2019-consolidated-dataset-narrow-format.csv")
data <- data[Var %like% "pop", .(Country, Region, SubRegion, Year, Value)]
names(data) <- c("country", "region", "subregion", "year", "pop")

# Expand the color palette
nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(9, "Set1"))(nb.cols)

# Plotting the Primary Energy consumption per capita
primary_energy_cons <- merge.data.frame(primary_energy_cons, data, by = c("country", "year"))
setDT(primary_energy_cons)[, primary_energy_consumption_pc := round(primary_energy_consumption/pop, 2)]
# Preparing for the customized labels
pec_label_plot <- primary_energy_cons[region %like% "Africa" & year == 2018][order(-primary_energy_consumption)]

primary_energy_cons[region %like% "Africa"] %>% 
  ggplot() +
  aes(year, primary_energy_consumption_pc, col = country) +
  geom_line(size = 1) +
  scale_color_manual(values = mycolors) +
  geom_text_repel(pec_label_plot, mapping = aes(x = 2021, y = primary_energy_consumption_pc, 
                                            label = country), size = 4) +
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

##############################
# OIL

# Plotting the oil consumption per capita
oil_cons <- merge.data.frame(oil_cons, data, by = c("country", "year"))
setDT(oil_cons)[, oil_consumption_pc := round(oil_consumption_barrels/pop, 2)]
# Preparing for the customized labels
pec_label_plot <- oil_cons[region %like% "Africa" & year == 2018][order(-oil_consumption_barrels)]

oil_cons[region %like% "Africa"] %>% 
  ggplot() +
  aes(year, oil_consumption_pc, col = country) +
  geom_line(size = 1) +
  scale_color_manual(values = mycolors) +
  geom_text_repel(pec_label_plot, mapping = aes(x = 2021, y = oil_consumption_pc, 
                                                label = country), size = 4) +
  geom_vline(xintercept = 1999, linetype = "dashed", color = "#8c8c8c", size = 1) +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "orange", size = 1) +
  labs(
    title = "Oil consumption per capita (bbl/day per 1000 people)",
    subtitle = "Time period : 1965 - 2018",
    x = "",
    y = "Oil consumption - bbl/day per 1000 people",
    caption = "Source : BP Statistical Review 2019"
  ) +
  scale_x_continuous(limits = c(1965, 2022), breaks = seq(1965, 2022, 5)) +
  theme_economist() +
  theme(legend.position = "none")

ggsave("oil_cons.png", path = "figs/", width = 12, height = 8) 

#####################
# NATURAL GAS


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

###################################
# Renewables

# Plotting the Primary Energy consumption per capita
renewables_cons <- merge.data.frame(renewables_cons, data, by = c("country", "year"))
setDT(renewables_cons)[, renewables_mtoe_pc := round(renewables_mtoe/pop, 2)]
# Preparing for the customized labels
rec_label_plot <- renewables_cons[region %like% "Africa" & year == 2018][order(-renewables_mtoe)]

renewables_cons %>% 
  ggplot() +
  aes(year, renewables_mtoe_pc, col = country) +
  geom_line(size = 1) +
  scale_color_manual(values = mycolors) +
  geom_text_repel(rec_label_plot, mapping = aes(x = 2021, y = renewables_mtoe_pc, 
                                                label = country), size = 4) +
  geom_vline(xintercept = 2000, linetype = "dashed", color = "#8c8c8c", size = 1) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "orange", size = 1) +
  labs(
    title = "Renewables energy consumption per capita (toe)",
    subtitle = "Time period : 1965 - 2018",
    x = "",
    y = "Energy consumption - toe",
    caption = "Source : BP Statistical Review 2019"
  ) +
  scale_x_continuous(limits = c(1965, 2022), breaks = seq(1965, 2022, 5)) +
  theme_economist() +
  theme(legend.position = "none")
