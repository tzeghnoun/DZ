source("bp_stats_load_clean.R")
library(ggrepel)
library(RColorBrewer)

primary_energy_cons <- wide_to_long_1(2) %>% 
  .[, .(primary_energy_consumption = round(as.numeric(.SD), 2)), by = .(country,year), 
    .SDcols=3]
# Add regions & clean unused rows (totals & remarks)  
primary_energy_cons <- get_regions(primary_energy_cons)

setDT(primary_energy_cons)
label_plot <- primary_energy_cons[region %like% "Africa" & year == 2018][order(-primary_energy_consumption)][]

primary_energy_cons[region %like% "Africa"] %>% 
  ggplot() +
  aes(year, primary_energy_consumption, col = country) +
  geom_line(size = 1) +
  geom_text_repel(label_plot, mapping = aes(x = 2021, y = primary_energy_consumption, label = country),
                  size = 5) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Primary energy consumption",
    subtitle = "(Million tonnes oil equivalent)",
    x = "",
    y = "Energy consumption - mtoe"
  ) +
  scale_x_continuous(limits = c(1965, 2022), breaks = seq(1965, 2022, 5)) +
  theme_economist() +
  theme(legend.position = "none")
