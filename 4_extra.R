#### 4: Extra material ####

# create directory for output (if it doesn't already exist)
dir.create("fig", showWarnings = FALSE)

# load libraries
library(readr)
library(dplyr)
library(gtsummary)
library(rnaturalearth)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(magick) # requires ImageMagick to be installed

# load data
locs <- read_csv("data/locations.csv") # location data
map <- ne_countries(returnclass = "sf") # map data

# remove Monaco
locs <- locs %>% filter(city != "Monaco")

# create map of cities in dataset

# create map
p_map <- ggplot() +
  geom_sf(data = map) +
  geom_point(data = locs, aes(x = lon, y = lat),
             size = 1.85, alpha = 1, stroke = NA) +
  ylim(-55, 90) +
  theme_classic() +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL)

# create table of cities by continent
map_tab <- data.frame(t(paste0(
  locs$continent %>% table(),
  " (",
  paste0(round(locs$continent %>% table() / nrow(locs) * 100, 0), "%"),
  ")")))
names(map_tab) <- names(locs$continent %>% table())
p_map_tab <- tableGrob(map_tab, theme=ttheme_minimal(), rows = NULL)

# save combined plot
p_map_final <- p_map / p_map_tab + plot_layout(heights = c(1, 0, 1))
ggsave(plot = p_map_final, filename = "fig/fig_s1.png", width = 7.5, height = 7, dpi = 300)
image_write(image_border(image_trim(image_read("fig/fig_s1.png")), "white", "15x15"), "fig/fig_s1.png")
             