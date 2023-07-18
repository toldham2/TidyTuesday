# Load necessary libraries
library(dplyr)
library(ggplot2)
library(maps)
library(tidyr)
library(stringr)
library(gganimate)
library(av)

# URL of the CSV file
zonann_temps <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/zonann_temps.csv'
  )

# Select 'Year' and the temperature anomalies for specific latitude bands
selected_data <- zonann_temps %>%
  select(
    Year,
    `64N-90N`,
    `44N-64N`,
    `24N-44N`,
    `EQU-24N`,
    `24S-EQU`,
    `44S-24S`,
    `64S-44S`,
    `90S-64S`
  )

# Rename 'EQU-24N' and '24S-EQU' to '0N-24N' and '24S-0N' respectively
selected_data <-
  rename(selected_data, `0N-24N` = `EQU-24N`, `24S-0S` = `24S-EQU`)

# Convert data to long format
data_long <-
  pivot_longer(selected_data,
               `64N-90N`:`90S-64S`,
               names_to = "LatitudeBand",
               values_to = "TempAnomaly")

#### Sparklines ####

# Specify the order of the facets
facet_order <-
  c('64N-90N',
    '44N-64N',
    '24N-44N',
    '0N-24N',
    '24S-0S',
    '44S-24S',
    '64S-44S',
    '90S-64S')

# Convert LatitudeBand to a factor with levels specified by facet_order
data_long$LatitudeBand <-
  factor(data_long$LatitudeBand, levels = facet_order)

# Set the default theme
proj_theme <- function() {
  theme(
    text = element_text(family = "Helvetica"),
    legend.position = "bottom",
    legend.justification = "center",
    legend.title = element_text(
      colour = "white",
      size = 10
    ),
    legend.background = element_rect(fill = "#041014"),
    legend.text = element_text(
      colour = "white",
      size = 10
    ),
    plot.background = element_rect(fill = "#041014", colour = "#041014"),
    panel.background = element_rect(fill = "#041014"),
    axis.text = element_text(colour = "white"),
    plot.title = element_text(
      colour = "white",
      size = 8,
      ),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_line(color = "lightgray", linewidth = 0.2)
  )
}

# Plot the sparklines
p_spark <- ggplot(data_long,
       aes(
         x = Year,
         y = TempAnomaly,
         group = LatitudeBand,
         color = TempAnomaly
       )) +
  geom_line(linewidth = 0.5) +
  scale_color_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    guide = "none"
  ) +
  facet_wrap(~ LatitudeBand, dir = "v", ncol = 1) +
  labs(x = "", y = "") +
  proj_theme() +
  transition_reveal(Year)

# Animate the plot
b_gif <- animate(
  p_spark,
  renderer = gifski_renderer(),
  width = 864,
  height = 864,
  res = 300,
  duration = 12
)


#### Map ####

# Add columns for the lower and upper latitude of each band
data_long <- data_long %>%
  mutate(
    LowerLatitude = case_when(
      str_detect(LatitudeBand, "N") ~ as.numeric(str_extract(LatitudeBand, "\\d+(?=N)")),
      str_detect(LatitudeBand, "S") ~ -as.numeric(str_extract(LatitudeBand, "\\d+(?=S)")),
      TRUE ~ -24
    ),
    UpperLatitude = case_when(
      str_detect(LatitudeBand, "N") ~ as.numeric(str_extract(LatitudeBand, "(?<=-)\\d+")),
      str_detect(LatitudeBand, "S") ~ -as.numeric(str_extract(LatitudeBand, "(?<=-)\\d+")),
      TRUE ~ 24
    )
  )

# Create a map
world_map <- map_data("world")

# Plot the map
p <- ggplot() +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "gray20",
    color = "white",
    size = 0.5
  ) +
  geom_rect(
    data = data_long,
    aes(
      xmin = -180,
      xmax = 180,
      ymin = LowerLatitude,
      ymax = UpperLatitude,
      fill = TempAnomaly
    ),
    alpha = 0.7,
    color = NA
  ) +
  scale_x_continuous(expand = c(0,-15)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colors = c("blue", "white", "red"),
    values = scales::rescale(c(-1, 0, 1)),
    guide = "colourbar"
  ) +
  labs(title = "Temperature Anomaly by Latitude Band: Year {frame_time}",
       subtitle = "These set of visualizations demonstrates the change in temperature anomalies from 1880—2022. A temperature anomaly is defined as a deviation from the average temperature for the area compared to its 1951-1980 means.<br>Temperature deviations are more frequent and intense in the arctic and antarctic latitude bands, with consistency increasing closer to the equator. You could say they're *polarized.*<br>The reason for this is the **Albedo Effect:** The poles are covered with ice and snow, which reflect a large amount of the sun's radiation back into space. As temperatures rise, more ice melts, reducing the reflectivity (albedo) of the Earth's surface and allowing more solar radiation to be absorbed, which in turn causes more warming. This feedback loop can lead to rapid temperature changes.") +
  proj_theme() +
  theme(axis.text = element_blank(),
        axis.ticks.y = element_blank())

# Animate the plot
animation <- p +
  transition_time(as.integer(Year)) +
  ease_aes('linear')

a_gif <- animate(
  animation,
  renderer = gifski_renderer(),
  width = 1350,
  height = 1080,
  res = 300,
  duration = 12
)

# Combine the plots

library(magick)
a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = FALSE)
for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = FALSE)
  new_gif <- c(new_gif, combined)
}

new_gif
