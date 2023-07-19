
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggridges)

# Load Data ---------------------------------------------------------------

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv')


# Visualize -----------------------------------------------------------------

# Create the ridgeline plot, facetted by kind
ggplot(df, aes(x = .pred_AI, y = detector, fill = detector)) +
  geom_density_ridges() +
  facet_grid(kind ~ .) +
  labs(x = ".pred_AI", y = "Detector", title = "Ridgeline plot of .pred_AI distributions grouped by detector") +
  theme_ridges()  # make it pretty!