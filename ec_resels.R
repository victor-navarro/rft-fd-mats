library(tidyverse)
source("helpers.R")

get_ec <- function(resels, z_scores = seq(0, 5, 0.1)) {
  return(resels * (4 * log(2)) * (2 * pi)^(-1.5) *
    z_scores * exp(-0.5 * z_scores^2))
}

z_scores <- seq(0, 5, .1)

# define resels
results <- expand_grid(resels = c(10, 50, 100, 200)) %>%
  rowwise() %>%
  mutate(z_scores = list(z_scores), ec = list(get_ec(resels, z_scores))) %>%
  unnest(c(z_scores, ec))
results %>% ggplot(aes(x = z_scores, y = ec, colour = as.factor(resels))) +
  geom_line(linewidth = 1.0) +
  theme_minimal() +
  labs(
    x = "Z-score Threshold",
    y = "EC",
    colour = "Resels",
  )

ggsave("ec_resels.png", width = 4, height = 3, dpi = 300)


z_scores <- seq(3, 4, .01)
results <- expand_grid(resels = c(10, 50, 100, 200)) %>%
  rowwise() %>%
  mutate(z_scores = list(z_scores), ec = list(get_ec(resels, z_scores))) %>%
  unnest(c(z_scores, ec))

# get labels for the Z that yields EC closest to 0.05
labels <- results %>%
  group_by(resels) %>%
  filter(abs(ec - 0.05) == min(abs(ec - 0.05))) %>%
  ungroup() %>%
  mutate(label = paste0(
    "Z: ", round(z_scores, 2),
    "\nEC: ", round(ec, 3)
  ), ec = ec + 0.08 * (1:4))

results %>% ggplot(aes(x = z_scores, y = ec, colour = as.factor(resels))) +
  geom_line(linewidth = 1.0) +
  geom_label(
    data = labels,
    aes(x = z_scores - 0.2, label = label), hjust = 0, vjust = 0,
    show.legend = FALSE,
  ) +
  theme_minimal() +
  labs(
    x = "Z-score Threshold",
    y = "EC",
    colour = "Resels",
  )

ggsave("ec_resels_zoom.png", width = 5, height = 4, dpi = 300)
