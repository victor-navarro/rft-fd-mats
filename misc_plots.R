library(tidyverse)

# A plot of the t-distribution with 30 degrees of freedom
# with a shaded are for the critical region at alpha = 0.05

vals <- seq(-4, 4, length.out = 1000)
df <- 30
alpha <- 0.05
t_dat <- data.frame(vals = vals, dens = dt(vals, df), type = "all")
critical_t <- qt(1 - alpha, df)
crit_vals <- t_dat[t_dat$vals > critical_t, ]
crit_vals$type <- "critical"
# join
t_dat <- rbind(t_dat, crit_vals)
# plot
t_dat %>%
  ggplot(aes(x = vals, y = dens, fill = type)) +
  geom_area(data = t_dat[t_dat$type == "all", ], aes(y = dens)) +
  geom_area(data = t_dat[t_dat$type == "critical", ], aes(y = dens)) +
  scale_fill_manual(values = c("all" = "#b6b6b8", "critical" = "#c22727")) +
  # add a line for the critical t-value
  geom_vline(xintercept = critical_t, linetype = "dashed", color = "#c22727") +
  annotate("label",
    x = 2, y = 0.2,
    label = sprintf("t = %1.3f", critical_t), color = "#c22727"
  ) +
  # an anotation of the proportion of the area under the curve
  annotate("text",
    x = 3, y = 0.1,
    label = sprintf("p = %0.3f", alpha), color = "black"
  ) +
  annotate("text",
    x = 0, y = 0.1,
    label = sprintf("p = %1.3f", 1 - alpha), color = "black"
  ) +
  theme_minimal() +
  labs(
    title = sprintf("t-distribution with %d degrees of freedom", df),
    x = "t-value",
    y = "Density",
    fill = "Type"
  ) +
  theme(legend.position = "none")


ggsave("t_dist.png", width = 5, height = 4, dpi = 300)

