library(tidyverse)
library(gsignal)
source("helpers.R")

n <- 1e4
size <- sqrt(n)
set.seed(1234)
dat <- data.frame(v = rnorm(n, 0, 1), x = 1:size, y = rep(1:size, each = size))
# z-critical
alpha <- 0.05
z <- qnorm(1 - alpha)
# get mask
dat$base_mask <- dat$v > z

# plot in ggplot
dat %>%
  ggplot(aes(x = x, y = y, fill = v)) +
  geom_tile(colour = NA) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(
    title = "Raw - No correction",
    subtitle = sprintf(
      "z = %1.2f, n = %d, n-sig = %d",
      z, n, sum(dat$base_mask)
    )
  )

ggsave("raw.png", width = 6.2, height = 6, dpi = 300)

# plot in ggplot
dat %>%
  ggplot(aes(x = x, y = y, fill = v)) +
  geom_tile(colour = NA) +
  scale_fill_viridis_c() +
  geom_tile(
    data = dat[dat$base_mask, ],
    aes(x = x, y = y), fill = NA, colour = "white", linewidth = 0.6
  ) +
  theme_minimal() +
  labs(
    title = "Raw - No correction",
    subtitle = sprintf(
      "z = %1.2f, n = %d, n-sig = %d",
      z, n, sum(dat$base_mask)
    )
  )

ggsave("raw_no_correction.png", width = 6.2, height = 6, dpi = 300)

# Bonferroni correction
z_bon <- qnorm(1 - alpha / (size * size))
# get mask

dat_conv$bon_mask <- dat_conv$v > z_bon

# plot in ggplot
dat %>%
  ggplot(aes(x = x, y = y, fill = v)) +
  geom_tile(colour = NA) +
  scale_fill_viridis_c() +
  geom_tile(
    data = dat[dat$bon_mask, ],
    aes(x = x, y = y), fill = NA, colour = "white", linewidth = 0.6
  ) +
  theme_minimal() +
  labs(
    title = "Raw - Bonferroni",
    subtitle = sprintf(
      "z = %1.2f, n = %d, n-sig = %d",
      z_bon, n, sum(dat$bon_mask)
    )
  )

ggsave("raw_bonferroni.png", width = 6.2, height = 6, dpi = 300)




# take samples and cast into matrix
dat_mat <- dat %>%
  select(v) %>%
  as.matrix() %>%
  matrix(nrow = size, ncol = size, byrow = TRUE)

# define a function to obtain a 2D gaussian kernel
# of a specific size



fwhm <- 10
sd_fwhm <- fwhm / (2 * sqrt(2 * log(2))) # full width at half maximum
# create a 2D gaussian kernel
kernel <- gaussian2d(99, sd_fwhm)

# plot the kernel
kernel_df <- data.frame(
  v = as.vector(kernel),
  x = rep(1:99, each = 99), y = 1:99
)
kernel_df %>%
  ggplot(aes(x = x, y = y, fill = v)) +
  geom_tile(colour = NA) +
  theme_minimal() +
  scale_fill_distiller(palette = "Spectral") +
  labs(
    title = "2D Gaussian kernel",
    subtitle = sprintf("FWHM = %d", fwhm)
  )
ggsave("2dkernel.png", width = 3.5, height = 3, dpi = 300)

# and get the middle row of the kernel to exemplify the FWHM
kernel_df %>%
  subset(y == 50 & x > 25 & x < 75) %>%
  ggplot(aes(x = x, y = v)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey") +
  geom_vline(
    xintercept = c(-5, 5) + 50,
    linetype = "dashed", colour = "#c56969"
  ) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Marginal of Gaussian kernel",
    subtitle = sprintf("FWHM = %d", fwhm)
  )
ggsave("2dkernel_marginal.png", width = 3.2, height = 3, dpi = 300)

#


# convolve
dat_mat_sd <- sd(dat_mat)
dat_mat_conv <- gsignal::conv2(dat_mat, kernel, shape = "same")
# rescale
dat_mat_conv <- (dat_mat_conv / sd(dat_mat_conv)) * dat_mat_sd

# put in data frame
dat_conv <- data.frame(
  v = as.vector(dat_mat_conv),
  x = rep(1:size, each = size), y = 1:size
)
dat_conv$base_mask <- dat_conv$v > z

dat_conv %>%
  ggplot(aes(x = x, y = y, fill = v)) +
  geom_tile(colour = NA) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(
    title = "Smoothed - No correction",
    subtitle = sprintf(
      "z = %1.2f, n = %d, n-sig = %d",
      z, n, sum(dat_conv$base_mask)
    )
  )

ggsave("smoothed.png", width = 6.2, height = 6, dpi = 300)


dat_conv %>%
  ggplot(aes(x = x, y = y, fill = v)) +
  geom_tile(colour = NA) +
  scale_fill_viridis_c() +
  geom_tile(
    data = dat_conv[dat_conv$base_mask, ],
    aes(x = x, y = y), fill = NA, colour = "white", linewidth = 0.6
  ) +
  theme_minimal() +
  labs(
    title = "Smoothed - No correction",
    subtitle = sprintf(
      "z = %1.2f, n = %d, n-sig = %d",
      z, n, sum(dat_conv$base_mask)
    )
  )

ggsave("smoothed_no_correction.png", width = 6.2, height = 6, dpi = 300)


# Bonferroni correction
# get mask
dat_conv$bon_mask <- dat_conv$v > z_bon

# plot in ggplot
dat_conv %>%
  ggplot(aes(x = x, y = y, fill = v)) +
  geom_tile(colour = NA) +
  scale_fill_viridis_c() +
  geom_tile(
    data = dat_conv[dat_conv$bon_mask, ],
    aes(x = x, y = y), fill = NA, colour = "white", linewidth = 0.6
  ) +
  theme_minimal() +
  labs(
    title = "Smoothed - Bonferroni",
    subtitle = sprintf(
      "z = %1.2f, n = %d, n-sig = %d",
      z_bon, n, sum(dat_conv$bon_mask)
    )
  )

ggsave("smoothed_bonferroni.png", width = 6.2, height = 6, dpi = 300)


z_rft <- 3.79 # for 100 resels
dat_conv$rft_mask <- dat_conv$v > z_rft

# plot in ggplot
dat_conv %>%
  ggplot(aes(x = x, y = y, fill = v)) +
  geom_tile(colour = NA) +
  scale_fill_viridis_c() +
  geom_tile(
    data = dat_conv[dat_conv$rft_mask, ],
    aes(x = x, y = y), fill = NA, colour = "white", linewidth = 0.6
  ) +
  theme_minimal() +
  labs(
    title = "Smoothed - RFT",
    subtitle = sprintf(
      "z = %1.2f, n = %d, n-sig = %d",
      rft_z, n, sum(dat_conv$rft_mask)
    )
  )

ggsave("smoothed_rft.png", width = 6.2, height = 6, dpi = 300)
