library(tidyverse)
library(magick)
library(gsignal)
source("helpers.R")

set.seed(12)

# read image convert to greyscale, upsample to 100 x 100 and convert to float
img <- image_read("mnist.jpg") %>%
  image_convert(type = "Grayscale") %>%
  image_scale("100x100!") %>%
  image_data() %>%
  as.numeric() %>%
  matrix(nrow = 100, ncol = 100)
img <- img / sd(img)

# make a data.frame
dat <- data.frame(
  v = as.vector(img),
  x = rep(1:100, each = 100), y = 1:100,
  type = "signal"
)

# create a matrix of random gaussian noise
noise <- matrix(rnorm(10000, sd = 1.0), nrow = 100, ncol = 100)
# add to data.frame
dat_noise <- data.frame(
  v = as.vector(noise),
  x = rep(1:100, each = 100), y = 1:100,
  type = "noise"
)
dat <- rbind(dat, dat_noise)

# add noise to the image
img_noise <- img + noise
# add to data.frame
dat_noise <- data.frame(
  v = as.vector(img_noise),
  x = rep(1:100, each = 100), y = 1:100,
  type = "signal+noise"
)
dat <- rbind(dat, dat_noise)


# smooth image and noise with a gaussian kernel
fwhm <- 10
sd_fwhm <- fwhm / (2 * sqrt(2 * log(2))) # full width at half maximum
kernel <- gaussian2d(15, sd_fwhm)


noise_conv <- conv2(noise - mean(noise), kernel, shape = "same")
img_conv <- conv2(img - mean(img), kernel, shape = "same")

# rescale
noise_conv <- noise_conv * sqrt(sd(noise)^2 / sd(noise_conv)^2)
img_conv <- img_conv * sqrt(sd(img)^2 / sd(img_conv)^2)
# remean
img_conv <- img_conv + mean(img)
noise_conv <- noise_conv + mean(noise)

img_noise_conv <- noise_conv + img_conv

# set critical z threshold
dat_conv <- data.frame(
  v = as.vector(img_noise_conv),
  x = rep(1:100, each = 100), y = 1:100
)
dat_conv$type <- "smoothed"
dat <- rbind(dat, dat_conv)

# now plot the data
p <- dat %>%
  ggplot(aes(x = x, y = y, fill = v)) +
  geom_tile(colour = NA) +
  scale_fill_viridis_c() +
  facet_wrap(~type) +
  theme_minimal() +
  labs(
    title = "MNIST",
  )

ggsave("mnist.png", width = 8, height = 8, dpi = 300)


# now we try different methods for thresholding
no_corr <- dat_conv
no_corr$type <- "No correction"
no_corr$mask <- no_corr$v > qnorm(1 - 0.05)

bon_corr <- dat_conv
bon_corr$type <- "Bonferroni"
bon_corr$mask <- bon_corr$v > qnorm(1 - (0.05 / (100 * 100)))

z_rft <- 3.79 # for 100 resels
rft_dat <- dat_conv
rft_dat$type <- "RFT"
rft_dat$mask <- rft_dat$v > z_rft

# join
mask_dat <- rbind(no_corr, bon_corr, rft_dat)
mask_dat$type <- factor(mask_dat$type, levels = c(
  "No correction", "Bonferroni", "RFT"
))

# now plot the data
p <- mask_dat %>%
  ggplot(aes(x = x, y = y, fill = v)) +
  geom_tile(colour = NA) +
  scale_fill_viridis_c() +
  geom_tile(
    data = mask_dat[mask_dat$mask, ],
    aes(x = x, y = y), fill = NA, colour = "white", linewidth = 0.6
  ) +
  facet_wrap(~type) +
  theme_minimal() +
  labs(
    title = "Different correction methods",
    subtitle = do.call(
      sprintf,
      c(
        fmt = "n-sig = %d, %d, %d",
        as.list(with(mask_dat, tapply(mask, type, sum)))
      )
    )
  )
ggsave("mnist_results.png", width = 10, height = 4, dpi = 300)
