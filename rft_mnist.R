library(tidyverse)
library(magick)
library(gsignal)
source("helpers.R")

set.seed(1234)

# read image convert to greyscale, upsample to 100 x 100 and convert to float
# -1 to 1 range
img <- image_read("mnist.jpg") %>%
  image_convert(type = "Grayscale") %>%
  image_scale("100x100!") %>%
  image_data() %>%
  as.numeric() %>%
  matrix(nrow = 100, ncol = 100)
# standardize
img <- (img - mean(img)) / sd(img)

# make a data.frame
dat <- data.frame(
  v = as.vector(img),
  x = rep(1:100, each = 100), y = 1:100,
  type = "signal"
)

# create a matrix of random gaussian noise
noise <- matrix(rnorm(10000), nrow = 100, ncol = 100)
# add to data.frame
dat_noise <- data.frame(
  v = as.vector(noise),
  x = rep(1:100, each = 100), y = 1:100,
  type = "noise"
)
dat <- rbind(dat, dat_noise)

# add noise to the image
img_noise <- img + noise
# standardize
# img_noise <- (img_noise - mean(img_noise)) / sd(img_noise)
# add to data.frame
dat_noise <- data.frame(
  v = as.vector(img_noise),
  x = rep(1:100, each = 100), y = 1:100,
  type = "signal+noise"
)
dat <- rbind(dat, dat_noise)


# smooth the image with a gaussian kernel
fwhm <- 10
sd_fwhm <- fwhm / (2 * sqrt(2 * log(2))) # full width at half maximum
kernel <- gaussian2d(99, sd_fwhm)

# convolve
img_noise_sd <- sd(img_noise)
img_noise_conv <- gsignal::conv2(img_noise, kernel, shape = "same")

# rescale
img_noise_conv <- (img_noise_conv / sd(img_noise_conv)) * img_noise_sd

# set critical z threshold
dat_conv <- data.frame(
  v = as.vector(img_noise_conv),
  x = rep(1:100, each = 100), y = 1:100
)
dat_conv$type <- "smoothed"
dat <- rbind(dat, dat_conv)

z_rft <- 3.06 # for 10 resels
dat$rft_mask <- dat$v > z_rft


# now plot the data
p <- dat %>%
  ggplot(aes(x = x, y = y, fill = v)) +
  geom_tile(colour = NA) +
  scale_fill_viridis_c() +
  geom_tile(
    data = dat[dat$rft_mask, ],
    aes(x = x, y = y), fill = NA, colour = "white", linewidth = 0.6
  ) +
  facet_wrap(~type) +
  theme_minimal() +
  labs(
    title = "Smoothed - RFT correction",
    subtitle = do.call(
      sprintf,
      c(
        fmt = "z = %1.2f, n-sig = %d, %d, %d, %d",
        list(z_rft), as.list(with(dat, tapply(rft_mask, type, sum)))
      )
    )
  )
ggsave("mnist_results.png", width = 8, height = 8, dpi = 300)
