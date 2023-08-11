# Mean and variability of global climate data
# BBL August 2023
# Exploratory look, for work with Eva Sinha

# Use WorldClim data and new terra/geodata packages

library(sp)
library(geodata)
library(terra)

prec_global <- worldclim_global("prec", path = "wc2.1/", res = 10)
tavg_global <- worldclim_global("tavg", path = "wc2.1/", res = 10)

pts <- expand.grid(x = c(-100, -90, -80), y = c(30, 40, 50))
ex <- terra::ext(-100, -85, 30, 50) # roughly the upper Midwest
t_mw_raw <- terra::extract(tavg_global, pts)
p_mw_raw <- terra::extract(prec_global, pts)
t_mw <- tibble(t_mean = apply(t_mw_raw, 1, mean),
               t_sd = apply(t_mw_raw, 1, sd))
t_mw <- na.omit(t_mw)
p_mw <- tibble(p_sum = apply(p_mw_raw, 1, sum),
               p_sd = apply(p_mw_raw, 1, sd))
p_mw <- na.omit(p_mw)


library(dplyr)
library(tidyr)
terra::as.data.frame(prec_global, xy = TRUE) %>%
  drop_na() -> precip_global
# Calculate annual sum for precip...
precip_global %>%
  dplyr::select(-x, -y) %>%
  apply(1, sum) -> p_sum_global
# Calculate annual sum for precip...
precip_global %>%
  dplyr::select(-x, -y) %>%
  apply(1, sd) -> p_sd_global

terra::as.data.frame(tavg_global, xy = TRUE) %>%
  drop_na() -> tmean_global
# Calculate annual sum for temperature
tmean_global %>%
  dplyr::select(-x, -y) %>%
  apply(1, mean) -> t_mean_global
# Calculate annual sum for temperature
tmean_global %>%
  dplyr::select(-x, -y) %>%
  apply(1, sd) -> t_sd_global

# Create tibble with corresponding coordinates
p_dat <- tibble(x = precip_global$x,
                y = precip_global$y,
                p_sum = as.vector(p_sum_global),
                p_sd = as.vector(p_sd_global))
t_dat <- tibble(x = tmean_global$x,
                y = tmean_global$y, 
                t_mean = as.vector(t_mean_global),
                t_sd = as.vector(t_sd_global))

# Plot
library(ggplot2)
theme_set(theme_bw())

ggplot(p_dat, aes(p_sum, p_sd)) + 
  geom_bin2d() + 
  labs(x = "MAP (mm)", y = "Monthly SD (mm)") +
  scale_fill_viridis_b(trans = "log", guide = "none") +
  # Add a bounding box aroundu the midwest points
  geom_polygon(data = p_mw[chull(p_mw),], color = "red", fill = NA)
ggsave("midwest_prec_meanvar.png")

ggplot(t_dat, aes(t_mean, t_sd)) + 
  geom_bin2d() + 
  labs(x = "MAT (degC)", y = "Monthly SD (degC)") +
  scale_fill_viridis_b(trans = "log", guide = "none") +
  # Add a bounding box aroundu the midwest points
  geom_polygon(data = t_mw[chull(t_mw),], color = "red", fill = NA)
ggsave("midwest_tavg_meanvar.png")


