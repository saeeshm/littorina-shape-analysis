# Author: Saeesh Mangwani
# Date: 2023-08-19

# Description: Investigating potential bias in our classification of shells
# using shellshaper - useful for us to detect consistent sources of divergence,
# which was helpful as we tried to define a clear and reproducible methodology
# for image marking

# ==== Libraries ====
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

# ==== Paths ====

# output folder for plots
plot_dir <- 'output/bias-analysis'
dir.create(plot_dir)

# ==== Global variables ====

# Custom theme for plots
theme_shape <- function(size=12, font='serif'){
  theme_minimal(base_size=size, base_family = font) +
    theme(plot.background = element_rect(fill='white', colour=NA))
}

# ==== Reading data ====

# Reading results from independently classified sample images
p1 <- read_csv('data/bias-test/parameters-saeesh.txt') %>% 
  mutate(person = 'Saeesh')
p2 <- read_csv('data/bias-test/parameters-gabs.txt') %>% 
  mutate(person = 'Gabriel')
# Joining to 1 table, named by person.
params <- bind_rows(p1, p2)

# ==== Plotting data to investigate potential bias ====

# Distributions of all params split by person - helps visualize potentially
# consistent bias
p1 <- params %>% 
  pivot_longer(gw:scaleFactor, names_to='var', values_to = 'value') %>% 
  ggplot() +
  geom_boxplot(aes(y=value, x=person), colour='darkgrey', alpha=0.8, show.legend=F) +
  geom_jitter(aes(y=value, x =person, colour=snailID), size=2, alpha=0.5, width=0.5,
              show.legend=F) +
  theme_shape() +
  facet_wrap(facets='var', scales = 'free_y') +
  labs(x=NULL, y=NULL, title='Scored results by person')
ggsave(file.path(plot_dir, 'bias-point-plot.png'), plot=p1, 
       width=7, height=6, dpi=300, scale=1.2)

# Calculating pairwise differences in our assigned values for each image 
diff_tab <- params %>% 
  pivot_longer(gw:scaleFactor, names_to='var', values_to = 'value') %>% 
  # Grouping by variable and snailID, so each group is just a table of 2 rows
  # with one value from each of me and Gabs
  group_by(var, snailID) %>% 
  group_modify(~{
    # Getting both values
    vals <- .x$value
    # Absolute difference
    diff <- vals[1] - vals[2]
    # Relative difference (i.e difference as a proportion of the original
    # magnitude of the data, represented by the starting point of the
    # difference)
    relv_diff <- diff/vals[1]
    # Adding both difference back to the original table
    .x %>% 
      mutate(diff = diff, relv_diff = relv_diff) %>% 
      dplyr::select(person, value, diff, relv_diff)
  }) %>% 
  ungroup()

# plotting distribution of relative differences - overall pretty well centered
# at 0
p2 <- diff_tab %>% 
  dplyr::select(-person, -value) %>% 
  distinct() %>% 
  ggplot(aes(y = relv_diff, x = var, colour=var)) +
  geom_hline(aes(yintercept=0), linetype='dashed', alpha=0.6) +
  geom_boxplot(fill=NA) +
  geom_jitter(alpha=0.6) +
  theme_shape() +
  guides(colour='none') +
  labs(x = NULL, y=NULL, title='Distribution of % differences')
ggsave(file.path(plot_dir, 'bias-reltv-diffs.png'), plot=p2, 
       width=7, height=5, dpi=300, scale=1.2)

# Creating a combined mosaic to synthesize both results
mosaic <- p1/p2 + plot_layout(heights = c(2,1))
ggsave(file.path(plot_dir, 'bias-analysis.png'), plot=mosaic, 
       width=7, height=7, dpi=300, scale=1.2)
