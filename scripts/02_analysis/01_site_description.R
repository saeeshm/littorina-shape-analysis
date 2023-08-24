# Author: Saeesh Mangwani
# Date: 2023-08-24

# Description: Summary of field site conditions (environmental characterization)

# ==== Libraries ====
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# ==== Reading data ====
fdat <- readr::read_csv('data/field-site-desc.csv')

# Custom theme for plots
theme_shape <- function(size=12, font='serif'){
  theme_minimal(base_size=size, base_family = font) +
    theme(plot.background = element_rect(fill='white', colour=NA))
}

# Preparing data
plotdat <- fdat %>% 
  mutate(tidal_height = case_when(
    tidal_height == "Low" ~ "Submerged",
    tidal_height == "Medium" ~ "Water\nline",
    tidal_height == 'High' ~ 'Upper\nintertidal'
  )) %>% 
  mutate(num_snails=num_snails*2) %>% 
  dplyr::select(site, tidal_height, num_snails, boulder_cover, pebbles_cover, algae_cover) %>% 
  mutate(site = factor(site, levels=c('Sheltered', 'Exposed'))) %>% 
  mutate(tidal_height = factor(tidal_height, 
                               levels=c('Submerged', 'Water\nline', 'Upper\nintertidal')))

# Shell density per site
plotdat %>% 
  group_by(site) %>% 
  summarize(sum(num_snails),
            n())

# ==== Preparing a summary plot ====


# pivot_longer(cols=num_snails:algae_cover, names_to='var', values_to='value') %>% 
ns <- plotdat %>% 
  ggplot(aes(x = tidal_height, y = num_snails, colour=tidal_height, group=tidal_height)) +
  geom_boxplot() +
  geom_jitter(alpha=0.5, width=0.35) + 
  facet_wrap('site') +
  theme_shape() +
  guides(colour='none') +
  labs(x=NULL, 
       y='Shell Density (per m2)',
       colour='Tidal Level')
bd <- plotdat %>% 
  ggplot(aes(x = tidal_height, y = boulder_cover, colour=tidal_height, group=tidal_height)) +
  geom_boxplot() +
  geom_jitter(alpha=0.5, width=0.35) + 
  facet_wrap('site') +
  theme_shape() +
  guides(colour='none') +
  labs(x=NULL, 
       y='% Coverage of Boulders',
       colour='Tidal Level')
pb <- plotdat %>% 
  ggplot(aes(x = tidal_height, y = pebbles_cover, colour=tidal_height, group=tidal_height)) +
  geom_boxplot() +
  geom_jitter(alpha=0.5, width=0.35) + 
  facet_wrap('site') +
  theme_shape() +
  guides(colour='none') +
  labs(x=NULL, 
       y='% Coverage of Pebbles/Sand',
       colour='Tidal Level')
ag <- plotdat %>% 
  ggplot(aes(x = tidal_height, y = algae_cover, colour=tidal_height, group=tidal_height)) +
  geom_boxplot() +
  geom_jitter(alpha=0.5, width=0.35) + 
  facet_wrap('site') +
  theme_shape() +
  guides(colour='none') +
  labs(x=NULL, 
       y='% Coverage of Algae',
       colour='Tidal Level')

# Creating mosaic
fplot <- (bd+pb)/(ag+ns)
ggsave(filename='output/site_descripion_mosaic.png', plot=fplot, width=7, height=5, 
       dpi=300, scale=1.3)

