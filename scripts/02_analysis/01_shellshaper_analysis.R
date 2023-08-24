# Author: Saeesh Mangwani
# Date: 2023-08-21

# Description: Analysis of shape data using outputs from the Shellshaper
# model:
# - Exploratory analysis of shape differentiation by factors using PCA
# - Testing for significant differences in shapes using MANOVA

# ==== Libraries ====
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(rstatix)
library(ggplot2)
library(ggfortify)
library(patchwork)
library(vegan)
library(biotools)

# ==== Paths ====

# output folder for plots
plot_dir <- 'output'

# ==== Reading data ====

# Shellshaper analysis results
shpr_og <- read_csv('data/landmarks/shellshaper_results.csv')

# Image descriptions to get factor levels
imgs <- read_csv('data/image-reference.csv')

# Filtering out samples that were low-tide, since we didn't have a balance
# enough sample set to make an adquate comparison with these data
imgs <- imgs %>% 
  filter(!tidal_height == 'Low') %>% 
  # Cleaning the ID variable by removing the JPG suffix
  mutate(snailID = str_remove_all(new_name, '\\.JPG')) %>% 
  # Selecting only relevant variables
  dplyr::select(snailID, species, tidal_height, site)

# ==== Global variables ====

# Custom theme for plots
theme_shape <- function(size=12, font='serif'){
  theme_minimal(base_size=size, base_family = font) +
    theme(plot.background = element_rect(fill='white', colour=NULL))
}

# ==== Tidying/preparing data ====

# Joining factor level information to shape data - note that we remove all
# low-tide variables at this stage
shpr_full <- shpr_og %>% 
  inner_join(imgs, by='snailID') %>% 
  dplyr::select(snailID, site, species, tidal_height, 2:12) %>% 
  mutate(c = c0/a0)

# Creating a long-form plotting dataframe of only the dependent variables
dlong <- shpr_full %>% 
  dplyr::select(snailID:scaleFactor, species, site) %>% 
  pivot_longer(cols=gw:scaleFactor, names_to='var', values_to='value')

# ==== Describing cluster patterns via PCA ====

# Fitting a PCA to visualizing clusters (Using only relevant variables)
pca <- prcomp(~gw + gh + r0 + h0 + c + apAngle, data=shpr_full, scale.=T)

# Biplot - which variables are correlated and how
biplot(pca)

# Clustering by site and species - a clear difference!
p1 <- autoplot(pca, data=shpr_full %>% 
           mutate(levs = interaction(site, species)), 
         colour='levs', shape='levs') +
  stat_ellipse(aes(x = PC1, y=PC2, group=species), colour='darkgrey') +
  scale_color_viridis_d() +
         # loadings=T, loadings.label=T, loadings.label.size=3,
         # frame=T, frame.alpha=0.1) +
  theme_shape() +
  labs(colour='Factor', shape='Factor')
ggsave(paste0(plot_dir, '/shpr_full_pca.png'), width=7, height=3.5, scale=1.1, dpi=300)

# Site, species and tidal_height - no pattern detected for tidal height at all
# levels
autoplot(pca, data=shpr_full %>% 
           mutate(levs = interaction(site, species)), 
         colour='tidal_height', shape='levs', size=3, alpha=0.8,
         frame=T, frame.type='norm') +
  theme_shape()

# Plotting them separately and joining -  a clearer pattern
ltr_index <- shpr_full %>% 
  mutate(id=1:nrow(.)) %>% 
  filter(species=='Littorea') %>% 
  pull(id) %>% 
  as.numeric()

# Littorea
pca_lit <- shpr_full %>% 
  filter(species == 'Littorea') %>% 
  prcomp(~gw + gh + r0 + h0 + c + apAngle, data=., scale.=T)

p1 <-  ggplot(pca$x[ltr_index,]) +
  # geom_hline(aes(yintercept=0), linetype='dashed') +
  # geom_vline(aes(xintercept=0), linetype='dashed') +
  geom_point(aes(x = PC2, y=PC1, 
                 colour=shpr_full %>% 
                   filter(species == 'Littorea') %>% pull(site), 
                 shape=shpr_full %>% 
                   filter(species == 'Littorea') %>% pull(site)), 
             size=2, alpha=0.8) +
  # stat_ellipse(aes(x = PC1, y=PC2, colour=site)) +
  scale_colour_brewer(palette='Dark2') +
  # xlim(c(-0.3, 0.3)) +
  # ylim(c(-0.3, 0.3)) +
  theme_shape() +
  labs(colour='Site', shape='Site')

sxt_index <- shpr_full %>% 
  mutate(id=1:nrow(.)) %>% 
  filter(species=='Saxatilis') %>% 
  pull(id) %>% 
  as.numeric()

p2 <- ggplot(pca$x[sxt_index,]) +
  # geom_hline(aes(yintercept=0), linetype='dashed') +
  # geom_vline(aes(xintercept=0), linetype='dashed') +
  geom_point(aes(x = PC2, y=PC1, 
                 colour=shpr_full %>% 
                   filter(species == 'Saxatilis') %>% pull(site), 
                 shape=shpr_full %>% 
                   filter(species == 'Saxatilis') %>% pull(site)), 
             size=2, alpha=0.8) +
  # stat_ellipse(aes(x = PC1, y=PC2, colour=site)) +
  scale_colour_brewer(palette='Dark2') +
  # xlim(c(-0.3, 0.3)) +
  # ylim(c(-0.3, 0.3)) +
  theme_shape() +
  labs(colour='Site', shape='Site')


# Saxatilis
pca_sxt <- shpr_full %>% 
  filter(species == 'Saxatilis') %>% 
  prcomp(~gw + gh + r0 + h0 + c + apAngle, data=., scale.=T)

p2 <- autoplot(pca_sxt, 
               data=shpr_full %>% 
                 filter(species == 'Saxatilis'), 
               colour='site', 
               shape='site', 
               size=3, alpha=0.8,
               # loadings=T, loadings.label=T,
               # frame=T, frame.type='norm'
               ) +
  # stat_ellipse(aes(x = PC1, y=PC2, colour=site)) +
  xlim(c(-0.3, 0.3)) +
  ylim(c(-0.3, 0.3)) +
  # scale_colour_brewer(palette='Dark2') +
  theme_shape()

# Joining plots - clear visualization of differentiation being more distinct for
# saxatilis than littorea
mosaic <- p1/p2
mosaic

# how do the biplots compare - similar variables involved minus apAngle
biplot(pca_lit)
biplot(pca_sxt)

# ==== Testing for significant differences - MANOVA ====

# Assumption checking ----------

# Number of observations per-level - is it a balanced design?
shpr_full %>% 
  group_by(site, species, tidal_height) %>% 
  summarize('num_obs' = n())

# Distributions of each shape variable - generally normal, including across
# splits by site and species
dlong %>% 
  ggplot() +
  geom_density(aes(x=value, colour=species), fill='white', alpha=0.6) +
  facet_wrap(c('var'), scales = 'free') +
  theme_shape()

# Testing for multivariate normality across all data variables - looks good
shpr_full %>% 
  select(gw:scaleFactor) %>% 
  as.matrix() %>% 
  t() %>% 
  mvnormtest::mshapiro.test()

# Testing for outliers - none
test <- shpr_full %>% 
  group_by(site, species) %>% 
  select(-tidal_height) %>% 
  as.data.frame()
rownames(test) <- test$snailID
test %>% 
  select(-snailID) %>% 
  mahalanobis_distance() %>% 
  filter(is.outlier)
  
# Collinearity - several fairly high correlations which warrant some variable
# selection - we can create a composite variable between c0/a0 (the extension
# parameter)as our selection
shpr_full %>% 
  select(gw:apAngle) %>% 
  cor_mat() %>% 
  cor_plot()

# Selecting the subset of variables
shpr <- shpr_full
  # mutate(c = c0/a0) %>% 
  # mutate(hr = h0/r0) %>% 
  # select(snailID, site, species, tidal_height, gw, gh, c, apAngle, r0)
dlong <- shpr %>% 
  pivot_longer(cols=gw:apAngle, names_to='var', values_to='value')

# Plotting correlations, much better now!
shpr %>% 
  # mutate(c = c0/a0) %>%
  # mutate(hr = h0/r0) %>%
  select(gw, gh, c, r0, apAngle) %>% 
  cor_mat() %>% 
  cor_plot()

# Covariances are not homogenous, but we can approach this by excluding low tide
# sammples which gives us a balanaced design
shpr %>% 
  select(gw:r0) %>% 
  box_m(. , test$site)

# Homogeneity of variances - levene's test
dlong %>% 
  # filter(tidal_height != "Low") %>% 
  # filter(!var %in% c('scaleFactor', 'shellLength', 'apex_x', 'apex_y')) %>% 
  mutate(lev = interaction(site, species)) %>% 
  group_by(var) %>% 
  levene_test(value ~ species)

# ==== Testing significance of cluster differences ====

# Fitting the MANOVA - full model
# model <- lm(cbind(gw, gh, c, apAngle, r0) ~ site + species + tidal_height + 
#               site*species + site*tidal_height + species*tidal_height + 
#               site*species*tidal_height, data=shpr)
# mnv <- Manova(model, test.statistic = "Pillai")
mnv <- manova(cbind(gw, gh, c, apAngle) ~ site + species + tidal_height + 
                site*species + site*tidal_height + species*tidal_height + 
                site*species*tidal_height, data=shpr)
summary(mnv)

mvpaircomp(mnv, "tidal_height", nesting.factor='site', test = "Pillai", adjust = "bonferroni")
mvpaircomp(mnv, "tidal_height", nesting.factor='species', test = "Pillai", adjust = "bonferroni")
mvpaircomp(mnv, "species", nesting.factor='site', test = "Pillai", adjust = "bonferroni")

# Visual summary of MANOVA
shpr %>% 
  rename('Aperture Angle (degrees)' = apAngle, 
         'Growth (width)' = gw,
         'Growth (height)' = gh,
         # 'Radius' = r0, 
         # 'Length' = h0, 
         'Circlipse extension' = c) %>%
  pivot_longer(cols=c(`Growth (width)`,
                      `Growth (height)`, 
                      # `Radius`, 
                      # `Length`, 
                      `Circlipse extension`, 
                      `Aperture Angle (degrees)`), 
               names_to='var', values_to='value') %>% 
  mutate(species=as.factor(species)) %>% 
  mutate(site = as.factor(site)) %>% 
  # mutate(value = ifelse(var == 'apAngle', value/10, value)) %>% 
  ggplot(aes(x = site, y=value, colour=species, shape=species, group=species)) +
  stat_summary(fun = mean,
               position=position_dodge(width=.3)) + 
  stat_summary(fun = mean, geom='line',
               position=position_dodge(width=.3)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1,
               position=position_dodge(width=.3),
               show.legend=F) +
  # geom_boxplot(aes(x = site, y=value, colour=species)) +
  # stat_summary(aes(x = site, y=value, colour=species), fun=mean, geom="line")
  # geom_jitter(aes(x = species, y=value, colour=site, group=site), alpha=0.5) +
  # scale_colour_brewer(palette='Dark2') +
  facet_wrap(c('var'), scales='free') +
  # facet_grid(rows=vars(tidal_height), cols=vars(var), scales = 'free', space='free') +
  theme_shape() +
  labs(x = NULL, y = NULL, colour='Species', shape='Species')
ggsave(paste0(plot_dir, '/shpr_manova_change_summary.png'))

# Fitting the MANOVA - separate models for littorea and saxatilis
# model <- lm(cbind(gw, gh, c, apAngle, r0) ~ site + species + tidal_height + 
#               site*species + site*tidal_height + species*tidal_height + 
#               site*species*tidal_height, data=shpr)
# mnv <- Manova(model, test.statistic = "Pillai")
mnv_ltr <- manova(cbind(gw, gh, c, apAngle, r0) ~ site + tidal_height + site*tidal_height, 
              data=shpr %>% 
                filter(species == "Littorea"))
summary(mnv_ltr)
mvpaircomp(mnv_ltr, "tidal_height", nesting.factor='site', test = "Pillai", adjust = "bonferroni")


# Manova summary littorina
dlong %>% 
  filter(species == "Littorea") %>% 
  mutate(species=as.factor(species)) %>% 
  mutate(site = as.factor(site)) %>% 
  ggplot(aes(x = site, y=value, colour=tidal_height, shape=tidal_height, group=tidal_height)) +
  stat_summary(fun = mean,
               position=position_dodge(width=.3)) + 
  stat_summary(fun = mean,
               position=position_dodge(width=.3), 
               geom='line') + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1,
               position=position_dodge(width=.3)) +
  # geom_boxplot(aes(x = site, y=value, colour=species)) +
  # stat_summary(aes(x = site, y=value, colour=species), fun=mean, geom="line")
  # geom_jitter(aes(x = species, y=value, colour=site, group=site), alpha=0.5) +
  scale_colour_brewer(palette='Set2') +
  facet_wrap(c('var'), scales = 'free') +
  theme_shape()

# Manova saxatilis
mnv_sxt <- manova(cbind(gw, gh, c, apAngle, r0) ~ site + tidal_height + site*tidal_height, 
                  data=shpr %>% 
                    filter(species == "Saxatilis"))
summary(mnv_sxt)

# ==== Testing dispersion ====

# Dataframe for creating the distance matrix
distdf <- shpr %>% 
  dplyr::select(gw, gh, a0, c0, apAngle) %>% 
  as.data.frame()
rownames(distdf) <- shpr$snailID

# Computing the euclidean distance matrix
dmx <- dist(distdf)

# Calcualting beta disperson
mod <- betadisper(dmx, group=shpr$site)
mod <- betadisper(dmx, group=shpr$tidal_height)
mod <- betadisper(dmx, group=shpr$species)
mod <- betadisper(dmx, group=interaction(shpr$species, shpr$site))
mod <- betadisper(dmx, group=interaction(shpr$site, shpr$tidal_height))
## Plot the groups and distances to centroids on the
## first two PCoA axes
# png(paste0(plot_dir, '/shpr_disp_species.png'), width=7, height=5, units = 'in', 
#     res = 150)
p1 <- plot(mod, axes=c(1,2))
# dev.off()

## Draw a boxplot of the distances to centroid for each group
boxplot(mod)

# Perform test on distances to centroid
anova(mod)
permutest(mod, pairwise = TRUE, permutations = 999)

## Tukey's Honest Significant Differences
mod.HSD <- TukeyHSD(mod)
plot(mod.HSD)
