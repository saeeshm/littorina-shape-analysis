# Author: Saeesh Mangwani
# Date: 2023-08-21

# Description: Analysis of shape data using outputs from the Shellshaper
# model (Larsson 2020):
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
library(biotools)
library(vegan)

# ==== Paths ====

# output folder for plots
plot_dir <- 'output'

# ==== Reading data ====

# Shellshaper analysis results
shpr_og <- read_csv('data/landmarks/shellshaper_results.csv')

# Image description data to get factor levels
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

# Joining factor level information to shape data
shpr <- shpr_og %>% 
  inner_join(imgs, by='snailID') %>% 
  # Selecting relevant data columns
  dplyr::select(snailID, site, species, tidal_height, 2:12) %>% 
  # Creating the curvature index variable (Larsson 2020)
  mutate(c = c0/a0)

# Creating a "long" version of the dataframe to use for plotting results
dlong <- shpr %>% 
  dplyr::select(snailID:scaleFactor, species, site) %>% 
  pivot_longer(cols=gw:scaleFactor, names_to='var', values_to='value')

# ==== Describing cluster patterns via PCA ====

# Fitting a PCA to visualizing clusters (using all relevant variables)
pca <- prcomp(~gw + gh + r0 + h0 + c + apAngle, data=shpr, scale.=T)

# Biplot - which variables are correlated and how
biplot(pca)

# Clustering by site and species - a clear difference!
p1 <- autoplot(pca, data=shpr %>% 
           mutate(levs = interaction(site, species)), 
         colour='levs', shape='levs') +
  stat_ellipse(aes(x = PC1, y=PC2, group=species), colour='darkgrey') +
  scale_color_viridis_d() +
  theme_shape() +
  labs(colour='Factor', shape='Factor')
ggsave(paste0(plot_dir, '/shpr_full_pca.png'), width=7, height=3.5, scale=1.1, dpi=300)

# Site, species and tidal_height - no clear pattern seen for tidal height
autoplot(pca, data=shpr %>% 
           mutate(levs = interaction(site, species)), 
         colour='tidal_height', shape='levs', size=3, alpha=0.8,
         frame=T, frame.type='norm') +
  theme_shape()

# ==== Testing for significant differences - MANOVA ====

# Assumption checking ----------

# Number of observations per-level - is it a balanced design?
shpr %>% 
  group_by(site, species, tidal_height) %>% 
  summarize('num_obs' = n())

# Distributions of each shape variable - generally normal, but some clear
# differences across species by site
dlong %>% 
  ggplot() +
  geom_density(aes(x=value, colour=species), fill='white', alpha=0.6) +
  facet_wrap(c('var', 'site'), scales = 'free') +
  theme_shape()

# Testing for multivariate normality across data variables
shpr %>% 
  select(gw, gh, r0, h0, c, apAngle) %>%
  as.matrix() %>% 
  t() %>% 
  mvnormtest::mshapiro.test()

# Testing for outliers - none
test <- shpr %>% 
  group_by(site, species) %>% 
  select(-tidal_height) %>% 
  as.data.frame()
rownames(test) <- test$snailID
test %>% 
  select(-snailID) %>% 
  mahalanobis_distance() %>% 
  filter(is.outlier)
  
# Collinearity - some limited multicollineartity between radius and height - to
# be expected
shpr %>% 
  select(gw, gh, r0, h0, c, apAngle) %>% 
  cor_mat() %>% 
  cor_plot()

# Covariances are not homogenous, but we can account for this by excluding low
# tide sammples which gives us a balanaced design
shpr %>% 
  select(gw:r0) %>% 
  box_m(. , test$site)

# ==== Testing significance of cluster differences ====

# Fitting the MANOVA - full model
mnv <- manova(cbind(gw, gh, r0, h0, c, apAngle) ~ site + species + tidal_height + 
                site*species + site*tidal_height + species*tidal_height + 
                site*species*tidal_height, data=shpr)
summary(mnv)

# Pairwise comparisons to investigate differences associated with tidal-height
mvpaircomp(mnv, "tidal_height", nesting.factor='site', test = "Pillai", adjust = "bonferroni")
mvpaircomp(mnv, "tidal_height", nesting.factor='species', test = "Pillai", adjust = "bonferroni")
mvpaircomp(mnv, "species", nesting.factor='site', test = "Pillai", adjust = "bonferroni")

# Visual summary of MANOVA - creating the plotting dataframe
plotdf <- shpr %>% 
  # More descriptive variable names
  rename('Aperture Angle (degrees)' = apAngle, 
         'Growth (width)' = gw,
         'Growth (height)' = gh,
         'Radius' = r0,
         'Length' = h0,
         'Circlipse extension' = c) %>%
  # Pivoting to longform for plotting
  pivot_longer(cols=c(`Growth (width)`,
                      `Growth (height)`, 
                      `Radius`,
                      `Length`,
                      `Circlipse extension`, 
                      `Aperture Angle (degrees)`), 
               names_to='var', values_to='value') %>% 
  # Converting categories to factors
  mutate(species=as.factor(species)) %>% 
  mutate(site = as.factor(site))

# Plotting comparisons across levels
plotdf %>% 
  ggplot(aes(x = site, y=value, colour=species, shape=species, group=species)) +
  stat_summary(fun = mean,
               position=position_dodge(width=.3)) + 
  stat_summary(fun = mean, geom='line',
               position=position_dodge(width=.3)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1,
               position=position_dodge(width=.3),
               show.legend=F) +
  facet_wrap(c('var'), scales='free') +
  theme_shape() +
  labs(x = NULL, y = NULL, colour='Species', shape='Species')
ggsave(paste0(plot_dir, '/shpr_manova_change_summary.png'))

# ==== Testing dispersion ====

# Dataframe for creating the distance matrix
distdf <- shpr %>% 
  dplyr::select(gw, gh, c, r0, h0, apAngle) %>% 
  as.data.frame()
rownames(distdf) <- shpr$snailID

# Computing the euclidean distance matrix
dmx <- dist(distdf)

# Calcualting beta disperson
mod <- betadisper(dmx, group=shpr$site)
mod <- betadisper(dmx, group=shpr$tidal_height)
# differences across species are significant
mod <- betadisper(dmx, group=shpr$species)
# Site-species interaction is near the 5% significance threshold. Pairwise
# comparisons show that the variability in littorea sheltered is the largest,
# significantly different from both saxatilis but not compared to littorea
# exposed
mod <- betadisper(dmx, group=interaction(shpr$species, shpr$site))
mod <- betadisper(dmx, group=interaction(shpr$site, shpr$tidal_height))

# Perform significance test on dispersion, based on distances to centroid
anova(mod)
permutest(mod, pairwise = TRUE, permutations = 999)

# Plot the groups and distances to centroids on the first two PCoA axes
# png(paste0(plot_dir, '/shpr_disp_species.png'), width=7, height=5,
#     units = 'in', res = 150)
plot(mod, axes=c(1,2))
# dev.off()

## Draw a boxplot of the distances to centroid for each group
boxplot(mod)

## Tukey's Honest Significant Differences
mod.HSD <- TukeyHSD(mod)
plot(mod.HSD)
