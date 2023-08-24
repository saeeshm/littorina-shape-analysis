# Author: Saeesh Mangwani
# Date: 2023-08-21

# Description: Analysis of shape data using TPS landmarks, with the Geomorph
# package:
# - Exploratory analysis of shape differentiation by factors using PCA
# - Testing for significant differences in shapes...

# ==== Libraries ====
library(geomorph)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

# ==== Reading data ====

# Landmarks from the TPS file
landmarks <- readmulti.tps('data/landmarks/all_landmarks.TPS', specID='imageID', 
                           negNA=T, readcurves=T)

# Image descriptions to get factor levels
imgs <- read_csv('data/image-reference.csv') %>% 
  mutate(new_name = str_remove_all(new_name, '\\.JPG'))

# Filtering out samples that were low-tide, since we didn't have a balance
# enough sample set to make an adquate comparison with these data
rmids <- imgs %>% 
  filter(!is.na(found)) %>% 
  filter(tidal_height == 'Low') %>% 
  pull(new_name)
imgs <- imgs %>% 
  filter(!tidal_height == 'Low')

# Removing landmarks with these IDs
landmarks <- landmarks[, , (!names(landmarks[1,1,]) %in% rmids)]

# Removing curved landmarks
# landmarks <- landmarks[1:9, , ]

# ==== Global variables ====

# Custom theme for plots
theme_shape <- function(size=12, font='serif'){
  theme_minimal(base_size=size, base_family = font) +
    theme(plot.background = element_rect(fill='white', colour=NULL))
}

# Index vector for species per id
specvec <- setNames(imgs$species, imgs$new_name)

# Index vector for site per id
sitevec <- setNames(imgs$site, imgs$new_name)

# Interaction vector (species and sites per id)
interacvec <- setNames(interaction(imgs$site, imgs$species), imgs$new_name)

# Index vector for tidal gradient
tidevec <- setNames(imgs$tidal_height, imgs$new_name)

# ==== Preparing landmarks ====

# Aligning landmark data using GPA  
tps <- gpagen(landmarks, 
              curves=define.sliders(10:29),
              print.progress = T, 
              ProcD = T,
              Proj=T)

# Getting the order of ids as a vector - useful for later indexing environmental
# factors in the right order
idvec <- tps$coords[1,1,] %>% names()

# ==== Exploratory analysis ====

# Plotting the average shape and variability post-alignment
plot(tps)

# Plotting PCA and colouring by species
mpca <- geomorph::gm.prcomp(tps$coords, scale.=T)
summary(mpca)

0.2408915
0.1557211
# Plotting PCA - variability across species is much less clearly defined as
# compared to shellshaper results
p2 <- mpca$x %>% 
  as.data.frame() %>% 
  ggplot() +
  # geom_hline(aes(yintercept=0), linetype='dashed') +
  # geom_vline(aes(xintercept=0), linetype='dashed') +
  geom_point(aes(x = Comp1, y=Comp2, 
                 colour=interacvec[rownames(mpca$x)], 
                 shape=interacvec[rownames(mpca$x)])) +
  scale_color_viridis_d() +
  labs(colour='Factor', shape='Factor') +
  ggnewscale::new_scale_color() +
  stat_ellipse(aes(x = Comp1, y=Comp2,
                   colour=specvec[rownames(mpca$x)]),
               show.legend=F) +
  scale_color_manual(values=c('darkgrey', 'darkgrey')) +
  # scale_color_brewer(palette = 'Accent') +
  theme_shape() +
  labs(x='PC1: 24.09%', y='PC2: 15.57%')
ggsave(paste0('output/tps_full_pca.png'), width=7, height=3.5, dpi=300, scale=1.1)
# mosaic <- p1+p2+plot_layout(guides='collect')
# ggsave(paste0('output/both_full_pca.png'), mosaic, width=7, height=4, dpi=300, scale = 1.35)

# Splitting pca by species (same as shellshaper)
ltr_comps <- mpca$x %>% as.data.frame()
ltr_comps <- ltr_comps[names(specvec[specvec == "Littorea"]), ] 
m1 <- ltr_comps %>% 
  ggplot() +
  # geom_hline(aes(yintercept=0), linetype='dashed') +
  # geom_vline(aes(xintercept=0), linetype='dashed') +
  geom_point(aes(x = Comp2, y=Comp1, 
                 colour=sitevec[rownames(ltr_comps)], 
                 shape=sitevec[rownames(ltr_comps)]), size=2) +
  stat_ellipse(aes(x = Comp2, y=Comp1,
                   colour=sitevec[rownames(ltr_comps)]),
               show.legend=F) +
  # scale_color_viridis_d() +
  labs(colour='Site', shape='Site') +
  scale_color_brewer(palette = 'Accent') +
  theme_shape() +
  labs(y='PC1: 24.09%', x='PC2: 15.57%')

sxt_comps <- mpca$x %>% as.data.frame()
sxt_comps <- sxt_comps[names(specvec[specvec == "Saxatilis"]), ] 
m2 <- sxt_comps %>% 
  ggplot() +
  # geom_hline(aes(yintercept=0), linetype='dashed') +
  # geom_vline(aes(xintercept=0), linetype='dashed') +
  geom_point(aes(x = Comp2, y=Comp1, 
                 colour=sitevec[rownames(sxt_comps)], 
                 shape=sitevec[rownames(sxt_comps)]), size=2) +
  stat_ellipse(aes(x = Comp2, y=Comp1,
                   colour=sitevec[rownames(sxt_comps)]),
               show.legend=F) +
  # scale_color_manual(values=c('darkgrey', 'darkgrey')) +
  # scale_color_viridis_d() +
  labs(colour='Factor', shape='Factor') +
  # scale_color_brewer(palette = 'Accent') +
  theme_shape() +
  labs(y='PC1 (24.09%)', x='PC2 (15.57%)')

mosaic <- m1/m2
mosaic

# Getting the reference (mean shape)
ref<-mshape(tps$coords)

# Getting the mean shape for each group of our main factors
get_mean_shape <- function(level){
  mshape(tps$coords[,,idvec[idvec %in% names(interacvec[interacvec == level])]])
}
gp1_mn<-get_mean_shape('Exposed.Littorea')
gp2_mn<-get_mean_shape('Sheltered.Littorea')
gp3_mn<-get_mean_shape('Exposed.Saxatilis')
gp4_mn<-get_mean_shape('Sheltered.Saxatilis')

# Plotting all mean shapes relative to the reference mean shape
png(paste0(plot_dir, '/tps-mean-shapes/landmarks_distribution.png'), 
    width=7, height=5, units = 'in', 
    res = 150)
plot(tps)
dev.off()

# Exposed littorea
png(paste0(plot_dir, '/tps-mean-shapes/exposed_littorea.png'), 
    width=7, height=5, units = 'in', 
    res = 150)
plotRefToTarget(ref, gp1_mn, mag=7, method='points', 
                links=define.sliders(10:29)[,2:3]
                )
dev.off()

# Sheltered Littorea
png(paste0(plot_dir, '/tps-mean-shapes/sheltered_littorea.png'), 
    width=7, height=5, units = 'in', 
    res = 150)
plotRefToTarget(ref, gp2_mn, mag=7, method='points', 
                links=define.sliders(10:29)[,2:3]
                )
dev.off()

# Exposed saxatilies
png(paste0(plot_dir, '/tps-mean-shapes/exposed_saxatilis.png'), 
    width=7, height=5, units = 'in', 
    res = 150)
plotRefToTarget(ref, gp3_mn, mag=6, method='points', 
                links=define.sliders(10:29)[,2:3]
                )
dev.off()

# Sheltered saxatilis
png(paste0(plot_dir, '/tps-mean-shapes/sheltered_saxatilis.png'), 
    width=7, height=5, units = 'in', 
    res = 150)
plotRefToTarget(ref, gp4_mn, mag=7, method='points', 
                links=define.sliders(10:29)[,2:3])
dev.off()

# ==== Exploring differences across factors ====

# Creating model dataframes ----------

# Creating a geomorph data frame for exploring differences across factors
gdf <- geomorph.data.frame(tps, species=specvec[idvec], 
                           site=sitevec[idvec], 
                           tide=tidevec[idvec])

# G-dfs separately for each species
# Aligning landmark data using GPA  
ltr_index <- (specvec == 'Littorea')[idvec]
gdf_ltr <- landmarks[,,ltr_index] %>% 
  gpagen(landmarks, 
         curves=define.sliders(10:29),
         print.progress = T, 
         ProcD = T,
         Proj=T) %>% 
  geomorph.data.frame(site=sitevec[idvec][ltr_index], 
                      tide=tidevec[idvec][ltr_index])
sxt_index <- (specvec == 'Saxatilis')[idvec]
gdf_sxt <- landmarks[,,sxt_index] %>% 
  gpagen(landmarks, 
         curves=define.sliders(10:29),
         print.progress = T, 
         ProcD = T,
         Proj=T) %>% 
  geomorph.data.frame(site=sitevec[idvec][sxt_index], 
                      tide=tidevec[idvec][sxt_index])

# Fitting models ----------

# Performing the procustes anova to analyze shape variation - nothing in the
# tide variables is significant, so we drop this factor
# m0 <- procD.lm(f1 = coords ~ site+species+tide+Csize+
#                  site*species+site*tide+site*Csize+
#                  species*tide+species*Csize+
#                  tide*Csize+
#                  site*species*tide+species*tide*Csize+site*tide*Csize+
#                  site*species*tide*Csize, 
#                data=gdf, int.first = T)
# summary(m0)

# Reduced model produces a significant result for all main and interaction
# factors, consistent with the findings from shellshaper
# m1 <- procD.lm(f1 = coords ~ site + species + Csize+
#                  site*species + site*Csize + species*Csize+
#                  site*species*Csize, 
#                data=gdf, int.first = T)
# summary(m1)
m0 <- procD.lm(f1 = coords ~ site + species + tide + 
                 site*species + site*tide + tide*species +
                 site*tide*species, 
               data=gdf, int.first = T)
summary(m0)

m1 <- procD.lm(f1 = coords ~ site + species + site*species, 
               data=gdf, int.first = T)
summary(m1)

# Separate models for only littorea
mltr0 <- procD.lm(f1 = coords ~ site+tide+Csize+
                    site*tide+site*Csize+tide*Csize+
                    site*tide*Csize, 
               data=gdf_ltr, int.first = T)
summary(mltr0)
mltr1 <- procD.lm(f1 = coords ~ site+Csize+site*Csize, 
                  data=gdf_ltr, int.first = T)
summary(mltr1)

# Separate models for only saxatilis
msxt0 <- procD.lm(f1 = coords ~ site+tide+Csize+
                    site*tide+site*Csize+tide*Csize+
                    site*tide*Csize, 
                  data=gdf_sxt, int.first = T)
summary(msxt0)
msxt1 <- procD.lm(f1 = coords ~ site+tide+Csize+site*Csize+site*tide, 
                  data=gdf_sxt, int.first = T)
summary(msxt1)

# ==== Dispersion analysis ====

# Creating dataframe of coordinate variables
distdf <- map_dfr(1:dim(gdf$coords)[3], ~{
  x <- t(gdf$coords[,,.x])[1,] %>% setNames(paste0(1:29, '_x'))
  y <- t(gdf$coords[,,.x])[2,] %>% setNames(paste0(1:29, '_y'))
  c(x, y)
})

# Computing the euclidean distance matrix
dmx <- dist(distdf)

# Calcualting beta disperson
mod <- betadisper(dmx, group=gdf$site)
mod <- betadisper(dmx, group=gdf$species)
mod <- betadisper(dmx, group=gdf$tide)
mod <- betadisper(dmx, group=interaction(gdf$species, gdf$site))
## Plot the groups and distances to centroids on the
## first two PCoA axes
# png(paste0(plot_dir, '/tps_disp_species.png'), width=7, height=5, units = 'in', 
#     res = 150)
p2 <- plot(mod, axes=c(1,2))
# dev.off()

plot(mod, axes=c(1,2))
## Draw a boxplot of the distances to centroid for each group
boxplot(mod)

# Perform test on distances to centroid
anova(mod)
permutest(mod, pairwise = TRUE, permutations = 999)

## Tukey's Honest Significant Differences
mod.HSD <- TukeyHSD(mod)
plot(mod.HSD)

# ==== Trajectory analysis ====

# Trajectory analysis for visualizing differences between groups (full model)
trj1 <- trajectory.analysis(m1, groups=gdf$site, traj.pts=gdf$species)
plot1 <- plot(trj1, 
              pch=as.numeric(factor(gdf$site)) + 20, 
              col = as.numeric(factor(gdf$species)),
              cex = 1.1)
add.trajectories(plot1, traj.pch = c(21, 22), start.bg = 1, end.bg = 2)
legend("topright", levels(factor(gdf$site)), pch =  c(21, 22), pt.bg = 1)

reveal.model.designs(m1)
summary(trj1, attribute='MD')
summary(trj1, attribute='TC', stat.table=T, angle.type='deg')
