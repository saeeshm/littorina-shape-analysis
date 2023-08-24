# Author: Saeesh Mangwani
# Date: 2023-08-20

# Description: Tidying the field data sheet for analysis

# ==== Libraries ====
library(readxl)
library(dplyr)

# ==== File paths ====

# Excel sheet containing field data
fdata_path <- 'data/raw-field-sampling-data.xlsx'

# ==== Reading data ====

# Table referencing images to files
imgs <- readxl::read_xlsx(fdata_path, sheet=1)
names(imgs) <- names(imgs) %>% tolower() %>% str_replace_all(' ', '_')

# Table describing field sites
field <- readxl::read_xlsx(fdata_path, sheet=2)
names(field) <- names(field) %>% tolower() %>% str_remove_all('\\(|\\)') %>% str_replace_all(' ', '_')

# ==== Tidying data ====

# Created a more descriptive image name for each image
imgs <- pmap_dfr(imgs, function(tray_id, found, photo_id, species, tidal_height, site, ...){
  # Short name for species
  spc <- ifelse(species == 'Saxatilis', 'sxt', 'ltr')
  # Getting the first letter of each tidal height class
  th <- str_sub(tidal_height, 1, 1)
  # 
  st <- ifelse(site == 'Exposed', 'EXP', 'SHL')
  pid <- photo_id %>% str_extract('\\d+')
  new_name <- paste0(paste(pid, st, spc, th, sep='_'), '.JPG')
  return(list('tray_id' = tray_id, 'found'=found, 'photo_id'=photo_id, 
              'species'=species, 'tidal_height'=tidal_height, 'site'=site, 
              'new_name'=new_name))
})

# ==== Writing to disk ====
write_csv(imgs, 'data/image-reference.csv')
write_csv(field, 'data/field-site-desc.csv')


