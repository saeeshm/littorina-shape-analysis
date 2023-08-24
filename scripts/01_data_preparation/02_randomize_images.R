# Author: Saeesh Mangwani
# Date: 2023-08-19

# Description: Randomly assigning shell images to each member in the group for
# landmarking, aiming to spread person-bias in landmarking across the factor
# levels

# ==== Libraries ====
library(dplyr)
library(readr)
library(stringr)
library(readxl)
library(purrr)
library(tidyr)

# ==== File paths ====

# Path to image reference dataset
imref_path <- 'data/image-reference.csv'

# Path to original shell images
raw_img_path <- 'data/images/shell-photos-raw'

# Path to renamed shell images (the output from the first part of the script)
rnm_img_path <- 'data/images/shell-photos-renamed'

# Path to assigned shell images
assignment_path <- 'data/images/assignments'

# ==== Reading data ====

# Table referencing images to files
imgs <- read_csv(imref_path)

# ==== Copying and renaming all raw shell images ====

# For each directory, creating a renamed copy of the file
walk(list.files(raw_img_path, full.names=T, pattern='^aug'), ~{
  fnames <- list.files(.x)
  # For each file
  for(name in fnames){
    # Getting new name
    newname <- imgs %>% filter(photo_id==name) %>% pull(new_name)
    # Getting the raw name and the new name from the image reference table
    ogpath <- file.path(.x, name)
    npath <- file.path(rnm_img_path, newname)
    # Status print
    print(npath)
    # Copying the raw image to the new location using the new name
    file.copy(ogpath, npath, overwrite = F)
  }
})

# ==== Helper functions for random splitting of samples ====

# Function to create an assignment index - splitting a specific number of ids
# into a certain number of groups
get_samp_sizes <- function(nsamps, ngrps){
  div <- round(nsamps/ngrps)
  remn <- nsamps %% div
  samp_sizes <- if (remn > 0){
    c(rep(div, ngrps-1), remn)
  }else{
    rep(div, ngrps)
  }
  return(samp_sizes)
}

# Function to randomly split a vector into a fixed number of groups
rand_split_ids <- function(ids, ngrps, seed=NULL){
  # Setting seed (NULL = reset)
  set.seed(seed)
  # Getting the sizes of each random split
  sizes <- get_samp_sizes(length(ids), ngrps)
  # Index vector for IDs already sampled
  rmids <- c()
  # List of sampled IDs
  samples <- vector('list', ngrps)
  # For each group size
  for(i in 1:length(sizes)){
    # print(i)
    size <- sizes[[i]]
    currids <- sample(ids, size, replace=F)
    # Getting samples
    samples[[i]] <- currids
    # Removing sampled ids from the total list of ids
    ids <- ids[!ids %in% currids]
    # print(ids)
  }
  return(samples)
}

# ==== Randomly splitting images per group ====

# Splitting data into groups by factor
dat_grps <- imgs %>% 
  filter(!is.na(found)) %>% 
  group_by(site, species, tidal_height) %>% 
  group_split()

# Using a seed for a reproducible randomness
seed <- 824

# 3-group split ----------
ngrps <- 3

# Group member names
members <- c('Laura', 'Nadine', 'Marion')

# For each data group
assignments <- map_dfr(dat_grps, ~{
  # Getting all sample ids
  ids <- .x$tray_id
  # Randomly splitting the ids into the required number of groups
  split_ids <- rand_split_ids(ids, ngrps, seed) %>% 
    setNames(members) %>% 
    as.data.frame() %>% 
    pivot_longer(everything(), names_to='name', values_to='tray_id')
})

# Joining the image IDs
assignments <- assignments %>% 
  left_join(imgs %>% select(tray_id, new_name))

# Copying files to individual person folders
for(member in members){
  # Getting file paths for assignment images
  files <- assignments %>% filter(name == member) %>% pull(new_name)
  paths <- file.path(rnm_img_path, files)
  # Copying images to folder
  out_dir <- file.path(assignment_path, member)
  dir.create(out_dir)
  walk(files, ~{
    print(.x)
    frompath <- file.path(rnm_img_path, .x)
    topath <- file.path(out_dir, .x)
    file.copy(from=frompath, to=topath, overwrite=F)
  })
}

# 2-group split ----------
ngrps <- 2

# Group member names
members <- c('Saeesh', 'Gabriel')

# For each data group
assignments <- map_dfr(dat_grps, ~{
  # Getting all sample ids
  ids <- .x$tray_id
  # Randomly splitting the ids into the required number of groups
  split_ids <- rand_split_ids(ids, ngrps, seed) %>% 
    setNames(sample(members))
  # Filling NA for someone that has less assigned this round
  lengths <- map_int(split_ids, length)
  whichless <- names(lengths)[lengths == min(lengths)]
  split_ids[[whichless]] <- c(split_ids[[whichless]], NA_integer_)
  # Converting to a dataframe
  split_ids <- split_ids %>% 
    as.data.frame() %>% 
    pivot_longer(everything(), names_to='name', values_to='tray_id') %>% 
    filter(!is.na(tray_id))
  return(split_ids)
})

# Joining the image IDs
assignments <- assignments %>% 
  left_join(imgs %>% select(tray_id, new_name))

# Copying files to individual person folders
for(member in members){
  # Getting file paths for assignment images
  files <- assignments %>% filter(name == member) %>% pull(new_name)
  paths <- file.path(rnm_img_path, files)
  # Copying images to folder
  out_dir <- file.path(assignment_path, member)
  dir.create(out_dir)
  walk(files, ~{
    print(.x)
    frompath <- file.path(rnm_img_path, .x)
    topath <- file.path(out_dir, .x)
    file.copy(from=frompath, to=topath, overwrite=F)
  })
}

# Individually check how many have been assigned to each person and randomly
# re-assign for fairness/balance
map_int(paste0('../images/assignments/', members), ~{
  .x %>% list.files() %>% length()
})
