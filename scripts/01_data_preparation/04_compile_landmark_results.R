# Author: Saeesh Mangwani
# Date: 2023-08-20

# Description: Compiling results from everyone's individual landmarking into
# single datasets

# ==== Libraries ====
library(dplyr)
library(purrr)
library(readr)
library(stringr)

# ==== Paths and global variables ====

# Path to all landmark results by person
landmark_path <- 'data/landmarks/inputs'

# Gathering shellshaper landmark data
members <- c('gabriel', 'saeesh')
shpr_paths <- list.files(landmark_path, full.names=T)[str_detect(list.files(landmark_path), paste(members, collapse='|'))] %>% 
  setNames(members)

# Shellshaper output file path
shpr_out <- 'data/landmarks/shellshaper_results.csv'

# ==== Compiling shellshaper results ====

# Reading data ----------
shpr <- imap_dfr(shpr_paths, ~{
  read_csv(.x) %>% mutate(analyst = .y)
})

# Counting samples per person - should add to 155
shpr$analyst %>% table()
shpr$analyst %>% table() %>% sum()

# Writing result table to disk
write_csv(shpr, shpr_out)
