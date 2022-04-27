# Angus Watters
# Calculate upstream & downstream distance between LHD sites and/or waterbodies

rm(list = ls())

# Libaries
library(tidyverse)
library(sf)
library(dataRetrieval)
library(nhdplusTools)
library(mapview)

# Source utility functions
source("utils/data_utils.R")

# *******************
# ---- Load data ---- 
# *******************

# Network of flowlines
networks      <- readRDS("data/spatial/networks/lhd_nhdplus_networks.rds")

# Original LHD dataset w/ new_id 
lhd_pts       <- readRDS("data/spatial/points/lhd_points.rds") 

# LHD comids
comid_df      <- readRDS("data/lhd/lhd_comid.rds")

# LHD points snapped to flowlines
fline_pts     <- readRDS("data/spatial/points/lhd_flowline_points.rds") 

# *******************************
# ---- Distance between LHDs ---- 
# *******************************

# Calculate upstream & downstream distance between LHD sites and/or waterbodies
lhd_distances <- segment_distance(
  points    = fline_pts, 
  flowlines = networks, 
  unit      = "miles"
)

# Join distances w/ original LHD dataset
lhd_pts_dist <-
  lhd_pts %>% 
  dplyr::left_join(
    lhd_distances, 
    by = "new_id"
  ) %>% 
  dplyr::left_join(
    dplyr::select(comid_df, new_id, comid = origin_comid, hydroseq, levelpathi),
    by = "new_id"
  ) %>%
  dplyr::select(
    new_id, lhd_id, wdid = assoc_wdid, comid, hydroseq, levelpathi, 
    us_length, ds_length, total_length, unit
  )

# Join distances w/ flowlines (NHDplus data)
fline_pts_dist <- 
  fline_pts %>% 
  dplyr::left_join(
    lhd_distances, 
    by = "new_id"
  )



# Save distances along flowlines data
saveRDS(lhd_pts_dist, "data/spatial/points/lhd_connectivity.rds")
saveRDS(fline_pts_dist, "data/spatial/points/lhd_flowline_connectivity.rds")
