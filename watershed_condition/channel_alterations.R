# Angus Watters
# Calculate length and % of network that is altered/unaltered for each US/DS LHD site

rm(list = ls())

# Libaries
library(tidyverse)
library(sf)
library(dataRetrieval)
library(nhdplusTools)
library(mapview)
library(tidygraph)
library(sfnetworks)
library(nngeo)

# Source utility functions
source("utils/data_utils.R")


# *****************************
# ---- Network alterations ----
# *****************************

# LHD Points w/ NHDplus info, remove some issue LHDs
lhd_pts <- readRDS("data/spatial/points/lhd_flowline_points.rds") %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4)
  ) %>% 
  filter(!COMID %in% c(17876471, 17876487,  17900363, 17900361))

# Path to clean directed stream networks
stream_network_path <- paste0(
  "data/spatial/networks/trim/", 
  list.files("data/spatial/networks/trim/", pattern = "trim")
)


# Empty lists to add to 
connect_lst <- list()
edge_lst    <- list()

# Loop through networks and calculate connectivity metrics 
for (i in 1:length(stream_network_path)) {
  
  net_txt <- gsub("data/spatial/networks/trim/", "", stream_network_path[i])
  
  ipct    <- paste0(round(100*(i/length(stream_network_path)), 2))
  
  logger::log_info("{ipct} %")
  logger::log_info("Calculating stream network connectivity\n File name: {net_txt}\n Full path: {stream_network_path[i]}")
  
  # Upstream tributary network
  ut_net <- readRDS(stream_network_path[i])

  # Prepare flowlines to use in sfnetwork
  flines <- prep_flines(
    flowlines = ut_net,
    split     = FALSE,
    channels  = TRUE
  )
  
  # Prepare points to use in sfnetwork
  pts  <- prep_points(
    flowlines = ut_net,
    points    = lhd_pts,
    wb        = TRUE
  )
  
  net    <- create_network(
    flowlines = flines,
    points    = pts
  )
  
  # Partition directed network edges between LHD points, waterbodies, and headwaters
  group_net <- 
    net %>%
    sfnetworks::activate("edges") %>%
    dplyr::mutate(group = group_custom())
  
  # Plot the results.
  nodes        <-  sf::st_as_sf(group_net, "nodes")
  edges        <-  sf::st_as_sf(group_net, "edges")
  
  # Calculate distances between LHDs, dams, reservoirs, headwaters
  connect <- network_channel_alt(
    flines      = edges,
    points      = pts,
    return_wide = FALSE
  ) %>%
    dplyr::mutate(network = net_txt)
  
  logger::log_info("Connectivity analysis complete: \n{net_txt}")
  
  edge_final <- 
    edges %>% 
    dplyr::mutate(network = net_txt)
  
  connect_lst[[i]] <- connect
  edge_lst[[i]]    <- edge_final
  
}

# ********************************************

# Long datasets with all points and distances (includes Waterbody points)
channel_alt_df   <- dplyr::bind_rows(connect_lst)
channel_edge_df  <- dplyr::bind_rows(edge_lst)

# Save long dataset
saveRDS(channel_alt_df, "data/channel_alterations/network_channel_alt.rds")
saveRDS(channel_edge_df, "data/channel_alterations/network_channel_alt_flowlines.rds")

# ********************
# ---- Widen data ----
# ********************

channel_alt_df <- readRDS("data/channel_alterations/network_channel_alt.rds")

# Widen datasets, 1 row per LHD
channel_alt_wide <-
  channel_alt_df %>% 
  dplyr::select(new_id, comid, group, position, 
                channel_pct_altered, channel_pct_unaltered, length_altered, length_unaltered) %>% 
  dplyr::filter(!grepl("waterbody", new_id)) %>% 
  dplyr::filter(!group %in% c(873)) %>% 
  tidyr::pivot_wider(
    id_cols     = c(new_id, comid),
    names_from  = "position",
    names_glue  = "{position}_{.value}",
    values_from = c(length_unaltered, length_altered, channel_pct_unaltered, channel_pct_altered)
  ) %>% 
  dplyr::select(new_id, comid,
                us_length_unaltered, us_length_altered, us_channel_pct_unaltered, us_channel_pct_altered,
                ds_length_unaltered, ds_length_altered, ds_channel_pct_unaltered, ds_channel_pct_altered)

# Save wide dataset
saveRDS(channel_alt_wide, "data/channel_alterations/network_channel_alt_pct.rds")

# ********************************************
# ********************************************









