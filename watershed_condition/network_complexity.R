# Angus Watters

# # Network Complexity
  # A barrier that has upstream tributaries of different size classes, such as small streams, small rivers, and large rivers, would contribute a more complex connected aquatic network if it was removed. 

# methods:
# 1. Stream and river reaches were assigned to size classes based on total drainage area:
    # Headwaters: < 10 km2
    # Creeks: ≥ 10 km2 and < 100 km2
    # Small rivers: ≥ 100 km2 and < 518 km2
    # Medium tributary rivers: ≥ 519 km2 and < 2,590 km2
    # Medium mainstem rivers: ≥ 2,590 km2 and < 10,000 km2
    # Large rivers: ≥ 10,000 km2 and < 25,000 km2
    # Great rivers: ≥ 25,000 km2

# 2. Each barrier is assigned the total number of unique size classes in its upstream functional network.

# Source: https://connectivity.sarpdata.com/metrics/complexity/

rm(list = ls())

# Libaries
library(tidyverse)
library(sf)
library(dataRetrieval)
library(nhdplusTools)
library(mapview)

# Source utility functions
source("utils/data_utils.R")

# ****************************
# ---- Network complexity ----
# ****************************

# LHD network connectivity
net_connect <- readRDS("data/spatial/networks/connectivity/lhd_network_connectivity.rds") %>% 
  dplyr::ungroup()

net_flines <- readRDS("data/spatial/networks/connectivity/network_connectivity_flowlines.rds") 

pts <- 
  net_connect %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(5070) %>% 
  dplyr::select(-us_comid_list, -ds_comid_list)

# flowlines <- readRDS("data/spatial/networks/connectivity/network_connectivity_flowlines.rds") 

# mapview::mapview(pts$geometry) + flowlines

# i <- 55
# i <- 4
size_class_lst <- list()

for (i in 1:nrow(net_connect)) {
  
  # Low head damn row "i"
  lhd <- net_connect[i, ]
  # pt  <- pts[i, ]
  
  i_new_id <- lhd$new_id
  i_comid  <- lhd$comid
  
  logger::log_info("\n\n ----- {i} of {nrow(net_connect)} -----\nLHD ID    --> {i_new_id}\nLHD COMID --> {i_comid}")
  
  
  # upstream comid list
  us_comids <- unlist(lhd$us_comid_list)
  
  # Downstream comid list
  ds_comids <- unlist(lhd$ds_comid_list)
  
  # All comids upstream and downstream of point
  net_comids <- unique(c(us_comids, ds_comids))
  
  # logger::log_info("\n\n{list(net_comids)}")
  logger::log_info("\n\nDownloading US/DS NHDPlus...")
  
  logger::log_info("Flowlines...")
  
  # US/DS flowlines 
  fline <-
    nhdplusTools::get_nhdplus(
      comid       = net_comids
    ) %>%
    sf::st_transform(5070) %>%
    dplyr::select(comid, lengthkm, ftype, fcode, totdasqkm, geometry)
  
  logger::log_info("\n\nCalculating size classes...")
  
  # Upstream network complexity
  us_fline <-   
    fline %>% 
    sf::st_drop_geometry() %>% 
    dplyr::filter(comid %in% us_comids) %>% 
    dplyr::mutate(
      position   = "us",
      size_class = dplyr::case_when(
        totdasqkm < 10                          ~ "headwater",
        totdasqkm >= 10 & totdasqkm < 100       ~ "creek",
        totdasqkm >= 100 & totdasqkm < 518      ~ "small_river",
        totdasqkm >= 519 & totdasqkm < 2590     ~ "med_tributary_river",
        totdasqkm >= 2590 & totdasqkm < 10000   ~ "med_mainstem_river",
        totdasqkm >= 10000 & totdasqkm < 25000  ~ "large_river",
        totdasqkm >= 25000                      ~ "great_river"
        )
      ) %>% 
    dplyr::relocate(comid, lengthkm, ftype, fcode, totdasqkm, size_class, position) %>% 
    dplyr::group_by(position) %>%
    dplyr::summarise(
      totdasqkm = sum(totdasqkm, na.rm = T),
      count     = n_distinct(size_class)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      new_id = i_new_id
    ) %>% 
    dplyr::relocate(new_id, position, size_class = count, totdasqkm) %>% 
    tidyr::pivot_wider(
      names_from  = "position",
      names_glue  = "{position}_{.value}",
      values_from = c(size_class, totdasqkm)
    )
  
  # Downstream network complexity
  ds_fline <-   
    fline %>% 
    sf::st_drop_geometry() %>% 
    dplyr::filter(comid %in% ds_comids) %>% 
    dplyr::mutate(
      position   = "ds",
      size_class = dplyr::case_when(
        totdasqkm < 10                          ~ "headwater",
        totdasqkm >= 10 & totdasqkm < 100       ~ "creek",
        totdasqkm >= 100 & totdasqkm < 518      ~ "small_river",
        totdasqkm >= 519 & totdasqkm < 2590     ~ "med_tributary_river",
        totdasqkm >= 2590 & totdasqkm < 10000   ~ "med_mainstem_river",
        totdasqkm >= 10000 & totdasqkm < 25000  ~ "large_river",
        totdasqkm >= 25000                      ~ "great_river",
      )
    ) %>% 
    dplyr::relocate(comid, lengthkm, ftype, fcode, totdasqkm, size_class, position) %>% 
    dplyr::group_by(position) %>%
    dplyr::summarise(
      totdasqkm = sum(totdasqkm, na.rm = T),
      count     = n_distinct(size_class)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      new_id = i_new_id
    ) %>% 
    dplyr::relocate(new_id, position, size_class = count, totdasqkm) %>% 
    tidyr::pivot_wider(
      names_from  = "position",
      names_glue  = "{position}_{.value}",
      values_from = c(size_class, totdasqkm)
    )
  
  # Total network complexity
  tot_fline <-  
    fline %>% 
    sf::st_drop_geometry() %>% 
    dplyr::mutate(
      position   = "total",
      size_class = dplyr::case_when(
        totdasqkm < 10                          ~ "headwater",
        totdasqkm >= 10 & totdasqkm < 100       ~ "creek",
        totdasqkm >= 100 & totdasqkm < 518      ~ "small_river",
        totdasqkm >= 519 & totdasqkm < 2590     ~ "med_tributary_river",
        totdasqkm >= 2590 & totdasqkm < 10000   ~ "med_mainstem_river",
        totdasqkm >= 10000 & totdasqkm < 25000  ~ "large_river",
        totdasqkm >= 25000                      ~ "great_river",
      )
    ) %>% 
    dplyr::relocate(comid, lengthkm, ftype, fcode, totdasqkm, size_class, position) %>% 
    dplyr::group_by(position) %>%
    dplyr::summarise(
      totdasqkm = sum(totdasqkm, na.rm = T),
      count     = n_distinct(size_class)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      new_id = i_new_id
    ) %>% 
    dplyr::relocate(new_id, position, size_class = count, totdasqkm) %>% 
    tidyr::pivot_wider(
      names_from  = "position",
      names_glue  = "{position}_{.value}",
      values_from = c(size_class, totdasqkm)
    )
  
  dir_fline <- 
    us_fline %>% 
    dplyr::left_join(
      ds_fline,
      by = "new_id"
      ) %>% 
    dplyr::left_join(
      tot_fline,
      by = "new_id"
    ) %>% 
    dplyr::relocate(new_id, ds_size_class, us_size_class, total_size_class, 
                    ds_totdasqkm, us_totdasqkm, total_totdasqkm)
  
  size_class_lst[[i]] <- dir_fline
  
  
}


size_class_df <- dplyr::bind_rows(size_class_lst)

# Save network complexity dataset
saveRDS(size_class_df, "data/network_complexity/network_complexity.rds")
# size_class_df <- readRDS("data/network_complexity/network_complexity.rds")






