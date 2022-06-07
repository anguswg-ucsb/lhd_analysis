# Angus Watters
# Retrieve catchment areas for all upstream and downstream comids from LHDs and calculate the landcover % using the NLCD

rm(list = ls())

# Libaries

# Data wrangling
library(dplyr)
library(tidyr)

# Spatial 
library(sf)
library(raster)
library(terra)
library(fasterize)
library(mapview)

# Data retrieval
library(nhdplusTools)
library(FedData)
# devtools::install_github("ropensci/FedData")

# Source utility functions
source("utils/data_utils.R")

# ********************************
# ---- Extract catchment NLCD ----
# ********************************

# LHD network connectivity
net_connect <- readRDS("data/spatial/networks/connectivity/lhd_network_connectivity.rds") %>% 
  dplyr::ungroup()

# spatial LHD points for viewing
pts <- 
  net_connect %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(5070) %>% 
  dplyr::select(-us_comid_list, -ds_comid_list)

# Empty list to iteratively add to
class_lst     <- list()
# land_type_lst <- list()

# Loop through each LHD US/DS network and retrieve catchments and NLCD landcover data for US/DS/Total network catchments
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
  
  logger::log_info("\n\n{c(net_comids)}")
  
  logger::log_info("\n\nDownloading US/DS NHDPlus...")
  
  # US/DS catchments
  catch <- 
    nhdplusTools::get_nhdplus(
      comid       = net_comids,
      realization = "catchment"
    ) %>% 
    sf::st_transform(5070) %>% 
    dplyr::mutate(
      new_id = 1:n()
    )
  
  # Upstream catchments NLCD
  us_catch <- 
    catch %>% 
    filter(featureid %in% us_comids) %>% 
    dplyr::summarize(geometry = sf::st_union(geometry)) %>% 
    dplyr::mutate(
      new_id = 1:n()
    ) %>% 
    st_landcover(mask = TRUE)
  
  # Downstream catchments NLCD
  ds_catch <- 
    catch %>% 
    filter(featureid %in% ds_comids) %>% 
    dplyr::summarize(geometry = sf::st_union(geometry)) %>% 
    dplyr::mutate(
      new_id = 1:n()
    ) %>% 
    st_landcover(mask = TRUE)
  
  # Union all catchment into single polygon
  net_catch <-
    catch %>% 
    dplyr::summarize(geometry = sf::st_union(geometry)) %>% 
    dplyr::mutate(
      new_id = 1:n()
    ) %>% 
    st_landcover(mask = TRUE)
  
  # Downstream NLCD class %
  ds_lc <- landcover_pct(
    nlcd         = ds_catch, 
    rel_position = "ds",
    id_label     = i_new_id
    )
  
  # upstream NLCD class %
  us_lc <- landcover_pct(
    nlcd         = us_catch, 
    rel_position = "us",
    id_label     = i_new_id
  )
  
  # full network NLCD class %
  net_lc <- landcover_pct(
    nlcd         = net_catch, 
    rel_position = "total",
    id_label     = i_new_id
  )
 
  # Join DS/US/Total to single dataset and calculate area in square kilometers
  landcover <- 
    ds_lc %>% 
    dplyr::left_join(
      us_lc, 
      by = "new_id"
      ) %>% 
    dplyr::left_join(
      net_lc, 
      by = "new_id"
    ) %>% 
    dplyr::mutate(
      ds_area_sqkm      = (ds_cells*900)/1000000,
      us_area_sqkm      = (us_cells*900)/1000000,
      total_area_sqkm   = (total_cells*900)/1000000
    ) %>% 
    dplyr::relocate(new_id, 
                    ds_cells, us_cells, total_cells,
                    ds_area_sqkm, us_area_sqkm, total_area_sqkm,
                    ds_nat_lc_pct, ds_unnat_lc_pct,
                    us_nat_lc_pct, us_unnat_lc_pct,
                    total_nat_lc_pct, total_unnat_lc_pct)
  
  class_lst[[i]]     <- landcover
  # land_type_lst[[i]] <- land_type_vals
  
}


# *****************************************************

class_df     <- dplyr::bind_rows(class_lst)

# Save natural/unnatural landtypes by LHD
saveRDS(
  class_df,
  here::here("data", "landcover", "catchment_landcover.rds")
  )

# land_type_df <- dplyr::bind_rows(land_type_lst)
# # Save individual landtypes by LHD
# saveRDS(land_type_df, here::here("data", "landcover", "catchment_landtype2.rds"))


# *****************************************************




