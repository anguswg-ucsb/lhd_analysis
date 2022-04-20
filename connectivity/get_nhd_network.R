# Angus Watters
# Retrieve NHDplus NHD flowlines & waterbodies data for LHD locations

rm(list = ls())
 
library(tidyverse)
library(sf)
library(dataRetrieval)
library(nhdplusTools)
library(mapview)

source("utils/utils.R")

lhd_path <- "data/lhd/Low Head Dam Inventory Final CIM 092920 - Inventory.csv"

# Read in LHD file
lhd <- readr::read_csv(lhd_path) %>% 
  janitor::clean_names() 

# Convert LHD dataframe to spatial points  using sf package 
lhd_pt <- 
    lhd %>% 
    sf::st_as_sf(
      coords = c("longitude", "latitude"), 
      crs    = 4326
    ) %>% 
    sf::st_transform(5070) %>% 
    rename(lhd_id = id) %>% 
    mutate(u_id = 1:n())  %>% 
    dplyr::relocate(u_id) 

mapview(lhd_pt)

fline_lst <- list()
comid_lst <- list()

for (i in 1:nrow(lhd_pt)) {

  # LHD point
  pt  <- 
    lhd_pt[i, ] %>%
    sf::st_transform(4326) %>%  
    sf::st_coordinates()
  
  # LHD id
  u_id <- lhd_pt$u_id[i] 
  
  # GNIS Stream ID in LHD dataset
  stream_id   <- as.character(lhd_pt$assoc_stream_id[i])
  
  # GNIS Stream name in LHD dataset
  stream_name <- as.character(lhd_pt$stream_name[i])

  logger::log_info("Retrieving NHD COMID\n- LHD ID: {u_id}\n- Stream ID: {stream_id}\n- Stream name: {stream_name}")

  fline <- nhdplusTools::get_nhdplus(
    comid = dataRetrieval::findNLDI(location =   pt)$comid
    )
  
  fline <-
    fline %>%
    nhdplusTools::align_nhdplus_names() %>% 
    dplyr::mutate(
      u_id         = u_id
    )  %>% 
    dplyr::mutate(dplyr::across(where(is.numeric), as.character)) %>% 
    dplyr::mutate(
      gnis_id = dplyr::case_when(
        grepl("^\\s*$", gnis_id)   ~ stream_id,
        TRUE                       ~ gnis_id
        ),
      gnis_name = case_when(
        grepl("^\\s*$", gnis_name) ~ stream_name,
        TRUE                       ~ gnis_name
        )
      ) 

  # point comid
  origin_df <- tibble::tibble(
    u_id         = u_id,
    origin_comid = as.integer(dataRetrieval::findNLDI(location =   pt)$comid),
    reach_code   = fline$REACHCODE,
    huc8         = substr(fline$REACHCODE, 0 ,8),
    hydroseq     = fline$Hydroseq,
    gnis_id      = fline$gnis_id,
    gnis_name    = fline$gnis_name,
    levelpathi   = fline$LevelPathI
    )

  # add comid to list 
  comid_lst[[i]] <- origin_df
  fline_lst[[i]] <- fline
}

# bind rows of dataframe lists
comid_df <- bind_rows(comid_lst)
fline_df <- bind_rows(fline_lst)

saveRDS(comid_df, "data/lhd/lhd_comid.rds")
saveRDS(fline_df, "data/lhd/lhd_flowline.rds")



# Find COMID of furthest downstream LHD on each mainstem 
start_comid <-
  comid_df %>%
  group_by(levelpathi) %>% 
  filter(hydroseq == min(hydroseq)) %>% 
  ungroup()

# length(unique(comid_df$levelpathi))
# length(unique(start_comid$origin_comid))

net_lst <- list()

# i = 2
for (i in 1:nrow(start_comid)) {

  downstream_comid <- start_comid$origin_comid[i]
  
  # GNIS Stream ID in LHD dataset
  stream_id   <- as.character(start_comid$gnis_id[i])
  
  # GNIS Stream name in LHD dataset
  stream_name <- as.character(start_comid$gnis_name[i])
  
  logger::log_info("Retrieving upstream mainstem network starting at:\nCOMID: {downstream_comid}")
  
  um_net <- navigate_network(
    start       = downstream_comid, 
    mode        = "UM",
    distance_km = 2000
    ) %>%
    nhdplusTools::align_nhdplus_names() %>% 
    mutate(
      check = ifelse(COMID %in% comid_df$origin_comid | WBAREACOMI != 0, 1, 0)
      ) 
  
  um_net_split <- 
    um_net %>%
    arrange(Hydroseq) %>% 
    group_split(cumsum(check == 1), keep = F)
  
  um_net_split <- 
    um_net_split %>% 
    setNames(paste0("segment_", 1:length(um_net_split))) %>% 
    dplyr::bind_rows(.id = "segment_id") %>% 
    dplyr::mutate(
      gnis_id = dplyr::case_when(
        grepl("^\\s*$", gnis_id)   ~ stream_id,
        TRUE                       ~ gnis_id
      ),
      gnis_name = case_when(
        grepl("^\\s*$", gnis_name) ~ stream_name,
        TRUE                       ~ gnis_name
      )
    ) %>% 
    mutate(across(where(is.numeric), as.character))
  
  net_lst[[i]] <- um_net_split
  
}

# Bind rows in list of dataframes
networks <- bind_rows(net_lst)

# Map View
mapview(networks) + lhd_pt

# save to data/lhd/
saveRDS(networks, "data/lhd/lhd_nhdplus_networks.rds")

  

