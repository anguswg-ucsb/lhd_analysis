# Angus Watters
# Calculate arbolate sum at each LHD site

rm(list = ls())

# Libaries
library(tidyverse)
library(sf)
library(dataRetrieval)
library(nhdplusTools)
library(mapview)
library(sfnetworks)

# Source utility functions
source("utils/data_utils.R")

# *******************
# ---- Load data ---- 
# *******************


# ***************************
# ---- Network flowlines ---- 
# ***************************

# # Network of flowlines
networks      <- readRDS("data/spatial/networks/lhd_nhdplus_networks.rds")
fline_connect <- readRDS("data/spatial/points/lhd_flowline_connectivity.rds")
arb_sum <- fline_connect %>% 
  dplyr::select(COMID, Hydroseq, ArbolateSu, us_length, ds_length, total_length)
# Download network of NHDplus flowlines
mapview(networks) + fline_pts
# LHD comids
comid_df       <- readRDS("data/lhd/lhd_comid.rds")

# Flowlines with lHD snapped points
fline_pts      <- readRDS("data/spatial/points/lhd_flowline_points.rds")
net2 <- networks %>% 
  dplyr::select(COMID, LevelPathI, gnis_name, gnis_id, Hydroseq)

down_comid <- 226643

ut_net <- nhdplusTools::navigate_network(
  start       = down_comid, 
  mode        = "UT", 
  distance_km = 200
) %>% 
  st_transform(5070)

plot(ut_net$geometry)
plot(ut_net2$geometry, col = "red", add = T)

# Comids on network
pts <- 
  fline_pts %>% 
  filter(COMID %in% ut_net$comid) %>% 
  dplyr::select(new_id, comid = COMID, hydroseq = Hydroseq, levelpathi = LevelPathI, ArbolateSu) %>% 
  st_transform(5070)

# Trim network down to only relevant paths
trim_ut_net <-
  ut_net %>%
  mutate(
    comid      = as.character(comid),   
    hydroseq   = as.character(hydroseq),
    levelpathi = as.character(levelpathi)
  ) %>% 
  filter(levelpathi %in% unique(pts$levelpathi)) %>% 
  dplyr::select(comid, hydroseq, levelpathi) %>% 
  st_transform(5070)

plot(ut_net$geometry)
plot(trim_ut_net$geometry, col = "red", add = T)
plot(pts$geometry, col = "green", add = T)

ggplot() +
  # geom_sf(data = ut_net, color = "black") +
  geom_sf(data = trim_ut_net, color = "red", size = 1) +
  geom_sf(data = pts, aes(size = ArbolateSu)) +
  theme(legend.position = "none")
plot(trim_ut_net)
# network of mainstems
flines  <-
  trim_ut_net %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(5070) %>% 
  sf::st_combine() %>% 
  sf::st_line_merge() %>% 
  sf::st_sf() %>% 
  dplyr::mutate(new_id = "path") %>% 
  dplyr::relocate(new_id, geometry)

fline_nodes <- 
  pts %>% 
  dplyr::select(new_id) 

flines_net <- bind_rows(fline_nodes, flines)

# get nodes of line segments
# flines_nodes <-
#   flines %>% 
#   sf::st_coordinates() %>% 
#   as.data.frame() %>% 
#   sf::st_as_sf(
#     coords = c("X", "Y"),
#     crs    = 5070
#   )
# trim_ut_net
flines_net <- bind_rows(pts, flines)


# length(unique(pts$LevelPathI))
# length(unique(ut_net$levelpathi))

# ********************
# ---- sfnetworks ----
# ********************

# STEP 1: Create a 'sfnetworks' object only based 'on connected lines(i.e.edges)

network <-
  flines_net %>% 
  filter(st_geometry_type(.) == "MULTILINESTRING") %>%   # select only lines from sf object
  st_cast("LINESTRING") %>% 
  # st_snap(.,., tolerance = 1000) %>%
  as_sfnetwork(directed = T)                                    # create network

autoplot(network)

# STEP 2: Create a 'sf' object with only points (i.e. nodes)
nodes <- 
  flines_net %>% 
  filter(st_geometry_type(.) == "POINT")

paths <- st_network_paths(
  x    = network,
  from = nodes[1,], 
  to   = nodes[-1,]
)
# paths = mapply(
#   st_network_paths,
#   from = shop,
#   to = cust,
#   MoreArgs = list(x = roads_clean)
# )["node_paths", ] %>%
#   unlist(recursive = FALSE)

# STEP 3: Add the nodes of the 'sf' object into the 'network'
new_network <-
  network %>% 
  st_network_blend(., nodes, tolerance = 10000)

ln_sf <- 
  new_network %>% 
  activate("edges") %>% 
  mutate(
    length     = edge_length()
    # new_new_id = as.character(1:n())
  ) %>% 
  st_as_sf()
plot(ln_sf$geometry[2])
plot(ln_sf$geometry)
# %>%  # snap nodes to network w/ tolerance
#   # filter(.,!is.na(new_id)) %>%                         # keep only nodes from sf object 'nodes'
#   st_as_sf() %>%                                       # convert to sf object 
#   as_sfnetwork(., edges_as_lines = T)             # reconstruct network with only nodes from the sf object 'nodes'
plot(new_network$geometry)
# plot 
# option 1 with autoplot:
autoplot(new_network)
# +
#   geom_sf_text(
#     data = st_as_sf(new_network),
#     aes(label = new_id),
#     size = 3,
#     hjust = 0
#   )

# ***********************************************************************
# points to split lines by
pts <- 
  fline_pts %>%
  sf::st_transform(5070) %>% 
  nhdplusTools::rename_geometry("geometry") %>% 
  dplyr::select(new_id)

# network of mainstems
flines  <-
  networks %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(5070) %>% 
  sf::st_combine() %>% 
  sf::st_line_merge() 

# get nodes of line segments
flines_nodes <-
  flines %>% 
  sf::st_coordinates() %>% 
  as.data.frame() %>% 
  sf::st_as_sf(
    coords = c("X", "Y"),
    crs    = 5070
  )


# Original LHD dataset w/ new_id 
# lhd_pts       <- readRDS("data/spatial/points/lhd_points.rds") 

# LHD comids
comid_df      <- readRDS("data/lhd/lhd_comid.rds")

# LHD points snapped to flowlines
fline_pts     <- readRDS("data/spatial/points/lhd_flowline_points.rds")

fline_connect <- readRDS("data/spatial/points/lhd_flowline_connectivity.rds")

# *****************
# ---- Arb sum ---- 
# *****************
down_comid <- 1596203

ut_net <- nhdplusTools::navigate_network(
  start       = down_comid, 
  mode        = "UT", 
  distance_km = 200
) %>% 
  st_transform(5070)

pts <- 
  fline_connect %>%
  filter(LevelPathI == 350009839) %>% 
  dplyr::select(new_id, COMID, hydroseq = Hydroseq, 
                us_length, ds_length, total_length, 
                arb_sum = ArbolateSu, LevelPathI, StreamOrde, streamleve) %>% 
  mutate(
    arb_sum          = as.numeric(arb_sum)
    # arb_sum_diff     = arb_sum - lag(arb_sum),
    # arb_sum_cuml     = cumsum(arb_sum)
  ) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  group_by(LevelPathI) %>% 
  arrange(hydroseq, .by_group = T) %>% 
  mutate(
    arb_sum_lead = lead(arb_sum),
    arb_sum_lag  = lag(arb_sum),
    arb_sum_diff = arb_sum - arb_sum_lead
  ) %>% 
  ungroup() %>% 
  dplyr::relocate(new_id, COMID, hydroseq, us_length, ds_length, total_length,
                  arb_sum, arb_sum_lead, arb_sum_lag, arb_sum_diff, geometry) 


aw_reach 
mapview(pts) + ut_net