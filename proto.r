library(readxl)
library(dplyr)
library(stringr)
library(tibble)
library(tidyr)

sites <- read.csv("../coleo/rcoleo/extdata/csv/sites.csv")
sites <- select(sites,No_de_référence_de_la_cellule,No_de_référence_du_site,Type_de_milieu,No_borne_forestière,Latitude,Longitude)
names(sites) <- c("cell_code","site_code","type","monit_prg_station_id","lat","lon")

sites_ls <- apply(sites,1,as.list)

# Creer geom points
loc <- apply(sites,1, function(x){
  if(!any(is.na(x["lat"]),is.na(x["lon"]))){
  return(geojson_list(as.numeric(c(x["lat"],x["lon"])))$features[[1]]$geometry)
} else {
  return(NA)
}})

# Fusionner les deux listes (locations + sites)
for(i in 1:length(sites_ls)){
  sites_ls[[i]]$loc <- loc[i][[1]]
  if(is.list(sites_ls[[i]]$loc)){
    sites_ls[[i]]$loc$crs <- list(type="name",properties=list(name="EPSG:4326"))
  }
}

postSites <- function(sites_ls) {

  endpoint <- rcoleo.env$endpoints$sites

  # Create resp
  responses <- list()

  for (i in 1:length(sites_ls)) {

    url <- paste0("http://localhost:8080/api/v0/cells?id=",sites_ls[[i]]$cell_code)

    sites_ls[[i]]$cell_id <- httr::content(httr::GET(url,config = httr::add_headers(
        "Content-type" = "application/json",
        "Authorization" = paste('Bearer',rcoleo.env$bearer))))[[1]]$id

    responses[[i]] <- post(endpoint, sites_ls[[i]])

  }

  return(responses)
}


getSites <- function(){

  endpoint <- rcoleo.env$endpoints$sites
  response <- get(endpoint)

  return(response$content)
}


getSpecies <- function(){

  endpoint <- rcoleo.env$endpoints$species
  response <- get(endpoint)

  return(response$content)
}
