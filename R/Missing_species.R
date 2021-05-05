#' Bird Species Observed on eBird but not iNat
#'
#' Find out which species you have observed in eBird but not in iNaturalist
#' @param user The target iNaturalist username
#' @return A list of bird species
#' @examples 
#' Species  <- Missing_species("whimbrelbirder")
#' @export
Missing_species <- function(user){
  
  eBird_data <- read.csv(choose.files())
  eBird_data <- tidyr::separate(eBird_data, "Species", into = c("Common_name", "Scientific_name"), sep = " - ")
  eBird_Species <- eBird_data$Common_name
  
  #API set-up
  resp <- httr::GET(paste("https://api.inaturalist.org/v1/observations/species_counts?user_id=", user, "&page=1&hrank=species", sep = ""))
  parsed <- httr::content(resp, as = "parsed")

  options(warn=-1)
  #Retreving data from the API
  for(x in 0:(parsed$total_results/500)+1){
    resp <- httr::GET(paste("https://api.inaturalist.org/v1/observations/species_counts?user_id=", user, "&page=", as.character(x), "&hrank=species", sep = ""))
    parsed <- httr::content(resp, as = "parsed")
    modJSON <- parsed$results 
    modJSON <- rlist::list.select(modJSON, taxon$preferred_common_name, taxon$iconic_taxon_name)
    if(x == 1){
      data <- rlist::list.stack(modJSON)
    }
    if(x > 1){
      dataz <- rlist::list.stack(modJSON)
      data <- rbind(data, dataz)
    }
  }
  options(warn=0)
  
  data <- data.frame(data$V1, data$V2)
  names(data) <- c("Species", "Iconic Taxon")
  data <- dplyr::filter(data, `Iconic Taxon`=="Aves")
  eBird_data$Logical <- eBird_data$Common_name %in% data$Species
  eBird_data <- dplyr::filter(eBird_data, Logical == FALSE)
  Species <- eBird_data$Common_name
  return(Species)
}

