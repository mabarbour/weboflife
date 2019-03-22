#' Get network info from Web of Life
#'
#' @param type interaction type
#' @param names would you like to include the names of species?
#' @return list of networks
#' @seealso
#' @export
#' @examples

get_network_info <- function(type, names = "yes"){

  # create an empty list to store the networks
  network_list <- list()

  # nested ifelse statement to turn interaction types into codes for Web of Life
  type_id <- ifelse(type == "plant-ant", "3",
                    ifelse(type == "plant-pollinator", "5",
                           ifelse(type == "plant-seed disperser", "6",
                                  ifelse(type == "anemone-fish", "11",
                                         ifelse(type == "host-parasite", "8",
                                                ifelse(type == "plant-herbivore", "10",
                                                       ifelse(type == "food web", "7", NA)))))))

  # create a file (json_networks) with the names of the networks we would like to download
  json_file <- paste("http://www.web-of-life.es/map_networkdetails_type.php?networkType=",
                     type_id,
                     #"&data=All",
                     sep = "")
  json_networks <- rjson::fromJSON(paste(readLines(json_file), collapse = ""))

  # would you like to include the names of the species? ("yes" or "no")
  speciesName <- names

  # download the networks
  for(i in 1: length(json_networks)){

    # identifying the network
    #if(json_networks[[i]]$countSpecies > 0) { # we get networks and subnetworks
    if(json_networks[[i]]$root == 0 & is.null(json_networks[[i]]$parentNetworkId)){ # we get networks without subnetworks
      networkName <- json_networks[[i]]$networkName
      print(networkName)

      # building the URL
      url <- paste("http://www.web-of-life.es/download/",
                   networkName,
                   "_",
                   speciesName, ".csv",
                   sep = "")

      # download the network from www.web-of-life.es
      data <- data.table::fread(url)

      # storing the networks as a data table
      assign(networkName,data)

      # storing the networks as a list
      network_list[[networkName]] <- (data)
    }
  }
  return(network_list)
}

