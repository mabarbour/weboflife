#' Get network info from Web of Life
#'
#' @param type interaction type
#' @param names would you like to include the names of species?
#' @return list of networks
#' @seealso
#' @export
#' @examples

get_network_info <- function(type){

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
  json_network_info <- jsonlite::fromJSON(paste(readLines(json_file), collapse = "")) %>%
    select(networkAuthor, networkQ, locationName, locationLatitude, locationLongitude) %>%
    mutate(networkQ = ifelse(networkQ == "\001", "weighted","binary"),
           locationLatitude = as.numeric(locationLatitude),
           locationLongitude = as.numeric(locationLongitude))

  #json_network_info_list <- lapply(json_network_info, FUN = function(x) as.data.frame(t(x))) #list()
  #json_network_info_df <- plyr::ldply(json_network_info_list) %>%
  #  select(networkAuthor, networkQ, locationName, locationLatitude, locationLongitude) %>%
  #  mutate(networkQ = ifelse(networkQ == "\001", "Q","B"))
  #for(i in 1:length(json_network_info)){
  #  json_network_info_list <- t(json_network_info[[i]])
  #}

  return(json_network_info)#df)
}

