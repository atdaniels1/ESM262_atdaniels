#' compute_diversity
#'
#' Compute a species diversity index
#' @param species list of species (names)
#' @return list with the following items
#' \describe{
#' \item{mode}{The most common fish}
#' \item{min}{The rarest fish}
#' \item{num_Total}{The total number of fish}
#' }
#' @examples
#' compute_diversity(c("salmon","tuna","steelhead","cod","tuna","steelhead","cod")))
#' @references
#' https://github.com/atdaniels1/ESM262_atdaniels/blob/main/fish.txt

Fish_species <- readLines(here::here("esm262_data", "fish.txt"))
Fish_species <- gsub("\"", "", Fish_species)


compute_diversity = function(Fish_species) {


  # Create a table for the fish names
  list_fish <- table(Fish_species)

  # which is the most frequent
  Fish.Most_Common = names(which.max(list_fish))

   # which is the most rare names

  Fish.Most_Rare = names(which.min(list_fish[list_fish != 1]))

  # number of species
  Fish.num_Total = length(Fish_species[Fish_species != "x"])



  # output from function
  return(list(mode=Fish.Most_Common, min=Fish.Most_Rare, num_Total= Fish.num_Total))
}


