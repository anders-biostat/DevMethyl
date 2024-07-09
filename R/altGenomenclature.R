#' Obtain alternate genome nomenclature
#'
#' `altGenomenclature` returns a list containing the genome identifiers of the genome version provided.
#'
#' @inheritParams plot_all
#'
#' @return List of both genome identifiers. The first element in the list is the GRC nomenclature, and the second element is the UCSC nomenclature.
#' @export
#'
#' @examples altGenomenclature("mouse", "mm10")
altGenomenclature <- function(species, genome) {

  df <- list_UCSC_genomes(species)

  row <- df[grep(genome, df$description), ]

  description <- row$description

  grc <- gsub(".*\\((.*?)\\/.+", "\\1", description)

  return(list(grc, row$genome) )

}
