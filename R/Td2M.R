#' TidyData to Matrix
#'
#' @param DataSet Dataset
#'
#' @return
#' @export
#'
#' @examples
Td2M <- function(DataSet){
  if (is.null(DataSet$Env)) {
    message("Env is null, Enviroment will appear like ''")
    DataSet$Env <- ''
  }

  if (is.null(DataSet$Trait)) {
    message("Trait is null, Trait will appear like ''")
    DataSet$Trait <- ''
  }

  data <- tidyr::unite(DataSet, 'TraitxEnv', Trait, Env, sep = "_")
  return(tidyr::spread(data, 'TraitxEnv', Response))
}

#' Matrix to TidyData
#'
#' @param DataSet Dataset
#'
#' @return
#' @export
#'
#' @examples
Td2M <- function(DataSet){


  if (is.null(DataSet$Env)) {
    DataSet$Env <- ''
  }

  if (is.null(DataSet$Trait)) {
    DataSet$Trait <- ''
  }

  data <- tidyr::unite(DataSet, 'TraitxEnv', Trait, Env, sep = "_")
  return(tidyr::spread(data, 'TraitxEnv', Response))
}