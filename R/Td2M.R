#' TidyData to Matrix
#'
#' @param Tidy_DataSet Tidy_DataSet
#'
#' @return
#' @export
#'
#' @examples
getMatrixForm <- function(Tidy_DataSet, withYears = FALSE){

  if (withYears) {
    names_diff <- which(names(Tidy_DataSet) %in% c('Years', 'Trait', 'Response') == FALSE)
    Tidy_DataSet <- Tidy_DataSet[ , c('Years', names(Tidy_DataSet)[names_diff], 'Trait', 'Response')]
    out <- tidyr::spread(Tidy_DataSet, 'Trait', Response)
  } else {
    if (is.null(Tidy_DataSet$Env)) {
      message("Env is null, Enviroment will appear like ''")
      Tidy_DataSet$Env <- ''
    }

    if (is.null(Tidy_DataSet$Trait)) {
      message("Trait is null, Trait will appear like ''")
      Tidy_DataSet$Trait <- ''
    }

    out <- tidyr::unite(Tidy_DataSet, 'TraitxEnv', Trait, Env, sep = "_")
    out <- tidyr::spread(out, 'TraitxEnv', Response)
  }

  return(out)
}

#' Matrix to TidyData
#'
#' @param Matrix Matrix
#'
#' @return
#' @export
#'
#' @examples
getTidyForm <- function(Matrix_DataSet, withYears = F){
  if (withYears) {
    return(tidyr::gather(Matrix_DataSet, 'Trait', 'Response', -c(1,2)))
  } else {
    data <- tidyr::gather(Matrix_DataSet, 'TraitxEnv', 'Response', -c(1))
    return(tidyr::separate(data, TraitxEnv, c("Trait", "Env"), sep = "_"))
  }
}