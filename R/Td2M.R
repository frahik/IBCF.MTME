#' Tidy data format to Matrix format
#'
#' @param Tidy_DataSet \code{data.frame} object that contains 4 columns:
#' \code{$Line}: Line or genotype identifier, and the name of this column could change.
#' \code{$Env}: Name of the evaluated environment (s).
#' \code{$Trait}: Name of the evaluated trait (s).
#' \code{$Response}: Variable response obtained for the row corresponding to line and environment.
#' @param onlyTrait \code{logical} by default is \code{FALSE}, if is \code{TRUE} only the column \code{$Trait} is transformed.
#'
#' @return A \code{data.frame} object with the \code{$Response} divided by \code{$Traits} columns.
#' @export
#'
#' @examples
#' \dontrun{
#'   data('Wheat_IBCF')
#'   M <- getMatrixForm(Wheat_IBCF)
#' }
#'
#' \dontrun{
#'   data('Year_IBCF')
#'   M.Y <- getMatrixForm(Year_IBCF, onlyTrait = T)
#' }
getMatrixForm <- function(Tidy_DataSet, onlyTrait = FALSE){

  if (onlyTrait) {
    names_diff <- which(names(Tidy_DataSet) %in% c('Trait', 'Response') == FALSE)
    Tidy_DataSet <- Tidy_DataSet[ , c(names(Tidy_DataSet)[names_diff], 'Trait', 'Response')]
    out <- tidyr::spread(Tidy_DataSet, 'Trait', 'Response')
  } else {
    if (is.null(Tidy_DataSet$Env)) {
      message("Env is null, Enviroment will appear like ''")
      Tidy_DataSet$Env <- ''
    }

    if (is.null(Tidy_DataSet$Trait)) {
      message("Trait is null, Trait will appear like ''")
      Tidy_DataSet$Trait <- ''
    }

    out <- tidyr::unite(Tidy_DataSet, 'TraitxEnv', 'Trait', 'Env', sep = "_")
    out <- tidyr::spread(out, 'TraitxEnv', 'Response')
  }

  return(out)
}

#' Matrix format to Tidy data format
#'
#' @param Matrix_DataSet A data.frame object with the response values divided in \eqn{n} environments or traits columns
#' @param onlyTrait \code{logical} by default is \code{FALSE}, if is \code{TRUE} only is considered the \code{$Trait} column.
#'
#' @return A \code{data.frame} object with the \code{$Response} divided by \code{$Traits} columns.
#' @export
#'
#' @examples
#' \dontrun{
#'   data('Wheat_IBCF')
#'   M <- getMatrixForm(Wheat_IBCF)
#'   Tidy <- getTidyForm(M)
#' }
#'
#' \dontrun{
#'   data('Year_IBCF')
#'   M.Y <- getMatrixForm(Year_IBCF, onlyTrait = T)
#'   Tidy.Y <- getTidyForm(M.Y, onlyTrait = T)
#' }
#'
getTidyForm <- function(Matrix_DataSet, onlyTrait = F){

  if (onlyTrait) {
    return(tidyr::gather(Matrix_DataSet, 'Trait', 'Response', -c(1:2)))
  } else {
    data <- tidyr::gather(Matrix_DataSet, 'TraitxEnv', 'Response', -c(1))
    return(tidyr::separate(data, 'TraitxEnv', c("Trait", "Env"), sep = "_"))
  }
}