#' Prepare and validate the data for the IBCF Model
#'
#' @description
#' @param ID The identifier of the observation
#' @param Environment The name of the environments where the observation was evaluated
#' @param Trait The name of the traits that was measured in the environment.
#' @param Response The measure of the trait.
#' @param data (data.frame object) The dataset where can be recovered the data.
#' @param messages (Logical) Should be printed the messages of the function?
#' @param get (string) By default the function return 'all' the transformations of the data, if you want only the tidy format of the data select 'tidy' or 'matrix' if you want only the matrix format.
#'
#' @return one or two list IBCFData object.
#'
#' @examples
#' \dontrun{
#'   library(IBCF.MTME)
#'
#'   ### Choose one
#'   data <- data.frame(Response = c(1,23,4,25,23,5))
#'   data <- data.frame(Response = c(1,23,4,25,23,5), Environment = c('E1', 'E1', 'E2', 'E1', 'E2', 'E2'))
#'   data <- data.frame(Response = c(1,23,4,25,23,5), Trait = c('T1', 'T1', 'T2', 'T1', 'T2', 'T2_'))
#'   data <- data.frame(Response = c(1,23,4,25,23,5,6,79), Trait = c('T1', 'T1', 'T2', 'T2', 'T2', 'T2', 'T1', 'T1_'), Environment = c('E1_', 'E2', 'E2', 'E1', 'E2', 'E1', 'E2', 'E1'))
#'   data <- data.frame(Response = c(1,23,4,25,23,5,6), Trait = c('T1', 'T1', 'T2', 'T2', 'T2', 'T2', 'T1'), Environment = c('E1_', 'E2', 'E2', 'E1', 'E2', 'E1', 'E2'))
#'
#'   ## Validate the dataset and obtain only the matrix format.
#'   Data <- IBCF.Data(Environment = 'Environment', Trait = 'Trait', Response = 'Response', data = data, get = 'Matrix')  # Could change get to 'all' or 'tidy'
#' }
#'
#' @export
#'
IBCFData <- function(ID = '', Environment = '', Trait = '', Response = '', data = NULL, messages = TRUE, get = 'all') {

  if (is.null(data)) {

  }

  if (Response == '' || is.null(data[, Response])) {
    message('[X] ERROR: Response was not provided')
    stop(call. = FALSE)
  }

  dataset <- data.frame(ID = '', Environment = '', Trait = '', Response = data[, Response])

  dataset$Environment <- tryCatch({
    if (Environment == '' || is.null(data[ , Environment])) {
      if (messages) {message('[!] There not Environment elements')}
      ''
    } else {
      check.Environment(data[ , Environment], messages)
    }
  }, error = function(e) {
    if (messages) {message('[!] Maybe something was bad in the Environment column or this simply not exist and you can ignore this')}
    ''
  })

  dataset$Trait <- tryCatch({
    if (Trait == '' || is.null(data[ , Trait])) {
      if (messages) {('[!] There not Trait elements.')}
      ''
    } else {
      check.Trait(data[ , Trait], messages)
    }
  }, error = function(e) {
    if (messages) {message('[!] Maybe something was bad in the Trait column or this simply not exist and you can ignore this')}
    ''
  })

  dataset <- dataset[order(dataset$Trait, dataset$Environment), ]

  dataset$ID <- tryCatch({
    if (ID == '' || is.null(data[ , ID])) {
      nTrait <- length(unique(dataset$Trait))
      nEnv <- length(unique(dataset$Environment))
      nID <- dim(dataset)[1] / (nTrait*nEnv)

      rep(1:nID, times = nTrait)
    } else {
      data[ , ID]
    }
  }, error = function(e) {
    if (messages) {message('[!] Maybe something was bad in the ID column')}
    ''
  }, finally = {
    nTrait <- length(unique(dataset$Trait))
    nEnv <- length(unique(dataset$Environment))
    nID <- dim(dataset)[1] / (nTrait*nEnv)
    print(cat(nTrait, nEnv, nID,  dim(dataset)[1]))
    if (nID %% 1 != 0) {
      warning('[!!] The dataset is unbalanced, might cause fatal errors')
    }
  })

  OUT <- list(TidyDataset = dataset,
              MatrixDataset = getMatrixForm(dataset, TRUE))

  if (get == 'Tidy') {
    OUT <- OUT[1]
  } else if (get == 'Matrix') {
    OUT <- OUT[2]
  }

  class(OUT) <- 'IBCFData'
  return(OUT)
}


check.Trait <- function(Traits, messages){
  badNames <- grepl("_", Traits, fixed = TRUE)

  if (any(badNames == TRUE)) {
    Traits <- gsub("_", "", Traits)
    if (messages) {message('[!] Some Traits names was changed')}
  }

  return(Traits)
}

check.Environment <- function(Environment, messages) {
  badNames <- grepl("_", Environment, fixed = TRUE)

  if (any(badNames == TRUE)) {
    Environment <- gsub("_", "", Environment)
    if (messages) {message('[!] Some Environment names was changed')}
  }

  return(Environment)
}

# data <- data.frame(Response = c(1,23,4,25,23,5))
# data <- data.frame(Response = c(1,23,4,25,23,5), Environment = c('E1', 'E1', 'E2', 'E1', 'E2', 'E2'))
# data <- data.frame(Response = c(1,23,4,25,23,5), Trait = c('T1', 'T1', 'T2', 'T1', 'T2', 'T2_'))
# data <- data.frame(Response = c(1,23,4,25,23,5,6,79), Trait = c('T1', 'T1', 'T2', 'T2', 'T2', 'T2', 'T1', 'T1_'), Environment = c('E1_', 'E2', 'E2', 'E1', 'E2', 'E1', 'E2', 'E1'))
# # data <- data.frame(Response = c(1,23,4,25,23,5,6), Trait = c('T1', 'T1', 'T2', 'T2', 'T2', 'T2', 'T1'), Environment = c('E1_', 'E2', 'E2', 'E1', 'E2', 'E1', 'E2'))
#
# Data <- IBCF.Data(Environment = 'Environment', Trait = 'Trait', Response = 'Response', data = data, get = 'all')