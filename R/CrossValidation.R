#' @title Cross-Validation with Random Partitions
#'
#' @description This method consists of randomly dividing the training data set and the test data set.
#' For each division, the approximation function is adjusted from the training data and calculates the output values for the test data set.
#' The result corresponds to the arithmetic mean of the values obtained for the different divisions.
#'
#' @param DataSet \code{data.frame} The data set object is a data.frame object that contains 4 columns in the Tidy data format:
#' \code{$Line} is the Line or genotype identifier, and the name of this column could change.
#' \code{$Env} is the name of the evaluated environment (s).
#' \code{$Trait} is the name of the evaluated trait (s).
#' \code{$Response} Variable response obtained for the row corresponding to line and environment.
#' @param NPartitions \code{integer} Number of Partitions for the Cross-Validation. Is 10 by default.
#' @param PTesting \code{Double} Percentage of Testing for the Cross-Validation. Is 0.35 by default.
#' @param Traits.testing \code{character} By default is null and use all the traits to fit the model, else only part of the traits specified be used to fit the model.
#' @param Set_seed \code{integer} Number of seed for reproducible research. Is NULL by default.
#'
#' @return \code{List} A list object with length of \code{NPartitions}, every index has a \code{matrix} \eqn{n \times x}, where \eqn{n} is the number of \code{NLines} and \eqn{x} is the number of  \code{NEnv} \eqn{\times} \code{NTraits}. The values inside is 1 for training and 2 for testing.
#'
#' @examples
#' \dontrun{
#'   library(IBCF.MTME)
#'   data('Wheat_IBCF')
#'
#'   CV.RandomPart(Wheat_IBCF)
#'   CV.RandomPart(Wheat_IBCF, NPartitions = 10)
#'   CV.RandomPart(Wheat_IBCF, Traits.testing = 'DH')
#'   CV.RandomPart(Wheat_IBCF, NPartitions = 10, PTesting = .35)
#'   CV.RandomPart(Wheat_IBCF, NPartitions = 10, Traits.testing = 'DH')
#'   CV.RandomPart(Wheat_IBCF, NPartitions = 10, PTesting = .35, Set_seed = 5)
#'   CV.RandomPart(Wheat_IBCF, NPartitions = 10, PTesting = .35, Traits.testing = 'DH')
#'   CV.RandomPart(Wheat_IBCF, NPartitions = 10, PTesting = .35, Traits.testing = 'DH', Set_seed = 5 )
#' }
#' @export
CV.RandomPart <- function(DataSet, NPartitions = 10, PTesting = .35, Traits.testing = NULL, Set_seed = NULL) {
  if (!is.null(Set_seed)) {
    set.seed(Set_seed)
  }

  if (length(unique(DataSet$Env)) == 0 ) {
    DataSet$Env <- ''
  }
  if (length(unique(DataSet$Trait)) == 0 ) {
    DataSet$Trait <- ''
  }

  new_Data <- getMatrixForm(DataSet, onlyTrait = FALSE)
  NLine <- dim(new_Data)[1]

  if (length(unique(DataSet$Env)) == 1 ) {
    NEnv <- length(unique(DataSet$Trait))
    NTraits <- length(unique(DataSet$Env))
  } else {
    NEnv <- length(unique(DataSet$Env))
    NTraits <- length(unique(DataSet$Trait))
  }

  Trait <- unique(DataSet$Trait)
  Env <- unique(DataSet$Env)

  p_list <- vector('list', NPartitions)
  names(p_list) <- paste0('p', 1:NPartitions)
  resp <- rep(1, NLine * NEnv)
  Y <- matrix(resp, ncol = NEnv, byrow = FALSE)

  for (i in 1:NPartitions) {
    y <- Y[,1:NEnv]
    n <- nrow(Y)
    percTST <- PTesting
    nTST <- round(percTST*n)
    nNA <- NEnv*nTST
    if (nNA < n) {
      indexNA <- sample(1:n,nNA,replace = FALSE)
    }
    if (nNA >= n) {
      nRep <- floor(nNA/n)
      remain <- sample(1:n, nNA %% n, replace = FALSE)
      a0 <- sample(1:n,n,replace = FALSE)
      indexNA <- rep(a0,nRep)

      if (length(remain) > 0) {
        a1 <- floor(length(indexNA)/nTST)*nTST
        a2 <- nNA - a1 - length(remain)
        bb <- sample(a0[!a0 %in% remain], a2, replace = FALSE)
        noInIndexNA <- c(rep(a0, nRep - 1), a0[!a0 %in% bb])
        indexNA <- c(noInIndexNA,bb,remain)
      }
    }

    indexEnv <- rep(1:NEnv, each = nTST)
    yNA <- y

    for (j in 1:NEnv) {
      if (NEnv < 2) {
        yNA[indexNA] <- 2
      } else {
        yNA[indexNA[indexEnv == j], j] <- 2
      }
    }

    A <- matrix(rep(1, NTraits), ncol = NTraits)
    B1 <- kronecker(A, yNA)

    Names_MFormat <- colnames(new_Data[, -1])  # FORMATO MATRIZ
    colnames(B1) <- Names_MFormat

    if (!is.null(Traits.testing)) {
      Traits_Selec_F <- c()
      for (T.T in Traits.testing) {
        Traits_Selec <- which(grepl(T.T, Names_MFormat) == TRUE)
        Traits_Selec_F <- c(Traits_Selec_F,Traits_Selec)
      }

      B1[, -c(Traits_Selec_F)] <- 1
    }

    p_list[[paste('p', i, sep = '')]] <- B1
  }


  out <- list(
    DataSet = new_Data,
    CrossValidation_list = p_list,
    Environments = Env,
    Traits.testing = Traits.testing,
    Traits = Trait,
    Observations = NLine
  )

  class(out) <- 'CrossValidation'
  return(out)
}
