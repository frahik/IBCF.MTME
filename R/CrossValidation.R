#' @title Cross-Validation with Random Partions
#'
#' @param NLine \code{integer} Number of Lines in the DataSet. By default are 10 Lines.
#' @param NEnv \code{integer} Number of Enviroments in the DataSet. Is 1 by default.
#' @param NTraits \code{integer} Number of Traits in the DataSet. Is 1 by default.
#' @param NPartitions \code{integer} Number of Partitions for the Cross-Validation. Is 10 by default.
#' @param PTesting \code{Double} Porcentage of Testing for the Cross-Validation. Is 0.35 by default.
#' @param Set_seed \code{integer} Number of seed for replicable research. Is NULL by default.
#'
#' @return \code{List} A list object with lenght of \code{NPartitions}, every indice has a \code{matrix} \eqn{n \times x}, where \eqn{n} is the number of \code{NLines} and \eqn{x} is the number of  \code{NEnv} \eqn{\times} \code{NTraits}. The values inside is 1 for training and 2 for testing.
#'
#' @examples
#' \dontrun{
#'   CV.RandomPart(NLine = 100, NEnv = 5)
#'   CV.RandomPart(NLine = 100, NTraits = 5)
#'   CV.RandomPart(NLine = 100, NEnv = 3, NTraits = 3)
#'   CV.RandomPart(NLine = 100, NEnv = 3, NTraits = 3, NPartitions = 20)
#'   CV.RandomPart(NLine = 100, NEnv = 3, NTraits = 3, NPartitions = 20, PTesting = .5)
#'   CV.RandomPart(NLine = 100, NEnv = 3, NTraits = 3, NPartitions = 20, PTesting = .5, Set_seed = 123)
#' }
#' @export
CV.RandomPart <- function(NLine = 10, NEnv = 1, NTraits = 1, NPartitions = 10, PTesting = .35, Set_seed = NULL) {
  if (!is.null(Set_seed)) {
    set.seed(Set_seed)
  }

  env <- c(1:NEnv)
  p_list <- vector('list',NPartitions)
  names(p_list) <- paste0('p',1:NPartitions)
  resp <- rep(1, NLine * NEnv)
  y <- resp
  Y <- matrix(resp, ncol = NEnv, byrow = FALSE)

  for (i in 1:NPartitions) {
    nEnv <- length(env)
    y <- Y[,env]
    n <- nrow(Y)
    percTST <- PTesting
    nTST <- round(percTST*n)
    nNA <- nEnv*nTST
    if (nNA < n) {
      indexNA <- sample(1:n,nNA,replace = FALSE)
    }
    if (nNA >= n) {
      nRep <- floor(nNA/n)
      remain <- sample(1:n, nNA %% n, replace = FALSE)
      a0 <- sample(1:n,n,replace = FALSE)
      indexNA <- rep(a0,nRep)

      if (length(remain) > 0) {
        a1 <- floor(length(indexNA)/nTST)*nTST;a2 <- nNA - a1 - length(remain)
        bb <- sample(a0[!a0 %in% remain], a2, replace = FALSE)
        noInIndexNA <- c(rep(a0, nRep - 1), a0[!a0 %in% bb])
        indexNA <- c(noInIndexNA,bb,remain)
      }
    }

    indexEnv <- rep(1:nEnv, each = nTST)
    yNA <- y

    for (j in 1:nEnv) {
      if (nEnv < 2) {
        yNA[indexNA] <- 2
      } else {
        yNA[indexNA[indexEnv == j], j] <- 2
      }
    }

    A <- matrix(rep(1, NTraits), ncol = NTraits)
    B <- kronecker(A, yNA)
    p_list[[paste0('p',i)]] <- B
  }
  return(p_list)
}