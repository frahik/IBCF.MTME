#' @title IBCF
#' @description Item Based Collaborative Filtering for multi-trait and multi-environment data.
#'
#' @param object \code{list} CrossValidation object, is obtained from CV.RandomPartition function.
#' @param dec \code{integer} Number of decimals to print in the results.
#'
#' @return A list with the next components
#' \item{NPartitions}{\code{integer} Number of partitions used for testing data}
#' \item{predictions_Summary}{\code{data.frame} A data.frame with the results of the test}
#' \item{Predictions}{\code{list} A list with the predicted results for each partition}
#'
#' @export
#'
#' @importFrom stats cor na.omit sd complete.cases
#' @examples
#'  \dontrun{
#'   library(IBCF.MTME)
#'   data('Wheat_IBCF')
#'
#'   CV <- CV.RandomPart(Wheat_IBCF)
#'   IBCF(CV)
#' }
#'
IBCF <- function(object, dec = 4) {
  if (!inherits(object, 'CrossValidation')) stop("This function only works for objects of class 'CrossValidation'")
  time.init <- proc.time()[3]
  nIL <- ncol(object$DataSet) - 1

  Y_avr <- matrix(0, ncol = nIL, nrow = nrow(object$DataSet))
  Ind_all <- Y_avr
  NPartitions <- length(object$CrossValidation_list)

  predicted <- vector('list', NPartitions)
  names(predicted) <- paste0('Partition', 1:NPartitions)

  results <- data.frame()
  for (j in seq_len(NPartitions)) {
    Part <- object$CrossValidation_list[[j]]

    pos.NA <- which(Part == 2, arr.ind = TRUE)
    pos.NA[, 2] <- c(pos.NA[, 2]) + 1
    pos.No_NA <- which(Part == 1, arr.ind = TRUE)
    # pos.No_NA[, 2] <- c(pos.No_NA[, 2])

    if (length(pos.NA) == 0) {
      stop('An error ocurred with the CrossValidation data')
    }

    Data.trn <- object$DataSet

    Data.trn[pos.NA] <- NA

    rows.Na <- which(apply(Data.trn, 1, function(x) any(is.na(x))) == TRUE)

    Means_trn <- apply(Data.trn[, -c(1)], 2, mean, na.rm = TRUE)
    SDs_trn <- apply(Data.trn[, -c(1)], 2, sd, na.rm = TRUE)

    Scaled_Col <- scale(Data.trn[, -c(1)])

    Means_trn_Row <- apply(Scaled_Col, 1, mean, na.rm = TRUE)
    SDs_trn_Row <- apply(Scaled_Col, 1, sd, na.rm = TRUE)

    if (any(is.na(SDs_trn_Row))) {
      Data.trn_scaled <- data.frame(ID = as.character(Data.trn[, c(1)]), Scaled_Col)
    } else {
      Scaled_Row <- t(scale(t(Scaled_Col)))
      Data.trn_scaled <- data.frame(ID = as.character(Data.trn[, c(1)]), Scaled_Row)
    }

    Hybrids.New <- Data.trn_scaled
    Hybrids.New[, 2:ncol(Data.trn_scaled)] <- NA

    ratings <- Data.trn_scaled

    x <- ratings[, 2:(ncol(ratings))]

    x[is.na(x)] <- 0

    item_sim <- lsa::cosine(as.matrix((x)))

    for (i in seq_len(length(rows.Na))) {
      pos <- rows.Na[i]
      ratings[pos, 2:ncol(ratings)] <- rec_itm_for_geno(pos, item_sim, ratings[,2:ncol(ratings)])
    }

    All.Pred <- data.matrix(ratings[,-1])


    if (any(is.na(SDs_trn_Row))) {
      All.Pred_O <- sapply(seq_len(ncol(All.Pred)), function(i) (All.Pred[,i]*SDs_trn[i] + Means_trn[i]))
    } else {
      All.Pred_O_Row <- t(sapply(seq_len(nrow(All.Pred)), function(i) (All.Pred[i,]*SDs_trn_Row[i] + Means_trn_Row[i])) )
      All.Pred_O <- sapply(seq_len(ncol(All.Pred_O_Row)), function(i) (All.Pred_O_Row[,i]*SDs_trn[i] + Means_trn[i]))
    }

    colnames(All.Pred_O) <- colnames(Data.trn_scaled[,-c(1)])
    All.Pred_O[pos.No_NA] <- NA
    All.Pred_O_tst <- All.Pred_O[rows.Na, ]

    predicted[[paste0('Partition', j)]] <- c(All.Pred_O)
    pos.No_NA[,2] <- pos.No_NA[,2] + 1
    DataSet_tst <- object$DataSet
    DataSet_tst[pos.No_NA] <- NA
    DataSet_tst <- DataSet_tst[rows.Na, ]

    Data.Obs_tst <- getTidyForm(DataSet_tst)
    posTST <- which(complete.cases(Data.Obs_tst) == TRUE)
    YYY <- All.Pred_O
    Ind <-  is.na(YYY)
    YYY[Ind] <- 0
    Y_avr <- Y_avr + YYY
    Ind <-  !(Ind)
    Ind_all <- Ind_all + Ind

    results <- rbind(results, data.frame(Position = posTST,
      Partition = j,
      Environment = Data.Obs_tst$Env[posTST],
      Trait = Data.Obs_tst$Trait[posTST],
      Observed = round(Data.Obs_tst$Response[posTST],dec),
      Predicted = round(c(All.Pred_O_tst[which(!is.na(All.Pred_O_tst))]), dec)))
  }
  Y_avr <- Y_avr/Ind_all
  Yhat_Obs_pred <- data.frame(object$DataSet, Y_avr)

  out <- list(NPartitions = NPartitions,
              predictions_Summary = results,
              observed = getTidyForm(object$DataSet)$Response,
              yHat = Y_avr,
              predicted_Partition = predicted,
              Data.Obs_Pred = Yhat_Obs_pred,
              executionTime = proc.time()[3] - time.init)
  class(out) <- 'IBCF'
  return(out)
}
