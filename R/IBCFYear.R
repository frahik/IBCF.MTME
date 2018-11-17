#' @title IBCF.Years
#' @description Item Based Collaborative Filtering for Years data
#'
#' @param DataSet \code{data.frame} A data set in Matrix Form.
#' @param colYears \code{string or integer} A name or the position of the 'Years' column just in case that is not the first column.
#' @param colID \code{string or integer} A name or the position of the 'ID' column, just in case that is not the second column.
#' @param Years.testing \code{vector} A vector with the names of the years to use in test.
#' @param Traits.testing \code{vector} A vector with the names of the traits to use in test.
#' @param dec \code{integer} Number of decimals to print in the results.
#'
#' @return A list with the next components
#' \item{Year.testing}{\code{vector} a vector with the Years used for the testing data}
#' \item{Traits.testing}{\code{vector} a vector with the Traits used for the testing data}
#' \item{Data_Obs_Pred}{\code{data.frame} Contains the values observed and predicted (the predicted values has '.1' after the name)}
#' \item{predictions_Summary}{\code{data.frame} Contains the summary of the correlation of the predictions and the MAAPE}
#'
#' @examples
#' \dontrun{
#'   library(IBCF.MTME)
#'   data('Year_IBCF')
#'   DataSet <- getMatrixForm(Year_IBCF, withYears = TRUE)
#'   IBCF.Years(DataSet , Years.testing = c('2015', '2016'), Traits.testing = c('T5', 'T6'))
#'
#' }
#'
#' @export
IBCF.Years <- function(DataSet, colYears = 1, colID = 2, Years.testing = '', Traits.testing = '', dec = 4) {
  time.init <- proc.time()[3]
  DataSet[, colYears] <- as.character(DataSet[, colYears]) #No factors admited
  No.Years <- length(unique(DataSet[, colYears]))
  No.Years.testing <- length(Years.testing)
  No.Traits <- length(colnames(DataSet)) - 2
  No.Traits.testing <- length(Traits.testing)
  No.Traits.Accepted <- round(0.7*No.Traits)
  colYears <- ifelse(is.numeric(colYears), colYears, which(names(DataSet) == colYears))
  colID <- ifelse(is.numeric(colID), colID, which(names(DataSet) == colID))

  if (No.Years.testing == No.Years) {
    stop("No.Years.testing must be less than No.Years in the whole data set") }

  if (No.Traits.testing > No.Traits.Accepted) {
    stop( "No.Traits.testing must be larger than No.Traits.Accepted in the whole data set")}

  pos.Years.testing <- which(DataSet[, colYears] %in% Years.testing)

  pos.Traits.testing <- which(colnames(DataSet) %in% Traits.testing)

  Data.trn <- DataSet
  Data.trn[pos.Years.testing, pos.Traits.testing] <- NA

  rows.Na <- which(apply(Data.trn[, -c(colYears, colID)],1,function(x)any(is.na(x))) == TRUE)

  Means_trn <- apply(Data.trn[, -c(colYears, colID)], 2, mean, na.rm = TRUE)
  SDs_trn <- apply(Data.trn[, -c(colYears, colID)], 2, sd, na.rm = TRUE)

  Scaled_Col <- scale(Data.trn[, -c(colYears, colID)])
  Means_trn_Row <- apply(Scaled_Col, 1, mean, na.rm = TRUE)
  SDs_trn_Row <- apply(Scaled_Col, 1, sd, na.rm = TRUE)

  if (any(is.na(SDs_trn_Row))) {
    Data.trn_scaled <- data.frame(ID = as.character(Data.trn[, colID]), Scaled_Col)
  } else {
    Scaled_Row <- t(scale(t(Scaled_Col)))
    Data.trn_scaled <- data.frame(ID = as.character(Data.trn[, colID]), Scaled_Row)
  }

  ratings <- within(Data.trn_scaled, rm('ID'))

  x <- ratings
  x[is.na(x)] <- 0
  item_sim <- lsa::cosine(as.matrix((x)))

  Hyb.pred <- within(Data.trn_scaled, rm('ID'))

  for (i in seq_len(length(rows.Na))) {
    pos <- rows.Na[i]
    Hyb.pred[pos,] <- c(rec_itm_for_geno(pos, item_sim, ratings))
  }

  All.Pred <- data.matrix(Hyb.pred)

  if (any(is.na(SDs_trn_Row))) {
    ## cambiar por all.Pred solo si ocurre error
    All.Pred_O <- sapply(seq_len(ncol(All.Pred)), function(i) (All.Pred[,i]*SDs_trn[i] + Means_trn[i]))
  } else {
    ## Si ocurre error, este se evita.
    All.Pred_O_Row <- t(sapply(seq_len(nrow(All.Pred)), function(i) (All.Pred[i,]*SDs_trn_Row[i] + Means_trn_Row[i])) )
    All.Pred_O <- sapply(seq_len(ncol(All.Pred_O_Row)), function(i) (All.Pred_O_Row[,i]*SDs_trn[i] + Means_trn[i]))
  }

  All.Pred_O <- data.frame(DataSet[, c(colYears, colID)], All.Pred_O)
    colnames(All.Pred_O) <- c(colnames(DataSet[, c(colYears, colID)]), colnames(Data.trn_scaled[, -1]))

  Data.Obs <- getTidyForm(DataSet[, c(colYears, colID, pos.Traits.testing)], onlyTrait = TRUE)
  Data.Pred <- getTidyForm(All.Pred_O[, c(colYears, colID, pos.Traits.testing)], onlyTrait = TRUE)
  Data.Obs_Pred <- data.frame(DataSet[pos.Years.testing, c(colYears, colID, pos.Traits.testing)],
                              All.Pred_O[pos.Years.testing, pos.Traits.testing])
  colnames(Data.Obs_Pred) <- c(colnames(DataSet)[ c(colYears, colID, pos.Traits.testing)], paste0(colnames(All.Pred_O)[pos.Traits.testing],'.1'))

  results <- data.frame(Position = pos.Years.testing,
                        Environment = Data.Obs[pos.Years.testing, 1],
                        Trait = Data.Obs$Trait[pos.Years.testing],
                        Observed = round(Data.Obs$Response[pos.Years.testing],dec),
                        Predicted = round(Data.Pred$Response[pos.Years.testing], dec))

  out <- list(Years.testing = Years.testing,
              Traits.testing = Traits.testing,
              predictions_Summary = results,
              observed = Data.Obs$Response,
              predicted = Data.Pred$Response,
              Data.Obs_Pred = Data.Obs_Pred,
              executionTime = proc.time()[3] - time.init
  )
  class(out) <- 'IBCFY'
  return(out)
}
