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
#'   DataSet <- getMatrixForm(Year_IBCF, onlyTrait = TRUE)
#'   IBCF.Years(DataSet , Years.testing = c('2015', '2016'), Traits.testing = c('T5', 'T6'))
#'
#' }
#'
#' @export
IBCF.Years <- function(DataSet, colYears = 1, colID = 2, Years.testing = '', Traits.testing = '', dec = 4) {
  time.init <- proc.time()[3]
  Records <- validate.model(DataSet, colYears, colID, Years.testing, Traits.testing)

  Years.tst <- which(Records$DataSet[, Records$colYears] %in% Years.testing)
  Traits.tst <- which(colnames(Records$DataSet) %in% Traits.testing)

  Data.trn <- Records$DataSet
  Data.trn[Years.tst, Traits.tst] <- NA

  rows.Na <- which(apply(Data.trn[, -c(Records$colYears, Records$colID)],1,function(x)any(is.na(x))) == TRUE)

  Means_trn <- apply(Data.trn[, -c(Records$colYears, Records$colID)], 2, mean, na.rm = TRUE)
  SDs_trn <- apply(Data.trn[, -c(Records$colYears, Records$colID)], 2, sd, na.rm = TRUE)

  Scaled_Col <- scale(Data.trn[, -c(Records$colYears, Records$colID)])
  Means_trn_Row <- apply(Scaled_Col, 1, mean, na.rm = TRUE)
  SDs_trn_Row <- apply(Scaled_Col, 1, sd, na.rm = TRUE)

  if (any(is.na(SDs_trn_Row))) {
    Data.trn_scaled <- data.frame(ID = as.character(Data.trn[, Records$colID]), Scaled_Col)
  } else {
    Scaled_Row <- t(scale(t(Scaled_Col)))
    Data.trn_scaled <- data.frame(ID = as.character(Data.trn[, Records$colID]), Scaled_Row)
  }

  ratings <- within(Data.trn_scaled, rm('ID'))

  x <- ratings
  x[is.na(x)] <- 0
  item_sim <- lsa::cosine(as.matrix((x)))

  Hyb.pred <- ratings

  for (i in seq_len(length(rows.Na))) {
    pos <- rows.Na[i]
    Hyb.pred[pos,] <- c(rec_itm_for_geno(pos, item_sim, ratings))
  }

  All.Pred <- data.matrix(Hyb.pred)

  if (any(is.na(SDs_trn_Row))) {
    All.Pred_O <- sapply(seq_len(ncol(All.Pred)), function(i) (All.Pred[,i]*SDs_trn[i] + Means_trn[i]))
  } else {
    All.Pred_O_Row <- t(sapply(seq_len(nrow(All.Pred)), function(i) (All.Pred[i,]*SDs_trn_Row[i] + Means_trn_Row[i])) )
    All.Pred_O <- sapply(seq_len(ncol(All.Pred_O_Row)), function(i) (All.Pred_O_Row[,i]*SDs_trn[i] + Means_trn[i]))
  }

  All.Pred_O <- data.frame(Records$DataSet[, c(Records$colYears, Records$colID)], All.Pred_O)
  colnames(All.Pred_O) <- c(colnames(Records$DataSet[, c(Records$colYears, Records$colID)]), colnames(Data.trn_scaled[, -1]))

  Data.Obs <- getTidyForm(Records$DataSet[Years.tst, c(Records$colYears, Records$colID, Traits.tst)], onlyTrait = TRUE)
  Data.Pred <- getTidyForm(All.Pred_O[Years.tst, c(Records$colYears, Records$colID, Traits.tst)], onlyTrait = TRUE)
  Data.Obs_Pred <- data.frame(Records$DataSet[Years.tst, c(Records$colYears, Records$colID, Traits.tst)],
                              All.Pred_O[Years.tst, Traits.tst])
  colnames(Data.Obs_Pred) <- c(colnames(Records$DataSet)[ c(Records$colYears, Records$colID, Traits.tst)], paste0(colnames(All.Pred_O)[Traits.tst],'.predicted'))

  results <- data.frame(Position = Years.tst,
                        Environment = Data.Obs[, colYears],
                        Trait = Data.Obs$Trait,
                        Observed = round(Data.Obs$Response, dec),
                        Predicted = round(Data.Pred$Response, dec))

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

validate.model <- function(DataSet, colYears, colID, Years.testing, Traits.testing){
  DataSet[, colYears] <- as.character(DataSet[, colYears]) #No factors admited
  No.Years <- length(unique(DataSet[, colYears]))
  No.Years.testing <- length(Years.testing)
  No.Traits <- length(colnames(DataSet)) - 2
  No.Traits.testing <- length(Traits.testing)
  No.Traits.Accepted <- round(0.7*No.Traits)
  colYears <- ifelse(is.numeric(colYears), colYears, which(names(DataSet) == colYears))
  colID <- ifelse(is.numeric(colID), colID, which(names(DataSet) == colID))
  colTraits.tst <- which(colnames(DataSet) %in% Traits.testing)

  if (No.Years.testing == No.Years) {
    stop("No.Years.testing must be less than No.Years in the whole data set.") }

  if (No.Traits.testing > No.Traits.Accepted) {
    stop( "No.Traits.testing must be larger than No.Traits.Accepted in the whole data set.")}

  if (colID == colYears) {
    stop("colID shouldn't be colYears.")
  }

  if(colID %in% colTraits.tst){
    stop("colID shouldn't be in Traits.testing")
  }

  if(colYears %in% colTraits.tst){
    stop("colYears shouldn't be in Traits.testing")
  }

  return(list(DataSet = DataSet, colYears = colYears, colID = colID))
}
