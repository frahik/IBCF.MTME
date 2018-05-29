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
#' \item{predictions_Summary}{\code{data.frame} Contains the summary of the correlation of the predictions and the mean squared error of the predictions (MSEP)}
#'
#' @examples
#' \dontrun{
#'   library(IBCF.MTME)
#'   data('Year_IBCF')
#'   DataSet <- getMatrixForm(Year_IBCF, withYears = T)
#'   IBCF.Years(DataSet , Years.testing = c('2015', '2016'), Traits.testing = c('T5', 'T6'))
#'
#' }
#'
#' @export
IBCF.Years <- function(DataSet, colYears = 1, colID = 2, Years.testing = '', Traits.testing = '', dec = 4) {

  if (inherits(DataSet, 'IBCFData')) {
    DataSet <- DataSet$MatrixDataset
    colYears <- 'Environment'
    colID <- 'ID'
  }

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

  Means_trn <- apply(Data.trn[, -c(colYears, colID)], 2, mean, na.rm = T)
  SDs_trn <- apply(Data.trn[, -c(colYears, colID)], 2, sd, na.rm = T)

  Scaled_Col <- scale(Data.trn[, -c(colYears, colID)])
  Means_trn_Row <- apply(Scaled_Col, 1, mean, na.rm = T)
  SDs_trn_Row <- apply(Scaled_Col, 1, sd, na.rm = T)

  if (any(is.na(SDs_trn_Row))) {
    Data.trn_scaled <- data.frame(ID = as.character(Data.trn[, colID]), Scaled_Col)
  } else {
    Scaled_Row <- t(scale(t(Scaled_Col)))
    Data.trn_scaled <- data.frame(ID = as.character(Data.trn[, colID]), Scaled_Row)
  }

  ratings <- within(Data.trn_scaled, rm(ID))

  x <- ratings
  x[is.na(x)] <- 0
  item_sim <- lsa::cosine(as.matrix((x)))

  Hyb.pred <- within(Data.trn_scaled, rm(ID))

  for (i in seq_len(length(rows.Na))) {
    pos <- rows.Na[i]
    Hyb.pred[pos,] <- c(rec_itm_for_geno(pos, item_sim, ratings))
  }

  All.Pred <- data.matrix(Hyb.pred)

  if (any(is.na(SDs_trn_Row))) {
    ## cambiar por all.Pred solo si ocurre error
    All.Pred_O <- sapply(1:ncol(All.Pred), function(i) (All.Pred[,i]*SDs_trn[i] + Means_trn[i]))
  } else {
    ## Si ocurre error, este se evita.
    All.Pred_O_Row <- t(sapply(1:nrow(All.Pred), function(i) (All.Pred[i,]*SDs_trn_Row[i] + Means_trn_Row[i])) )
    All.Pred_O <- sapply(1:ncol(All.Pred_O_Row), function(i) (All.Pred_O_Row[,i]*SDs_trn[i] + Means_trn[i]))
  }

  All.Pred_O <- data.frame(DataSet[, c(colYears, colID)], All.Pred_O)
    colnames(All.Pred_O) <- c(colnames(DataSet[, c(colYears, colID)]), colnames(Data.trn_scaled[, -1]))

  Data.Obs <- getTidyForm(DataSet[, c(colYears, colID, pos.Traits.testing)], onlyTrait = T)$Response
  Data.Pred <- getTidyForm(All.Pred_O[, c(colYears, colID, pos.Traits.testing)], onlyTrait = T)$Response
  Data.Obs_Pred <- data.frame(DataSet[pos.Years.testing, c(colYears, colID, pos.Traits.testing)],
                              All.Pred_O[pos.Years.testing, pos.Traits.testing])
  colnames(Data.Obs_Pred) <- c(colnames(DataSet)[ c(colYears, colID, pos.Traits.testing)], paste0(colnames(All.Pred_O)[pos.Traits.testing],'.1'))
  Pearson <- c()
  MSEP <- c()
  Year_Trait <- c()

  for (q in seq_len(length(Years.testing))) {
    pos.Years_q <- which(c(DataSet[, colYears]) == Years.testing[q])

    DataSet_tst <- DataSet[pos.Years_q, pos.Traits.testing]
    All.Pred_O_tst <- All.Pred_O[pos.Years_q, pos.Traits.testing]

    Cor_all_tst <- cor(DataSet_tst, All.Pred_O_tst, use = "pairwise.complete.obs")

    Dif_Obs_pred <- DataSet_tst - All.Pred_O_tst
    Dif_Obs_pred2 <- Dif_Obs_pred^2

    MSEP_vec = tryCatch({
      apply(Dif_Obs_pred2, 2, mean, na.rm = T)
    }, error = function(e) {
      mean(Dif_Obs_pred2, na.rm = T)
    })

    if (length(Cor_all_tst)[1] == 1) {
      Cor_vec = Cor_all_tst
    }else{
      Cor_vec = diag(Cor_all_tst)
    }

    Pearson <- c(Pearson, round(Cor_vec, digits = dec))
    MSEP <- c(MSEP, round(MSEP_vec, digits = dec))
    Names_Trait_env <- c(paste(Years.testing[q], colnames(DataSet_tst), sep = "_"))
    Year_Trait <- c(Year_Trait, Names_Trait_env)
  }
  names(Pearson) <- Year_Trait
  names(MSEP) <- Year_Trait
  Summary_predictions <- data.frame(Year_Trait, Pearson, MSEP)

  out <- list(Years.testing = Years.testing,
              Traits.testing = Traits.testing,
              predictions_Summary = Summary_predictions,
              observed = Data.Obs,
              predicted = Data.Pred,
              Data.Obs_Pred = Data.Obs_Pred
  )
  class(out) <- 'IBCFY'
  return(out)
}
