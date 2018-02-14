#' @title IBCF.Years
#' @description Item Based Collaborative Filterign for Years data
#'
#' @param DataSet \code{data.frame} A data set in Matrix Form.
#' @param colYears \code{string or integer} A name or the position of the 'Years' column just in case that is't the first column.
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
IBCF.Years <- function(DataSet, colYears = 1, Years.testing = '', Traits.testing = '', dec = 4) {
  No.Years <- length(unique(DataSet[, colYears]))
  No.Years.testing <- length(Years.testing)
  No.Traits <- length(colnames(DataSet)) - 2
  No.Traits.testing <- length(Traits.testing)
  No.Traits.Accepted <- round(0.7*No.Traits)

  if (No.Years.testing == No.Years) {
    stop("No.Years.testing must be less than No.Years in the whole data set") }

  if (No.Traits.testing > No.Traits.Accepted) {
    stop( "No.Traits.testing must be larger than No.Traits.Accepted in the whole data set")}

  pos.Years.testing <- which(DataSet[, colYears] %in% Years.testing)

  pos.Traits.testing <- which(colnames(DataSet) %in% Traits.testing)

  Data.trn <- DataSet
  Data.trn[pos.Years.testing, pos.Traits.testing] <- NA

  rows.Na <- which(apply(Data.trn,1,function(x)any(is.na(x))) == TRUE)

  Means_trn <- apply(Data.trn[,-c(1,2)], 2, mean, na.rm = T)
  SDs_trn <- apply(Data.trn[,-c(1,2)], 2, sd, na.rm = T)

  # Mean_and_SD <- data.frame(cbind(Means_trn, SDs_trn))

  Scaled_Col <- scale(Data.trn[,-c(1,2)])
  Means_trn_Row <- apply(Scaled_Col, 1, mean, na.rm = T)
  SDs_trn_Row <- apply(Scaled_Col, 1, sd, na.rm = T)

  if (any(is.na(SDs_trn_Row))) {
    Data.trn_scaled <- data.frame(ID = as.character(Data.trn[, c(1)]), Scaled_Col)
  } else {
    Scaled_Row <- t(scale(t(Scaled_Col)))
    Data.trn_scaled <- data.frame(ID = as.character(Data.trn[, c(1)]), Scaled_Row)
  }

  ratings <- Data.trn_scaled[,-1]

  x <- ratings
  x[is.na(x)] <- 0
  item_sim <- lsa::cosine(as.matrix((x)))

  Hyb.pred <- as.data.frame(Data.trn_scaled[,-1])

  for (i in 1:length(rows.Na)) {
    pos <- rows.Na[i]
    Hyb.pred[pos,] <- c(rec_itm_for_geno(pos,item_sim,ratings))
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

  colnames(All.Pred_O) <- colnames(Data.trn_scaled[,-1])

  Data.Obs_Pred <- data.frame(DataSet[pos.Years.testing, c(1, 2, pos.Traits.testing)], All.Pred_O[pos.Years.testing, (pos.Traits.testing - 2)])

  Pearson <- c()
  MSEP <- c()
  Year_Trait <- c()

  DataSet_P <- DataSet[,-c(1,2)]

  for (q in 1:length(Years.testing)) {
    pos.Years_q <- which(c(DataSet[, colYears]) == Years.testing[q])

    DataSet_tst <- DataSet_P[pos.Years_q, (pos.Traits.testing - 2)]
    All.Pred_O_tst <- All.Pred_O[pos.Years_q, (pos.Traits.testing - 2)]

    Cor_all_tst <- cor(DataSet_tst,All.Pred_O_tst)

    Dif_Obs_pred <- DataSet_tst - All.Pred_O_tst
    Dif_Obs_pred2 <- Dif_Obs_pred^2

    MSEP_vec = tryCatch({
      apply(Dif_Obs_pred2, 2, mean)
    }, error = function(e) {
      mean(Dif_Obs_pred2)
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
              Data_Obs_Pred = Data.Obs_Pred,
              predictions_Summary = Summary_predictions)
  class(out) <- 'IBCFY'
  return(out)
}
