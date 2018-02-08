#' @title IBCF.Years
#' @description Item Based Collaborative Filterign for Years data
#'
#' @param DataSet \code{data.frame}
#' @param Years.testing \code{vector}
#' @param Traits.testing \code{vector}
#'
#' @return A list with the next components
#' \item{Year.testing}{\code{vector} a vector with the Years used for the testing data}
#' \item{Traits.testing}{\code{vector} a vector with the Traits used for the testing data}
#' \item{Data_Obs_Pred}{\code{data.frame} Contains the values observed and predicted (the predicted values has '.1' after the name)}
#' \item{predictions_Summary}{\code{data.frame} Contains the summary of the correlation of the predictions and de mean squared error of the predictions (MSEP)}
#'
#' @examples
#'
#'
#' @export
IBCF.Years <- function(DataSet, Years.testing = '', Traits.testing = '') {
  No.Years <- length(unique(DataSet$Years))
  No.Years.testing <- length(Years.testing)
  No.Traits <- length(colnames(DataSet))-2
  No.Traits.testing <- length(Traits.testing)
  No.Traits.Accepted <- round(0.7*No.Traits)

  if (No.Years.testing==No.Years) {
    stop("No.Years.testing must be less than No.Years in the whole data set") }

  if (No.Traits.testing > No.Traits.Accepted) {
    stop( "No.Traits.testing must be larger than No.Traits.Accepted in the whole data set")}

  #####Data set to use###########################
  Data.Final <- data.frame(DataSet)

  ##########Years for testing set####################
  Years.Missing <- Years.testing

  ############Traits for testing set##################
  Traits.missing <- Traits.testing
  nIL <- ncol(Data.Final) - 2

  pos.Years.Missing <- c()
  for (q in 1:length(Years.Missing)) {
    pos.Years1 <- which(c(Data.Final[,1])==Years.Missing[q])
    pos.Years.Missing <- c(pos.Years.Missing, pos.Years1)
  }

  pos.Traits.missing <- c()
  for (q in 1:length(Traits.missing)) {
    pos.Traits1 <- which(grepl(Traits.missing[q], colnames(Data.Final))==T)
    pos.Traits.missing <- c(pos.Traits.missing, pos.Traits1)
  }

  Data.trn <- Data.Final
  Data.trn[pos.Years.Missing,pos.Traits.missing] <- NA

  rows.Na <- which(apply(Data.trn,1,function(x)any(is.na(x)))==TRUE)

  Means_trn <- apply(Data.trn[,-c(1,2)], 2, mean, na.rm = T)
  SDs_trn <- apply(Data.trn[,-c(1,2)], 2, sd, na.rm = T)

  Mean_and_SD <- data.frame(cbind(Means_trn, SDs_trn))

  Scaled_Col <- scale(Data.trn[,-c(1,2)])
  Means_trn_Row <- apply(Scaled_Col, 1, mean, na.rm=T)
  SDs_trn_Row <- apply(Scaled_Col, 1, sd, na.rm=T)

  Scaled_Row <- t(scale(t(Scaled_Col)))

  Data.trn_scaled <- data.frame(cbind(Data.trn[,c(1,2)], Scaled_Row))

  Hybrids.New <- Data.trn_scaled
  Hybrids.New[, 2:ncol(Data.trn_scaled)] <- NA

  ratings1 <- Data.trn_scaled[,-1]
  ratings <- ratings1

  x <- ratings[,2:(ncol(ratings))]
  x[is.na(x)] <- 0
  item_sim <- lsa::cosine(as.matrix((x)))

  ##############Positions with no missing values########################
  pos.used <- c(1:nrow(ratings1))
  pos.complete <- pos.used[-rows.Na]
  pos.w.Na <- rows.Na

  Hyb.pred <- as.data.frame(Data.trn_scaled[,-1])
  pos.lim <- length(pos.w.Na)

  for (i in 1:pos.lim) {
    pos <- pos.w.Na[i]
    Hyb.pred[pos,c(2:ncol(Hyb.pred))] <- c(rec_itm_for_geno(pos,item_sim,ratings)[2:ncol(Hyb.pred)])
  }
  All.Pred <- data.matrix(Hyb.pred[,-1])

  All.Pred_O_Row <- t(sapply(1:nrow(All.Pred), function(i) (All.Pred[i,]*SDs_trn_Row[i]+Means_trn_Row[i])) )
  All.Pred_O_Row

  All.Pred_O <- sapply(1:ncol(All.Pred_O_Row), function(i) (All.Pred_O_Row[,i]*SDs_trn[i]+Means_trn[i]))

  colnames(All.Pred_O) <- colnames(Data.trn_scaled[,-c(1,2)])

  Data.Obs_Pred <- data.frame(Data.Final[pos.Years.Missing,c(1,2,pos.Traits.missing)],All.Pred_O[pos.Years.Missing,(pos.Traits.missing-2)])

  Pearson <- c()
  MSEP <- c()
  Year_Trait <- c()

  Data.Final_P <- Data.Final[,-c(1,2)]

  for (q in 1:length(Years.Missing)) {
    pos.Years_q <- which(c(Data.Final[,1])==Years.Missing[q])

    Data.Final_tst <- Data.Final_P[pos.Years_q,(pos.Traits.missing-2)]
    All.Pred_O_tst <- All.Pred_O[pos.Years_q,(pos.Traits.missing-2)]

    Cor_all_tst <- cor(Data.Final_tst,All.Pred_O_tst)
    Cor_all_tst

    Dif_Obs_pred <- Data.Final_tst-All.Pred_O_tst
    Dif_Obs_pred2 <- Dif_Obs_pred^2
    MSEP <- apply(Dif_Obs_pred2, 2, mean)
    Cor_vec <- diag(Cor_all_tst)
    MSEP_vec <- MSEP
    Pearson <- c(Pearson,Cor_vec)
    MSEP <- c(MSEP,MSEP_vec)
    Names_Trait_env <- c(paste(Years.Missing[q], colnames(Data.Final_tst), sep = "_"))
    Year_Trait <- c(Year_Trait, Names_Trait_env)
  }
  names(Pearson) <- Year_Trait
  names(MSEP) <- Year_Trait
  Summary_predictions <- data.frame(cbind(Year_Trait, Pearson, MSEP))

  out <- list(Years.testing=Years.testing,
              Traits.testing=Traits.testing,
              Data_Obs_Pred=Data.Obs_Pred,
              predictions_Summary=Summary_predictions)
  class(out) <- 'IBCFY'
  return(out)
  }






