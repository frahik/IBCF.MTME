#' @title Cross-Validation with Random Partions
#'
#' @param DataSet \code{data.frame} Number of Lines in the DataSet. By default are 10 Lines.
#' @param NPartitions \code{integer} Number of Partitions for the Cross-Validation. Is 10 by default.
#' @param PTesting \code{Double} Porcentage of Testing for the Cross-Validation. Is 0.35 by default.
#' @param Set_seed \code{integer} Number of seed for replicable research. Is NULL by default.
#'
#' @return \code{List} A list object with lenght of \code{NPartitions}, every indice has a \code{matrix} \eqn{n \times x}, where \eqn{n} is the number of \code{NLines} and \eqn{x} is the number of  \code{NEnv} \eqn{\times} \code{NTraits}. The values inside is 1 for training and 2 for testing.
#'
#' @examples
#' \dontrun{
#'   library(IBCF.MTME)
#'   data('Wheat_IBCF')
#'
#'   CV.RandomPart(Wheat_IBCF)
#'   CV.RandomPart(Wheat_IBCF, NPartitions = 10)
#'   CV.RandomPart(Wheat_IBCF, NPartitions = 10, PTesting = .35)
#'   CV.RandomPart(Wheat_IBCF, NPartitions = 10, PTesting = .35, Set_seed = 5)
#' }
#' @export
CV.RandomPart <- function(DataSet, NPartitions = 10, PTesting = .35, Set_seed = NULL) {


  if (!is.null(Set_seed)) {
    set.seed(Set_seed)
  }

  new_Data <- getMatrixForm(DataSet, withYears = F)
  NLine <- dim(new_Data)[1]

  if (length(unique(DataSet$Env)) < 2) {
    No_Env_I <- length(unique(DataSet$Env))
    NEnv <- length(unique(DataSet$Trait))
    NTraits <- No_Env_I
    Trait <- unique(DataSet$Env)
    Env <- unique(DataSet$Trait)
  } else {
    NEnv <- length(unique(DataSet$Env))
    NTraits <- length(unique(DataSet$Trait))
    Trait <- unique(DataSet$Trait)
    Env <- unique(DataSet$Env)
  }


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
        a1 <- floor(length(indexNA)/nTST)*nTST;a2 <- nNA - a1 - length(remain)
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
    B <- kronecker(A, yNA)
    p_list[[paste0('p',i)]] <- B
  }


  out <- list(
    DataSet = new_Data,
    CrossValidation_list = p_list,
    Environments = Env,
    Traits = Trait,
    Observations = NLine
  )

  class(out) <- 'CrossValidation'
  return(out)
}


#' @title Cross-Validation with K-Folds
#'
#' @param DataSet \code{data.frame} Tidy data.frame, with $Line especified on it, also works taking account $Env and $Trait.
#' @param K \code{integer} Number of folds.
#' @param Set_seed \code{integer} Number of seed
#'
#' @return \code{List} A list object with lenght of
#'
#' @examples
#' \dontrun{
#'   library(IBCF.MTME)
#'   data('Wheat_IBCF')
#'   DataSet <- data.frame(Data.Trigo_IBCF[,-1])
#'   DataSet <- Matrix2Tidy(DataSet)
#'   CV.KFold(DataSet = DataSet, K = 5, Set_seed = 10)
#' }
#' @export
CV.KFold <- function(DataSet, K = 5, Set_seed = NULL){
  stop('Is not implemented yet')

  if (!is.null(Set_seed)) {
    set.seed(Set_seed)
  }

  if (is.null(DataSet$Line)){
    stop('Lines are not specified')
  }

  Data <- getMatrixForm(DataSet)
  NLine <- dim(Data)[1]
  if (length(unique(DataSet$Env)) < 2) {
    No_Env_I <- length(unique(DataSet$Env))
    NEnv <- length(unique(DataSet$Trait))
    NTraits <- No_Env_I
    Trait <- unique(DataSet$Env)
    Env <- unique(DataSet$Trait)
  } else {
    NEnv <- length(unique(DataSet$Env))
    NTraits <- length(unique(DataSet$Trait))
    Trait <- unique(DataSet$Trait)
    Env <- unique(DataSet$Env)
  }

  pos <- matrix(NA, nrow = dim(Data)[1], ncol = dim(Data)[2], dimnames = list(NULL, names(Data)))

  if ((is.null(DataSet$Env) || length(unique(DataSet$Env)) == 1) && (is.null(DataSet$Trait) || length(unique(DataSet$Trait)) == 1)) {
    pm <- sample(dim(DataSet)[1])
    grs <- cut(seq(1, length(pm)), breaks = K, labels = FALSE)
    g_list <- vector('list', K)
    pos <- vector('list', K)
    ng <- 0
    names(g_list) <- paste0('partition', 1:K)
    for (i in 1:K) {
      g_list[[paste0('partition', i)]] <- pm[grs == i]
      ng[i] <- length(g_list[[paste0('partition', i)]])
      pos[[paste0('partition', i)]] <- matrix(1:dim(Data)[1], nrow = dim(Data)[1], ncol = dim(Data)[2], dimnames = list(NULL, names(Data)))

      pos[[paste0('partition', i)]][g_list[[paste0('partition', i)]]== pos[[paste0('partition', i)]]] <- NA
      pos[[paste0('partition', i)]][g_list[[paste0('partition', i)]]!= pos[[paste0('partition', i)]]] <- 2
      pos[[paste0('partition', i)]][is.na(pos[[paste0('partition', i)]])] <- 1
    }
    return(g_list)
  }

  UL <- unique(DataSet$Line)
  #Number of sites where each line appear
  n_UL <- length(UL)
  nSLA <- rep(NA, n_UL)

  nEAL <- table(DataSet[, c('Line')])#Number of Sites that appear  each line
  L_nE <- data.frame(Line = names(nEAL), nE = c(nEAL))

  #A list of Positions in data set dat_F that will conform the groups
  g_list <- vector('list', K)
  names(g_list) <- paste0('partition', 1:K)

  #Lines that will appear in all groups because
  # only appear in only one Site
  Pos1 <- which(L_nE$nE == 1)
  Pos_1_dat_F <- match(L_nE$Line[Pos1], DataSet$Line)
  #dat_F[Pos_1_dat_F,]

  #Tama?o de cada partici?n sin considerar las lineas
  # que se incluir?n por defaul (las que aparecen en un solo ambiente)
  n <- dim(DataSet)[1]
  nR <- n - length(Pos1)
  ifelse(nR %% K == 0,
         ng <- rep(nR / K, K),
         ng <- rep(trunc(nR / K), K) + c(rep(1, nR - trunc(nR / K) * K), rep(0, K - (nR - trunc( nR / K ) * K))))
  #ng
  Pos_all <- 1:n
  #---------------------------------------------------------------
  #First group
  #---------------------------------------------------------------
  if (length(Pos1) == 0) {
    dat_F_k <- DataSet
  }
  else{
    dat_F_k <- DataSet[-Pos_1_dat_F,]
  }
  #Lineas ?nicas restantes
  UL_k <- unique(dat_F_k$Line)
  Pos_R_k <- rep(NA, length(UL_k))
  for (j in 1:length(UL_k)) {
    Pos_j_k <-  which(DataSet$Line == UL_k[j])
    Pos_R_k[j] <- sample(Pos_j_k, 1)
  }
  Pos_R_k <- Pos_R_k

  Pos_k_2_dat_F <- sample(Pos_all[-c(Pos_1_dat_F, Pos_R_k)], ng[1])
  g_list[[1]] <- c(Pos_1_dat_F, Pos_k_2_dat_F)

  #---------------------------------------------------------------
  #Group 2,3, .., K
  #---------------------------------------------------------------
  for (k in 2:(K - 1))  {
    #Assigned positions
    Pos_k_a_R <- unique(unlist(g_list[1:(k - 1)]))
    dat_F_k <- DataSet[-Pos_k_a_R,]
    UL_k <- unique(dat_F_k$Line)
    #A las lineas que no aparecen en el grupo k-1, se remueve un
    # site donde aparencen para garantizar que ?stas aparezcan
    # en al menos un site
    UL_k <- UL_k[(UL_k %in% DataSet[Pos_k_a_R,]$Line) == FALSE]
    if (length(UL_k) > 0) {
      #Posiciones de lineas a mantener fuera del grupo k
      Pos_R_k <- rep(NA, length(UL_k))

      for (j in 1:length(UL_k)) {
        Pos_j_k <-  which((DataSet$Line == UL_k[j]))
        if (length(Pos_j_k) > 1) {
          Pos_R_k[j] <- sample(Pos_j_k, 1)
        }
      }
      Pos_R_k <- na.omit(Pos_R_k)
      Pos_k_2_dat_F <- sample(Pos_all[-c(Pos_k_a_R, Pos_R_k)], ng[k])
      g_list[[k]] <- c(Pos_1_dat_F, Pos_k_2_dat_F)
    }
    else{
      Pos_k_2_dat_F <- sample(Pos_all[-c(Pos_k_a_R)], ng[k])
      g_list[[k]] <- c(Pos_1_dat_F, Pos_k_2_dat_F)
    }
  }

  k <- K
  Pos_k_a_R <- unique(unlist(g_list[1:(k - 1)]))
  Pos_k_2_dat_F <- sample(Pos_all[-c(Pos_k_a_R)], ng[k])
  g_list[[k]] <- c(Pos_1_dat_F, Pos_k_2_dat_F)

  n_CL <- length(Pos_1_dat_F)


  out <- list(
    DataSet = Data,
    CrossValidation_list = g_list,
    Environments = Env,
    Traits = Trait,
    Observations = NLine
  )

  class(out) <- 'CrossValidation'
  # return(out)

  return(g_list)

}
