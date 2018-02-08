#' @title Summary
#'
#' @description Summary of IBCF object
#'
#' @param object \code{IBCF object} Objeto IBCF, resultado de ejecutar IBCF
#'
#' @export
summary.IBCF <- function(object,...){
  if (!inherits(object, "IBCF")) stop("This function only works for objects of class 'IBCF'")
  paste('--------------------> Summary of data & model <--------------------')
  print(object$Summary_predictions)
}

#' @title Plot IBCF graph
#'
#' @description Graphs from IBCF object
#'
#' @param object \code{IBCF object} Objeto IBCF, resultado de ejecutar IBCF()
#'
#' @export
plot.IBCF <- function(x, select = 'Pearson', ...){
  ### Check that object is compatible
  if (!inherits(x, "IBCF")) stop("This function only works for objects of class 'IBCF'")

  results <- x$Summary_predictions

  results$Trait_Env <- results$Trait_Env[order(results[, select])]
  results[, select] <- results[order(results[, select]), select]

  if (select == "Pearson") {
    results$SE <- results$Cor_SE * 1.96
  } else if (select == "MSEP") {
    results$SE <- results$MSEP_SE * 1.96
  }

  x <- 1:length(results$Trait_Env)
  plot(x, results[, select], ylim = range(c(results[, select] - results$SE, results[, select] + results$SE)),
       type = 'p', ylab = 'Response', ann = F, xaxt = "n", las = 2)
  axis(1, at = x, lab = results$Trait_Env, las = 2)
  arrows(x, results[, select] - results$SE, x, results[, select] + results$SE, code = 3, length = 0.02, angle = 90)
}


#' @title plot.IBCFY
#'
#' @description Solo es una prueba
#'
#' @param object \code{IBCFY object} Objeto IBCFY, resultado de ejecutar IBCF.Year()
#'
#' @export
plot.IBCFY <- function(x, select = 'Pearson', ...){
  ### Check that object is compatible
  if (!inherits(x, "IBCFY")) stop("This function only works for objects of class 'IBCF'")

  results <- x$predictions_Summary
  vector <- as.numeric(paste(results[, select]))
  names(vector) <- names(results[, select])

  if (select == 'Pearson')
    select <- 'Pearson Correlation'

  barplot(vector, type = 'p', ylab = select, las = 1)
}


