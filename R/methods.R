#' @title Summary
#'
#' @description Summary of IBCF object
#'
#' @param object \code{IBCF object} IBCF object, result of use the IBCF() function
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
summary.IBCF <- function(object,...){
  if (!inherits(object, "IBCF")) stop("This function only works for objects of class 'IBCF'")
  return(object$predictions_Summary)
}


#' @title Summary
#'
#' @description Summary of IBCFY object
#'
#' @param object \code{IBCFY object} IBCFY object, result of use the IBCF.Years() function
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
summary.IBCFY <- function(object,...){
  if (!inherits(object, "IBCFY")) stop("This function only works for objects of class 'IBCFY'")
  return(object$predictions_Summary)
}


#' @title Plot IBCF graph
#'
#' @description Plot from IBCF object
#'
#' @param x \code{IBCF object} IBCF object, result of use the IBCF() function
#' @param select \code{character} By default ('Pearson'), plot the Pearson Correlations of the IBCF Object, else ('MSEP'), plot the MSEP of the IBCF Object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics arrows axis plot
#' @export
plot.IBCF <- function(x, select = 'Pearson', ...){
  ### Check that object is compatible
  if (!inherits(x, "IBCF")) stop("This function only works for objects of class 'IBCF'")

  results <- x$predictions_Summary

  results$Trait_Env <- results$Trait_Env[order(results[, select])]
  results[, select] <- results[order(results[, select]), select]

  if (select == "Pearson") {
    results$SE <- results$SE_Cor * 1.96
    ylab <- "Pearson's Correlation"
  } else if (select == "MSEP") {
    results$SE <- results$SE_MSEP * 1.96
    ylab <- select
  }

  x <- 1:length(results$Trait_Env)
  plot(x, results[, select], ylim = range(c(results[, select] - results$SE, results[, select] + results$SE)),
       type = 'p', ylab = ylab, xlab = '', xaxt = "n", las = 2, ...)
  axis(1, at = x, labels = results$Trait_Env, las = 2)
  arrows(x, results[, select] - results$SE, x, results[, select] + results$SE, code = 3, length = 0.02, angle = 90)
}


#' @title barplot.IBCFY
#'
#' @description Barplot of the results from IBCFY object
#'
#' @param height \code{IBCFY object} IBCFY object, result of use the IBCF.Years() function
#' @param select \code{character} By default ('Pearson'), plot the Pearson Correlations of the IBCF Object, else ('MSEP'), plot the MSEP of the IBCF Object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics barplot
#' @export
barplot.IBCFY <- function(height, select = 'Pearson', ...){
  ### Check that object is compatible
  if (!inherits(height, "IBCFY")) stop("This function only works for objects of class 'IBCF'")

  results <- height$predictions_Summary
  vector <- as.numeric(paste(results[, select]))
  names(vector) <- results[, 1]
  vector <- vector[order(vector)]

  if (select == 'Pearson')
    select <- 'Pearson Correlation'

  barplot(vector, ylab = select, ...)
}


