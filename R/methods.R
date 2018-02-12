#' @title Summary
#'
#' @description Summary of IBCF object
#'
#' @param object \code{IBCF object} Objeto IBCF, resultado de ejecutar IBCF
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
summary.IBCF <- function(object,...){
  if (!inherits(object, "IBCF")) stop("This function only works for objects of class 'IBCF'")
  print(object$Summary_predictions)
}


#' @title Summary
#'
#' @description Summary of IBCFY object
#'
#' @param object \code{IBCFY object} Objeto IBCFY, resultado de ejecutar IBCF.Years
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
summary.IBCFY <- function(object,...){
  if (!inherits(object, "IBCFY")) stop("This function only works for objects of class 'IBCFY'")
  print(object$predictions_Summary)
}


#' @title Plot IBCF graph
#'
#' @description Graphs from IBCF object
#'
#' @param x \code{IBCF object} Objeto IBCF, resultado de ejecutar IBCF()
#' @param select \code{character} By default ('Pearson'), plot the Pearson Correlations of the IBCF Object, else ('MSEP'), plot the MSEP of the IBCF Object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics arrows axis plot
#' @export
plot.IBCF <- function(x, select = 'Pearson', ...){
  ### Check that object is compatible
  if (!inherits(x, "IBCF")) stop("This function only works for objects of class 'IBCF'")

  results <- x$Summary_predictions

  results$Trait_Env <- results$Trait_Env[order(results[, select])]
  results[, select] <- results[order(results[, select]), select]

  if (select == "Pearson") {
    results$SE <- results$SE_Cor * 1.96
    ylab <- 'Pearson Correlation'
  } else if (select == "MSEP") {
    results$SE <- results$SE_MSEP * 1.96
    ylab <- select
  }

  x <- 1:length(results$Trait_Env)
  plot(x, results[, select], ylim = range(c(results[, select] - results$SE, results[, select] + results$SE)),
       type = 'p', ylab = ylab, xlab = '', xaxt = "n", las = 2)
  axis(1, at = x, labels = results$Trait_Env, las = 2)
  arrows(x, results[, select] - results$SE, x, results[, select] + results$SE, code = 3, length = 0.02, angle = 90)
}


#' @title barplot.IBCFY
#'
#' @description Solo es una prueba
#'
#' @param height \code{IBCFY object} Objeto IBCFY, resultado de ejecutar IBCF.Year()
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
  names(vector) <- names(results[, select])

  if (select == 'Pearson')
    select <- 'Pearson Correlation'

  barplot(vector, ylab = select, las = 1)
}


