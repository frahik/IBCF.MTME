#' @title Summary
#'
#' @description Summary of IBCF object
#'
#' @param object \code{IBCF object} IBCF object, result of use the IBCF() function
#' @param information \code{string} ...
#' @param digits \code{numeric} ...
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom stats cor
#' @importFrom dplyr summarise group_by select '%>%' mutate_if funs n ungroup
#'
#' @export
summary.IBCF <- function(object, information = 'compact', digits = 4, ...){
  object$predictions_Summary %>%
    group_by(Environment, Trait, Partition) %>%
    summarise(Pearson = cor(Predicted, Observed, use = 'pairwise.complete.obs'),
              MAAPE = mean(atan(abs(Observed-Predicted)/abs(Observed)))) %>%
    select(Environment, Trait, Partition, Pearson, MAAPE) %>%
    ungroup() %>%
    mutate_if(is.numeric, funs(round(., digits))) %>%
    as.data.frame() -> presum

  presum %>%  group_by(Environment, Trait) %>%
    summarise(SE_MAAPE = sd(MAAPE, na.rm = TRUE)/sqrt(n()), MAAPE = mean(MAAPE, na.rm = TRUE),
              SE_Pearson = sd(Pearson, na.rm = TRUE)/sqrt(n()), Pearson = mean(Pearson, na.rm = TRUE))  %>%
    select(Environment, Trait, Pearson, SE_Pearson, MAAPE, SE_MAAPE) %>%
    ungroup() %>%
    mutate_if(is.numeric, funs(round(., digits))) %>%
    as.data.frame() -> finalSum

  out <- switch(information,
                compact = finalSum,
                complete = presum,
                extended = {
                  finalSum$Partition <- 'All'
                  presum$Partition <- as.character(presum$Partition)
                  presum$SE_Pearson <- NA
                  presum$SE_MAAPE <- NA
                  rbind(presum, finalSum)
                }
  )
  return(out)
}

#' @title Summary
#'
#' @description Summary of IBCFY object
#'
#' @param object \code{IBCFY object} IBCFY object, result of use the IBCF.Years() function
#' @param digits \code{numeric} Number of digits of the output.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom stats cor
#' @importFrom dplyr summarise group_by select '%>%' mutate_if funs n ungroup
#'
#' @export
summary.IBCFY <- function(object, digits = 4, ...) {
  object$predictions_Summary %>%
    group_by(Environment, Trait) %>%
    summarise(Pearson = cor(Predicted, Observed, use = 'pairwise.complete.obs'),
              MAAPE = mean(atan(abs(Observed-Predicted)/abs(Observed)))) %>%
    select(Environment, Trait, Pearson, MAAPE) %>%
    ungroup() %>%
    mutate_if(is.numeric, funs(round(., digits))) %>%
    as.data.frame() -> out

  return(out)
}

#' @title Plot IBCF graph
#'
#' @description Plot from IBCF object
#'
#' @param x \code{IBCF object} IBCF object, result of use the IBCF() function
#' @param select \code{character} By default ('Pearson'), plot the Pearson Correlations of the IBCF Object, else ('MAAPE'), plot the MAAPE of the IBCF Object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics arrows axis plot
#' @export
plot.IBCF <- function(x, select = 'Pearson', ...){
  results <- summary(x)
  results <- results[order(results[, select]), ]

  if (select == "Pearson") {
    results$SE <- results$SE_Pearson * 1.96
    ylab <- "Pearson's Correlation"
  } else if (select == "MAAPE") {
    results$SE <- results$SE_MAAPE * 1.96
    ylab <- select
  }

  results$TxE <- paste0(results$Trait, '_', results$Env)
  plot.x <- seq_len(length(results$TxE))

  plot(plot.x, results[, select], ylim = range(c(results[, select] - results$SE, results[, select] + results$SE)),
      type = 'p', ylab = ylab, xlab = '', xaxt = "n")
  axis(1, at = plot.x, labels = results$TxE, las = 2)
  arrows(plot.x, results[, select] - results$SE, plot.x, results[, select] + results$SE, code = 3, length = 0.02, angle = 90)
}


#' @title barplot.IBCFY
#'
#' @description Barplot of the results from IBCFY object
#'
#' @param height \code{IBCFY object} IBCFY object, result of use the IBCF.Years() function
#' @param select \code{character} By default ('Pearson'), plot the Pearson Correlations of the IBCF Object, else ('MAAPE'), plot the MAAPE of the IBCF Object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics barplot
#' @export
barplot.IBCFY <- function(height, select = 'Pearson', ...){
  results <- summary(height)
  vector <- as.numeric(paste(results[, select]))
  names(vector) <- paste0(results[, 2], '_', results[, 1])
  vector <- vector[order(vector)]

  if (select == 'Pearson')
    select <- 'Pearson Correlation'
  else
    select <- 'MAAPE'

  barplot(vector, ylab = select, ...)
}

#' Print IBCF information object
#'
#' @param x IBCF object
#' @param ...   Further arguments passed to or from other methods.
#'
#' @return printeable object
#' @importFrom utils head
#' @export
#'
print.IBCF <- function(x, ...){
  cat('Item Based Collaborative Filtering Model: \n',
      'Fitted with ', x$NPartitions, ' random partitions\n',
      'Runtime: ', x$executionTime ,' seconds \n\n',
      'Some predicted values: \n')

  print.default(format(head(x$predictions_Summary$Predicted, 20), digits = 3), print.gap = 2L, quote = FALSE)

  cat('\nPredictive capacity of the model: \n')

  print.data.frame(head(summary(x, 'compact', digits = 3)), print.gap = 2L, quote = FALSE)

  cat('\n Use str() function to found more datailed information.')
  invisible(x)
}

#' Print IBCFY information object
#'
#' @param x IBCFY object
#' @param ...   Further arguments passed to or from other methods.
#'
#' @return printeable object
#' @importFrom utils head
#' @export
#'
print.IBCFY <- function(x, ...){
  cat('Item Based Collaborative Filtering Model: \n',
      'Evaluated Environment/Year (s): ', x$Years.testing, '\n',
      'Evaluated Trait (s): ', x$Traits.testing, '\n',
      'Runtime: ', x$executionTime ,' seconds \n\n',
      'Some predicted values: \n')

  print.default(format(head(x$predictions_Summary$Predicted, 20), digits = 3), print.gap = 2L, quote = FALSE)

  cat('\nPredictive capacity of the model: \n')

  print.data.frame(head(summary(x, 'compact', digits = 3)), print.gap = 2L, quote = FALSE)

  cat('\n Use str() function to found more datailed information.')
  invisible(x)
}
