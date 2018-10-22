#' IBCF.MTME: Item Based Collaborative Filtering for Multi-Trait and Multi-Environment Data.
#'
#' The Item Based Collaborative Filtering for Multi-Trait and Multi-Environment Data (IBCF.MTME) package was developed to implement the item based collaborative filtering (IBCF) method for continues phenotypes in the context of plant breeding where data are collected for various traits that were studied in various environments. It is important to point out that the main difference of this package with the available packages that can implement IBCF is that this package was developed for continuous phenotypes which cannot be implemented in the current packages that can implement IBCF that only work for binary and ordinary phenotypes.
#'
#'
#' @docType package
#' @name IBCF.MTME
NULL

if (getRversion() >= "2.15.1")
  utils::globalVariables(
    c('.',
      'Environment',
      'Trait',
      'Partition',
      'Predicted',
      'Observed',
      'Pearson',
      'MAAPE',
      'SE_Pearson',
      'SE_MAAPE',
      'n'
    )

  )