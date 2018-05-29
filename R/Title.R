rec_itm_for_geno <- function(geno_no, item_sim, ratings)  {
  genoRatings <- ratings[geno_no,]

  non_rated_items <- colnames(genoRatings)[which(is.na(genoRatings))]
  rated_items <- colnames(genoRatings)[which(!is.na(genoRatings))]

  #create weighted similarity for all the rated items by geno
  non_rated_pred_score <- list()

  for (j in seq_len(length(non_rated_items))) {
    temp_sum <- 0
    df <- item_sim[which(rownames(item_sim) == non_rated_items[j]),]

    for (i in seq_len(length(rated_items))) {
      temp_sum <- temp_sum + abs(df[which(names(df) == rated_items[i])])
    }

    weight_mat <- df * ratings[geno_no,]

    non_rated_pred_score <- c(non_rated_pred_score, rowSums(weight_mat, na.rm = T)/temp_sum)
  }

  pred_rat_mat <- as.data.frame(non_rated_pred_score)
  names(pred_rat_mat) <- non_rated_items

  for (k in 1:ncol(pred_rat_mat)) {
    ratings[geno_no,][which(names(ratings[geno_no,]) == names(pred_rat_mat)[k])] = pred_rat_mat[1,k]
  }

  return(ratings[geno_no,])
}


