#' @title Summary.IBCF
#'
#' @description Solo es una prueba
#'
#' @param object \code{IBCF object} Objeto IBCF, resultado de ejecutar IBCF
#'
#' @export
summary.IBCF=function(object,...){

    if(!inherits(object, "IBCF")) stop("This function only works for objects of class 'IBCF'")

    tmp<-paste('--------------------> Summary of data & model <--------------------')
    cat(tmp,'\n\n')

    tmp<-paste(' Number of phenotypes=', sum(!is.na(object$response)))
    cat(tmp,'\n')

    cat(' Min (TRN)= ', min(object$response,na.rm=TRUE),'\n')
    cat(' Max (TRN)= ', max(object$response,na.rm=TRUE),'\n')
    cat(' Variance of phenotypes (TRN)=', round(var(object$response,na.rm=TRUE),4),'\n')
    cat(' Residual variance=',round(object$varE,4),'\n')

    n<-length(object$response)

    if(any(is.na(object$response)))
    {
     		tst<-which(is.na(object$response))

     		cat(' N-TRN=',n-length(tst), ' N-TST=',length(tst),'\n')

     		cat(' Correlation TRN=',round(cor(object$response[-tst],object$predicted[-tst]),4),'\n')

   }else{
       cat(' N-TRN=',n,'  N-TST=0', '\n\n')
   }

   cat('\n')
   cat(' -- Linear Predictor -- \n')
   cat('\n')
   cat(' Intercept included by default\n')

  for(k in 1:length(object$ETA)){
    if(object$ETA[[k]]$model=="FIXED"){


      if(!is.null(names(object$ETA)[k])){
        cat(" Coefficientes in ETA[",k,"] (",names(object$ETA)[k],") were asigned a flat prior\n")
      }else{
        cat(" Coefficientes in ETA[",k,"] (no-name) are asigned a flat prior\n")
      }


    }else{
      if(object$ETA[[k]]$model=="RKHS"){


        if(!is.null(names(object$ETA)[k])){
          cat(" Coefficientes in ETA[",k,"] (",names(object$ETA)[k],") were assumed to be normally distributed with zero mean and \n covariance (or its eigendecoposition) provided by user \n")
        }else{
          cat(" Coefficientes in ETA[",k,"] (no-name) were assumed to be normally distributed with zero mean and \n covariance (or its eigendecoposition) provided by user \n")
        }


      }else{


        if(!is.null(names(object$ETA)[k])){
				  cat(" Coefficientes in ETA[",k,"] (",names(object$ETA)[k],") modeled as in ", object$ETA[[k]]$model,"\n")
			  }else{
				cat(" Coefficientes in ETA[",k,"] (no-name) modeled as in ", object$ETA[[k]]$model,"\n")
			  }


		  }
	  }
  }

   cat('\n------------------------------------------------------------------\n');
}

#' @title residuals.IBCF
#'
#' @description Solo es una prueba
#'
#' @param object \code{IBCF object} Objeto IBCF, resultado de ejecutar IBCF
#'
#'
#' @export
residuals.IBCF <- function(object,...) {
    if(!inherits(object, "IBCF")) stop("This function only works for objects of class 'IBCF'")
	object$response-object$predicted
}


#' @title plot.IBCF
#'
#' @description Solo es una prueba
#'
#' @param object \code{IBCF object} Objeto IBCF, resultado de ejecutar IBCF()
#'
#' @export
plot.IBCF <- function(x, select = 'Pearson', ...){
  ### Check that object is compatible
  if (!inherits(x, "IBCF")) stop("This function only works for objects of class 'IBCF'")

  results <- x$Ave_predictions

  results$Env_Trait <- results$Env_Trait[order(results[, select])]
  results[, select] <- results[order(results[, select]), select]

  if (select == "Pearson") {
    results$SE <- results$Cor_SE * 1.96
  } else if (select == "MSEP") {
    results$SE <- results$MSEP_SE * 1.96
  }

  x <- 1:length(results$Env_Trait)
  plot(x, results[, select], ylim = range(c(results[, select] - results$SE, results[, select] + results$SE)),
       type = 'p', ylab = 'Response', ann = F, xaxt = "n", las = 2)
  axis(1, at = x, lab = results$Env_Trait, las = 2)
  arrows(x, results[, select] - results$SE, x, results[, select] + results$SE, code = 3, length = 0.02, angle = 90)
}


