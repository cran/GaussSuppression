


#' Cell suppression with synthetic decimal numbers
#' 
#' \code{\link{GaussSuppressionFromData}} is run and decimal numbers are added to output by
#' a modified (for sparse matrix efficiency) version of \code{\link[RegSDC]{SuppressDec}}. 
#'
#' @param data Input daata as a data frame 
#' @param ... Further parameters to \code{\link{GaussSuppressionFromData}}
#' @param output NULL (default), `"publish"`, `"inner"`, `"publish_inner"`, or `"publish_inner_x"` (x also).
#' @param digits Parameter to \code{\link[SSBtools]{RoundWhole}}. Values close to whole numbers will be rounded.
#' @param nRep NULL or an integer. When >1, several decimal numbers will be generated.
#' @param rmse Desired root mean square error of decimal numbers. 
#'            Variability around the expected, according to the linear model, inner frequencies.
#'            The expected frequencies are calculated from the non-suppressed publishable frequencies.     
#' @param sparseLimit Limit for the number of rows of a reduced x-matrix within the algorithm. When exceeded, a new sparse algorithm is used.
#' @param rndSeed If non-NULL, a random generator seed to be used locally within the function without affecting the random value stream in R. 
#' @param runIpf When TRUE, additional frequencies are generated by iterative proportional fitting using \code{\link[SSBtools]{Mipf}}.
#' @param eps  Parameter to \code{\link[SSBtools]{Mipf}}.
#' @param iter Parameter to \code{\link[SSBtools]{Mipf}}.
#' @param mismatchWarning  Whether to produce the warning "`Mismatch between whole numbers and suppression`", when relevant.
#'                       When `nRep>1`, all replicates must satisfy the whole number requirement for non-suppressed cells.  
#'                       When `mismatchWarning` is integer (`>0`), this will be used as parameter `digits` to \code{\link[SSBtools]{RoundWhole}} 
#'                       when doing mismatch checking (can be quite low when `nRep>1`). 
#' @param whenDuplicatedInner Function to be called when default output and when cells marked as inner correspond to 
#'                            several input cells (aggregated) since they correspond to published cells. 
#' @param whenMixedDuplicatedInner Function to be called in the case above when some inner cells correspond
#'                                 to published cells (aggregated) and some not (not aggregated).
#' @return A data frame where inner cells and cells to be published are combined or output according to parameter `output`. 
#' 
#' @importFrom SSBtools RoundWhole Match Mipf
#' @importFrom RegSDC SuppressDec
#' @importFrom Matrix crossprod
#' @importFrom stats runif
#' @export
#' 
#' @author Øyvind Langrsud
#'
#' @examples
#' z1 <- SSBtoolsData("z1")
#' GaussSuppressDec(z1, 1:2, 3)
#' GaussSuppressDec(z1, freqVar = "ant", formula = ~ region + hovedint, maxN = 10)
GaussSuppressDec = function(data, 
                            ..., 
                            output = NULL, 
                            digits = 9, 
                            nRep = NULL,
                            rmse = pi/3,
                            sparseLimit = 500,
                            rndSeed = 123,
                            runIpf = FALSE,
                            eps = 0.01,
                            iter = 100,
                            mismatchWarning = TRUE,
                            whenDuplicatedInner = NULL,
                            whenMixedDuplicatedInner = warning){

  if (!is.null(rndSeed)) {
    if (!exists(".Random.seed")) 
      if (runif(1) < 0) 
        stop("Now seed exists")
    exitSeed <- .Random.seed
    on.exit(.Random.seed <<- exitSeed)
    set.seed(rndSeed)
  }
  
  if(!is.null(output)){
    if(!(output %in% c("publish_inner_x", "publish_inner", "inner", "publish")))
      stop('Allowed non-NULL values of parameter output are "publish_inner_x", "publish_inner", "inner" and "publish".')
    
  } else {
    output <- ""
  }
  
  if (is.null(nRep)) {
    freqDecNames <- "freqDec"
    nRep <- 1
  } else {
    freqDecNames <- paste0("freqDec", paste(seq_len(nRep)))[seq_len(nRep)]
  }
  
  a <- GaussSuppressionFromData(data, ..., output = "publish_inner_x")
  
  startRow <- attr(a$publish, "startRow", exact = TRUE)

  freqVar <- attr(a$inner, "freqVar")
  weightVar <- attr(a$inner, "weightVar")
  numVar <- attr(a$inner, "numVar")
  
  dimVarPub <- colnames(a$publish)
  # dimVarPub <- dimVarPub[!(dimVarPub %in% c("freq", "primary", "suppressed", "weight", numVar))]
  dimVarPub <- dimVarPub[!(dimVarPub %in% c(freqVar, "primary", "suppressed", weightVar, numVar))]
  dimVarPub <- dimVarPub[(dimVarPub %in% colnames(a$inner))]
  
  z <- as.matrix(a$publish[freqVar])
  y <- as.matrix(a$inner[freqVar])
  
  if (nRep) {
    yDec <- SuppressDec(a$x, z = z, y = y, suppressed = a$publish$suppressed, digits = digits, nRep = nRep, rmse = rmse, sparseLimit = sparseLimit)
    zDec <- RoundWhole(as.matrix(Matrix::crossprod(a$x, yDec)), digits = digits)
  } else {
    yDec <- matrix(0, nrow(y),0)
    zDec <- matrix(0, nrow(z),0)
  }
  
  if(runIpf){
    freqDecNames <- c(freqDecNames, "freqIpf")
    yIpf <- Mipf(x= a$x[,!a$publish$suppressed, drop=FALSE], z = z[!a$publish$suppressed, 1, drop=FALSE], iter = iter, eps = eps)
    cat("\n")
    yDec <- cbind(yDec,  as.matrix(yIpf))
    zDec <- cbind(zDec,  as.matrix(Matrix::crossprod(a$x, yIpf)))
  }
  
  
  colnames(yDec) <- freqDecNames
  colnames(zDec) <- freqDecNames
  
  
  a$publish <- cbind(a$publish, zDec)
  rownames(a$publish) <- NULL
  a$inner <- cbind(a$inner, yDec)
  
  if (nRep & mismatchWarning) {
    
    if (is.numeric(mismatchWarning)) {
      digitsPrimary <- mismatchWarning
    } else {
      digitsPrimary <- digits
    }
    
    # Re-use primary-function originally made for SuppressionFromDecimals
    suppressionFromDecimals <- PrimaryDecimals(freq = a$publish[[freqVar]], num = a$publish[freqDecNames[1:nRep]], nDec = nRep, digitsPrimary = digitsPrimary)
    
    if (any(a$publish$suppressed != suppressionFromDecimals))
      warning("Mismatch between whole numbers and suppression.")
  }
  
  if (!is.null(startRow)) {
    attr(a$publish, "startRow") <- startRow
  }
  
  if (output == "publish_inner_x") 
    return(a)
  
  if (output == "publish_inner") 
    return(a[c("publish", "inner")])
  
  if (output == "publish") 
    return(a$publish)
  
  if (output == "inner") 
    return(a$inner)
  
  ma <- Match(a$inner[dimVarPub], a$publish[dimVarPub])
  
  anyDuplicated_ma <- anyDuplicated(ma[!is.na(ma)])
  
  a$publish$isPublish <- TRUE
  a$publish$isInner <- FALSE
  a$publish$isInner[ma[!is.na(ma)]] <- TRUE
  
  if (!anyNA(ma)) {
    if(anyDuplicated_ma & !is.null(whenDuplicatedInner)){
      whenDuplicatedInner("Duplicated inner rows aggregated.")
    }
    return(a$publish)
  }
  
  if(anyDuplicated_ma & !is.null(whenMixedDuplicatedInner)){
    whenMixedDuplicatedInner("Duplicated inner rows, some aggregated.")
  }
  
  a$inner <- a$inner[is.na(ma), unique(c(dimVarPub, numVar, freqDecNames, freqVar, weightVar)), drop = FALSE]
  
  # rename in a way that takes into account possible overlap between freqVar, weightVar, numVar
  #  renameIndex <- ncol(a$inner)
  #  if (length(weightVar)) {
  #    names(a$inner)[renameIndex] <- "weight"
  #    renameIndex <- renameIndex - 1L
  #  }
  #  if (length(freqVar)) {   # but never 0 in current application
  #    names(a$inner)[renameIndex] <- "freq"
  #  }

  a$inner$isPublish <- FALSE
  a$inner$isInner <- TRUE
  
  a$inner$primary <- NA
  a$inner$suppressed <- NA
  
  rbind(a$publish, a$inner)
  
}
