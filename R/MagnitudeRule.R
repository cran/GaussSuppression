#' Dominance `(n,k)` or p% rule for magnitude tables
#'
#' Supports application of multiple values for `n` and `k`. The function works
#' on magnitude tables containing negative cell values by calculating
#'  contribution based on absolute values.
#'
#' This method only supports suppressing a single numeric variable. There are
#' multiple ways of handling sampling weights in the dominance rule. the default
#' method implemented here compares unweighted sample values with the corresponding
#' weighted cell totals. if `domWeightMethod` is set to `"tauargus"`, the
#' method implemented in tauArgus is used. For more information on this
#' method, see "Statistical Disclosure Control" by Hundepool et al (2012,
#'  p. 151).
#'
#' @param data the dataset
#' @param x ModelMatrix generated by parent function
#' @param numVar vector containing numeric values in the data set
#' @param n Parameter `n` in dominance rule.
#' @param k Parameter `k` in dominance rule.
#' @param pPercent Parameter in the p% rule, when non-NULL.  
#'                 Parameters `n` and  `k` will then be ignored.
#'                 Technically, calculations are performed internally as if 
#'                 `n = 1:2`. The results of these intermediate calculations can 
#'                 be viewed by setting `allDominance = TRUE`.
#' @param protectZeros parameter determining whether cells with value 0 should
#'  be suppressed.
#' @param charVar Variable in data holding grouping information. Dominance will
#'  be calculated after aggregation within these groups.
#' @param removeCodes A vector of `charVar` codes that are to be excluded when 
#'     calculating dominance percentages. Essentially, the corresponding numeric 
#'     values from `dominanceVar` or `numVar` are set to zero before proceeding 
#'     with the dominance calculations. With empty `charVar` row indices are 
#'     assumed and conversion to integer is performed.
#' @param sWeightVar variable with sampling weights to be used in dominance rule
#' @param domWeightMethod character representing how weights should be treated
#' in the dominance rule. See Details.
#' @param allDominance Logical parameter. If `TRUE`, adds primary columns for each 
#' pair of parameters n,k in the dominance rules
#' @param outputWeightedNum logical value to determine whether weighted numerical
#' value should be included in output. Default is `TRUE` if `sWeightVar` is provided.
#' @param dominanceVar When specified, `dominanceVar` is used in place of `numVar`. 
#'          Specifying `dominanceVar` is beneficial for avoiding warnings when there 
#'          are multiple `numVar` variables. Typically, `dominanceVar` will be one 
#'          of the variables already included in `numVar`.
#' @param ... unused parameters
#'
#' @return logical vector that is `TRUE` in positions corresponding to cells
#' breaching the dominance rules.
#' @export
#' 
#' @examples
#'   set.seed(123)
#' z <- SSBtools::MakeMicro(SSBtoolsData("z2"), "ant")
#' z$value <- sample(1:1000, nrow(z), replace = TRUE)
#' 
#' GaussSuppressionFromData(z, dimVar = c("region", "fylke", "kostragr", "hovedint"), 
#' numVar = "value", candidates = CandidatesNum, primary = DominanceRule, preAggregate = FALSE,
#' singletonMethod = "sub2Sum", n = c(1, 2), k = c(65, 85), allDominance = TRUE)
#' 
#' 
#'num <- c(100,
#'          90, 10,
#'          80, 20,
#'          70, 30,
#'          50, 25, 25,
#'          40, 20, 20, 20,
#'          25, 25, 25, 25)
#' v1 <- c("v1",
#'         rep(c("v2", "v3", "v4"), each = 2),
#'         rep("v5", 3),
#'         rep(c("v6", "v7"), each = 4))
#' sw <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1)
#' d <- data.frame(v1 = v1, num = num, sw = sw)
#' 
#' # without weights
#' GaussSuppressionFromData(d, formula = ~v1 - 1, 
#'  numVar = "num",  n = c(1,2), k = c(80,70),
#'   preAggregate = FALSE, allDominance = TRUE, candidates = CandidatesNum,
#'   primary = DominanceRule)
#'
#' # with weights, standard method
#' GaussSuppressionFromData(d, formula = ~v1 - 1,
#'  numVar = "num",  n = c(1,2), k = c(80,70), sWeightVar = "sw",
#'  preAggregate = FALSE, allDominance = TRUE, candidates = CandidatesNum,
#'  primary = DominanceRule)
#'
#' # with weights, tauargus method
#' GaussSuppressionFromData(d, formula = ~v1 - 1,
#'  numVar = "num",  n = c(1,2), k = c(80,70), sWeightVar = "sw",
#'  preAggregate = FALSE, allDominance = TRUE, candidates = CandidatesNum,
#'  primary = DominanceRule, domWeightMethod = "tauargus")
#'
#' @author Daniel Lupp and Øyvind Langsrud
#'
MagnitudeRule <- function(data,
                          x,
                          numVar,
                          n = NULL,
                          k = NULL,
                          pPercent = NULL,
                          protectZeros = FALSE,
                          charVar = NULL,
                          removeCodes = character(0), 
                          sWeightVar = NULL,
                          domWeightMethod = "default",
                          allDominance = FALSE,
                          outputWeightedNum = !is.null(sWeightVar),
                          dominanceVar = NULL,
                          ...) {
  if (!is.null(pPercent)) {
    n <- 1:2
    k <- c(0, 0)
  }
  
  if (length(n) != length(k))
    stop("You must provide an equal number of inputs for n and k.")
  
  if(length(dominanceVar)){
    if(length(dominanceVar) != 1){
      stop("dominanceVar must be a single variable")
    }
    numVar <- dominanceVar
  }
  
  
  if (is.null(numVar))
    stop("You must provide a numeric variable numVar to use the dominance rule.")
  
  tauArgusDominance <- as.character(domWeightMethod) == "tauargus"
  
  if (length(charVar) & tauArgusDominance)
    stop("the tauArgus weight method does not work with charVar.")
  if (length(numVar) > 1) {
    warning("Multiple numVar were supplied, only the first is suppressed.")
    numVar <- numVar[1]
  }
  if (!is.null(sWeightVar) & tauArgusDominance) {
    if (any(data[[sWeightVar]] < 1))
      warning("Some sample weights are < 1. Consider using other weighted domininace method.")
  }
  
  abs_inputnum <- abs(data[, numVar, drop = FALSE])
  
  if (length(charVar)) {
    if (length(charVar) == 1) {
      charVar_groups <- data[[charVar]]
      sweight <- NULL
    } else {
      stop("Only single charVar implemented")
    }
  } else {
    charVar_groups <- NULL
    if (is.null(sWeightVar))
      sweight <- as.matrix(rep(1, nrow(data)))
    else
      sweight <- as.matrix(data[, sWeightVar, drop = FALSE])
  }
  
  if (length(removeCodes)) {
    if (length(charVar)) {
      abs_inputnum[charVar_groups %in% removeCodes, ] <- 0
    } else {
      abs_inputnum[as.integer(removeCodes), ] <- 0
    }
  }
  
  abs_num <- as.data.frame(as.matrix(crossprod(x, as.matrix(abs_inputnum))))
  abs_inputnum <- abs_inputnum[[numVar]]

  prim <-
    mapply(
      function (a, b)
        FindDominantCells(
          x,
          abs_inputnum,
          abs_num,
          a,
          b,
          charVar_groups = charVar_groups,
          samplingWeight = sweight,
          tauArgusDominance = tauArgusDominance,
          returnContrib = TRUE
        ),
      n,
      k
    )
  if (is.null(pPercent)) {
    primary <- sapply(seq_len(ncol(prim)), function(x) prim[, x] >= k[x]/100)
    dominant <- apply(primary, 1, function(x) Reduce(`|`, x))
  } else {
    dominant <- abs(1 - prim[, 2]) < abs(pPercent/100 * prim[, 1])
  }
  colnames(prim) <- paste0("primary.", paste(n, k, sep = ":"))
  if (!protectZeros)
    output <- list(primary = dominant)
  else
    output <- list(primary = (dominant | (abs_num == 0)))
  if (outputWeightedNum) {
    wnum <- data.frame(v1 = as.vector(crossprod(x, sweight)),
                       v2 = as.vector(crossprod(x, sweight * data[[numVar]])))
    names(wnum) <- c(sWeightVar, paste0("weighted.", numVar))
    output[["numExtra"]] <- wnum
  }
  if (allDominance) {
    if ("numExtra" %in% names(output)) 
      output[["numExtra"]] <- cbind(output[["numExtra"]], as.data.frame(prim))
    else 
      output[["numExtra"]] <- as.data.frame(prim)
  }
  if (length(names(output)) == 1)
    output <- unlist(output)
  output
}


#' @rdname MagnitudeRule
#' @note Explicit  `protectZeros` in wrappers 
#'       since default needed by \code{\link{GaussSuppressionFromData}}
#' @export
DominanceRule <- function(data, n, k, 
                          protectZeros = FALSE, ...) {
  MagnitudeRule(data = data, n = n, k = k, 
                protectZeros = protectZeros, ...) 
}


#' @rdname MagnitudeRule
#' @export
PPercentRule <- function(data, pPercent,  
                         protectZeros = FALSE, ...) {
  MagnitudeRule(data = data, pPercent = pPercent,
                protectZeros = protectZeros, ...)
}



#' Method for finding dominant cells according to (possibly multiple) n,k
#' dominance rules.
#'
#' Supports functionality for grouping contributions according to holding
#' variables, as well as calculating dominance in surveys with a given sampling
#' weight. Two methods are implemented, depending on whether the sampling
#' weights sum to total population. The parameter `tauArgusDominance`
#' determines this. If `FALSE`, unweighted contributions are compared to weighted
#' cell values. If `TRUE`, the method described in  in the
#' book "Statistical Disclosure Control" (Hundepool et al 2012, p. 151) is used.
#'
#' @param x model matrix describing relationship between input and published
#' cells
#' @param inputnum vector of numeric contributions for each of the input records
#' @param num vector of numeric values for each of the published cells
#' @param n vector of integers describing n parameters in n,k rules. Must be
#' same length as `k` parameter.
#' @param k vector of numeric values describing k parameters in n,k rules, where
#' percentages are described as numbers less than 100. Must be same length as
#' `n` parameter.
#' @param charVar_groups vector describing which input records should be grouped
#' @param samplingWeight vector of sampling weights associated to input records
#' @param tauArgusDominance logical value, default `FALSE`. determines how to
#' handle sampling weights in the dominance rule (see details).
#' @param returnContrib logical value, default `FALSE`. If `TRUE` return value is 
#' the percentage of the first n contributors
#'
#' @return logical vector describing which publish-cells need to be suppressed.
#'
FindDominantCells <- function(x,
                              inputnum,
                              num,
                              n,
                              k,
                              charVar_groups,
                              samplingWeight,
                              tauArgusDominance = FALSE,
                              returnContrib = FALSE) {
  if (is.null(samplingWeight)) {
    # without sampling weight, calculate dominance directly from numerical values
    max_cont <-
      MaxContribution(x, inputnum, n = n, groups = charVar_groups)
    max_cont[is.na(max_cont)] <- 0
    if (returnContrib) {
      out <- as.vector(rowSums(max_cont)/unlist(num))
      out[is.nan(out)] <- 0
      return(out)
    } else {
      return(as.vector(num > 0 & rowSums(max_cont) > num * k / 100))
    }
  } else {
    # with sampling weights, need to weight the numerical values
    max_cont_index <-
      MaxContribution(x,
                      inputnum,
                      n = n,
                      groups = charVar_groups,
                      index = TRUE)
    max_cont <-
      apply(max_cont_index, 2, function(t)
        ifelse(is.na(t), 0, inputnum[t]))
    
    if (!tauArgusDominance) {
      weighted_num <- crossprod(x, inputnum * samplingWeight)
      ncontributions <- rowSums(max_cont)
    }
    else {
      cont_weights <-
        apply(max_cont_index, 2, function(t)
          samplingWeight[t])
      # last_index_t describes cumulative sum of contributing weights, used to
      # calculate which of the contributions need to be considered
      if (n == 1)
        last_index_t <- cont_weights
      else
        last_index_t <-
          t(apply(cont_weights, 1, function(t)
            cumsum(t)))
      # index of last contribution to be added to upsampled contribution
      last_index <- apply(last_index_t, 1,
                          function(t) {
                            ind <- which(t >= n)[1]
                            ifelse(!length(ind), sum(!is.na(t)), ind)
                          })
      # only keep the first n contributors (counting weights) and ensure
      # rowSums(cont_weights) == n
      for (ind in seq(length(last_index))) {
        cont_weights[ind, last_index[ind]] <- ifelse(last_index[ind] == 1,
                                                     n,
                                                     n - last_index_t[ind, last_index[ind] - 1])
        cont_weights[ind, 1:ncol(cont_weights) > last_index[ind]] <-
          0
      }
      cont_weights[is.na(cont_weights)] <- 0
      # sampling weights multiplied with contributions and added up
      ncontributions <- rowSums(max_cont * cont_weights)
      weighted_num <- crossprod(x, inputnum * samplingWeight)
    }
    if (returnContrib) {
      out <- as.vector(ncontributions/weighted_num)
      out[is.nan(out)] <- 0
      return(out)
    }
    return(as.vector(weighted_num > 0 &
                       ncontributions >= weighted_num * k / 100))
  }
}
