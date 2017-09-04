#' Subsets Data frame Based On Variable Types.
#' @param    df - Input Data frame We Wish To Subset.
#' @return   Returns List of Data frames.
#' @examples
#' numv<-c(1,2,3)
#' chrv<-c("a","b","c")
#' df<-data.frame(numv,chrv)
#' subbytype(df)
#' @description Returns a list of 6 data frames.
#'              List's first element contains subset of all Factor variables of the input data frame.
#'              Second element contains subset of all numeric and Integer variables of the input data frame.
#'              Third element contains subset of all logical variables of the input data frame.
#'              Fourth element contains subset of all complex variables of the input data frame.
#'              Fifth element contains subset of all character variables of the input data frame.
#'              Sixth element contains subset of all raw variables of the input data frame. 
#' @author "Sandip Kumar Gupta", "sandip_nitj@yahoo.co.in"
#' @export
subbytype = function(df) {
  if(length(df) > 0) {
    cname = names(df)
    facvar <- vector()
    numvar <- vector()
    logvar <- vector()
    comvar <- vector()
    chrvar <- vector()
    rawvar <- vector()
    for (icol in 1:length(cname)) {
      if (is.factor(df[, icol])) {
        facvar[length(facvar)+1] <- c(facvar, icol)
      } else if (is.numeric(df[, icol]) || is.integer(df[, icol])) {
        numvar[length(numvar)+1] <- icol
      } else if (is.logical(df[, icol])) {
        logvar <- c(logvar, icol)
      } else if (is.complex(df[, icol])) {
        comvar <- c(comvar, icol)
      } else if (is.character(df[, icol])) {
        chrvar[length(chrvar)+1] <- icol
      } else if (is.raw(df[, icol])) {
        rawvar <- c(rawvar, icol)
      }
    }
    dflist <-
      list(df[facvar], df[numvar], df[logvar], df[comvar], df[chrvar], df[rawvar])
    names(dflist) <-
      c("Factor",
        "Numerical",
        "Logical",
        "Complex",
        "Charecter",
        "Raw")
  } else {
    dflist<-list()
  }
  return(dflist)
}
