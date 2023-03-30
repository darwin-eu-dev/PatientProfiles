#' It checks whether windows are valid
#' @param window the window input eg (-365, -1)
#' @return valid window input for functions, or throw error/warnings
#' @examples
#' \donttest{
#' x<- list(1,2)
#' checkWindow(x)
#' }
#' }
#' @noRd
checkWindow <- function(window) {
  # if input in a single value, use it as both window start and end
  if (length(window) == 1 && lengths(window) == 1 |length(unique(unlist(window))) == 1) {
    window <- list(c(unique(unlist(window)), unique(unlist(window))))
    warning("Only 1 window value provided, use as both window start and window end")
  }
  
  #
  if (length(window) > 1 && any(lengths(window) == 1)) {
    window[lengths(window)==1]<- lapply(window[lengths(window)==1],
                                        function(x) c(unlist(x[lengths(x)==1]),unlist(x[lengths(x)==1])))
    warning("Window list contains element with only 1 value provided, 
            use it as both window start and window end")
  }
  
  
  if (is.vector(window) & !is.list(window)) {
    window <- list(window)
  }

  # Find if any NA, throw warning that it will be changed to Inf, change it later
  if (any(sapply(window, is.na))) {
    warning("NA found in window, changed to Inf")
  }


  # change inf to NA to check for floats, as Inf won't pass integerish check
  window <- lapply(window, function(x) replace(x, is.infinite(x), NA))

  checkmate::assertList(window, types = "integerish")

  # if any element of window list has length over 2, throw error
  if (any(lengths(window) > 2)) {
    stop("window can only contain two values: windowStart and windowEnd")
  }

  # change NA back to Inf
  window <- lapply(window, function(x) replace(x, is.na(x) & which(is.na(x)) == 2, Inf))
  window <- lapply(window, function(x) replace(x, is.na(x) & which(is.na(x)) == 1, -Inf))
  
  checkValues <- function(x) {
    tryCatch(
      {
        if (!any(is.infinite(x[1]), is.infinite(x[2]))) {
          stopifnot(x[1] <= x[2])
        }
        x
      },
      error = function(e) NULL
    )
  }

  if (any(sapply(lapply(window, checkValues), is.null))) {
    stop("First element in window must be smaller or equal to the second one")
  }

  return(window)
}
