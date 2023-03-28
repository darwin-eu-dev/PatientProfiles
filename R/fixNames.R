#' @noRd
varyingParameters <- function(parameters) {
  x <- lapply(parameters, function(x) {
    length(unique(x)) == 1
  }) %>%
    unlist()
  x <- names(x[!x])
  return(x)
}

#' @noRd
computeName <- function(parameters) {
  x <- varyingParameters(parameters)
  if (length(x) > 0) {
    x <- paste0("{", paste0(x, collapse = "}_{"), "}")
  }
  return(x)
}

#' @noRd
checkName <- function(name, parameters) {
  checkmate::assertCharacter(name, len = 1, min.chars = 1, any.missing = FALSE)
  x <- varyingParameters(parameters)
  elements <- str_match_all(name, "\\{([^\\{\\}]+)\\}")
  elements <- elements[[1]][, 2]
  x <- x[!(x %in% elements)]
  if (length(x) > 0) {
    stop(paste0("variables: ", paste0(x, collapse = ", "), " not included in name."))
  }
  invisible(NULL)
}
