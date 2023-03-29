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
  elements <- elements[!(elements %in% names(parameters))]
  if (length(elemenets) >0 ) {
    stop(paste0(
      "variables: ",
      paste0(elements, collapse = ", "),
      " contained in name and not included in iput parameters."
    ))
  }
  invisible(NULL)
}

#' @noRd
repairName <- function(name, parameters, colnamesTable) {
  nameEquivalence <- expand.grid(parameters) %>%
    dplyr::as_tibble() %>%
    dplyr::distinct() %>%
    dplyr::mutate(generated_name = glue::glue(.env$name)) %>%
    dplyr::mutate(corrected_name = .data$generated_name) %>%
    dplyr::select("generated_name", "corrected_name")
  k <- 1
  id <- which(nameEquivalence$generated_name %in% colnamesTable)
  while(length(id) > 0) {
    nameEquivalence$corrected_name[id] <-
      paste0(nameEquivalence$generated_name[id], "_", k)
    id <- which(nameEquivalence$corrected_name %in% colnamesTable)
    k <- k + 1
  }
  return(nameEquivalence)
}
