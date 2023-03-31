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
  if (length(elemenets) > 0) {
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
  while (length(id) > 0) {
    nameEquivalence$corrected_name[id] <-
      paste0(nameEquivalence$generated_name[id], "_", k)
    id <- which(nameEquivalence$corrected_name %in% colnamesTable)
    k <- k + 1
  }
  return(nameEquivalence)
}

#' @noRd
checkX <- function(x) {
  if (!isTRUE(inherits(x, "tbl_dbi"))) {
    stop("x is not a valid table")
  }
  if ("person_id" %in% colnames(x) && "subject_id" %in% colnames(x)) {
    stop(paste0(
      "x can only contain an individual identifier, please remove 'person_id'",
      " or 'subject_id'"
    ))
  }
  if (!("person_id" %in% colnames(x)) && !("subject_id" %in% colnames(x))) {
    stop(paste0(
      "x must contain an individual identifier ('person_id'",
      " or 'subject_id')"
    ))
  }
  return(dplyr::if_else("person_id" %in% colnames(x), "cdm_table", "cohort"))
}

#' @noRd
checkCdm <- function(cdm) {
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    stop("cdm must be a CDMConnector CDM reference object")
  }
  invisible(NULL)
}

#' @noRd
checkIndexDate <- function(indexDate, x) {
  checkmate::assertCharacter(indexDate, any.missing = FALSE, len = 1)
  if (!(indexDate %in% colnames(x))) {
    stop(glue::glue("indexDate ({indexDate}) should be a column in x"))
  }
  invisible(NULL)
}

#' @noRd
checkCategory <- function (category) {
  checkmate::assertList(
    category, types = "integerish", any.missing = FALSE, unique = TRUE,
    min.len = 1
  )

  if (is.null(names(category))) {
    names(category) <- rep("", length(category))
  }

  # check length
  category <- lapply(category, function(x) {
    if (length(x) == 1) {
      x <- c(x, x)
    } else if (length(x) > 2) {
      stop(
        paste0(
          "Categories should be formed by a lower bound and an upper bound, ",
          "no more than two elements should be provided."
        ),
        call. = FALSE
      )
    }
    return(x)
  })

  # check lower bound is smaller than upper bound
  checkLower <- unlist(lapply(category, function(x) {
    x[1] <= x[2]
  }))
  if (!(all(checkLower))) {
    stop("Lower bound should be equal or smaller than upper bound")
  }

  # built tibble
  result <- lapply(category, function(x) {
    dplyr::tibble(lower_bound = x[1], upper_bound = x[2])
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(category_label = names(.env$category)) %>%
    dplyr::mutate(category_label = dplyr::if_else(
      .data$category_label == "",
      paste0(.data$lower_bound, " to ", .data$upper_bound),
      .data$category_label
    )) %>%
    dplyr::arrange(.data$lower_bound)

  # check overlap
  if (nrow(result) > 1) {
    lower <- result$lower_bound[2:nrow(result)]
    upper <- result$upper_bound[1:(nrow(result)-1)]
    if (!all(lower > upper)) {
      stop("There can not be overlap between categories")
    }
  }
  return(result)
}

#' @noRd
checkAgeGroup <- function(ageGroup) {
  checkmate::assertList(ageGroup, min.len = 1, null.ok = TRUE)
  if (!is.null(ageGroup)) {
    if (is.numeric(ageGroup[[1]])) {
      ageGroup <- list("age_group" = ageGroup)
    }
    for (k in seq_along(ageGroup)) {
      invisible(checkCategory(ageGroup[[k]]))
    }
    if (is.null(names(ageGroup))) {
      names(ageGroup) <- paste0("age_group_", 1:length(ageGroup))
    }
    if ("" %in% names(ageGroup)) {
      id <- which(names(ageGroup) == "")
      names(ageGroup)[id] <- paste0("age_group_", id)
    }
  }
  return(ageGroup)
}

#' It checks whether windows are valid
#' @param window the window input eg (-365, -1)
#' @return valid window input for functions, or throw error/warnings
#'
#' @noRd
checkWindow <- function(window, list = TRUE) {
  # Find if any NA, throw warning that it will be changed to Inf, change it later
  if (any(unlist(lapply(window, is.na)))) {
    cli::cli_abort("NA found in window, please change no limit to Inf ot -Inf")
  }

  # if any element of window list has length over 2, throw error
  if (any(lengths(window) > 2)) {
    cli::cli_abort("window can only contain two values: windowStart and windowEnd")
  }

  if (list == TRUE) {
    # if input in a single value, use it as both window start and end
    if (length(window) == 1 && lengths(window) == 1 | length(unique(unlist(window))) == 1) {
      window <- list(c(unique(unlist(window)), unique(unlist(window))))
      cli::cli_warn("Only 1 window value provided, use as both window start and window end")
    }

    # eg if list(1,2,3), change to list(c(1,1), c(2,2), c(3,3))
    if (length(window) > 1 && any(lengths(window) == 1)) {
      window[lengths(window) == 1] <- lapply(
        window[lengths(window) == 1],
        function(x) c(unlist(x[lengths(x) == 1]), unlist(x[lengths(x) == 1]))
      )
      cli::cli_warn("Window list contains element with only 1 value provided,
            use it as both window start and window end")
    }


    if (is.vector(window) & !is.list(window)) {
      window <- list(window)
    }

    # change inf to NA to check for floats, as Inf won't pass integerish check
    window <- lapply(window, function(x) replace(x, is.infinite(x), NA))

    checkmate::assertList(window, types = "integerish")


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
      cli::cli_abort("First element in window must be smaller or equal to the second one")
    }
  } else {
    if (is.list(window)) {
      cli::cli_abort("Input window is a list, please set list = TRUE")
    }

    # change inf to NA to check for floats, as Inf won't pass integerish check
    window <- replace(window, is.infinite(window), NA)
    checkmate::assertIntegerish(window, max.len = 2, min.len = 1)
    if (length(unique(window)) == 1) {
      window <- c(unique(window), unique(window))
      cli::cli_warn("Only 1 window value provided, use as both window start and window end")
    }

    #change all NA back to Inf. The one on the left to -Inf, right to Inf
    window <- replace(window, is.na(window) & which(is.na(window)) == 2, Inf)
    window <- replace(window, is.na(window) & which(is.na(window)) == 1, -Inf)

    #check if window[1] < window[2]
    if (!isTRUE(window[1] <= window[2])) {
      cli::cli_abort("First element in window must be smaller or equal to the second one")
    }
  }

  return(window)
}

#' @noRd
checkNewName <- function(name, x) {
  for (k in seq_along(name)) {
    if (name[k] %in% colnames(x)) {
      id <- 1
      newName <- paste0(name[k], "_", id)
      while (newName %in% colnames(x)) {
        id <- id + 1
        newName <- paste0(name[k], "_", id)
      }
      warning(glue::glue(
        "{name[k]} already exists in x, it was renamed to {newName}"
      ))
    }
  }
}
