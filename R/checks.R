#' @noRd
checkX <- function(x) {
  if (!isTRUE(inherits(x, "tbl_dbi"))) {
    cli::cli_abort("x is not a valid table")
  }
  if ("person_id" %in% colnames(x) && "subject_id" %in% colnames(x)) {
    cli::cli_abort(paste0(
      "x can only contain an individual identifier, please remove 'person_id'",
      " or 'subject_id'"
    ))
  }
  if (!("person_id" %in% colnames(x)) && !("subject_id" %in% colnames(x))) {
    cli::cli_abort(paste0(
      "x must contain an individual identifier ('person_id'",
      " or 'subject_id')"
    ))
  }
  person_variable <- dplyr::if_else(
    "person_id" %in% colnames(x), "person_id", "subject_id"
  )
  return(person_variable)
}

#' @noRd
checkCdm <- function(cdm, tables = NULL) {
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cdm must be a CDMConnector CDM reference object")
  }
  if (!is.null(tables)) {
    tables <- tables[!(tables %in% names(cdm))]
    if (length(tables) > 0) {
      cli::cli_abort(paste0(
        "tables: ",
        paste0(tables, collapse = ", "),
        "are nor present in the cdm object"
      ))
    }
  }
  invisible(NULL)
}

#' @noRd
checkVariableInX <- function(indexDate, x, nullOk = FALSE, name = "indexDate") {
  checkmate::assertCharacter(
    indexDate,
    any.missing = FALSE, len = 1, null.ok = nullOk
  )
  if (!is.null(indexDate) && !(indexDate %in% colnames(x))) {
    cli::cli_abort(glue::glue("{name} ({indexDate}) should be a column in x"))
  }
  invisible(NULL)
}

#' @noRd
checkCategory <- function(category) {
  checkmate::assertList(
    category,
    types = "integerish", any.missing = FALSE, unique = TRUE,
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
      cli::cli_abort(
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
    cli::cli_abort("Lower bound should be equal or smaller than upper bound")
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
    upper <- result$upper_bound[1:(nrow(result) - 1)]
    if (!all(lower > upper)) {
      cli::cli_abort("There can not be overlap between categories")
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

#' @noRd
checkWindow <- function(window) {
  if (!is.list(window)) {
    window <- list(window)
  }

  # Find if any NA, throw warning that it will be changed to Inf, change it later
  if (any(unlist(lapply(window, is.na)))) {
    cli::cli_abort("NA found in window, please change use Inf or -Inf instead")
  }

  originalWindow <- window
  # change inf to NA to check for floats, as Inf won't pass integerish check
  window <- lapply(window, function(x) replace(x, is.infinite(x), NA))
  checkmate::assertList(window, types = "integerish")
  window <- originalWindow

  # if any element of window list has length over 2, throw error
  if (any(lengths(window) > 2)) {
    cli::cli_abort("window can only contain two values: windowStart and windowEnd")
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

  windowTbl <- dplyr::tibble(
    lower = lapply(window, function(x) {
      x[1]
    }) %>% unlist(),
    upper = lapply(window, function(x) {
      x[2]
    }) %>% unlist(),
    window_name = getWindowNames(window) %>% unlist()
  )

  if (any(windowTbl$lower > windowTbl$upper)) {
    cli::cli_abort("First element in window must be smaller or equal to the second one")
  }
  if (any(is.infinite(windowTbl$lower) & windowTbl$lower == windowTbl$upper & sign(windowTbl$upper) == 1)) {
    cli::cli_abort("Not both elements in the window can be +Inf")
  }
  if (any(is.infinite(windowTbl$lower) & windowTbl$lower == windowTbl$upper & sign(windowTbl$upper) == -1)) {
    cli::cli_abort("Not both elements in the window can be -Inf")
  }

  return(windowTbl)
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
      name[k] <- newName
    }
  }
  return(name)
}

#' @noRd
getWindowNames <- function(window) {
  getname <- function(element) {
    element <- tolower(as.character(element))
    element <- gsub("-", "m", element)
    return(paste0(element[1], "_to_", element[2]))
  }
  windowNames <- names(window)
  if (is.null(windowNames)) {
    windowNames <- lapply(window, getname)
  } else {
    windowNames[windowNames == ""] <- lapply(window[windowNames == ""], getname)
  }
  return(windowNames)
}

#' @noRd
checkFilter <- function(filterVariable, filterId, idName, x) {
  if (is.null(filterVariable)) {
    checkmate::testNull(filterId)
    checkmate::testNull(idName)
    filterTbl <- NULL
  } else {
    checkVariableInX(filterVariable, x, FALSE, "filterVariable")
    checkmate::assertNumeric(filterId, any.missing = FALSE)
    checkmate::assertNumeric(utils::head(x, 1) %>% dplyr::pull(dplyr::all_of(filterVariable)))
    if (is.null(idName)) {
      idName <- paste0("id", filterId)
    } else {
      checkmate::assertCharacter(
        idName,
        any.missing = FALSE, len = length(filterId)
      )
    }
    filterTbl <- dplyr::tibble(
      id = filterId,
      id_name = idName
    )
  }
  return(filterTbl)
}

#' @noRd
checkNameStyle <- function(nameStyle, filterTbl, windowTbl, value) {
  checkmate::assertCharacter(nameStyle, len = 1, any.missing = FALSE, min.chars = 1)
  filterChange <- !is.null(filterTbl) && nrow(filterTbl) > 1
  windowChange <- !is.null(windowTbl) && nrow(windowTbl) > 1
  valueChange <- length(value) > 1
  changed <- c(
    c("{id_name}")[filterChange],
    c("{window_name}")[windowChange],
    c("{value}")[valueChange]
  )
  containWindow <- grepl("\\{window_name\\}", nameStyle)
  containFilter <- grepl("\\{id_name\\}", nameStyle)
  containValue <- grepl("\\{value\\}", nameStyle)
  contained <- c(
    c("{id_name}")[containFilter],
    c("{window_name}")[containWindow],
    c("{value}")[containValue]
  )
  if (!all(changed %in% contained)) {
    variablesNotContained <- changed[!(changed %in% contained)]
    cli::cli_abort(paste0(
      "Variables: ",
      paste0(variablesNotContained, collapse = ", "),
      "have multiple possibilities and should be cotained in nameStyle"
    ))
  }
}

#' @noRd
checkValue <- function(value, x, name) {
  checkmate::assertCharacter(value, any.missing = FALSE, min.len = 1)
  checkmate::assertTRUE(
    all(value %in% c("flag", "count", "date", "time", colnames(x)))
  )
  valueOptions <- c("flag", "count", "date", "time")
  valueOptions <- valueOptions[valueOptions %in% colnames(x)]
  if (length(valueOptions) > 0) {
    cli::cli_warn(paste0(
      "Variables: ",
      paste0(valueOptions, collapse = ", "),
      " are also present in ",
      name,
      ". But have theyr own functionality inside the package. If you want to
      obtain that column please rename and run again."
    ))
  }
  return(value[!(value %in% c("flag", "count", "date", "time"))])
}

#' @noRd
checkCohortNames <- function(x, targetCohortId, name) {
  if (!("GeneratedCohortSet" %in% class(x))) {
    cli::cli_abort(
      "cdm[[targetCohortTable]]) must be a 'GeneratedCohortSet'. Please use a
      generateCohortSet function or create it with
      CDMConnector::newGeneratedCohortSet()."
    )
  }
  cohort <- CDMConnector::cohortSet(x)
  filterVariable <- "cohort_definition_id"
  if (is.null(targetCohortId)) {
    if (is.null(cohort)) {
      idName <- NULL
      filterVariable <- NULL
      targetCohortId <- NULL
    } else {
      cohort <- dplyr::collect(cohort)
      idName <- cohort$cohort_name
      targetCohortId <- cohort$cohort_definition_id
    }
  } else {
    if (is.null(cohort)) {
      idName <- paste0(name, "_", targetCohortId)
    } else {
      idName <- cohort %>%
        dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId) %>%
        dplyr::arrange(.data$cohort_definition_id) %>%
        dplyr::pull("cohort_name")
    }
  }
  parameters <- list(
    "filter_variable" = filterVariable,
    "filter_id" = targetCohortId,
    "id_name" = idName
  )
  return(parameters)
}

#' @noRd
checkSnakeCase <- function(name) {
 for(n in name) {
   n <- gsub("[a-z]","",n)
   n <- gsub("[0-9]","",n)
   n <- gsub("_","",n)
   if(nchar(n) > 0) {
     cli::cli_abort(paste0(deparse(substitute(name)), " is not written in snake case, please check characters: ",gsub(""," ",n)))
   }
 }
}
