# Copyright 2024 DARWIN EU (C)
#
# This file is part of PatientProfiles
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

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
  personVariable <- dplyr::if_else(
    "person_id" %in% colnames(x), "person_id", "subject_id"
  )
  invisible(personVariable)
}

#' @noRd
checkCdm <- function(cdm, tables = NULL) {
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cdm a cdm_reference object.")
  }
  if (!is.null(tables)) {
    tables <- tables[!(tables %in% names(cdm))]
    if (length(tables) > 0) {
      cli::cli_abort(paste0(
        "tables: ",
        paste0(tables, collapse = ", "),
        " are not present in the cdm object"
      ))
    }
  }
  invisible(NULL)
}

#' @noRd
checkVariableInX <- function(indexDate, x, nullOk = FALSE, name = "indexDate") {
  assertCharacter(indexDate, length = 1, null = nullOk)
  if (!is.null(indexDate) && !(indexDate %in% colnames(x))) {
    cli::cli_abort(glue::glue("{name} ({indexDate}) should be a column in x"))
  }
  invisible(NULL)
}

#' @noRd
checkAgeGroup <- function(ageGroup, overlap = FALSE) {
  assertList(ageGroup, null = TRUE)
  if (!is.null(ageGroup)) {
    if (is.numeric(ageGroup[[1]])) {
      ageGroup <- list("age_group" = ageGroup)
    }
    for (k in seq_along(ageGroup)) {
      invisible(checkCategory(ageGroup[[k]], overlap))
      if (any(ageGroup[[k]] |> unlist() |> unique() < 0)) {
        cli::cli_abort("ageGroup can't contain negative values")
      }
    }
    if (is.null(names(ageGroup))) {
      names(ageGroup) <- paste0("age_group_", 1:length(ageGroup))
    }
    if ("" %in% names(ageGroup)) {
      id <- which(names(ageGroup) == "")
      names(ageGroup)[id] <- paste0("age_group_", id)
    }
  }
  invisible(ageGroup)
}

#' @noRd
checkWindow <- function(window, call = parent.frame()) {
  if (!is.list(window)) {
    cli::cli_abort("window must be a list", call = call)
  }

  # Find if any NA, throw warning that it will be changed to Inf, change it later
  if (any(unlist(lapply(window, is.na)))) {
    cli::cli_abort("NA found in window, please use Inf or -Inf instead", call = call)
  }

  originalWindow <- window
  # change inf to NA to check for floats, as Inf won't pass integerish check
  window <- lapply(window, function(x) replace(x, is.infinite(x), NA))
  assertList(window, class = "numeric", call = call)
  assertNumeric(window |> unlist(), integerish = TRUE, na = TRUE, call = call)
  window <- originalWindow

  # if any element of window list has length over 2, throw error
  if (any(lengths(window) > 2)) {
    cli::cli_abort("window can only contain two values: windowStart and windowEnd", call = call)
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

  names(window) <- getWindowNames(window)
  lower <- lapply(window, function(x) {
    x[1]
  }) %>% unlist()
  upper <- lapply(window, function(x) {
    x[2]
  }) %>% unlist()

  if (any(lower > upper)) {
    cli::cli_abort("First element in window must be smaller or equal to the second one", call = call)
  }
  if (any(is.infinite(lower) & lower == upper & sign(upper) == 1)) {
    cli::cli_abort("Not both elements in the window can be +Inf", call = call)
  }
  if (any(is.infinite(lower) & lower == upper & sign(upper) == -1)) {
    cli::cli_abort("Not both elements in the window can be -Inf", call = call)
  }

  invisible(window)
}

#' @noRd
checkNewName <- function(name, x) {
  renamed <- name[name %in% colnames(x)]
  if (length(renamed) > 0) {
    mes <- paste0(
      "The following columns will be overwritten: ",
      paste0(renamed, collapse = ", ")
    )
    cli::cli_warn(message = mes)
  }
  invisible(name)
}

#' @noRd
getWindowNames <- function(window) {
  getname <- function(element) {
    element <- tolower(as.character(element))
    element <- gsub("-", "m", element)
    invisible(paste0(element[1], "_to_", element[2]))
  }
  windowNames <- names(window)
  if (is.null(windowNames)) {
    windowNames <- lapply(window, getname)
  } else {
    windowNames[windowNames == ""] <- lapply(window[windowNames == ""], getname)
  }
  invisible(windowNames)
}

#' @noRd
checkFilter <- function(filterVariable, filterId, idName, x) {
  if (is.null(filterVariable)) {
    filterId <- NULL
    idName <- NULL
    filterTbl <- NULL
  } else {
    checkVariableInX(filterVariable, x, FALSE, "filterVariable")
    omopgenerics::assertNumeric(filterId, na = FALSE)
    omopgenerics::assertNumeric(utils::head(x, 1) %>%
                               dplyr::pull(dplyr::all_of(filterVariable)))
    if (is.null(idName)) {
      idName <- paste0("id", filterId)
    } else {
      omopgenerics::assertCharacter(idName,
                                    na = FALSE,
                                    length = length(filterId))
    }
    filterTbl <- dplyr::tibble(
      id = filterId,
      id_name = idName
    )
  }
  invisible(filterTbl)
}

#' @noRd
checkValue <- function(value, x, name) {
  omopgenerics::assertCharacter(value, na = FALSE)
  omopgenerics::assertTrue(all(value %in%
                                 c("flag", "count", "date", "days",
                                   colnames(x))))
  valueOptions <- c("flag", "count", "date", "days")
  valueOptions <- valueOptions[valueOptions %in% colnames(x)]
  if (length(valueOptions) > 0) {
    cli::cli_warn(paste0(
      "Variables: ",
      paste0(valueOptions, collapse = ", "),
      " are also present in ",
      name,
      ". But have their own functionality inside the package. If you want to
      obtain that column please rename and run again."
    ))
  }
  invisible(value[!(value %in% c("flag", "count", "date", "days"))])
}

#' @noRd
checkCohortNames <- function(x, targetCohortId, name) {
  if (!("cohort_table" %in% class(x))) {
    cli::cli_abort("cdm[[targetCohortTable]]) must be a 'cohort_table'.")
  }
  cohort <- omopgenerics::settings(x)
  filterVariable <- "cohort_definition_id"
  if (is.null(targetCohortId)) {
    cohort <- dplyr::collect(cohort)
    idName <- cohort$cohort_name
    targetCohortId <- cohort$cohort_definition_id
  } else {
    idName <- cohort %>%
      dplyr::filter(
        as.integer(.data$cohort_definition_id) %in%
          as.integer(.env$targetCohortId)
      ) %>%
      dplyr::arrange(.data$cohort_definition_id) %>%
      dplyr::pull("cohort_name")
    if (length(idName) != length(targetCohortId)) {
      cli::cli_abort(
        "some of the cohort ids given do not exist in the cohortSet of
          cdm[[targetCohortName]]"
      )
    }
  }
  parameters <- list(
    "filter_variable" = filterVariable,
    "filter_id" = sort(targetCohortId),
    "id_name" = idName
  )
  invisible(parameters)
}

#' @noRd
checkSnakeCase <- function(name, verbose = TRUE, null = FALSE, call = parent.frame()) {
  assertCharacter(name, call = call, null = null)
  if (is.null(name)) {
    return(invisible(name))
  }
  wrong <- FALSE
  for (i in seq_along(name)) {
    n <- name[i]
    n <- gsub("[a-z]", "", n)
    n <- gsub("[0-9]", "", n)
    n <- gsub("_", "", n)
    if (nchar(n) > 0) {
      oldname <- name[i]
      name[i] <- gsub("([[:upper:]])", "\\L\\1", perl = TRUE, name[i])
      name[i] <- gsub("[^a-z,0-9.-]", "_", name[i])
      name[i] <- gsub("-", "_", name[i])
      if (verbose) {
        cli::cli_alert(paste0(oldname, " has been changed to ", name[i]))
      }
      wrong <- TRUE
    }
  }
  if (wrong && verbose) {
    cli::cli_alert("some provided names were not in snake_case")
    cli::cli_alert("names have been changed to lower case")
    cli::cli_alert("special symbols in names have been changed to '_'")
  }
  return(invisible(name))
}

#' @noRd
checkExclude <- function(exclude) {
  if (!is.null(exclude) & !is.character(exclude)) {
    cli::cli_abort("eclude must a character vector or NULL")
  }
}

#' @noRd
checkTable <- function(table) {
  if (!("tbl" %in% class(table))) {
    cli::cli_abort("table should be a tibble")
  }
}

#' @noRd
checkStrata <- function(list, table, type = "strata") {
  errorMessage <- paste0(type, " should be a list that point to columns in table")
  if (!is.list(list)) {
    cli::cli_abort(errorMessage)
  }
  if (length(list) > 0) {
    if (!is.character(unlist(list))) {
      cli::cli_abort(errorMessage)
    }
    if (!all(unlist(list) %in% colnames(table))) {
      notPresent <- list |>
        unlist() |>
        unique()
      notPresent <- notPresent[!notPresent %in% colnames(table)]
      cli::cli_abort(paste0(
        errorMessage,
        ". The following columns were not found in the data: ",
        paste0(notPresent, collapse = ", ")
      ))
    }
  }
  if (!is.null(names(list))) {
    cli::cli_inform(c("!" = "names of {type} will be ignored"))
  }
  names(list) <- NULL
  return(list)
}

#' @noRd
checkVariablesFunctions <- function(variables, estimates, table) {
  errorMessage <- "variables should be a unique named list that point to columns in table"
  assertList(x = variables, class = "character")
  assertList(x = estimates, class = "character")
  if (length(variables) != length(estimates)) {
    cli::cli_abort("Variables and estimates must have the same length")
  }
  if (!is.null(names(variables)) & !is.null(names(estimates))) {
    if (!identical(sort(names(variables)), sort(names(estimates)))) {
      cli::cli_abort("Names from variables and estimates must be the same")
    }
    variables <- variables[order(names(variables))]
    estimates <- estimates[order(names(estimates))]
  }

  if (length(variables) == 0) {
    return(dplyr::tibble(
      "variable_name" = character(),
      "estimate_name" = character(),
      "variable_type" = character(),
      "estimate_type" = character()
    ))
  }

  functions <- lapply(seq_along(variables), function(k) {
    tidyr::expand_grid(
      variable_name = variables[[k]],
      estimate_name = estimates[[k]]
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::inner_join(variableTypes(table), by = "variable_name") |>
    dplyr::inner_join(
      availableEstimates(fullQuantiles = TRUE) |>
        dplyr::select(-"estimate_description"),
      by = c("variable_type", "estimate_name")
    )

  # check binary
  binaryVars <- functions |>
    dplyr::filter(
      .data$variable_type %in% c("numeric", "integer") &
        .data$estimate_name %in% c("count", "percentage")
    ) |>
    dplyr::select("variable_name") |>
    dplyr::distinct() |>
    dplyr::pull()
  if (length(binaryVars) > 0) {
    notBinary <- character()
    for (binVar in binaryVars) {
      x <- table |>
        dplyr::select(dplyr::all_of(binVar)) |>
        dplyr::distinct() |>
        dplyr::pull()
      if (length(x) <= 3) {
        if (!all(as.numeric(x) %in% c(0, 1, NA))) {
          notBinary <- c(notBinary, binVar)
        }
      } else {
        notBinary <- c(notBinary, binVar)
      }
    }
    functions <- functions |>
      dplyr::filter(
        !.data$variable_name %in% .env$notBinary |
          !.data$estimate_name %in% c("count", "percentage")
      )
  }

  return(functions)
}

#' @noRd
checkCensorDate <- function(x, censorDate) {
  check <- x %>%
    dplyr::select(dplyr::all_of(censorDate)) %>%
    utils::head(1) %>%
    dplyr::pull() %>%
    inherits("Date")
  if (!check) {
    cli::cli_abort("{censorDate} is not a date variable")
  }
}

assertClass <- function(x,
                        class,
                        null = FALSE,
                        call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""), " must have class: ",
    paste0(class, collapse = ", "), "; but has class: ",
    paste0(base::class(x), collapse = ", "), "."
  )
  if (is.null(x)) {
    if (null) {
      return(invisible(x))
    } else {
      cli::cli_abort(
        "{paste0(substitute(x), collapse = '')} can not be NULL.",
        call = call
      )
    }
  }
  if (!all(class %in% base::class(x))) {
    cli::cli_abort(errorMessage, call = call)
  }
  invisible(x)
}

correctStrata <- function(strata, overall) {
  if (length(strata) == 0 | overall) {
    strata <- c(list(character()), strata)
  }
  strata <- unique(strata)
  return(strata)
}

assertNameStyle <- function(nameStyle,
                            values = list(),
                            call = parent.frame()) {
  # initial checks
  omopgenerics::assertCharacter(nameStyle, length = 1,
                                na = FALSE, minNumCharacter = 1, call = call)
  omopgenerics::assertList(values, named = TRUE)
  omopgenerics::assertClass(call, class = "environment")

  # check name style
  err <- character()
  for (k in seq_along(values)) {
    valk <- values[[k]]
    nm <- paste0("\\{", names(values)[k], "\\}")
    if (length(valk) > 1 & !grepl(pattern = nm, x = nameStyle)) {
      err <- c(err, paste0("{{", names(values)[k], "}}"))
    }
  }

  # error
  if (length(err) > 0) {
    names(err) <- rep("*", length(err))
    cli::cli_abort(
      message = c("The following elements are not present in nameStyle:", err),
      call = call
    )
  }

  return(invisible(nameStyle))
}

warnOverwriteColumns <- function(x, nameStyle, values = list()) {
  if (length(values) > 0) {
    nameStyle <- tidyr::expand_grid(!!!values) |>
      dplyr::mutate("tmp_12345" = glue::glue(.env$nameStyle)) |>
      dplyr::pull("tmp_12345") |>
      as.character() |>
      unique()
  }

  extraColumns <- colnames(x)[colnames(x) %in% nameStyle]
  if (length(extraColumns) > 0) {
    ms <- extraColumns
    names(ms) <- rep("*", length(ms))
    cli::cli_inform(message = c(
      "!" = "The following columns will be overwritten:", ms
    ))
    x <- x |> dplyr::select(!dplyr::all_of(extraColumns))
  }

  return(x)
}
assertCharacter <- function(x,
                            length = NULL,
                            na = FALSE,
                            null = FALSE,
                            named = FALSE,
                            minNumCharacter = 0,
                            call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a character",
    errorLength(length),
    errorNa(na),
    errorNull(null),
    errorNamed(named),
    ifelse(
      minNumCharacter > 0,
      paste("; at least", minNumCharacter, "per element"),
      ""
    ),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {
    # assert class
    if (!is.character(x)) {
      cli::cli_abort(errorMessage, call = call)
    }

    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert length
    assertLength(x, length, errorMessage, call)

    # assert na
    assertNa(x, na, errorMessage, call)

    # assert named
    assertNamed(x, named, errorMessage, call)

    # minimum number of characters
    if (any(nchar(xNoNa) < minNumCharacter)) {
      cli::cli_abort(errorMessage, call = call)
    }
  }

  return(invisible(x))
}
assertList <- function(x,
                       length = NULL,
                       na = FALSE,
                       null = FALSE,
                       named = FALSE,
                       class = NULL,
                       call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a list",
    errorLength(length),
    errorNa(na),
    errorNull(null),
    errorNamed(named),
    ifelse(
      !is.null(class),
      paste("; elements must have class:", paste0(class, collapse = ", ")),
      ""
    ),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {
    # assert class
    if (!is.list(x)) {
      cli::cli_abort(errorMessage, call = call)
    }

    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert length
    assertLength(x, length, errorMessage, call)

    # assert na
    assertNa(x, na, errorMessage, call)

    # assert named
    assertNamed(x, named, errorMessage, call)

    # assert class
    if (!is.null(class)) {
      flag <- lapply(xNoNa, function(y) {
        any(class %in% base::class(y))
      }) |>
        unlist() |>
        all()
      if (flag != TRUE) {
        cli::cli_abort(errorMessage, call = call)
      }
    }
  }

  return(invisible(x))
}
assertChoice <- function(x,
                         choices,
                         length = NULL,
                         na = FALSE,
                         null = FALSE,
                         named = FALSE,
                         call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a choice between: ",
    paste0(choices, collapse = ", "),
    errorLength(length),
    errorNa(na),
    errorNull(null),
    errorNamed(named),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {
    # assert class
    if (!all(class(x) == class(choices))) {
      cli::cli_abort(errorMessage, call = call)
    }

    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert length
    assertLength(x, length, errorMessage, call)

    # assert na
    assertNa(x, na, errorMessage, call)

    # assert named
    assertNamed(x, named, errorMessage, call)

    # assert choices
    if (base::length(xNoNa) > 0) {
      if (!all(xNoNa %in% choices)) {
        cli::cli_abort(errorMessage, call = call)
      }
    }
  }

  return(invisible(x))
}
assertLogical <- function(x,
                          length = NULL,
                          na = FALSE,
                          null = FALSE,
                          named = FALSE,
                          call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a logical",
    errorLength(length),
    errorNa(na),
    errorNull(null),
    errorNamed(named),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {
    # assert class
    if (!is.logical(x)) {
      cli::cli_abort(errorMessage, call = call)
    }

    # assert length
    assertLength(x, length, errorMessage, call)

    # assert na
    assertNa(x, na, errorMessage, call)

    # assert named
    assertNamed(x, named, errorMessage, call)
  }

  return(invisible(x))
}
assertNumeric <- function(x,
                          integerish = FALSE,
                          min = -Inf,
                          max = Inf,
                          length = NULL,
                          na = FALSE,
                          null = FALSE,
                          named = FALSE,
                          call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a numeric",
    ifelse(integerish, "; it has to be integerish", ""),
    ifelse(is.infinite(min), "", paste0("; greater than", min)),
    ifelse(is.infinite(max), "", paste0("; smaller than", max)),
    errorLength(length),
    errorNa(na),
    errorNull(null),
    errorNamed(named),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {
    # assert class
    if (!is.numeric(x)) {
      cli::cli_abort(errorMessage, call = call)
    }

    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert integerish
    if (integerish & base::length(xNoNa) > 0) {
      err <- max(abs(xNoNa - round(xNoNa)))
      if (err > 0.0001) {
        cli::cli_abort(errorMessage, call = call)
      }
    }

    # assert lower bound
    if (!is.infinite(min) & base::length(xNoNa) > 0) {
      if (base::min(xNoNa) < min) {
        cli::cli_abort(errorMessage, call = call)
      }
    }

    # assert upper bound
    if (!is.infinite(max) & base::length(xNoNa) > 0) {
      if (base::max(xNoNa) > max) {
        cli::cli_abort(errorMessage, call = call)
      }
    }

    # assert length
    assertLength(x, length, errorMessage, call)

    # assert na
    assertNa(x, na, errorMessage, call)

    # assert named
    assertNamed(x, named, errorMessage, call)
  }

  return(invisible(x))
}
assertClass <- function(x,
                        class,
                        null = FALSE,
                        call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""), " must have class: ",
    paste0(class, collapse = ", "), "; but has class: ",
    paste0(base::class(x), collapse = ", "), "."
  )
  if (is.null(x)) {
    if (null) {
      return(invisible(x))
    } else {
      cli::cli_abort(
        "{paste0(substitute(x), collapse = '')} can not be NULL.",
        call = call
      )
    }
  }
  if (!all(class %in% base::class(x))) {
    cli::cli_abort(errorMessage, call = call)
  }
  invisible(x)
}
assertLength <- function(x, length, errorMessage, call) {
  if (!is.null(length) && base::length(x) != length) {
    cli::cli_abort(errorMessage, call = call)
  }
  invisible(x)
}
errorLength <- function(length) {
  if (!is.null(length)) {
    str <- paste0("; with length = ", length)
  } else {
    str <- ""
  }
  return(str)
}
assertNa <- function(x, na, errorMessage, call) {
  if (!na && any(is.na(x))) {
    cli::cli_abort(errorMessage, call = call)
  }
  invisible(x)
}
errorNa <- function(na) {
  if (na) {
    str <- ""
  } else {
    str <- "; it can not contain NA"
  }
  return(str)
}
assertNamed <- function(x, named, errorMessage, call) {
  if (named && length(names(x)[names(x) != ""]) != length(x)) {
    cli::cli_abort(errorMessage, call = call)
  }
  invisible(x)
}
errorNamed <- function(named) {
  if (named) {
    str <- "; it has to be named"
  } else {
    str <- ""
  }
  return(str)
}
assertNull <- function(x, null, errorMessage, call) {
  if (!null && is.null(x)) {
    cli::cli_abort(errorMessage, call = call)
  }
  return(!is.null(x))
}
errorNull <- function(null) {
  if (null) {
    str <- ""
  } else {
    str <- "; it can not be NULL"
  }
  return(str)
}

# checks demographics
validateX <- function(x, call) {
  assertClass(x, class = "cdm_table", call = call)
  cols <- colnames(x)
  n <- sum(c("person_id", "subject_id") %in% cols)
  if (n == 0) cli::cli_abort("No person indentifier (person_id/subject_id) found in x.", call = call)
  if (n == 2) cli::cli_abort("Only person_id or subject_id can be present in x.", call = call)
  return(x)
}
validateLogical <- function(x, null = FALSE, call) {
  if (null) {
    return(NULL)
  }
  err <- paste(substitute(x), "must be TRUE or FALSE") |> rlang::set_names("!")
  if (!is.logical(x)) cli::cli_abort(message = err, call = call)
  if (length(x) != 1) cli::cli_abort(message = err, call = call)
  if (is.na(x)) cli::cli_abort(message = err, call = call)
  return(x)
}
validateIndexDate <- function(indexDate, null, x, call) {
  if (null) {
    return(NULL)
  }
  assertCharacter(indexDate, length = 1, call = call)
  if (!indexDate %in% colnames(x)) {
    cli::cli_abort("indexDate must be a column in x.", call = call)
  }
  xx <- x |>
    dplyr::select(dplyr::all_of(indexDate)) |>
    utils::head(1) |>
    dplyr::pull()
  if (!inherits(xx, "Date") && !inherits(xx, "POSIXt")) {
    cli::cli_abort("x[[{indexDate}]] is not a date column.", call = call)
  }
  return(indexDate)
}
validateColumn <- function(col, null, call) {
  if (null) {
    return(NULL)
  }

  nm <- paste0(substitute(col))

  err <- "{nm} must be a snake_case character vector"
  if (!is.character(col)) cli::cli_abort(message = err, call = call)
  if (length(col) != 1) cli::cli_abort(message = err, call = call)
  if (is.na(col)) cli::cli_abort(message = err, call = call)

  scCol <- omopgenerics::toSnakeCase(col)

  if (scCol != col) {
    cli::cli_warn(
      c("!" = "{nm} has been modified to be snake_case, {col} -> {scCol}"),
      call = call
    )
  }

  return(scCol)
}
validateAgeMissingMonth <- function(ageMissingMonth, null, call) {
  if (null) {
    return(ageMissingMonth)
  }

  if (is.character(ageMissingMonth)) {
    ageMissingMonth <- as.numeric(ageMissingMonth)
  }
  assertNumeric(ageMissingMonth, integerish = TRUE, min = 1, max = 12, call = call)
  ageMissingMonth <- as.integer(ageMissingMonth)

  return(ageMissingMonth)
}
validateAgeMissingDay <- function(ageMissingDay, null, call) {
  if (null) {
    return(ageMissingDay)
  }

  if (is.character(ageMissingDay)) {
    ageMissingDay <- as.numeric(ageMissingDay)
  }
  assertNumeric(ageMissingDay, integerish = TRUE, min = 1, max = 12, call = call)
  ageMissingDay <- as.integer(ageMissingDay)

  return(ageMissingDay)
}
validateAgeGroup <- function(ageGroup, call) {
  if (length(ageGroup) == 0) {
    return(NULL)
  }
  assertList(ageGroup, call = call)
  if (is.numeric(ageGroup[[1]])) {
    ageGroup <- list("age_group" = ageGroup)
  }
  for (k in seq_along(ageGroup)) {
    invisible(checkCategory(ageGroup[[k]], call = call))
    if (any(ageGroup[[k]] |> unlist() |> unique() < 0)) {
      cli::cli_abort("ageGroup can't contain negative values", call = call)
    }
    if (is.null(names(ageGroup[[k]]))) {
      nms <- rep("", length(ageGroup[[k]]))
    } else {
      nms <- names(ageGroup[[k]])
    }
    for (i in seq_along(nms)) {
      if (nms[i] == "") {
        if (is.infinite(ageGroup[[k]][[i]][2])) {
          nms[i] <- paste(round(ageGroup[[k]][[i]][1]), "or above")
        } else {
          nms[i] <- paste(
            round(ageGroup[[k]][[i]][1]), "to", round(ageGroup[[k]][[i]][2])
          )
        }
      }
    }
    names(ageGroup[[k]]) <- nms
  }
  if (is.null(names(ageGroup))) {
    names(ageGroup) <- paste0("age_group_", 1:length(ageGroup))
  }
  if ("" %in% names(ageGroup)) {
    id <- which(names(ageGroup) == "")
    names(ageGroup)[id] <- paste0("age_group_", id)
  }
  invisible(ageGroup)
}
validateMissingValue <- function(x, null, call) {
  if (null) {
    return(NULL)
  }
  nm <- paste0(substitute(x))
  err <- "{nm} must be a character of length 1." |> rlang::set_names("!")
  if (!is.character(x)) cli::cli_abort(message = err, call = call)
  if (length(x) != 1) cli::cli_abort(message = err, call = call)
  return(x)
}
validateType <- function(x, null, call) {
  if (null) {
    return(NULL)
  }
  nm <- paste0(substitute(x))
  err <- "{nm} must be a choice between 'date' or 'days'." |>
    rlang::set_names("!")
  if (!is.character(x)) cli::cli_abort(message = err, call = call)
  if (length(x) != 1) cli::cli_abort(message = err, call = call)
  if (!x %in% c("date", "days")) cli::cli_abort(message = err, call = call)
  return(x)
}
validateName <- function(name, call = parent.frame()) {
  assertCharacter(name, length = 1, null = TRUE, call = call)
}

checkCategory <- function(category, overlap = FALSE, type = "numeric", call = parent.frame()) {
  assertList(category, class = type, call = call)

  if (is.null(names(category))) {
    names(category) <- rep("", length(category))
  }

  # check length
  category <- lapply(category, function(x) {
    if (length(x) == 1) {
      x <- c(x, x)
    } else if (length(x) > 2) {
      cli::cli_abort(
        "Please specify only two values (lower bound and upper bound) per category",
        call = call
      )
    }
    invisible(x)
  })

  # check lower bound is smaller than upper bound
  checkLower <- unlist(lapply(category, function(x) {
    x[1] <= x[2]
  }))
  if (!(all(checkLower))) {
    "Lower bound should be equal or smaller than upper bound" |>
      cli::cli_abort(call = call)
  }

  # built tibble
  result <- lapply(category, function(x) {
    dplyr::tibble(lower_bound = x[1], upper_bound = x[2])
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(category_label = names(.env$category)) %>%
    dplyr::mutate(category_label = dplyr::if_else(
      .data$category_label == "",
      dplyr::case_when(
        is.infinite(.data$lower_bound) & is.infinite(.data$upper_bound) ~ "any",
        is.infinite(.data$lower_bound) ~ paste(.data$upper_bound, "or below"),
        is.infinite(.data$upper_bound) ~ paste(.data$lower_bound, "or above"),
        TRUE ~ paste(.data$lower_bound, "to", .data$upper_bound)
      ),
      .data$category_label
    )) %>%
    dplyr::arrange(.data$lower_bound)

  # check overlap
  if (!overlap) {
    if (nrow(result) > 1) {
      lower <- result$lower_bound[2:nrow(result)]
      upper <- result$upper_bound[1:(nrow(result) - 1)]
      if (!all(lower > upper)) {
        "There can not be overlap between categories" |>
          cli::cli_abort(call = call)
      }
    }
  }

  invisible(result)
}
