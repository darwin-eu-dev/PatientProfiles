# Copyright 2023 DARWIN EU (C)
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
checkCategory <- function(category, overlap = FALSE, type = "numeric") {
  checkmate::assertList(
    category,
    types = type, any.missing = FALSE, unique = TRUE,
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
    invisible(x)
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
        cli::cli_abort("There can not be overlap between categories")
      }
    }
  }

  invisible(result)
}

#' @noRd
checkAgeGroup <- function(ageGroup, overlap = FALSE) {
  checkmate::assertList(ageGroup, min.len = 1, null.ok = TRUE)
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
checkWindow <- function(window) {
  if (!is.list(window)) {
    cli::cli_abort("window must be a list")
  }

  # Find if any NA, throw warning that it will be changed to Inf, change it later
  if (any(unlist(lapply(window, is.na)))) {
    cli::cli_abort("NA found in window, please use Inf or -Inf instead")
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

  invisible(windowTbl)
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
  invisible(filterTbl)
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
    variablesNotContained <- gsub("[{}]", "", variablesNotContained)
    variablesNotContained <- gsub("id_name", "cohort_name", variablesNotContained)
    cli::cli_abort(paste0(
      "Variables: ",
      paste0(variablesNotContained, collapse = ", "),
      " have multiple possibilities and should be cotained in nameStyle"
    ))
  }
}

#' @noRd
checkValue <- function(value, x, name) {
  checkmate::assertCharacter(value, any.missing = FALSE, min.len = 1)
  checkmate::assertTRUE(
    all(value %in% c("flag", "count", "date", "days", colnames(x)))
  )
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
  }
  parameters <- list(
    "filter_variable" = filterVariable,
    "filter_id" = sort(targetCohortId),
    "id_name" = idName
  )
  invisible(parameters)
}

#' @noRd
checkSnakeCase <- function(name, verbose = TRUE) {
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
  invisible(name)
}

#' @noRd
checkVariableType <- function(variableType) {
  assertChoice()
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
checkStrata <- function(strata, table) {
  errorMessage <- "strata should be a list that point to columns in table"
  if (!is.list(strata)) {
    cli::cli_abort(errorMessage)
  }
  if (length(strata) > 0) {
    if (!is.character(unlist(strata))) {
      cli::cli_abort(errorMessage)
    }
    if (!all(unlist(strata) %in% colnames(table))) {
      cli::cli_abort(errorMessage)
    }
  }
}

#' @noRd
checkVariablesFunctions <- function(variables, functions, table) {
  errorMessage <- "variables should be a unique named list that point to columns in table"
  if (!is.list(variables)) {
    cli::cli_abort(errorMessage)
  }
  if (length(names(variables)) != length(variables)) {
    cli::cli_abort(errorMessage)
  }
  if (!is.character(unlist(variables))) {
    cli::cli_abort(errorMessage)
  }
  if (!all(unlist(variables) %in% colnames(table))) {
    cli::cli_abort(errorMessage)
  }
  errorMessage <- "functions should be a unique named list that point to functions. Check suported functions using availableFunctions()."
  if (!is.list(functions)) {
    cli::cli_abort(errorMessage)
  }
  if (length(names(functions)) != length(functions)) {
    cli::cli_abort(errorMessage)
  }
  if (!is.character(unlist(functions))) {
    cli::cli_abort(errorMessage)
  }
  if (!all(unlist(functions) %in% unique(formats$format_key))) {
    cli::cli_abort(errorMessage)
  }
  if (!identical(sort(names(variables)), sort(names(functions)))) {
    cli::cli_abort("Names from variables and functions must be the same")
  }
  vt <- variableTypes(table)
  requiredFunctions <- NULL
  for (nam in names(variables)) {
    requiredFunctions <- requiredFunctions %>%
      dplyr::union_all(
        tidyr::expand_grid(
          variable = variables[[nam]],
          format_key = functions[[nam]]
        )
      )
  }
  suportedFunctions <- vt %>%
    dplyr::select("variable", "variable_type") %>%
    dplyr::left_join(
      formats %>%
        dplyr::select("variable_type", "format_key"),
      by = "variable_type",
      relationship = "many-to-many"
    )
  nonSuportedFunctions <- requiredFunctions %>%
    dplyr::anti_join(suportedFunctions, by = c("variable", "format_key"))
  if (nrow(nonSuportedFunctions) > 0) {
    nonSuportedFunctions <- nonSuportedFunctions %>%
      dplyr::left_join(vt, by = "variable")
    errorMessage <- "Non supported functions found."
    vars <- unique(nonSuportedFunctions$variable)
    for (v in vars) {
      errorMessage <- paste0(
        errorMessage, " '", v, "' is `",
        vt$variable_type[vt$variable == v], "` and formats: ",
        paste0(
          nonSuportedFunctions$format_key[nonSuportedFunctions$variable == v],
          collapse = ", "
        ),
        " are not suported."
      )
    }
    cli::cli_abort(errorMessage)
  }
}

#' @noRd
checkSuppressCellCount <- function(suppressCellCount) {
  checkmate::assertIntegerish(
    suppressCellCount,
    lower = 0, len = 1, any.missing = F
  )
}

#' @noRd
checkBigMark <- function(bigMark) {
  checkmate::checkCharacter(bigMark, min.chars = 0, len = 1, any.missing = F)
}

#' @noRd
checkDecimalMark <- function(decimalMark) {
  checkmate::checkCharacter(decimalMark, min.chars = 1, len = 1, any.missing = F)
}

#' @noRd
checkSignificantDecimals <- function(significantDecimals) {
  checkmate::assertIntegerish(
    significantDecimals,
    lower = 0, len = 1, any.missing = F
  )
}

#' @noRd
checkTableIntersect <- function(tableIntersect, cdm) {
  checkmate::assertList(tableIntersect)
  arguments <- getArguments(addTableIntersect)
  if (length(tableIntersect) > 0) {
    if (!is.list(tableIntersect[[1]])) {
      tableIntersect <- list(tableIntersect)
    }
  }
  lapply(tableIntersect, function(x) {
    checkmate::assertList(x, names = "named")
    checkmate::assertTRUE(all(names(x) %in% c(arguments$all, "value")))
    checkmate::assertTRUE(all(arguments$compulsory %in% names(x)))
  })
  return(tableIntersect)
}

getArguments <- function(fun) {
  arguments <- formals(fun)
  compulsory <- character()
  for (k in seq_along(arguments)) {
    x <- arguments[[k]]
    if (missing(x)) {
      compulsory <- c(compulsory, names(arguments)[k])
    }
  }
  compulsory <- compulsory[compulsory != "x"]
  all <- names(arguments)
  return(list(all = all, compulsory = compulsory))
}

#' @noRd
checkCohortIntersect <- function(cohortIntersect, cdm) {
  checkmate::assertList(cohortIntersect)
  arguments <- getArguments(addCohortIntersect)
  if (length(cohortIntersect) > 0) {
    if (!is.list(cohortIntersect[[1]])) {
      cohortIntersect <- list(cohortIntersect)
    }
  }
  lapply(cohortIntersect, function(x) {
    checkmate::assertList(x, names = "named")
    checkmate::assertTRUE(all(names(x) %in% c(arguments$all, "value")))
    checkmate::assertTRUE(all(arguments$compulsory %in% names(x)))
  })
  return(cohortIntersect)
}

#' @noRd
checkConceptIntersect <- function(conceptIntersect, cdm) {
  checkmate::assertList(conceptIntersect, names = "named")
  arguments <- getArguments(addConceptIntersect)
  if (length(conceptIntersect) > 0) {
    if (!identical(lapply(conceptIntersect, class) |> unlist() |> unname() |> unique(), "list")) {
      conceptIntersect <- list(conceptIntersect)
    }
  }
  lapply(conceptIntersect, function(x) {
    checkmate::assertList(x, names = "named")
    checkmate::assertTRUE(all(names(x) %in% c(arguments$all, "value")))
    checkmate::assertTRUE(all(arguments$compulsory %in% names(x)))
  })
  return(conceptIntersect)
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

#' @noRd
checkOtherVariables <- function(otherVariables, cohort, call = rlang::env_parent()) {
  errorMessage <- "otherVariables must point to columns in cohort."
  if (!is.character(otherVariables)) {
    cli::cli_abort(errorMessage, call = call)
  }
  if (!all(otherVariables %in% colnames(cohort))) {
    cli::cli_abort(errorMessage, call = call)
  }
  invisible(otherVariables)
}

#' Assert whether a nameStyle contains the needed information.
#'
#' @param nameStyle nameStyle object to check.
#' @param values Parameters options that must be contained.
#' @param call An environment for cli functions.
#'
#' @return An error if nameStyle is not properly formatted.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' assertNameStyle("my_name", values = list(
#'   "variable1" = 1, "variable2" = c("a", "b", "c")
#' ))
#'
#' assertNameStyle("my_name_{variable2}", values = list(
#'   "variable1" = 1, "variable2" = c("a", "b", "c")
#' ))
#'
#' assertNameStyle("my_name_{variable2}", values = list(
#'   "variable1" = c(1, 2), "variable2" = c("a", "b", "c")
#' ))
#'
#' assertNameStyle("my_name_{variable1}_{variable2}", values = list(
#'   "variable1" = c(1, 2), "variable2" = c("a", "b", "c")
#' ))
#' }
#'
assertNameStyle <- function(nameStyle,
                            values = list(),
                            call = parent.frame()) {
  # initial checks
  checkmate::assertCharacter(nameStyle, len = 1, any.missing = FALSE, min.chars = 1)
  checkmate::assertList(values, any.missing = FALSE, names = "named")
  checkmate::assertClass(call, "environment")

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
assertTibble <- function(x,
                         numberColumns = NULL,
                         numberRows = NULL,
                         columns = NULL,
                         null = FALSE,
                         call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a tibble",
    ifelse(is.null(numberColumns), "", paste0("; with at least ", numberColumns, " columns")),
    ifelse(is.null(numberRows), "", paste0("; with at least ", numberRows, " rows")),
    ifelse(is.null(columns), "", paste0("; the following columns must be present: ", paste0(columns, collapse = ", "))),
    errorNull(null),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {
    # assert class
    if (!("tbl" %in% class(x))) {
      cli::cli_abort(errorMessage, call = call)
    }

    # assert numberColumns
    if (!is.null(numberColumns)) {
      if (length(x) != numberColumns) {
        cli::cli_abort(errorMessage, call = call)
      }
    }

    # assert numberRows
    if (!is.null(numberRows)) {
      if (nrow(x) != numberRows) {
        cli::cli_abort(errorMessage, call = call)
      }
    }

    # assert columns
    if (!is.null(columns)) {
      if (!all(columns %in% colnames(x))) {
        cli::cli_abort(errorMessage, call = call)
      }
    }
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
    paste0(base::class(x), collapse = ", ") ,"."
  )
  if (is.null(x)) {
    if (null) {
      return(invisible(x))
    } else {
      cli::cli_abort(
        "{paste0(substitute(x), collapse = '')} can not be NULL.", call = call
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
