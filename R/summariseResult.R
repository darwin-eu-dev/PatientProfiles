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

#' Summarise variables using a set of estimate functions. The output will be
#' a formatted summarised_result object.
#'
#' @param table Table with different records.
#' @param group List of groups to be considered.
#' @param includeOverallGroup TRUE or FALSE. If TRUE, results for an overall
#' group will be reported when a list of groups has been specified.
#' @param strata List of the stratifications within each group to be considered.
#' @param includeOverallStrata TRUE or FALSE. If TRUE, results for an overall
#' strata will be reported when a list of strata has been specified.
#' @param variables Variables to summarise, it can be a list to point to different
#' set of estimate names.
#' @param estimates Estimates to obtain, it can be a list to point to different
#' set of variables.
#' @param functions deprecated.
#' @param counts Whether to compute number of records and number of subjects.
#' @param verbose Whether to print progress.
#'
#' @return A summarised_result object with the summarised data of interest.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' library(dplyr)
#'
#' cdm <- mockPatientProfiles()
#' x <- cdm$cohort1 %>%
#'   addDemographics() %>%
#'   collect()
#' result <- summariseResult(x)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
summariseResult <- function(table,
                            group = list(),
                            includeOverallGroup = FALSE,
                            strata = list(),
                            includeOverallStrata = TRUE,
                            variables = NULL,
                            functions = lifecycle::deprecated(),
                            estimates = c("min", "q25", "median", "q75", "max", "count", "percentage"),
                            counts = TRUE,
                            verbose = TRUE) {
  if (lifecycle::is_present(functions)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "summariseResult(functions)",
      with = "summariseResult(estimates)"
    )
    if (missing(estimates)) {
      estimates <- functions
    }
  }

  # initial checks
  checkTable(table)

  if (is.null(variables)) {
    variables <- colnames(table)
    variables <- variables[!grepl("_id", variables)]
  }

  if (inherits(table, "cdm_table")) {
    cdm_name <- omopgenerics::cdmName(omopgenerics::cdmReference(table))
  } else {
    cdm_name <- "unknown"
  }

  # create the summary for overall
  if (table %>%
      dplyr::count() %>%
      dplyr::pull() == 0) {
    if (counts) {
      result <- dplyr::tibble(
        "group_name" = "overall", "group_level" = "overall",
        "strata_name" = "overall", "strata_level" = "overall",
        "variable_name" = c("number_records", "number_subjects"),
        "variable_level" = NA_character_, "estimate_name" = "count",
        "estimate_type" = "integer", "estimate_value" = "0"
      )
    } else {
      result <- omopgenerics::emptySummarisedResult()
      return(result)
    }
  } else {
    if (!is.list(variables)) {
      variables <- list(variables)
    }
    if (!is.list(estimates)) {
      estimates <- list(estimates)
    }
    estimates <- lapply(estimates, function(x) {
      if ("missing" %in% x) {
        x <- x[x != "missing"]
        x <- c(x, "count_missing", "percentage_missing")
        cli::cli_warn(
          "'missing' is no longer an option for {.arg estimates}. Use
          'count_missing' or 'percentage_missing'."
        )
      }
      return(x)
    })
    if (!is.list(group)) {
      group <- list(group)
    }
    if (!is.list(strata)) {
      strata <- list(strata)
    }
    checkStrata(group, table, type = "group")
    checkStrata(strata, table)
    functions <- checkVariablesFunctions(variables, estimates, table)

    # if (nrow(functions) == 0) {
    #   cli::cli_alert_warning(
    #     "No estimate can be computed with the current arguments, check `availableEstimates()` for the estimates that this function supports"
    #   )
    #   #return(omopgenerics::emptySummarisedResult())
    # } else
    if (verbose) {
      mes <- c("i" = "The following estimates will be computed:")
      variables <- functions$variable_name |> unique()
      for (vark in variables) {
        mes <- c(mes, "*" = paste0(
          vark, ": ", paste0(functions$estimate_name[functions$variable_name == vark], collapse = ", ")
        ))
      }
      cli::cli_inform(message = mes)
    }

    # only required variables
    colOrder <- colnames(table)
    table <- table |>
      dplyr::select(dplyr::any_of(unique(c(
        unlist(strata), unlist(group), functions$variable_name, "person_id",
        "subject_id"
      ))))

    # collect if necessary
    collectFlag <- functions %>%
      dplyr::filter(grepl("q", .data$estimate_name)) %>%
      nrow() > 0
    if (collectFlag) {
      table <- table %>% dplyr::collect()
    }

    # correct dates and logicals
    dates <- functions |>
      dplyr::filter(.data$variable_type %in% c("date", "logical")) |>
      dplyr::distinct(.data$variable_name) |>
      dplyr::pull()
    table <- table |>
      dplyr::mutate(dplyr::across(
        .cols = dplyr::all_of(dates),
        .fns = as.integer
      ))

    # correct strata and group
    group <- correctStrata(group, includeOverallGroup)
    strata <- correctStrata(strata, includeOverallStrata)

    if (verbose) {
      cli::cli_alert("Start summary of data, at {Sys.time()}")
      nt <- length(group) * length(strata)
      k <- 0
      cli::cli_progress_bar(
        total = nt,
        format = "{cli::pb_bar}{k}/{nt} group-strata combinations @ {Sys.time()}"
      )
    }
    resultk <- 1
    result <- list()
    for (groupk in group) {
      for (stratak in strata) {
        result[[resultk]] <- summariseInternal(
          table, groupk, stratak, functions, counts
        ) |>
          # order variables
          orderVariables(colOrder, unique(unlist(estimates)))
        resultk <- resultk + 1
        if (verbose) {
          k <- k + 1
          cli::cli_progress_update()
        }
      }
    }
    result <- result |> dplyr::bind_rows()
    if (verbose) {
      cli::cli_inform(c("v" = "Summary finished, at {Sys.time()}"))
    }
  }

  # TO REMOVE
  result$variable_name[result$variable_name == "number_subjects"] <- "number subjects"
  result$variable_name[result$variable_name == "number_records"] <- "number records"

  # format summarised_result
  result <- result |>
    dplyr::mutate(
      "result_id" = as.integer(1),
      "cdm_name" = .env$cdm_name,
      "result_type" = "summarise_table",
      "package_name" = "PatientProfiles",
      "package_version" = as.character(utils::packageVersion("PatientProfiles")),
      "additional_name" = "overall",
      "additional_level" = "overall"
    ) |>
    omopgenerics::newSummarisedResult()

  return(result)
}

summariseInternal <- function(table, groupk, stratak, functions, counts) {
  result <- list()

  # group by relevant variables
  strataGroupk <- unique(c(groupk, stratak))

  if (length(strataGroupk) == 0) {
    table <- table |>
      dplyr::mutate("strata_id" = as.integer(1))
    strataGroup <- dplyr::tibble(
      "strata_id" = as.integer(1),
      "group_name" = "overall",
      "group_level" = "overall",
      "strata_name" = "overall",
      "strata_level" = "overall"
    )
  } else {
    strataGroup <- table |>
      dplyr::select(dplyr::all_of(strataGroupk)) |>
      dplyr::distinct() |>
      dplyr::mutate("strata_id" = dplyr::row_number())
    if (strataGroup |> dplyr::ungroup() |> dplyr::tally() |> dplyr::pull() == 1) {
      table <- table |>
        dplyr::mutate("strata_id" = as.integer(1))
    } else {
      table <- table |>
        dplyr::inner_join(strataGroup, by = strataGroupk)
    }
    # format group strata
    strataGroup <- strataGroup |>
      dplyr::collect() |>
      visOmopResults::uniteGroup(cols = groupk, keep = TRUE) |>
      visOmopResults::uniteStrata(cols = stratak, keep = TRUE) |>
      dplyr::select(
        "strata_id", "group_name", "group_level", "strata_name", "strata_level"
      )
  }
  table <- table |>
    dplyr::select(dplyr::any_of(c(
      "strata_id", "person_id", "subject_id", unique(functions$variable_name)
    ))) |>
    dplyr::group_by(.data$strata_id)

  # count subjects and records
  if (counts) {
    result$counts <- countSubjects(table)
  }

  # summariseNumeric
  result$numeric <- summariseNumeric(table, functions)

  # summariseBinary
  result$binary <- summariseBinary(table, functions)

  # summariseCategories
  result$categories <- summariseCategories(table, functions)

  # summariseMissings
  result$missings <- summariseMissings(table, functions)

  result <- result |>
    dplyr::bind_rows() |>
    dplyr::inner_join(strataGroup, by = "strata_id") |>
    dplyr::select(-"strata_id") |>
    dplyr::arrange(.data$strata_level)

  return(result)
}

countSubjects <- function(x) {
  i <- "person_id" %in% colnames(x)
  j <- "subject_id" %in% colnames(x)
  if (i) {
    if (j) {
      cli::cli_warn(
        "person_id and subject_id present in table, `person_id` used as person
        identifier"
      )
    }
    personVariable <- "person_id"
  } else if (j) {
    personVariable <- "subject_id"
  }
  result <- list()
  result$record <- x %>%
    dplyr::summarise(
      "estimate_value" = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::collect() |>
    dplyr::mutate(
      "variable_name" = "number_records"
    )
  if (i | j) {
    result$subject <- x %>%
      dplyr::summarise(
        "estimate_value" = dplyr::n_distinct(.data[[personVariable]]),
        .groups = "drop"
      ) %>%
      dplyr::collect() |>
      dplyr::mutate(
        "variable_name" = "number_subjects"
      )
  }
  result <- dplyr::bind_rows(result) |>
    dplyr::mutate(
      "estimate_type" = "integer",
      "estimate_name" = "count",
      "variable_level" = NA_character_,
      "estimate_value" = as.character(.data$estimate_value)
    )
  return(result)
}

summariseNumeric <- function(table, functions) {
  funs <- functions |>
    dplyr::filter(
      .data$variable_type %in% c("date", "numeric", "integer") &
        !grepl("count|percentage", .data$estimate_name)
    )

  if (nrow(funs) == 0) {
    return(NULL)
  }

  res <- list()
  uniqueEstimates <- funs$estimate_name |> unique()
  uniqueVariables <- funs$variable_name |> unique()
  if (length(uniqueEstimates) <= length(uniqueVariables)) {
    for (est in uniqueEstimates) {
      varEst <- funs |>
        dplyr::filter(.data$estimate_name == .env$est) |>
        dplyr::pull("variable_name")
      res[[est]] <- table |>
        dplyr::summarise(
          dplyr::across(
            .cols = dplyr::all_of(varEst),
            .fns = !!getFunctions(est),
            .names = "estimate_{.col}"
          ),
          .groups = "drop"
        ) |>
        dplyr::collect() |>
        dplyr::mutate(dplyr::across(
          .cols = dplyr::all_of(paste0("estimate_", varEst)),
          .fns = as.numeric
        )) |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(paste0("estimate_", varEst)),
          names_to = "variable_name",
          values_to = "estimate_value"
        ) |>
        dplyr::mutate(
          "variable_name" = substr(.data$variable_name, 10, nchar(.data$variable_name)),
          "variable_level" = NA_character_,
          "estimate_name" = .env$est
        )
    }
  } else {
    for (vark in uniqueVariables) {
      estVar <- funs |>
        dplyr::filter(.data$variable_name == .env$vark) |>
        dplyr::pull("estimate_name")
      res[[vark]] <- table |>
        dplyr::summarise(
          dplyr::across(
            .cols = dplyr::all_of(vark),
            .fns = !!getFunctions(estVar),
            .names = "variable_{.fn}"
          ),
          .groups = "drop"
        ) |>
        dplyr::collect() |>
        dplyr::mutate(dplyr::across(
          .cols = dplyr::all_of(paste0("variable_", estVar)),
          .fns = as.numeric
        )) |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(paste0("variable_", estVar)),
          names_to = "estimate_name",
          values_to = "estimate_value"
        ) |>
        dplyr::mutate(
          "estimate_name" = substr(.data$estimate_name, 10, nchar(.data$estimate_name)),
          "variable_level" = NA_character_, "variable_name" = .env$vark
        )
    }
  }

  # add estimate_type + correct dates
  res <- res |>
    dplyr::bind_rows() |>
    dplyr::inner_join(
      funs |>
        dplyr::select("variable_name", "estimate_name", "estimate_type"),
      by = c("variable_name", "estimate_name")
    ) |>
    dplyr::mutate("estimate_value" = dplyr::case_when(
      .data$estimate_type == "date" ~
        as.character(as.Date(round(.data$estimate_value), origin = "1970-01-01")),
      .data$estimate_type == "integer" ~
        as.character(round(.data$estimate_value)),
      .data$estimate_type == "numeric" ~ as.character(.data$estimate_value)
    ))

  return(res)
}

summariseBinary <- function(table, functions) {
  binFuns <- functions |>
    dplyr::filter(
      .data$variable_type != "categorical" &
        .data$estimate_name %in% c("count", "percentage")
    )
  binNum <- binFuns |> dplyr::pull("variable_name") |> unique()
  if (length(binNum) > 0) {
    num <- table |>
      dplyr::summarise(dplyr::across(
        .cols = dplyr::all_of(binNum),
        ~ sum(.x, na.rm = TRUE),
        .names = "counts_{.col}"
      )) |>
      dplyr::collect() |>
      dplyr::mutate(dplyr::across(
        .cols = dplyr::all_of(paste0("counts_", binNum)),
        .fns = as.numeric
      ))
    binDen <- binFuns |>
      dplyr::filter(.data$estimate_name == "percentage") |>
      dplyr::pull("variable_name")
    res <- num |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(paste0("counts_", binNum)),
        names_to = "variable_name",
        values_to = "estimate_value"
      ) |>
      dplyr::mutate(
        "variable_name" = substr(.data$variable_name, 8, nchar(.data$variable_name)),
        "estimate_name" = "count",
        "estimate_type" = "integer"
      )
    if (length(binDen) > 0) {
      den <- table |>
        dplyr::summarise(dplyr::across(
          .cols = dplyr::all_of(binDen),
          ~ sum(as.integer(!is.na(.x)), na.rm = TRUE),
          .names = "den_{.col}"
        )) |>
        dplyr::collect() |>
        dplyr::mutate(dplyr::across(
          .cols = dplyr::all_of(paste0("den_", binDen)),
          .fns = as.numeric
        ))
      percentages <- num |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(paste0("counts_", binNum)),
          names_to = "variable_name",
          values_to = "numerator"
        ) |>
        dplyr::mutate(
          "variable_name" = substr(.data$variable_name, 8, nchar(.data$variable_name))
        ) |>
        dplyr::inner_join(
          den |>
            tidyr::pivot_longer(
              cols = dplyr::all_of(paste0("den_", binDen)),
              names_to = "variable_name",
              values_to = "denominator"
            ) |>
            dplyr::mutate(
              "variable_name" = substr(.data$variable_name, 5, nchar(.data$variable_name))
            ),
          by = c("strata_id", "variable_name")
        ) |>
        dplyr::mutate(
          "estimate_value" = 100*.data$numerator/.data$denominator,
          "estimate_name" = "percentage",
          "estimate_type" = "percentage"
        ) |>
        dplyr::select(-c("numerator", "denominator"))
      res <- res |> dplyr::union_all(percentages)
    }

    res <- res |>
      dplyr::mutate(
        "estimate_value" = as.character(.data$estimate_value),
        "variable_level" = NA_character_
      )
  } else {
    res <- NULL
  }

  return(res)
}

summariseCategories <- function(table, functions) {
  catFuns <- functions |>
    dplyr::filter(.data$variable_type == "categorical")
  result <- list()
  catVars <- unique(catFuns$variable_name)
  if(length(catVars) > 0) {
    den <- table |>
      dplyr::tally(name = "denominator") |>
      dplyr::collect() |>
      dplyr::ungroup()
    for (catVar in catVars) {
      est <- catFuns |>
        dplyr::filter(.data$variable_name == .env$catVar) |>
        dplyr::pull("estimate_name")
      result[[catVar]] <- table |>
        dplyr::group_by(.data$strata_id, .data[[catVar]]) |>
        dplyr::tally(name = "count") |>
        dplyr::collect() |>
        dplyr::ungroup() |>
        dplyr::inner_join(den, by = "strata_id") |>
        dplyr::mutate(
          "percentage" = as.character(100 * .data$count / .data$denominator),
          "count" = as.character(.data$count)
        ) |>
        dplyr::select(!"denominator") |>
        tidyr::pivot_longer(
          cols = c("count", "percentage"),
          names_to = "estimate_name",
          values_to = "estimate_value"
        ) |>
        dplyr::mutate(
          "variable_name" = .env$catVar,
          "estimate_type" = dplyr::if_else(
            .data$estimate_name == "count", "integer", "percentage"
          )
        ) |>
        dplyr::select(
          "strata_id", "variable_name",
          "variable_level" = dplyr::all_of(catVar), "estimate_name",
          "estimate_type", "estimate_value"
        ) |>
        dplyr::filter(.data$estimate_name %in% .env$est)

    }
  }
  return(dplyr::bind_rows(result))
}

summariseMissings <- function(table, functions) {
  result <- list()

  # counts
  mVars <- functions |>
    dplyr::filter(.data$estimate_name %in% c("count_missing", "percentage_missing")) |>
    dplyr::pull("variable_name") |>
    unique()
  if (length(mVars) > 0) {
    result <- table |>
      dplyr::summarise(
        dplyr::across(
          .cols = dplyr::all_of(mVars),
          ~ sum(as.integer(is.na(.x)), na.rm = TRUE),
          .names = "cm_{.col}"
        ),
        "den" = dplyr::n()
      ) |>
      dplyr::collect() |>
      dplyr::mutate(dplyr::across(
        .cols = dplyr::all_of(c("den", paste0("cm_", mVars))),
        .fns = as.numeric
      )) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(paste0("cm_", mVars)),
        names_to = "variable_name",
        values_to = "count_missing"
      ) |>
      dplyr::mutate("percentage_missing" = 100*.data$count_missing/.data$den) |>
      dplyr::select(-"den") |>
      tidyr::pivot_longer(
        cols = c("count_missing", "percentage_missing"),
        names_to = "estimate_name",
        values_to = "estimate_value"
      ) |>
      dplyr::mutate(
        "variable_name" = substr(.data$variable_name, 4, nchar(.data$variable_name)),
        "variable_level" = NA_character_,
        "estimate_value" = as.character(.data$estimate_value)
      ) |>
      dplyr::inner_join(
        functions |>
          dplyr::filter(.data$estimate_name %in% c("count_missing", "percentage_missing")) |>
          dplyr::select("variable_name", "estimate_name", "estimate_type"),
        by = c("variable_name", "estimate_name")
      )
  } else {
    result <- NULL
  }
  return(result)
}

orderVariables <- function(res, cols, est) {
  if (length(est) == 0) {
    return(res)
  }
  orderVars <- dplyr::tibble("variable_name" = c(
    "number_records", "number_subjects", cols
  )) |>
    dplyr::mutate("id_variable" = dplyr::row_number())
  orderEst <- dplyr::tibble("estimate_name" = est) |>
    dplyr::mutate("id_estimate" = dplyr::row_number())
  res <- res |>
    dplyr::left_join(orderVars, by = c("variable_name")) |>
    dplyr::left_join(orderEst, by = c("estimate_name")) |>
    dplyr::arrange(.data$id_variable, .data$id_estimate, .data$variable_level) |>
    dplyr::select(-c("id_variable", "id_estimate"))
  return(res)
}
