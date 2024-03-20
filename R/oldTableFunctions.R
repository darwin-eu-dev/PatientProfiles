tableCohortOverlap  <- function(result,
                                # cohortNameReference = NULL,
                                # cohortNameComparator = NULL,
                                # strataName = "overall",
                                # strataLevel = "overall",
                                splitStrata = TRUE,
                                # cdmName = NULL,
                                # variableName = c("number records",
                                #                  "number subjects"),
                                type = "gt",
                                minCellCount = 5,
                                .options = list()) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "cohort_overlap")
  checkmate::assertChoice(type, c("gt", "flextable", "tibble"))
  # checkmate::assertCharacter(cohortNameReference, null.ok = TRUE)
  # checkmate::assertCharacter(cohortNameComparator, null.ok = TRUE)
  # checkmate::assertCharacter(strataName, null.ok = TRUE)
  # checkmate::assertCharacter(strataLevel, null.ok = TRUE)
  # checkmate::assertCharacter(cdmName, null.ok = TRUE)
  checkmate::assertLogical(splitStrata)
  # checkmate::assertCharacter(variableName, null.ok = TRUE)
  checkmate::assertList(.options)

  # split table
  x <- result |>
    visOmopResults::splitGroup()

  # check overlap:
  if (nrow(x |> dplyr::filter(cohort_name_reference != cohort_name_comparator)) == 0) {
    cli::cli_abort("There are no overlapping cohorts in the result object provided.")
  }

  # add default values
  # selectors <- defaultColumnSelectors(
  #   x, list("cohort_name_reference" = cohortNameReference, "cohort_name_comparator" = cohortNameComparator,
  #           "strata_name" = strataName, "cdm_name" = cdmName, "variable_name" = variableName))
  # cohortNameReference <- selectors$cohort_name_reference
  # cohortNameComparator <- selectors$cohort_name_comparator
  # strataName <- selectors$strata_name
  # cdmName <- selectors$cdm_name
  # if (is.null(strataLevel)) {
  #   strataLevel <- unique(x$strata_level[x$strata_name %in% strataName])
  # }
  .options <- defaultOverlapOptions(.options)

  # format table
  x <- x |>
    # dplyr::filter(.data$cdm_name %in% .env$cdmName) |>
    # dplyr::filter(.data$variable_name == .env$variableName) |>
    # dplyr::filter(.data$strata_name %in% .env$strataName) |>
    # dplyr::filter(.data$strata_level %in% .env$strataLevel) |>
    dplyr::mutate(estimate_value = as.numeric(.data$estimate_value)) |>
    getTidyOverlap() |>
    # dplyr::filter(.data$cohort_name_reference %in% .env$cohortNameReference) |>
    # dplyr::filter(.data$cohort_name_comparator %in% .env$cohortNameComparator) |>
    # to support header in visOmopResults 0.1.0
    dplyr::mutate(
      total = .data$reference + .data$comparator,
      reference = .data$reference - .data$overlap,
      comparator = .data$comparator - .data$overlap
    ) |>
    dplyr::mutate(
      only_in_reference = dplyr::if_else(
        (.data$reference < .env$minCellCount) | is.na(.data$reference),
        paste0("<", niceNum(.env$minCellCount, .options, "integer")),
        formatOverlapEstimate(.data$reference, .data$reference/.data$total * 100, .env$.options)
      ),
      only_in_comparator = dplyr::if_else(
        (.data$comparator < .env$minCellCount) | is.na(.data$comparator),
        paste0("<", niceNum(.env$minCellCount, .options, "integer")),
        formatOverlapEstimate(.data$comparator, .data$comparator/.data$total * 100, .env$.options)
      ),
      overlap = dplyr::if_else(
        (.data$overlap < .env$minCellCount) | is.na(.data$overlap),
        paste0("<",niceNum(.env$minCellCount, .options, "integer")),
        formatOverlapEstimate(.data$overlap, .data$overlap*2/.data$total * 100, .env$.options)),
      estimate_name = "N (%)"
    ) |>
    dplyr::select(c("cdm_name", "cohort_name_reference", "cohort_name_comparator",
                    "strata_name", "strata_level", "variable_name", "estimate_name",
                    "only_in_reference", "only_in_comparator", "overlap"))

  # split strata
  if (splitStrata) {
    x <- x |>
      visOmopResults::splitStrata()
  } else {
    x <- x |>
      dplyr::mutate(
        strata_name = stringr::str_to_sentence(gsub("&&&", "and", .data$strata_name)),
        strata_level = stringr::str_to_sentence(gsub("&&&", "and", .data$strata_level))
      )
  }

  # nice column names and values
  x <- x |>
    dplyr::mutate(
      cohort_name_reference = stringr::str_to_sentence(gsub("_", " ", .data$cohort_name_reference)),
      cohort_name_comparator = stringr::str_to_sentence(gsub("_", " ", .data$cohort_name_comparator)),
      variable_name = stringr::str_to_sentence(gsub("_", " ", .data$variable_name))
    ) |>
    dplyr::rename_with(~stringr::str_to_sentence(gsub("_", " ", gsub("&&&", "and", .x)))) |>
    dplyr::rename("CDM name" = "Cdm name")

  if (type == "gt") {
    x <- x |>
      visOmopResults::gtTable(
        style = .options$style,
        na = .options$na,
        title = .options$title,
        subtitle = .options$subtitle,
        caption = .options$caption,
        groupNameCol = .options$groupNameCol,
        groupNameAsColumn = .options$groupNameAsColumn,
        groupOrder = .options$groupOrder,
        colsToMergeRows = .options$colsToMergeRows
      )
  } else if (type == "flextable") {
    x <- x |>
      visOmopResults::fxTable(
        style = .options$style,
        na = .options$na,
        title = .options$title,
        subtitle = .options$subtitle,
        caption = .options$caption,
        groupNameCol = .options$groupNameCol,
        groupNameAsColumn = .options$groupNameAsColumn,
        groupOrder = .options$groupOrder,
        colsToMergeRows = .options$colsToMergeRows
      )
  }

  return(x)
}
