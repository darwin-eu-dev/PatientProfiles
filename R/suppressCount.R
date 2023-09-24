#' Function to suppress counts in summarised objects
#'
#' @param result Result object
#' @param minCellCount Minimum count of records to report results.
#' @param variable Columns to suppress counts.
#' @param estimateType Types to suppress counts.
#' @param group Columns to group by and supress.
#' @param groupCount Identifier of the group count.
#'
#' @return Table with suppressed counts
#'
#' @export
suppressCount <- function(result,
                          minCellCount = 5,
                          variable = "estimate",
                          estimateType = "count",
                          group = c(
                            "group_name", "group_level", "strata_name",
                            "strata_level"
                          ),
                          groupCount = c("number subjects", "number records"),
                          linkEstimates = list("count" = "percentage")) {
  # initial checks
  checkmate::assertTibble(result)
  checkSuppressCellCount(minCellCount)
  checkmate::assertCharacter(variable, any.missing = FALSE, min.len = 1)
  checkmate::assertCharacter(estimateType, any.missing = FALSE, null.ok = TRUE)
  checkmate::assertCharacter(group, any.missing = FALSE, null.ok = TRUE)
  checkmate::assertCharacter(groupCount, any.missing = FALSE, null.ok = TRUE)
  checkmate::assertTRUE(all(c(variable, group) %in% colnames(result)))
  checkmate::assertList(linkEstimates, any.missing = FALSE)
  checkmate::assertTRUE(length(linkEstimates) == length(names(linkEstimates)))

  # loop for different columns
  for (col in variable) {
    result <- result %>%
      # as numeric
      dplyr::mutate(!!col := suppressWarnings(as.numeric(.data[[col]]))) %>%
      # obscure groups flag
      obscureGroups(minCellCount, col, estimateType, group, groupCount) %>%
      # obscure records
      obscureRecords(minCellCount, col, estimateType) %>%
      # obscure linked
      obscureLinked(linkEstimates, variable) %>%
      # obscure col
      obscureColumn(col, minCellCount, groupCount, linkEstimates)
  }

  return(result)
}

filterData <- function(result, variable, estimateType, minCellCount) {
  if (!is.null(estimateType)) {
    result <- result %>%
      dplyr::filter(.data$estimate_type %in% .env$estimateType)
  }
  result <- result %>%
    dplyr::filter(
      .data[[variable]] < .env$minCellCount & .data[[variable]] > 0
    )
  return(result)
}
obscureGroups <- function(result, minCellCount, variable, estimateType, group, groupCount) {
  if (!is.null(group)) {
    groupsToObscure <- result %>%
      dplyr::select(dplyr::all_of(c(group, variable))) %>%
      dplyr::filter(.data$variable %in% .env$groupCount) %>%
      filterData(variable, estimateType, minCellCount) %>%
      dplyr::select(dplyr::all_of(group)) %>%
      dplyr::distinct() %>%
      dplyr::mutate(obscure_group = 1)
    result <- result %>%
      dplyr::left_join(groupsToObscure, by = group) %>%
      dplyr::mutate(
        obscure_group = dplyr::if_else(is.na(.data$obscure_group, 0, 1))
      )
  } else {
    result <- result %>% dplyr::mutate(obscure_group = 0)
  }
  return(result)
}
obscureRecords <- function(result, minCellCount, variable, estimateType) {
  recordsToObscure <- filterData(variable, estimateType, minCellCount) %>%
    dplyr::mutate(obscure_record = 1)
  result <- result %>%
    dplyr::left_join(recordsToObscure, by = colnames(result)) %>%
    dplyr::mutate(
      obscure_record = dplyr::if_else(is.na(.data$obscure_record, 0, 1))
    )
  return(result)
}
obscureLinked <- function(result, linkEstimates, variable) {
  cols <- colnames(result)
  cols <- cols[!(cols %in% variable)]
  result <- result %>%
    dplyr::mutate(obscure_linked = 0)
  for (k in seq_alon(linkEstimates)) {
    result <- result %>%
      dplyr::left_join(
        result %>%
          dplyr::filter(
            .data$estimate_type == names(linkEstimates)[k],
            .data$obscure_record == 1,
            .data$obscure_group != 1
          ) %>%
          dplyr::inner_join(dplyr::tibble(
            estimate_type = names(linkEstimates)[k],
            new_estimate_type = linkEstimates[[k]]
          )) %>%
          dplyr::select(-dplyr::all_of(c("estimate_type", variable))) %>%
          dplyr::rename("estimate_type" = "new_estimate_type") %>%
          dplyr::mutate(obscure_linked_k = 1),
        by = cols
      ) %>%
      dplyr::mutate(obscure_linked = dplyr::if_else(
        .data$obscure_linked_k == 1, 1, .data$obscure_linked
      )) %>%
      dplyr::select(-"obscure_linked_k")
  }
  return(result)
}
obscureColumn <- function(result, col, minCellCount, groupCount, linkEstimates) {
  minCellCount <- paste0("<", minCellCount)
  result <- result %>%
  dplyr::mutate(
    !!col := dplyr::if_else(
      .data$obscure_group == 1,
      dplyr::if_else(
        .data$variable %in% .env$groupCount,
        .env$minCellCount,
        as.character(NA)
      ),
      dplyr::if_else(
        .data$obscure_record == 1,
        .env$minCellCount,
        dplyr::if_else(
          .data$obscure_linked == 1,
          as.character(NA),
          as.character(.data[[col]])
        )
      )
    )
  ) %>%
    dplyr::select(-c("obscure_record", "obscure_group", "obscure_linked"))
  return(result)
}
