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

#' Categorize a numeric variable
#'
#' @param x Table with individuals in the cdm.
#' @param variable Target variable that we want to categorize.
#' @param categories List of lists of named categories with lower and upper
#' limit.
#' @param missingCategoryValue Value to assign to those individuals not in
#' any named category. If NULL or NA, missing will values will be
#' given.
#' @param overlap TRUE if the categories given overlap.
#' @param name Name of the new table, if NULL a temporary table is returned.
#'
#' @return The x table with the categorical variable added.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' result <- cdm$cohort1 %>%
#'   addAge() %>%
#'   addCategories(
#'     variable = "age",
#'     categories = list("age_group" = list(
#'       "0 to 39" = c(0, 39), "40 to 79" = c(40, 79), "80 to 150" = c(80, 150)
#'     ))
#'   )
#' mockDisconnect(cdm = cdm)
#' }
addCategories <- function(x,
                          variable,
                          categories,
                          missingCategoryValue = "None",
                          overlap = FALSE,
                          name = NULL) {

  comp <- newTable(name)
  assertClass(x, "cdm_table")
  assertCharacter(variable, length = 1)
  if (!variable %in% colnames(x)) {
    cli::cli_abort("{variable} is not a column of x")
  }
  var <- dplyr::pull(utils::head(x, 1), variable)
  if (!inherits(var, "numeric") &
      !inherits(var, "integer") &
      !inherits(var, "Date")) {
    cli::cli_abort("{variable} must be a numeric or date variable")
  }
  assertList(categories, class = "list")
  assertCharacter(missingCategoryValue, length = 1, na = TRUE)

  if (length(unique(names(categories))) < length((names(categories)))) {
    cli::cli_abort(
      "Categories have repeated names, please rename the groups."
    )
  }

  if (is.null(names(categories))) {
    nam <- paste0("category_", seq_along(categories))
  } else {
    nam <- names(categories)
  }

  x <- warnOverwriteColumns(x, nameStyle = nam)

  if (
    utils::head(x, 1) %>%
      dplyr::pull(dplyr::all_of(variable)) %>%
      inherits("Date")
  ) {
    rand1 <- paste0("extra_", sample(letters, 5, TRUE) %>% paste0(collapse = ""))
    rand2 <- paste0("extra_", sample(letters, 6, TRUE) %>% paste0(collapse = ""))
    x <- x %>%
      dplyr::mutate(!!rand1 := as.Date("1970-01-01")) %>%
      dplyr::mutate(!!rand2 := !!CDMConnector::datediff(rand1, variable)) %>%
      dplyr::select(-dplyr::all_of(rand1))
    variable <- rand2
    categories <- lapply(categories, function(x) {
      lapply(x, function(y) {
        y <- as.numeric(y)
        y[is.na(y)] <- Inf
        return(y)
      })
    })
    date <- TRUE
  } else {
    date <- FALSE
  }

  categoryTibble <- list()
  for (k in seq_along(categories)) {
    categoryTibble[[nam[k]]] <- checkCategory(categories[[k]],
      overlap = overlap
    )
    if (date & is.null(names(categories[[k]]))) {
      categoryTibble[[nam[k]]] <- categoryTibble[[nam[k]]] %>%
        dplyr::mutate(category_label = paste(
          as.Date(.data$lower_bound, origin = "1970-01-01"), "to",
          as.Date(.data$lower_bound, origin = "1970-01-01")
        ))
    }
  }

  tablePrefix <- omopgenerics::tmpPrefix()

  for (k in seq_along(categories)) {
    categoryTibbleK <- categoryTibble[[k]]
    nm <- names(categoryTibble)[k]
    if (!overlap) {
      sqlCategories <- "#ELSE#"
      for (i in 1:nrow(categoryTibbleK)) {
        lower <- categoryTibbleK$lower_bound[i]
        upper <- categoryTibbleK$upper_bound[i]
        category <- categoryTibbleK$category_label[i]
        if (is.infinite(lower)) {
          if (is.infinite(upper)) {
            sqlCategories <- gsub(
              "#ELSE#", paste0("\"", category, "\""), sqlCategories
            )
            break
          } else {
            newSql <- paste0(
              "dplyr::if_else(.data[[variable]] <= ", upper, ", \"", category,
              "\" , #ELSE#)"
            )
          }
        } else {
          if (is.infinite(upper)) {
            newSql <- paste0(
              "dplyr::if_else(.data[[variable]] >= ", lower, ", \"", category,
              "\" , #ELSE#)"
            )
          } else {
            newSql <- paste0(
              "dplyr::if_else(.data[[variable]] >= ", lower,
              " & .data[[variable]] <= ", upper, ", \"", category,
              "\" , #ELSE#)"
            )
          }
        }
        sqlCategories <- gsub("#ELSE#", newSql, sqlCategories)
      }
      if (is.na(missingCategoryValue)) {
        sqlCategories <- gsub("#ELSE#", "NA_character_", sqlCategories)
      } else {
        sqlCategories <- gsub("#ELSE#", paste0("\"", ifelse(
          is.null(missingCategoryValue), NA, missingCategoryValue
        ), "\""), sqlCategories)
      }
      sqlCategories <- sqlCategories %>%
        rlang::parse_exprs() %>%
        rlang::set_names(glue::glue(nm))
      x <- x %>%
        dplyr::mutate(!!!sqlCategories)
    } else {
      x <- dplyr::mutate(x, !!nm := as.character(NA))
      for (i in 1:nrow(categoryTibbleK)) {
        lower <- categoryTibbleK$lower_bound[i]
        upper <- categoryTibbleK$upper_bound[i]
        category <- categoryTibbleK$category_label[i]
        x <- x %>%
          dplyr::mutate(!!nm := dplyr::if_else(
            is.na(.data[[nm]]) &
              .data[[variable]] >= .env$lower &
              .data[[variable]] <= .env$upper,
            .env$category,
            dplyr::if_else(
              !is.na(.data[[nm]]) &
                .data[[variable]] >= .env$lower &
                .data[[variable]] <= .env$upper,
              paste0(.data[[nm]], " and ", .env$category),
              .data[[nm]]
            )
          ))
      }
      # add missing as category
      if (!is.null(missingCategoryValue) && !is.na(missingCategoryValue)) {
        x <- x %>%
          dplyr::mutate(!!nm := dplyr::if_else(!is.na(.data[[nm]]),
            .data[[nm]],
            .env$missingCategoryValue
          ))
      }
    }

    x <- x %>%
      dplyr::compute(
        name = omopgenerics::uniqueTableName(tablePrefix), temporary = FALSE
      )
  }

  if (date) {
    x <- x %>% dplyr::select(-dplyr::all_of(variable))
  }

  x <- x |> dplyr::compute(name = comp$name, temporary = comp$temporary)

  cdm <- omopgenerics::cdmReference(x)
  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  return(x)
}
