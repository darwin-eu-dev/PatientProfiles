# load packages ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(gt)
library(dplyr)
library(DT)

# read results from data folder ----
devtools::load_all()
cdm <- DrugUtilisation::mockDrugUtilisation(numberIndividuals = 100)
cdm$cohort1 <- cdm$cohort1 %>% addSex()
summaryCharacteristics <- summariseCharacteristics(
  cohort = cdm$cohort1,
  strata = list("sex" = "sex"),
  ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)),
  tableIntersect = list(
    "Visits" = list(
      tableName = "visit_occurrence", value = "count", window = c(-365, 0)
     )
  ),
  cohortIntersect = list(
    "Medications" = list(
      targetCohortTable = "cohort2", value = "flag", window = c(-365, 0)
    )
  ),
  minCellCount = 1
) %>%
  mutate(group = paste0(.data$group_name, ": ", .data$group_level)) %>%
  mutate(strata = paste0(.data$strata_name, ": ", .data$strata_level)) %>%
  select(-c("group_name", "group_level", "strata_name", "strata_level")) %>%
  relocate("cdm_name", "group", "strata")

getWideTibble <- function(columns, wide) {
  tib <- columns %>% dplyr::select("name")
  count <- 1
  for (k in seq_along(wide)) {
    tib <- tib %>%
      dplyr::mutate(!!paste0("sep_", k) := names(wide)[k])
    cols <- wide[[k]]
    if ("name" %in% names(cols)) {
      cols <- columns %>%
        dplyr::select("name", dplyr::all_of(unname(cols))) %>%
        tidyr::pivot_wider(
          names_from = unname(cols["name"]), values_from = unname(cols["level"])
        )
      for (i in 2:length(cols)) {
        tib <- tib %>%
          dplyr::mutate(!!paste0("cat_", count) := colnames(cols)[i]) %>%
          dplyr::left_join(
            cols %>%
              dplyr::select(
                "name", !!paste0("lab_", count) := colnames(cols)[i]
              ),
            by = "name"
          )
        count <- count + 1
      }
    } else {
      tib <- tib %>%
        dplyr::left_join(
          columns %>%
            dplyr::select(
              "name", !!paste0("lab_", count) := unname(cols["level"])
            ),
          by = "name"
        )
      count <- count + 1
    }
  }
  return(tib)
}
arrangeLong <- function(summaryResult, long, wideTibble) {
  for (k in seq_along(long)) {
    x <- long[[k]]
    if ("name" %in% names(x)) {
      newCol <- names(long)[k]
      name <- unname(x["name"])
      level <- unname(x["level"])
      newName <- summaryResult %>%
        dplyr::select(dplyr::all_of(unname(x))) %>%
        dplyr::distinct() %>%
        dplyr::mutate(!!paste0("break_", newCol) := dplyr::if_else(
          is.na(.data[[level]]), as.character(NA), .data[[name]]
        )) %>%
        dplyr::mutate(!!newCol := dplyr::if_else(
          is.na(.data[[level]]), .data[[name]],
          paste0("     ", .data[[level]])
        ))
      summaryResult <- summaryResult %>%
        dplyr::left_join(newName, by = unname(x)) %>%
        dplyr::select(-dplyr::all_of(unname(x)))
    } else {
      summaryResult <- summaryResult %>%
        dplyr::rename(!!names(long)[k] := unname(x["level"]))
    }
  }
  summaryResult <- summaryResult %>%
    dplyr::select(
      dplyr::starts_with("break_"),
      dplyr::all_of(c(names(long), wideTibble$name))
    )
  return(summaryResult)
}
addBreaks <- function(summaryResult) {
  findOrderId <- function(summaryResult, lab, colNew) {
    min(summaryResult$order_id[summaryResult[[colNew]] == lab]) - 0.5
  }
  x <- colnames(summaryResult)
  x <- x[substr(x, 1, 6) == "break_"]
  for (col in x) {
    colNew <- substr(col, 7, nchar(col))
    labels <- summaryResult[[col]] %>% unique()
    labels <- labels[!is.na(labels)]
    summaryResult <- summaryResult %>%
      dplyr::mutate(order_id = dplyr::row_number())
    breakTibble <- dplyr::tibble(!!colNew := labels) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        order_id = findOrderId(summaryResult, .data[[colNew]], colNew)
      )
    summaryResult <- summaryResult %>%
      dplyr::union_all(breakTibble) %>%
      dplyr::arrange(.data$order_id) %>%
      dplyr::select(-"order_id")
  }
  return(summaryResult)
}
addWideLabels <- function(gtTable, wideTibble) {
  wideTibble[is.na(wideTibble)] <- "-"
  # style <- list(
  #   sep = list(gt::cell_fill(color = "#969696"), gt::cell_text(weight = "bold"))#c(background = "gray", join = TRUE),
  #   cat = c(background = "none", join = TRUE),
  #   lab = c(background = "none", join = TRUE)
  # )
  for (k in length(wideTibble):2) {
    x <- wideTibble[[k]]
    type <- substr(names(wideTibble)[k], 1, 3)
    ii <- 1
    spannerIds <- NULL
    for (i in unique(x)) {
      id <- bwlabel(as.numeric(x == i))
      for (j in seq_len(max(id))) {
        spannerId <- paste0(names(wideTibble)[k], "_", ii, "_", j)
        gtTable <- gtTable %>%
          gt::tab_spanner(
            label = i, columns = dplyr::all_of(wideTibble$name[id == j]),
            id = spannerId
          )
        spannerIds <- c(spannerIds, spannerId)
      }
      ii <- ii + 1
    }
    # if (type == "sep") {
    # } else if (type == "cat") {
    # } else if (type == "lab") {
    # }
    if (type == "sep") {
      gtTable <- gtTable %>%
        gt::tab_style(
          style = list(gt::cell_fill(color = "#c8c8c8"), gt::cell_text(weight = "bold")),
          locations = gt::cells_column_spanners(spanners = spannerIds)
        )
    } else if (type == "cat") {
      gtTable <- gtTable %>%
        gt::tab_style(
          style = gt::cell_fill(color = "#c8c8c8"),
          locations = gt::cells_column_spanners(spanners = spannerIds)
        )
    }
  }
  return(gtTable)
}
bwlabel <- function(x) {
  runs <- rle(x)
  labels <- rep(0, length(x))
  count <- 1
  for (i in 1:length(runs$lengths)) {
    if (runs$values[i] == 1) {
      labels[sum(runs$lengths[1:i]) - runs$lengths[i] + 1:runs$lengths[i]] <- count
      count <- count + 1
    }
  }
  return(labels)
}
pivotSettings <- function(summaryResult, toWide) {
  order <- summaryResult %>%
    dplyr::select("variable") %>%
    dplyr::distinct() %>%
    dplyr::mutate(order = dplyr::row_number())
  summaryResult <- formatNumbers(
    summaryResult, decimals = c(default = 0), decimalMark = ".", bigMark = ","
  )
  summaryResult <- tidyEstimates(
    summaryResult,
    format = c(
      "N (%)" = "count (percentage%)", "mean (sd)",
      "median [min; q25 - q75; max]",
      "median [q25 - q75]", "[min - max]",
      "N" = "count"
    ),
    keepNotFromatted = TRUE
  )
  long = list(
    #"Variable" = c(name = "variable", level = "variable_level"),
    "Variable" = c(level = "variable"),
    "Level" = c(level = "variable_level"),
    "Format" = c(level = "format")
  )
  wide = list(
    "CDM name" = c(level = "cdm_name"),
    "Group" = c(level = "group"),
    "Strata" = c(level = "strata")
  )
  getColumns <- function(elements) {
    lapply(elements, unname) %>%
      unlist() %>%
      unname()
  }
  join <- getColumns(wide)
  columns <- summaryResult %>%
    dplyr::select(dplyr::all_of(join)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(name = paste0("cohort", dplyr::row_number()))
  wideTibble <- getWideTibble(columns, wide)
  summaryResult <- summaryResult %>%
    dplyr::left_join(columns, by = join) %>%
    dplyr::select("name", dplyr::all_of(getColumns(long)), "estimate") %>%
    tidyr::pivot_wider(names_from = "name", values_from = "estimate") %>%
    dplyr::left_join(order, by = "variable") %>%
    dplyr::arrange(.data$order, .data$variable_level) %>%
    dplyr::select(-"order")
  summaryResult <- arrangeLong(summaryResult, long, wideTibble)
  summaryResult <- addBreaks(summaryResult)
  summaryResult <- cleanResult(summaryResult, c("Variable", "Level"))
  gtTable <- summaryResult %>%
    gt() %>%
    tab_style(
      style = list(cell_borders(
        sides = "right", color = "#000000", weight = px(1)
      )),
      locations = list(cells_body(columns = "Format"))
    ) %>%
    addWideLabels(wideTibble)
  gtTable <- cleanBorders(gtTable, summaryResult) %>%
    cols_width(
      # num ~ px(150),
      # ends_with("r") ~ px(100),
      # starts_with("cohort") ~ px(200),
      Variable ~ px(250),
      Level ~ px(100),
      Format ~ px(250),
      everything() ~ px(200)
    )
  gtTable
}

# ui shiny ----
ui <- dashboardPage(
  dashboardHeader(title = "Study title"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Summary characteristics", tabName = "summary_characteristics")
    )
  ),
  ## body ----
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "summary_characteristics",
        h3("Summary characteristics"),
        downloadButton(
          outputId = "summary_characteristics_download_raw",
          label = "Download raw data"
        ),
        br(),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "summary_characteristics_cdm_name",
            label = "CDM name",
            choices = unique(summaryCharacteristics$cdm_name),
            selected = unique(summaryCharacteristics$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "summary_characteristics_group",
            label = "Group",
            choices = unique(summaryCharacteristics$group),
            selected = unique(summaryCharacteristics$group),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "summary_characteristics_strata",
            label = "Strata",
            choices = unique(summaryCharacteristics$strata),
            selected = unique(summaryCharacteristics$strata),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "summary_characteristics_variable",
            label = "Variables",
            choices = unique(summaryCharacteristics$variable),
            selected = unique(summaryCharacteristics$variable),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "summary_characteristics_estimate_type",
            label = "Estimate type",
            choices = unique(summaryCharacteristics$estimate_type),
            selected = unique(summaryCharacteristics$estimate_type),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            downloadButton(
              outputId = "summary_characteristics_download_raw_filtered",
              label = "Download table as csv"
            ),
            DTOutput("summary_characteristics_table_raw") %>% withSpinner()
          ),
          tabPanel(
            "Tidy table",
            downloadButton(
              outputId = "summary_characteristics_download_tidy_word",
              label = "Download table as word"
            ),
            gt_output("summary_characteristics_table_tidy") %>% withSpinner()
          )
        )
      )
    )
  )
)

# server shiny ----
server <- function(input, output, session) {
  ## summary characteristics ----
  ### get data ----
  get_summary_characteristics_data <- reactive({
    summaryCharacteristics %>%
      filter(variable %in% input$summary_characteristics_variable) %>%
      filter(cdm_name %in% input$summary_characteristics_cdm_name) %>%
      filter(estimate_type %in% input$summary_characteristics_estimate_type) %>%
      filter(group %in% input$summary_characteristics_group) %>%
      filter(strata %in% input$summary_characteristics_strata)
  })
  ### get raw table ----
  output$summary_characteristics_table_raw <- renderDataTable({
    summaryResult <- get_summary_characteristics_data()
    validate(need(nrow(summaryResult) > 0, "No results for selected inputs"))
    datatable(
      summaryResult %>% select(-"result_type"),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  ### get tidy table ----
  output$summary_characteristics_table_tidy <- render_gt({
    summaryResult <- get_summary_characteristics_data()
    validate(need(nrow(summaryResult) > 0, "No results for selected inputs"))
    print(summaryResult)
    pivotSettings(summaryResult)
  })
  ### download raw data ----
  output$summary_characteristics_download_raw <- downloadHandler(
    filename = function() {
      "summaryCharacteristicsTable.csv"
    },
    content = function(file) {
      write.csv(summaryCharacteristics, file, row.names = FALSE)
    }
  )
  ### download raw data filtered ----
  output$summary_characteristics_download_raw_filtered <- downloadHandler(
    filename = function() {
      "summaryCharacteristicsTableFiltered.csv"
    },
    content = function(file) {
      write.csv(get_summary_characteristics_data(), file, row.names = FALSE)
    }
  )
  ### download tidy table as word ----
  output$summary_characteristics_download_tidy_word <- downloadHandler(
    filename = function() {
      "summaryCharacteristicsTable.docx"
    },
    content = function(file) {
      x <- pivotSettings(get_summary_characteristics_data())
      gtsave(x, file)
    }
  )
}

# run shiny ----
shinyApp(ui, server)

#  MAIN HEADER
# .skin-blue .main-header .logo {
#   background-color: #004DFF;
# }
# .skin-blue .main-header .logo:hover {
#   background-color: #004DFF;
# }

# MAIN SIDEBAR
# .skin-blue .main-sidebar {
#   background-color: #004DFF;
# }
