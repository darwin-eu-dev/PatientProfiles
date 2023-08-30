# load packages ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(gt)
library(dplyr)

# read results from data folder ----
devtools::load_all()
cdm <- mockPatientProfiles()
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
)

getSettingsTibble <- function(columns, settings) {
  oldColumns <- columns
  oldSettings <- settings

  names <- names(settings)
  names(settings) <- NULL
  set <- lapply(seq_along(settings), function(x) {
    nam <- names[x]
    if (nam != "") {
      unique(columns[[nam]])
    } else {
      settings[x]
    }
  })
  names(set) <- names
  for (k in seq_along(settings)) {
    if (names[k] != "") {
      columns <- columns %>%
        tidyr::pivot_wider(
          names_from = dplyr::all_of(names[k]),
          values_from = dplyr::all_of(settings[k])
        )
    }
  }
  for (nam in names[names != ""]) {
    columns <- columns %>%
      dplyr::mutate(!!nam := as.character(NA))
  }

  attr(columns, "settings") <- set

}

pivotSettings <- function(summaryResult) {
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
      "median [min; q25 - q75; max]", "median [q25 - q75]", "[min - max]",
      "N" = "count"
    ),
    keepNotFromatted = TRUE
  )
  settings <- c(
    "cdm_name", "group_name" = "group_level", "strata_name" = "strata_level"
  )
  join <- c(settings, names(settings))
  names(join) <- NULL
  join <- join[join != ""]
  columns <- summaryResult %>%
    dplyr::select(dplyr::all_of(join)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(name = paste0("cohort", dplyr::row_number()))
  columns
  summaryResult <- summaryResult %>%
    dplyr::left_join(columns, by = join) %>%
    dplyr::select(
      "name", "variable", "variable_level", "format", "estimate"
    ) %>%
    tidyr::pivot_wider(names_from = "name", values_from = "estimate") %>%
    dplyr::left_join(order, by = "variable") %>%
    dplyr::arrange(.data$order, .data$variable_level) %>%
    dplyr::select(-"order") %>%
    dplyr::rename(
      "Variable" = "variable", "Level" = "variable_level", "Format" = "format"
    )
  summaryResult <- cleanResult(summaryResult)
  gtTable <- summaryResult %>%
    gt() %>%
    tab_style(
      style = list(cell_borders(
        sides = "right", color = "#000000", weight = px(1)
      )),
      locations = list(cells_body(columns = "Format"))
    )
  gtTable <- cleanBorders(gtTable, summaryResult)
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
            inputId = "summary_characteristics_variable",
            label = "Variables",
            choices = unique(summaryCharacteristics$variable),
            selected = unique(summaryCharacteristics$variable),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Pivot estimates",
            downloadButton(
              outputId = "summary_characteristics_download_estimates_word",
              label = "Download table as word"
            ),
            downloadButton(
              outputId = "summary_characteristics_download_estimates_html",
              label = "Download table as html"
            ),
            gt_output("summary_characteristics_table_estimates") %>% withSpinner()
          ),
          tabPanel(
            "Pivot settings",
            downloadButton(
              outputId = "summary_characteristics_download_settings_word",
              label = "Download table as word"
            ),
            downloadButton(
              outputId = "summary_characteristics_download_settings_html",
              label = "Download table as html"
            ),
            gt_output("summary_characteristics_table_settings") %>% withSpinner()
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
      filter(variable %in% input$summary_characteristics_variable)
  })
  ### get pivot settings table ----
  output$summary_characteristics_table_settings <- render_gt({
    summaryResult <- get_summary_characteristics_data()
    validate(need(nrow(summaryResult) > 0, "No results for selected inputs"))
    pivotSettings(summaryResult)
  })
  ### download pivot settings as html ----
  output$summary_characteristics_download_settings_html <- downloadHandler(
    filename = function() {
      "summaryCharacteristicsTable.html"
    },
    content = function(file) {
      x <- pivotSettings(get_summary_characteristics_data())
      gtsave(x, file)
    }
  )
  ### download pivot settings as word ----
  output$summary_characteristics_download_settings_word <- downloadHandler(
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
