# load packages ----
library(PatientProfiles)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(gt)
library(dplyr)

# read results from data folder ----
cdm <- mockPatientProfiles()
summaryCharacteristics <- summariseCharacteristics(
  cohort = cdm$cohort1,
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
  ### pivot settings ----
  output$summary_characteristics_table_settings <- render_gt({
    data <- get_summary_characteristics_data()
    validate(need(nrow(data) > 0, "No results for selected inputs"))
    gt(data)
  })
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
