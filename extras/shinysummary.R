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
