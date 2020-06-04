library(shiny)
library(shinyWidgets)
library(DT)
library(tibble)
library(dplyr)
library(purrr)

choices <- rownames(mtcars) %>% head(10)

ui <- fluidPage(
  tags$h1('Smart dropdowns'),
  fluidRow(
    column(4,
           dropdownButton(
             label = 'All cars',
             circle = FALSE,
             status = "primary",
             inputId = 'dropdown',
             tagList(
               prettyCheckboxGroup(
                 inputId = "checkboxes",
                 label = NULL,
                 status = "primary",
                 choices = choices,
                 selected = choices
               ),
               fluidRow(
                 column(6, uiOutput('reset_button')),
                 column(6, uiOutput('save_button'))
               )
             )
           )
    ),
    column(8, DTOutput('table'))
  )
)

server <- function(input, output, session) {
  # Track saved choices
  saved_choices <- reactiveVal(value = choices)

  # Reset button rendering (only shown when there are differences to default)
  output$reset_button <- renderUI({
    if (!identical(choices, input$checkboxes)) {
      actionButton(
        inputId = 'reset',
        label = 'Reset'
      )
    }
  })

  # Functionality of reset button
  observeEvent(input$reset, updatePrettyCheckboxGroup(session, 'checkboxes', selected = choices))

  # Save button rendering
  output$save_button <- renderUI({
    status <- ifelse(identical(input$checkboxes, saved_choices()), 'light', 'primary')
    actionButton(
      inputId = 'save',
      label = 'Save',
      class = paste0('btn btn-', status)
    )
  })

  # Functionality of save button
  observeEvent(input$save, saved_choices(input$checkboxes))

  # Set label for dropdown depending on selected news types
  observeEvent(saved_choices(), {
    label <- saved_choices() %>%  when(
      all(choices %in% .) ~ 'all cars selected',
      length(.) == 0 ~ 'no cars selected',
      TRUE ~ paste(length(.), 'cars selected')
    )
    updateActionButton(session, inputId = 'dropdown', label = label)
  }, ignoreInit = FALSE, ignoreNULL = FALSE)

  # Save settings when dropdown is closed
  observeEvent(input$dropdown_state, {
    if (!input$dropdown_state) {
      if (!identical(saved_choices(), input$checkboxes)) {
        saved_choices(input$checkboxes)
      }
    }
  })

  output$table <- renderDT({
    mtcars %>%
      rownames_to_column(var = 'car') %>%
      filter(car %in% saved_choices()) %>%
      datatable()
  })
}

shinyApp(ui, server)
