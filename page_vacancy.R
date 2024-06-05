page_vacancyUI <- function(...) {
  
  shiny::fluidPage(
    
    # Overview 
    "Residential Vacancy rates" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),
    
    
    # Chart option selectors
    shinydashboard::box(
      title = "Explorer options",
      width = 4,
      collapsible = TRUE,
      shinyWidgets::multiInput( #widget for the area selection
        inputId = "areas",
        label = "Select Area",
        choices = sort(areas),
        selected = c("Melbourne", "Melbourne City"),
        width = "100%",
        options = list(
          non_selected_header = "All regions:",
          selected_header = "Selected regions:"
        )
      ) %>% fluidRow(),
      br(),
      shiny::fluidRow(
        style = "background-color: #FFFFFF; border-radius: 1rem;margin:1px;padding:10px;",
        shiny::column(
          12,
          #    
          # Data transformation
          # shinyWidgets::radioGroupButtons(
          #   "rents_explorer_growth", #this name needs to match with server input name
          #   status = "secondary",
          #   label = "Data Transformation",
          #   choiceNames = c("Level", "Annual growth"),
          #   choiceValues = c(F,T),
          #   selected = TRUE,
          #   justified = T
          # )
        )
      )#,
      
    ) %>%
      tagAppendAttributes(
        style = "background:var(--twilight);",
        .cssSelector = ".box"
      ) %>%
      tagAppendAttributes(
        style = "padding:15px;",
        .cssSelector = ".box-body"
      ) %>%
      tagAppendAttributes(
        style = "font-size:20px;font-weight:bold;",
        .cssSelector = ".box-header h3"
      ),
    #to_col_xl(),
    column(
      8,
      div(
        class = "box",
        style = "padding:15px;",
        fluidRow(
          column(
            12,
            shinycssloaders::withSpinner(
              highchartOutput("vacancy", height = "600px"),
              type = 8,
              color = "#000000"
            )
          )
        )
      )
    )#,
    
    #footer()
  )
}
