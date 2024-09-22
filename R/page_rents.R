

page_rentsUI <- function(...) {
  
  shiny::fluidPage(
    
    # Overview - LGA rents
    "Weekly median rents by LGA" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),
    
    
    # Chart option selectors
    shinydashboard::box(
      title = "Explorer options",
      width = 4,
      collapsible = TRUE,
      shinyWidgets::multiInput( #widget for the LGA selection
        inputId = "vic_lgas",
        label = "Select Local Government Areas",
        choices = sort(lgas),
        selected = c("Kingston", "Merri-bek"),
        width = "100%",
        options = list(
          non_selected_header = "All LGAs:",
          selected_header = "Selected LGA:"
        )
      ) %>% fluidRow(),
      br(),
      shiny::fluidRow(
        style = "background-color: #FFFFFF; border-radius: 1rem;margin:1px;padding:10px;",
        shiny::column(
          12,
      #    
      # Data transformation
          shinyWidgets::radioGroupButtons(
            "rents_explorer_growth", #this name needs to match with server input name
            status = "secondary",
            label = "Data Transformation",
            choiceNames = c("Level", "Annual growth"),
            choiceValues = c(F,T),
            selected = TRUE,
            justified = T
          )
      )
      )#,
          # shinyWidgets::radioGroupButtons(
          #   inputId = "merch_explorer_facets",
          #   label = "Chart facet",
          #   choices = c(
          #     "Country" = "country_dest",
          #     "Good" = "sitc"
          #   ),
          #   selected = "country_dest",
          #   status = "secondary",
          #   justified = T
          # ),
          # shinyWidgets::radioGroupButtons(
          #   inputId = "merch_origin",
          #   label = "Export origin",
          #   choices = c(
          #     "Vic only" = "Victoria",
          #     "Australia"  = "Australia"
          #   ),
          #   selected = "Victoria",
          #   status = "secondary",
          #   justified = T
          # )
        #)
      #)
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
              highchartOutput("vic_rents", height = "600px"),
              type = 8,
              color = "#000000"
            )
          )
        )
      )
    )#,
    
    #footer()
    # Regions rents
    # "Weekly median rents by Regions" %>%
    #   h2() %>% div(class = "inner") %>%
    #   div(class = "small-box") %>% column(12, .) %>%
    #   fluidRow(),
    # 
    # 
    # # Chart option selectors
    # shinydashboard::box(
    #   title = "Explorer options",
    #   width = 4,
    #   collapsible = TRUE,
    #   shinyWidgets::multiInput( #widget for the LGA selection
    #     inputId = "vic_lgas",
    #     label = "Select Regions",
    #     choices = sort(lgas),
    #     selected = c("Kingston", "Merri-bek"),
    #     width = "100%",
    #     options = list(
    #       non_selected_header = "All LGAs:",
    #       selected_header = "Selected LGA:"
    #     )
    #   ) %>% fluidRow(),
    #   br(),
    #   shiny::fluidRow(
    #     style = "background-color: #FFFFFF; border-radius: 1rem;margin:1px;padding:10px;",
    #     shiny::column(
    #       12,
    #       #    
    #       # Data transformation
    #       shinyWidgets::radioGroupButtons(
    #         "rents_explorer_growth", #this name needs to match with server input name
    #         status = "secondary",
    #         label = "Data Transformation",
    #         choiceNames = c("Level", "Annual growth"),
    #         choiceValues = c(F,T),
    #         selected = TRUE,
    #         justified = T
    #       )
    #     )
    #   )
    # ) %>%
    #   tagAppendAttributes(
    #     style = "background:var(--twilight);",
    #     .cssSelector = ".box"
    #   ) %>%
    #   tagAppendAttributes(
    #     style = "padding:15px;",
    #     .cssSelector = ".box-body"
    #   ) %>%
    #   tagAppendAttributes(
    #     style = "font-size:20px;font-weight:bold;",
    #     .cssSelector = ".box-header h3"
    #   ),
    # #to_col_xl(),
    # column(
    #   8,
    #   div(
    #     class = "box",
    #     style = "padding:15px;",
    #     fluidRow(
    #       column(
    #         12,
    #         shinycssloaders::withSpinner(
    #           highchartOutput("vic_rents", height = "600px"),
    #           type = 8,
    #           color = "#000000"
    #         )
    #       )
    #     )
    #   )
    # )#
  )
}


#page_rentsUI()