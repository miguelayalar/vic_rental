
# load libraries ----
library(shiny)
library(shinydashboard)
library(highcharter)


# load resources ----
source("R/page_rents.R")
source("R/page_vacancy.R")
source("R/load_data.R")
source("R/viz_vic_rents.R")
source("R/viz_vic_vacancies.R")
source("R/theme.R")


# UI for application ----
ui <- function() {
  
  custom_header <- shinydashboard::dashboardHeader()
  custom_header$children[[2]] <- NULL
  custom_header <- custom_header %>%
    tagAppendChildren(
      .cssSelector = ".navbar-custom-menu",
      fluidRow(
        div(class = "col", span(HTML("Latest</br>Rents data"), class = "badgeDescript"))
        #div(class = "col", badge_bop()),
        #div(class = "col", badge_goods()),
        #div(class = "col", badge_services())
      )
    )
  
  
  shinydashboard::dashboardPage(
    title = "Victorian Housing Dashboard",
    header = custom_header,
    sidebar = shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(id = 'tabs',
                                  shinydashboard::menuItem("Rents", tabName = "vic_rents", selected = TRUE), #%>% 
                                  shinydashboard::menuItem("Vacancies", tabName = "vacancy"), 
                                  shinydashboard::menuItem("Vacancies", tabName = "vacancy") %>%
                                  #shinydashboard::menuItem("Balance of Payments", tabName = "bop"),
                                  #shinydashboard::menuItem("FAQ & sources", tabName = "methodology"),
                                  #shinydashboard::menuItem("Disclaimer", tabName = "disclaimer") %>%
                                    shiny::tagAppendAttributes(
                                      style = "display:none;"
                                    )
      ),
      width = "250px"
    ),
    body = shinydashboard::dashboardBody(
      tags$head( #adding stylng using css
        # Bootstrap 5 for xl columns
        tags$link(
          href = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
          rel = "stylesheet",
          integrity = "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
          crossorigin = "anonymous"
        ),
        # Gloabl vic css
        tags$link(href = "globalvic.css", rel = "stylesheet"),
        # Launchpad links (client side execution)
        #shiny::tags$script("$(document).ready(function(){$('a.launchpadLink').click(function(){$('.sidebar-menu a[data-value=\"launchpad\"]').trigger('click');})});"),
        #shiny::tags$script("$(document).ready(function(){$('a.rentsLink').click(function(){$('.sidebar-menu a[data-value=\"vic_rents\"]').trigger('click');})});"),
        #shiny::tags$script("$(document).ready(function(){$('a.bopLink').click(function(){$('.sidebar-menu a[data-value=\"bop\"]').trigger('click');})});"),
        #shiny::tags$script("$(document).ready(function(){$('a.servicesLink').click(function(){$('.sidebar-menu a[data-value=\"services\"]').trigger('click');})});"),
        #shiny::tags$script("$(document).ready(function(){$('a.methodologyLink').click(function(){$('.sidebar-menu a[data-value=\"methodology\"]').trigger('click');})});"),
        #shiny::tags$script("$(document).ready(function(){$('a.disclaimerLink').click(function(){$('.sidebar-menu a[data-value=\"disclaimer\"]').trigger('click');})});"),
        # Resize highcharts on sidebar collapse
        shiny::tags$script(src = "sidebar_chart_resize.JS")
      ),
      shiny::tags$script("$('html').attr(\"lang\", \"en\")"),
      shiny::tags$script("$('section.content').attr(\"role\", \"main\")"),
      shinydashboard::tabItems(
        #shinydashboard::tabItem("launchpad", page_launchpadUI('lp')),
        shinydashboard::tabItem("vic_rents", page_rentsUI()),
        shinydashboard::tabItem("vacancy", page_vacancyUI())
        #shinydashboard::tabItem("services", page_servicesUI()),
        #shinydashboard::tabItem("methodology", page_methodology()),
        #shinydashboard::tabItem("disclaimer", page_disclaimerUI())
      )
    )
  )
}






server <- function(input, output, session) {
  
  load_data() %>% dplyr::distinct(lga)  %>%
    dplyr::pull(lga) %>%
    assign("lgas", ., envir = .GlobalEnv)
  
  #set_hcharts_options()
  
  
  output$vic_rents <- renderHighchart({
    highcharts_vic_lga_rents(
      lgas = req(input$vic_lgas),
      growth = input$rents_explorer_growth
    )
  })
  
  output$vacancy <- renderHighchart({
    highcharts_vacancy(
      regions = req(input$areas)
    )
  })
  
}



# # Run the application 
shinyApp(ui = ui, server = server)
 