library(tidyverse)
library(sf)
library(shiny)
library(shinythemes)
library(tigris)
library(bslib)
library(ggiraph)

ui <- fluidPage(
  theme = shinytheme("darkly"),
  navbarPage(title = "Federal Funding in Rural Schools",
             tabPanel(title = "School Funding Problems",
                      tags$div(style = "text-align:justify",
                               tags$h4("Introduction"),
                               tags$p("Rural school districts are tasked with overcoming systemic deficiencies in infrastructure, access to
government services, and high poverty rates."),
                               tags$p("Grants and other programs from the federal government have the potential to act as an equalizer between
districts situated in different contexts."),
                               tags$h4("Defining Rurality"),
                               tags$p("There is a lack of attention paid to rural education from policymakers and scholars leading to unequal and
inequitable material conditions for rural school districts and their communities."),
                               tags$p("Education policy programs, like the Rural Low Income School (RLIS) program and Small, Rural School
Achievement (SRSA) programs exist as the result of rural school districts failing to meet policy goals and
achievement guidelines established for all public schools, rather than a coherent policy vision specifically
for rural schools."),
                               HTML("<a href='https://doi.org/10.1016/j.jrurstud.2022.12.025'><p>Researchers have argued</a> that the school classifications are too narrowly focused on population
density and proximity distance to metropolitan areas, failing to consider the services and amenities
available within a community. These authors propose policymakers and researchers redefine rurality to include community measures
such as environmental, social, and economic resources.</p>"))),
             tabPanel(title = "Federal Funding Across Pennsylvania",
                      sidebarLayout(
                        sidebarPanel(checkboxGroupInput("show_locales_input",
                                                        "Select which districts to show:",
                                                        choiceNames = c("Rural", "Town", "Suburban", "Urban"),
                                                        choiceValues = c("Rural", "Town", "Suburban", "Urban"),
                                                        selected = "Rural")),
                        mainPanel(navset_card_underline(nav_panel("Nutrition Funding",
                                                                  tags$p("Federal nutrition funding in schools comes from 
                                                                         two acts: the National School Lunch Act and the
                                                                         Child Nutrition Act."),
                                                                  plotOutput("child_nutr_plot")),
                                                        nav_panel("Career & Technical Education", 
                                                                  tags$p("Career and technical education funding comes
                                                                         from the Carl D. Perkins Career and Technical 
                                                                         Education Act."),
                                                                  plotOutput("cte_plot")),
                                                        nav_panel("IDEA Funding", 
                                                                  tags$p("A significant amount of federal special education
                                                                         funding comes from the Individuals with Disabilities
                                                                         Education Act (IDEA)."),
                                                                  plotOutput("idea_plot")),
                                                        nav_panel("Title I Funding",
                                                                  tags$p("A significant amount of federal funding to assist
                                                                         with students from families with low income comes
                                                                         from Title I of the Elementary and Secondary
                                                                         Education Act (ESEA) and its subsequent reauthorizations."),
                                                                  plotOutput("title_i_plot"))))
                      )),
             tabPanel(title = "School District Locator"))
)

thematic::thematic_shiny(font = "auto")

server <- function(input, output) {
  school_districts <- st_read("Shiny Data//school_districts.shp")
  fed_rev_shiny <- read_rds("Shiny Data//fed_rev_shiny.rds") %>%
    merge(., school_districts, by.x = "LEAID", by.y = "GEOID") %>%
    st_as_sf() %>%
    st_transform(., crs = 32129)
    
  
  statewide_data <- reactive({
    fed_rev_shiny %>%
      filter(locale %in% input$show_locales_input)
      })
  
  output$child_nutr_plot <- renderPlot({
    statewide_data() %>%
      select(child_nutr, Total, geometry) %>%
      mutate("Nutrition Funding per Student" = child_nutr/Total) %>%
      mutate(`Nutrition Funding per Student` = ifelse(`Nutrition Funding per Student` > 0 &
                                                  !is.infinite(`Nutrition Funding per Student`) &
                                                  !is.na(`Nutrition Funding per Student`),
                                                `Nutrition Funding per Student`,
                                                0)) %>%
      ggplot() +
      geom_sf(aes(fill = `Nutrition Funding per Student`), color = "#222222", linewidth = 0.1) +
      guides(fill = guide_legend(byrow = TRUE)) +
      scale_size_continuous(breaks = c(0, 300, 600, 900, 1200, 1500)) +
      theme_void() +
      theme(text = element_text(color = "#FFFFFF"),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 20))
  })
  
  output$cte_plot <- renderPlot({
    statewide_data() %>%
      select(cte, Total, geometry) %>%
      mutate("CTE Funding per Student" = cte/Total) %>%
      mutate(`CTE Funding per Student` = ifelse(`CTE Funding per Student` > 0 &
                                                   !is.infinite(`CTE Funding per Student`) &
                                                   !is.na(`CTE Funding per Student`),
                                                 `CTE Funding per Student`,
                                                 0)) %>%
      ggplot() +
      geom_sf(aes(fill = `CTE Funding per Student`), color = "#222222", linewidth = 0.1) +
      guides(fill = guide_legend(byrow = TRUE)) +
      #scale_size_continuous(breaks = c(0, 15000, 30000, 450000, 60000)) +
      theme_void() +
      theme(text = element_text(color = "#FFFFFF"),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 20))
  })
  
  output$idea_plot <- renderPlot({
    statewide_data() %>%
      select(idea, Total, geometry) %>%
      mutate("IDEA Funding per Student" = idea/Total) %>%
      mutate(`IDEA Funding per Student` = ifelse(`IDEA Funding per Student` > 0 &
                                                   !is.infinite(`IDEA Funding per Student`) &
                                                   !is.na(`IDEA Funding per Student`),
                                                 `IDEA Funding per Student`,
                                                 0)) %>%
      ggplot() +
      geom_sf(aes(fill = `IDEA Funding per Student`), color = "#222222", linewidth = 0.1) +
      guides(fill = guide_legend(byrow = TRUE)) +
      #scale_size_continuous(breaks = c(0, 5000, 10000, 150000, 20000)) +
      theme_void() +
      theme(text = element_text(color = "#FFFFFF"),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 20),
            legend.spacing.y = unit(1000, "cm"))
  })
  
  output$title_i_plot <- renderPlot({
    statewide_data() %>%
      select(title_i, Total, geometry) %>%
      mutate("Title I Funding per Student" = title_i/Total) %>%
      mutate(`Title I Funding per Student` = ifelse(`Title I Funding per Student` > 0 &
                                                   !is.infinite(`Title I Funding per Student`) &
                                                   !is.na(`Title I Funding per Student`),
                                                 `Title I Funding per Student`,
                                                 0)) %>%
      ggplot() +
      geom_sf(aes(fill = `Title I Funding per Student`), color = "#222222", linewidth = 0.1) +
      guides(fill = guide_legend(byrow = TRUE)) +
      #scale_size_continuous(breaks = c(0, 5000, 10000, 150000, 20000)) +
      theme_void() +
      theme(text = element_text(color = "#FFFFFF"),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 20),
            legend.spacing.y = unit(1000, "cm"))
  })
}

shinyApp(ui = ui, server = server)