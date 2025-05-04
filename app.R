library(tidyverse)
library(sf)
library(shiny)
library(shinythemes)
library(tigris)
library(bslib)
library(ggiraph)
library(formattable)

ui <- fluidPage(
  tags$style('.p {text-align: justify;}',
             '.h4 {text-align: justify;}'),
  theme = shinytheme("darkly"),
  navbarPage(title = "Federal Funding in Rural Schools",
             tabPanel(title = "School Funding Problems",
                      tags$div(style = "text-align:justify",
                               tags$h3("Introduction"),
                               tags$p("Rural school districts are tasked with overcoming systemic deficiencies in infrastructure, access to
government services, and high poverty rates."),
                               tags$p("Grants and other programs from the federal government have the potential to act as an equalizer between
districts situated in different contexts."),
                               tags$h3("Defining Rurality"),
                               tags$p("There is a lack of attention paid to rural education from policymakers and scholars leading to unequal and
inequitable material conditions for rural school districts and their communities."),
                               tags$p("Education policy programs, like the Rural Low Income School (RLIS) program and Small, Rural School
Achievement (SRSA) programs exist as the result of rural school districts failing to meet policy goals and
achievement guidelines established for all public schools, rather than a coherent policy vision specifically
for rural schools."),
                               HTML("<a href='https://doi.org/10.1016/j.jrurstud.2022.12.025'><p>Researchers have argued</a> that the school classifications are too narrowly focused on population
density and proximity distance to metropolitan areas, failing to consider the services and amenities
available within a community. These authors propose policymakers and researchers redefine rurality to include community measures
such as environmental, social, and economic resources.</p>"),
                               tags$h3("Demography and the Role of Community in Rural School Districts"),
                               tags$p("Demographers have historically shown how educational attainment contributes to and causes out-
migration for rural communities."),
                               HTML("<a href='https://psycnet.apa.org/doi/10.1080/0161956X.2016.1151734'>Researchers have demonstrated</a><p>
                                    this relationship in regard to out-migration in rural school districts by showing how students of low-income backgrounds were more likely to leave their communities.
Rural schools can decrease out- migration by connecting students with their community through job shadow opportunities, apprenticeships, and school-to-work programs. Place-based education that focuses on 
                                    addressing local needs with academic discipline also connects students with their communities. This approach promotes sustainable community development"),
                               tags$h3("Equity of Federal Education Policies"),
                               tags$p("Title 1 funding distribution criteria disproportionately benefits urban school districts with larger numbers of low-income students more closely concentrated geographically."),
                               HTML("<a href='https://eric.ed.gov/?id=EJ1048755'></a><p>found that rural districts consisting of lower staff numbers struggled to find time to complete compliance paperwork related to the Elementary and Secondary Education Act, another federal education funding policy.</p>"),
                               tags$h3("Importance to Civil Rights"),
                               tags$p("All federal funding ties to civil rights through the federal government’s enforcement of civil rights protections for students and staff; disregard or unwillingness to comply with civil rights protections eventually leads to loss of federal funding.
A child’s ability to learn in an inclusive and safe environment should be an “accident of geography”; that is, students should have access to a quality education they rightfully deserve regardless of where they are born and where they are living while going to school."))),
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
             tabPanel(title = "School District Locator",
                      sidebarLayout(
                        sidebarPanel(selectInput(inputId = "school_district",
                                                 label = "Select a school district:",
                                                 choices = read_rds("Shiny Data//fed_rev_shiny.rds") %>%
                                                   pull(district_name),
                                                 selected = "Central Valley SD")),
                        mainPanel(tags$h3("School Demographics"),
                                  tags$h4("Racial Composition"),
                                  plotOutput("race_demo_plot", height = "150px"),
                                  tags$p(textOutput("district")),
                                  tags$h4("Poverty Rate"),
                                  plotOutput("pov_comp", height = "150px"),
                                  tags$h3("Federal Funding"),
                                  tags$p(" "),
                                  formattableOutput("fed_rev_table"),
                                  tags$p(" "),
                                  plotOutput("fed_rev_comp")))),
             tabPanel(title = "About",
                      tags$h3("Authors"),
                      tags$p("This tool was built by Michael Cattell and Scott Levengood."),
                      tags$h3("Data"),
                      tags$p("Data for this tool was sourced from different sources.
                             Finance data came from the National Center for Education Statistics's
                             School District Finance Survey. Enrollment data came from the National
                             Center for Education Statistics's Common Core of Data. Poverty data came
                             from the U.S. Census Bureau's Small Area Income and Poverty Estimates (SAIPE).")))
)

thematic::thematic_shiny(font = "auto")

server <- function(input, output) {
  
  ### Data loading
  
  school_districts <- st_read("Shiny Data//school_districts.shp")
  fed_rev_shiny <- read_rds("Shiny Data//fed_rev_shiny.rds") %>%
    merge(., school_districts, by.x = "LEAID", by.y = "GEOID") %>%
    st_as_sf() %>%
    st_transform(., crs = 32129)
  
  sta_avg_race <- fed_rev_shiny %>%
    group_by(race) %>%
    reframe("pct_enr" = sum(enrollment) / sum(Total))
  
  sta_avg_pov_value <- read_csv("Shiny Data//poverty_data_2021_22.csv") %>%
    filter(state == "PA") %>%
    mutate(LEAID = paste(42, LEAID, sep = "")) %>%
    reframe("young_pov_pct" = sum(young_pov_pop) / sum(young_pop)) %>%
    pull(young_pov_pct)
  
  
  sta_avg_pov_text <- read_csv("Shiny Data//poverty_data_2021_22.csv") %>%
    filter(state == "PA") %>%
    mutate(LEAID = paste(42, LEAID, sep = "")) %>%
    reframe("young_pov_pct" = round(sum(young_pov_pop) / sum(young_pop) * 100, digits = 2)) %>%
    paste0(., "%")
  
  statewide_data <- reactive({
    fed_rev_shiny %>%
      filter(locale %in% input$show_locales_input)
      })
  
  sta_rev_avg <- fed_rev_shiny %>%
    pivot_longer(cols = c(title_i, idea, cte, child_nutr)) %>%
    group_by(name) %>%
    reframe("med_rev" = mean(value)/mean(Total))
  
  ### Statewide plots
  
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
  
  ### District Locator
  
  output$race_demo_plot <- renderPlot({
    fed_rev_shiny %>%
      filter(district_name == input$school_district) %>%
      select(race, enrollment, Total) %>%
      reframe("pct_enr" = enrollment/Total,
              "race" = race,
              "Total" = Total,
              "Other" = Total - sum(enrollment)) %>%
      pivot_wider(names_from = "race", values_from = "pct_enr") %>%
      mutate(Other = Other / Total) %>%
      mutate(year = 1234) %>%
      pivot_longer(cols = c("Black", "Asian", "Hispanic", "White", "Other")) %>%
      select(year, name, value) %>%
      ggplot(aes(x = year, y = value, fill = name)) +
      geom_col() +
      coord_flip() +
      labs(y = "% Enrollment") +
      scale_y_continuous(labels = scales::percent) +
      guides(fill = guide_legend(title = "Race")) +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_text(size = 20),
            axis.text.x = element_text(size = 12),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 16))
  })

    output$district <- renderText({paste(input$school_district, "has",
                                         fed_rev_shiny %>%
                                           filter(district_name == input$school_district) %>%
                                           pull(Total) %>% max(), "students enrolled in their schools.")})
    
    output$pov_comp <- renderPlot({
      pov_value <- read_csv("Shiny Data//poverty_data_2021_22.csv") %>%
        filter(state == "PA") %>%
        mutate(LEAID = paste(42, LEAID, sep = "")) %>%
        mutate("young_pov_pct" = (young_pov_pop) / (young_pop)) %>%
        merge(., fed_rev_shiny, by = "LEAID") %>%
        filter(district_name == input$school_district) %>%
        pull(young_pov_pct) %>%
        max()
      
      pov_df <- data_frame("value" = c(pov_value, sta_avg_pov_value),
                           "null" = c(5, 5),
                           "empty" = c(input$school_district, "State Average"))
      
      ggplot(pov_df, aes(x = value, y = null, label = str_wrap(empty, width = 15))) +
        geom_point(data = pov_df %>% filter(empty == input$school_district), aes(x = value, y = null, label = empty), size = 6) +
        geom_point(data = pov_df %>% filter(empty == "State Average"), aes(x = value, y = null, label = empty), size = 3) +
        geom_text(hjust = 0.5, vjust = -0.5, size = 5) +
        scale_x_continuous(labels = scales::percent,
                           limits = c(0,0.45)) +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.background = element_blank(),
              axis.title.x = element_text(size = 20),
              axis.text.x = element_text(size = 12),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 16))
    })
    
    output$fed_rev_table <- renderFormattable({
      fed_rev_shiny %>%
        filter(district_name == input$school_district) %>%
        pivot_longer(cols = c(title_i, idea, cte, child_nutr)) %>%
        select(name, value, Total) %>%
        unique() %>%
        merge(sta_rev_avg, ., by = "name") %>%
        mutate("value" = value / Total) %>%
        rename(`Funding Category` = "name",
               `District Value` = "value",
               `Statewide Average` = "med_rev") %>%
        pivot_longer(cols = c(`District Value`, `Statewide Average`)) %>%
        pivot_wider(names_from = `Funding Category`, values_from = value) %>% 
        rename(`Nutrition Funding` = "child_nutr", 
               `Career & Technical Education Funding` = "cte", 
               `IDEA Funding` = "idea", 
               `Title I Funding` = "title_i") %>% 
        rename(state_dist_id = "name") %>% 
        pivot_longer(cols = c(`Nutrition Funding`, 
                              `Career & Technical Education Funding`, 
                              `IDEA Funding`, 
                              `Title I Funding`)) %>%
        filter(state_dist_id == "District Value") %>%
        select(name, value) %>%
        rename(`Funding Category` = "name",
               `Revenue per Student` = "value") %>%
        mutate(`Revenue per Student` = paste0("$", round(`Revenue per Student`, digits = 2))) %>%
        formattable(.,
                    align = c("l", "c"))
    })
    
    output$fed_rev_comp <- renderPlot({
      fed_rev_shiny %>%
        filter(district_name == input$school_district) %>%
        pivot_longer(cols = c(title_i, idea, cte, child_nutr)) %>%
        select(name, value, Total) %>%
        unique() %>%
        merge(sta_rev_avg, ., by = "name") %>%
        mutate("value" = value / Total) %>%
        rename(`Funding Category` = "name",
               `District Value` = "value",
               `Statewide Average` = "med_rev") %>%
        pivot_longer(cols = c(`District Value`, `Statewide Average`)) %>%
        pivot_wider(names_from = `Funding Category`, values_from = value) %>% 
        rename(`Nutrition Funding` = "child_nutr", 
               `Career & Technical Education Funding` = "cte", 
               `IDEA Funding` = "idea", 
               `Title I Funding` = "title_i") %>% 
        rename(state_dist_id = "name") %>% 
        pivot_longer(cols = c(`Nutrition Funding`, 
                              `Career & Technical Education Funding`, 
                              `IDEA Funding`, 
                              `Title I Funding`)) %>%
        ggplot(aes(y = name, x = value)) +
        geom_line(linewidth = 2) +
        geom_point(aes(color = state_dist_id), size = 5) +
        labs(x = "Revenue per Student",
             y = "Funding Category") +
        scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
        scale_x_continuous(labels = scales::dollar_format(prefix="$")) +
        theme(panel.background = element_blank(),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 12),
              legend.title = element_blank(),
              legend.text = element_text(size = 16))
    })
    
    
}

shinyApp(ui = ui, server = server)
