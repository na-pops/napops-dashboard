## app.R ##
library(shiny)
library(shinydashboard)
library(leaflet)
library(viridis)
library(ggplot2)
library(GGally)
library(ggpubr)
library(sf)
theme_set(theme_pubclean())

load("data/summary_statistics.rda")

load("data/tau.rda")
tau <- tau_df; rm(tau_df)
tau$Roadside_Status <- ifelse(tau$Roadside == 1, "On-Road", "Off-road")
forest_level <- c(1.0, 0.0)

load("data/phi.rda")
phi <- phi_df; rm(phi_df)
time_values <- c(1, 3, 5, 10)

load("data/project_coverage_bcr_state.rda")
load("data/project_coverage_bcr.rda")
load("data/project_coverage_state.rda")

project_coverage <- bcr_coverage

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "NA-POPS Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Project Overview", tabName = "overview"),
      menuItem("Removal", tabName = "removal"),
     # menuSubItem("Time Since Local Sunrise", tabName = "tssr"),
      #menuSubItem("Julian Day", tabName = "jd"),
      menuItem("Distance", tabName = "distance"),
      menuSubItem("Covariate Analysis", tabName = "dis-cov"),
      menuSubItem("Perceptability Curves", tabName = "q"),
      menuSubItem("EDR Curves", tabName = "edr"),
      selectInput(inputId = "sp", 
                  label = "Species",
                  choices = unique(tau$Species),
                  selected = unique(tau$Species)[1])
    )
  ),
  
  dashboardBody(
    tabItems(
      # Project Overview
      tabItem(tabName = "overview",
              fluidRow(
                valueBox(value = summary_stats$n_species,
                         subtitle = "Species Modelled",
                         icon = icon("crow"),
                         width = 3,
                         color = "olive"),
                valueBox(value = summary_stats$total_projects,
                         subtitle = "Projects",
                         icon = icon("project-diagram"),
                         width = 3,
                         color = "olive"),
                valueBox(value = summary_stats$n_samples,
                         subtitle = "Sampling Events",
                         icon = icon("clipboard"),
                         width = 3,
                         color = "olive"),
                valueBox(value = paste0(format(round(summary_stats$n_observations / 1e6, 2), trim = TRUE), "M +"),
                         subtitle = "Observations",
                         icon = icon("binoculars"),
                         width = 3,
                         color = "olive")
              ),
              h5("Click on a region to view a regional summary"),
              fluidRow(
                column(width = 7,
                       box(leafletOutput("project_coverage_map"),
                           width = NULL)
                       ),
                column(width = 5,
                       infoBoxOutput("proj_region_name", width = NULL),
                       infoBoxOutput("proj_region_ncounts", width = NULL)
                       )
              )
              ),
      
      # Removal Modelling
      tabItem(tabName = "removal",
              # fluidRow(
              #   box(title = "Species-specific Coverage Map for Removal Modelling",
              #       "TO DO",
              #       width = 7),
              #   box(title = "Region specific summary statistics",
              #       "TO DO",
              #       width = 5)
              # ),
              fluidRow(
                column(width = 7,
                  tabBox(
                    side = "left",
                    tabPanel("TSSR",
                             plotOutput("tssr_curve"),
                             sliderInput(inputId = "jd", 
                                         label = "Julian Day:", 
                                         min = 91, max = 200, value = 152)),
                    tabPanel("JD",
                             plotOutput("jd_curve"),
                             sliderInput(inputId = "tssr", 
                                         label = "Time Since Local Sunrise:", 
                                         min = -2, max = 6, value = 1)),
                  width = NULL)
                ),
                
                column(width = 5,
                       box(title = "Species-specific Coverage Map for Removal Modelling",
                           "TO DO",
                           width = NULL),
                       box(title = "Region specific summary statistics",
                           "TO DO",
                           width = NULL)
                       )
              )
      ),
      
      # Distance Modelling
      tabItem(tabName = "q",
                fluidRow(
                  box(plotOutput("distance_curve"),
                      width = 12)
              )),
      
      tabItem(tabName = "edr",
              fluidRow(
                box(plotOutput("edr_curve"),
                    width = 12)
              ))
    )
  )
)



















server <- function(input, output) { 
  
  ################ Project Overview Functions ############################
  
  output$project_coverage_map <- renderLeaflet({
    
    pal <- colorNumeric(viridis(10), NULL)
    
    leaflet(project_coverage) %>%
      addPolygons(stroke = FALSE,
                  fillColor = ~pal(sqrt_ncounts),
                  layerId = ~ST_12,
                  fillOpacity = 1.0) %>%
      addLegend(position = 'bottomright', pal = pal, 
                values = project_coverage$sqrt_ncounts, title = 'SQRT(Counts)')
    
  })
  
  # Click event for the map (will use to generate chart)
  click_region <- eventReactive(input$project_coverage_map_shape_click, {
    x <- input$project_coverage_map_shape_click
    y <- x$id
    return(y)
  })
  
  region_data <- reactive({
    # Fetch data for the clicked tract
    return(project_coverage[project_coverage$ST_12 == click_region(), ])
  })
  
  output$proj_region_name <- renderInfoBox({
    infoBox(
      title = "Region",
      value = project_coverage[project_coverage$ST_12 == click_region(), ]$ST_12,
      color = "green",
      icon = icon("map"),
      fill = TRUE
    )
  })
  
  output$proj_region_ncounts <- renderInfoBox({
    infoBox(
      title = "Sampling Events",
      value = project_coverage[project_coverage$ST_12 == click_region(), ]$ncounts,
      color = "green",
      icon = icon("clipboard"),
      fill = TRUE
    )
  })
  
  ################ Distance Functions ############################  
  
  output$distance_curve <- renderPlot({
    # Empty plot list
    distance_plot_list <- vector(mode = "list", length = length(forest_level))
    i <- 1
    
    for (fc in c(1.0, 0.0))
    {
      distance_plot_list[[i]] <- 
        ggplot(data = tau[which(tau$Forest == fc &
                                       tau$Species == input$sp),]) +
        geom_line(aes(x = Radius, y = q, color = Roadside_Status)) +
        geom_ribbon(aes(x = Radius, ymin = q_2.5, ymax = q_97.5, color = Roadside_Status),
                    alpha = 0.25) +
        #stat_summary(aes(x = Radius, y = q, group = as.factor(Roadside), color = as.factor(Roadside)), fun = mean, geom = "smooth", size = 1.25) +
        ylim(0, 1) +
        #theme(legend.position = "none") +
        NULL
      i <- i + 1  
    }
    
    ggmatrix(
      distance_plot_list,
      ncol = length(forest_level),
      nrow = 1,
      xAxisLabels = c("Forest", "Non-forest"),
      title = paste0("Species: ",
                     input$sp),
      legend = 2)
  
  })
  
  output$edr_curve <- renderPlot({
    
  })
  
  output$tssr_curve <- renderPlot({
    # Empty plot list
    tssr_plot_list <- vector(mode = "list", length = length(time_values))
    
    i <- 1
    
    for (tv in time_values)
    {
      tssr_plot_list[[i]] <- 
        ggplot(data = phi[which(phi$JD == input$jd & 
                                       phi$Time == tv &
                                       phi$Species == input$sp),]) +
        geom_line(aes(x = TSSR, y = p)) +
        geom_ribbon(aes(x = TSSR, ymin = p_2.5, ymax = p_97.5),
                    alpha = 0.25) +
        # stat_summary(aes(x = TSSR, y = p), fun = mean, geom = "smooth", size = 1.25) +
        ylim(0, 1) +
        theme(legend.position = "none")
      i <- i + 1
    }
    
    ggmatrix(
      tssr_plot_list,
      ncol = length(time_values),
      nrow = 1,
      xAxisLabels = c("1 min", "3 min", "5 min", "10 min"),
      title = paste0("Species ",
                     input$sp)
    )
  })
  
  
  output$jd_curve <- renderPlot({
    
    # Empty plot list
    jd_plot_list <- vector(mode = "list", length = length(time_values))
    
    i <- 1
    
    for (tv in time_values)
    {
      jd_plot_list[[i]] <- 
        ggplot(data = phi[which(phi$TSSR == input$tssr & 
                                       phi$Time == tv &
                                       phi$Species == input$sp),]) +
        geom_line(aes(x = JD, y = p)) +
        geom_ribbon(aes(x = JD, ymin = p_2.5, ymax = p_97.5),
                    alpha = 0.25) +
        #stat_summary(aes(x = JD, y = p), fun = mean, geom = "smooth", size = 1.25) +
        ylim(0, 1) +
        theme(legend.position = "none")
      i <- i + 1
    }
    
    ggmatrix(
      jd_plot_list,
      ncol = length(time_values),
      nrow = 1,
      xAxisLabels = c("1 min", "3 min", "5 min", "10 min"),
      title = paste0("Species ",
                     input$sp)
    )
  })
  
}

shinyApp(ui, server)