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

load("../results/quant-summary/summary_statistics.rda")
load("../results/spatial-summary/project_coverage_bcr_state.rda")
load("../results/spatial-summary/project_coverage_bcr.rda")
load("../results/spatial-summary/project_coverage_state.rda")

tau_files <- list.files(path = "../results/simulations/tau")
for (f in tau_files)
{
  load(paste0("../results/simulations/tau/", f))
}

#
forest_level <- c(1.0, 0.0)

load("data/phi.rda")
phi <- phi_df; rm(phi_df)
time_values <- c(1, 3, 5, 10)

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
      menuSubItem("Effective Detection Radius", tabName = "edr"),
      selectInput(inputId = "sp", 
                  label = "Species",
                  choices = unique(tau_1$Species),
                  selected = unique(tau_1$Species)[1])
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
              tabBox(
                side = "left",
                tabPanel("Null Model",
                         plotOutput("q_curve_null")),
                tabPanel("Road Model",
                         plotOutput("q_curve_road")),
                tabPanel("Forest Coverage Model",
                         plotOutput("q_curve_forest")),
                tabPanel("Road AND Forest Model (Additive)",
                         plotOutput("q_curve_additive")),
                tabPanel("Road AND Forest Model (Interaction)",
                         plotOutput("q_curve_interaction")),
                width = NULL)
              ),
      
      tabItem(tabName = "edr",
              tabBox(
                side = "left",
                tabPanel("Null Model",
                         plotOutput("edr_null")),
                tabPanel("Road Model",
                         plotOutput("edr_road")),
                tabPanel("Forest Coverage Model",
                         plotOutput("edr_forest")),
                tabPanel("Road AND Forest Model (Additive)",
                         plotOutput("edr_additive")),
                tabPanel("Road AND Forest Model (Interaction)",
                         plotOutput("edr_interaction")),
                width = NULL)
              )
    )
  )
)



















server <- function(input, output) { 
  
  ################ Project Overview Functions ############################
  
  output$project_coverage_map <- renderLeaflet({
    
    pal <- colorNumeric(viridis(10), NULL)
    
    leaflet(project_coverage) %>%
      addPolygons(stroke = FALSE,
                  fillColor = ~pal(ncounts),
                  layerId = ~ST_12,
                  fillOpacity = 1.0) %>%
      addLegend(position = 'bottomright', pal = pal, 
                values = project_coverage$ncounts, title = 'Sampling Events')
    
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
  output$q_curve_null <- renderPlot({

    ggplot(data = tau_1[which(tau_1$Species == input$sp),]) +
    geom_line(aes(x = Radius, y = q)) +
    geom_ribbon(aes(x = Radius, ymin = q_2.5, ymax = q_97.5),
                alpha = 0.25) +
    ylim(0, 1) +
    theme(legend.position = "none") +
    ggtitle(paste0("Species: ", input$sp)) +
    NULL
    
  })
  
  output$q_curve_road <- renderPlot({
    
    tau_2$Roadside_Status <- ifelse(tau_2$Roadside == 1, "On-Road", "Off-road")
    
    ggplot(data = tau_2[which(tau_2$Species == input$sp),]) +
      geom_line(aes(x = Radius, y = q, color = Roadside_Status)) +
      geom_ribbon(aes(x = Radius, ymin = q_2.5, ymax = q_97.5, color = Roadside_Status),
                  alpha = 0.25) +
      ylim(0, 1) +
      #theme(legend.position = "none") +
      ggtitle(paste0("Species: ", input$sp)) +
      NULL
    
  })
  
  output$q_curve_forest <- renderPlot({
    
    tau_3$Forest_Coverage <- ifelse(tau_3$Forest == 1.0, "Forest", "Non-forest")
    
    ggplot(data = tau_3[which(tau_3$Species == input$sp &
                                tau_3$Forest %in% c(1.0, 0.0)),]) +
      geom_line(aes(x = Radius, y = q, color = Forest_Coverage)) +
      geom_ribbon(aes(x = Radius, ymin = q_2.5, ymax = q_97.5, color = Forest_Coverage),
                  alpha = 0.25) +
      ylim(0, 1) +
      #theme(legend.position = "none") +
      ggtitle(paste0("Species: ", input$sp)) +
      NULL
    
  })
  
  output$q_curve_additive <- renderPlot({
    # Empty plot list
    tau_4$Roadside_Status <- ifelse(tau_4$Roadside == 1, "On-Road", "Off-road")
    distance_plot_list <- vector(mode = "list", length = length(forest_level))
    i <- 1
    
    for (fc in c(1.0, 0.0))
    {
      distance_plot_list[[i]] <- 
        ggplot(data = tau_4[which(tau_4$Forest == fc &
                                    tau_4$Species == input$sp),]) +
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
  
  output$q_curve_interaction <- renderPlot({
    # Empty plot list
    tau_5$Roadside_Status <- ifelse(tau_5$Roadside == 1, "On-Road", "Off-road")
    distance_plot_list <- vector(mode = "list", length = length(forest_level))
    i <- 1
    
    for (fc in c(1.0, 0.0))
    {
      distance_plot_list[[i]] <- 
        ggplot(data = tau_5[which(tau_5$Forest == fc &
                                       tau_5$Species == input$sp),]) +
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
  
  output$edr_null <- renderPlot({
    to_plot <- tau_1[which(tau_1$Species == sp), c("tau", "tau_2.5", "tau_97.5")]
    to_plot <- cbind(data.frame(EDR = 1), to_plot)
    
    ggplot(data = to_plot[1,]) +
      geom_errorbar(aes(x = EDR,
                        ymin = tau_2.5,
                        ymax = tau_97.5,
                        width = 0.1)) +
      geom_point(aes(x = EDR, y = tau, size = 2)) +
      geom_text(aes(x = EDR, y = tau, label = tau)) +
      #ylim(min(tau_2$tau_2.5) - 10, max(tau_2$tau_97.5) + 10) +
      theme(legend.position = "none") +
      ggtitle(paste0("Species: ", input$sp)) +
      NULL 
  })
  
  output$edr_road <- renderPlot({
    tau_2$Roadside_Status <- ifelse(tau_2$Roadside == 1, "On-Road", "Off-road")
    to_plot <- tau_2[which(tau_2$Species == input$sp),
                     c("tau", "tau_2.5", "tau_97.5", "Roadside_Status")]
    to_plot <- to_plot[!duplicated(to_plot$Roadside_Status), ]
    
    ggplot(data = to_plot) +
      geom_errorbar(aes(x = Roadside_Status,
                        ymin = tau_2.5,
                        ymax = tau_97.5,
                        width = 0.2)) +
      geom_point(aes(x = Roadside_Status, y = tau, size = 2)) +
      geom_text(aes(x = Roadside_Status, y = tau, label = tau)) +
      #ylim(min(tau_2$tau_2.5) - 10, max(tau_2$tau_97.5) + 10) +
      theme(legend.position = "none") +
      ggtitle(paste0("Species: ", input$sp)) +
      NULL 
  })
  
  output$edr_forest <- renderPlot({
    tau_3 <- tau_3[which(tau_3$Forest %in% c(0.0, 1.0)), ]
    tau_3$Forest_Coverage <- ifelse(tau_3$Forest == 1.0, "Forest", "Non-forest")
    to_plot <- tau_3[which(tau_3$Species == input$sp),
                     c("tau", "tau_2.5", "tau_97.5", "Forest_Coverage")]
    to_plot <- to_plot[!duplicated(to_plot$Forest_Coverage), ]
    
    ggplot(data = to_plot) +
      geom_errorbar(aes(x = Forest_Coverage,
                        ymin = tau_2.5,
                        ymax = tau_97.5,
                        width = 0.2)) +
      geom_point(aes(x = Forest_Coverage, y = tau, size = 2)) +
      geom_text(aes(x = Forest_Coverage, y = tau, label = tau)) +
      #ylim(min(tau_2$tau_2.5) - 10, max(tau_2$tau_97.5) + 10) +
      theme(legend.position = "none") +
      ggtitle(paste0("Species: ", input$sp)) +
      NULL 
  })
  
  output$edr_additive <- renderPlot({
    # Empty plot list
    to_plot <- tau_4[which(tau_4$Forest %in% c(0.0, 1.0) &
                             tau_4$Radius == 50), ]
    to_plot$Roadside_Status <- ifelse(to_plot$Roadside == 1, "On-Road", "Off-road")
    to_plot <- to_plot[which(to_plot$Species == input$sp),
                     c("tau", "tau_2.5", "tau_97.5", "Roadside_Status", "Forest")]
    #to_plot <- to_plot[!duplicated(to_plot$Forest), ]
    
    min_y <- min(to_plot$tau_2.5) - 10
    max_y <- max(to_plot$tau_97.5) + 10
    
    distance_plot_list <- vector(mode = "list", length = length(forest_level))
    i <- 1
    
    for (fc in c(1.0, 0.0))
    {
      distance_plot_list[[i]] <- 
        ggplot(data = to_plot[which(to_plot$Forest == fc),]) +
        geom_errorbar(aes(x = Roadside_Status,
                          ymin = tau_2.5,
                          ymax = tau_97.5,
                          width = 0.2)) +
        geom_point(aes(x = Roadside_Status, y = tau, size = 2)) +
        geom_text(aes(x = Roadside_Status, y = tau, label = tau)) +
        ylim(min_y, max_y) +
        theme(legend.position = "none") +
        #ggtitle(paste0("Species: ", input$sp)) +
        NULL 
      i <- i + 1  
    }
    
    ggmatrix(
      distance_plot_list,
      ncol = length(forest_level),
      nrow = 1,
      xAxisLabels = c("Forest", "Non-forest"),
      title = paste0("Species: ",
                     input$sp))
    
  })
  
  output$edr_interaction <- renderPlot({
    # Empty plot list
    to_plot <- tau_5[which(tau_5$Forest %in% c(0.0, 1.0) &
                             tau_5$Radius == 50), ]
    to_plot$Roadside_Status <- ifelse(to_plot$Roadside == 1, "On-Road", "Off-road")
    to_plot <- to_plot[which(to_plot$Species == input$sp),
                       c("tau", "tau_2.5", "tau_97.5", "Roadside_Status", "Forest")]
    #to_plot <- to_plot[!duplicated(to_plot$Forest), ]
    
    min_y <- min(to_plot$tau_2.5) - 10
    max_y <- max(to_plot$tau_97.5) + 10
    
    distance_plot_list <- vector(mode = "list", length = length(forest_level))
    i <- 1
    
    for (fc in c(1.0, 0.0))
    {
      distance_plot_list[[i]] <- 
        ggplot(data = to_plot[which(to_plot$Forest == fc),]) +
        geom_errorbar(aes(x = Roadside_Status,
                          ymin = tau_2.5,
                          ymax = tau_97.5,
                          width = 0.2)) +
        geom_point(aes(x = Roadside_Status, y = tau, size = 2)) +
        geom_text(aes(x = Roadside_Status, y = tau, label = tau)) +
        ylim(min_y, max_y) +
        theme(legend.position = "none") +
        #ggtitle(paste0("Species: ", input$sp)) +
        NULL 
      i <- i + 1  
    }
    
    ggmatrix(
      distance_plot_list,
      ncol = length(forest_level),
      nrow = 1,
      xAxisLabels = c("Forest", "Non-forest"),
      title = paste0("Species: ",
                     input$sp))
    
  })
  
  ################ Removal Functions ############################  
  
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