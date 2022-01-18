####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: napops-dashboard
# app.R
# Created August 2020
# Last Updated January 2022

####### Import Libraries and External Files #######

library(shiny)
library(shinydashboard)
library(leaflet)
library(viridis)
library(ggplot2)
library(GGally)
library(ggpubr)
library(sf)
library(napops)
theme_set(theme_pubclean())

laea = st_crs("+proj=laea +lat_0=45 +lon_0=-95")

# Load quantitative summary files
load("../results/quant-summary/summary_statistics.rda")
load("../results/quant-summary/dis_species_summary.rda")
load("../results/quant-summary/rem_species_summary.rda")

# Load spatial summary files
load("../results/spatial-summary/project_coverage_bcr.rda")
load("../results/spatial-summary/dis_coverage_bcr.rda")
load("../results/spatial-summary/rem_coverage_bcr.rda")

forest_level <- c(1.0, 0.0)

# Load files related to removal sampling and set constants
load("../results/aic/rem_aic.rda")
phi_files <- list.files(path = "../results/simulations/phi")
for (f in phi_files)
{
  load(paste0("../results/simulations/phi/", f))
}

# Read in date
date <- readChar("../results/date.txt", nchars = 30)

rem_models <- c("(1) Null Model", "(2) Ordinal Day (OD) Model",
                "(3) OD + OD^2 Model", "(4) Time-since-sunrise (TSSR) Model",
                 "(5) TSSR + TSSR^2 Model",
                 "(6) TSSR + OD Model",
                "(7) TSSR + OD + OD^2 Model",
                "(8) TSSR + TSSR^2 + OD Model",
                "(9) TSSR + TSSR^2 + OD + OD^2 Model")
time_values <- c(1, 3, 5, 10)

project_coverage <- bcr_coverage

####### UI Setup ##################################

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "NA-POPS Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Project Overview", tabName = "overview"),
      menuItem("Removal", tabName = "removal"),
      menuSubItem("Species Overview", tabName = "rem-sp"),
      menuSubItem("Availability (p) Curves", tabName = "p"),
      menuItem("Distance", tabName = "distance"),
      menuSubItem("Species Overview", tabName = "dis-sp"),
      menuSubItem("Perceptability (q) Curves", tabName = "q"),
      menuSubItem("Effective Detection Radius", tabName = "edr"),
      selectInput(inputId = "sp", 
                  label = "Species",
                  choices = unique(tau_1$Species),
                  selected = unique(tau_1$Species)[1]),
      h5(paste0("Models Run: ", date))
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
              h2("Infographic about removal modelling process to go here."),
              h4("In the mean time, click some of the submenus under \"Removal\" to
                 view some graphs!")
              ),
      tabItem(tabName = "rem-sp",
              fluidRow(
                valueBoxOutput("rem_species", width = 4),
                valueBoxOutput("rem_sampling", width = 4),
                valueBoxOutput("rem_bcr", width = 4)
              ),
              fluidRow(
                column(width = 7,
                       box(leafletOutput("rem_coverage_map"),
                           width = NULL)
                ),
                column(width = 5,
                       tabBox(
                         side = "left",
                         tabPanel("Ordinal Day",
                                  plotOutput("rem_od_hist")),
                         tabPanel("Time Since Sunrise",
                                  plotOutput("rem_tssr_hist")),
                         width = NULL
                       )
                )
              )
      ),
      tabItem(tabName = "p",
              fluidRow(
                column(width = 8,
                  tabBox(
                    side = "left",
                    tabPanel("TSSR",
                             plotOutput("tssr_curve"),
                             sliderInput(inputId = "od", 
                                         label = "Ordinal Day:", 
                                         min = 91, max = 200, value = 152)),
                    tabPanel("OD",
                             plotOutput("od_curve"),
                             sliderInput(inputId = "tssr", 
                                         label = "Time Since Local Sunrise:", 
                                         min = -2, max = 6, value = 1)),
                  width = NULL)
                ),
                column(width = 4,
                       selectInput(inputId = "p_mod", 
                                   label = "Model",
                                   choices = rem_models,
                                   selected = rem_models[1]),
                       h2("How to Interpret"),
                       "The plots on the left display the probability that a bird
                       gives a cue (availability, p), modelled by Ordinal Day (OD) and Time-since-local-sunrise (TSSR). 
                       Use the sliders to see how the availability curve changes with different 
                       values of OD and TSSR.
                       
                       The table below ranks the models for this particular species based 
                       on AIC from most parsimonious to least parsimonious.",
                       tableOutput("p_aic")
                       )
              )
      ),
      
      # Distance Modelling
      tabItem(tabName = "distance",
              h2("Infographic about distance sampling process to go here."),
              h4("In the mean time, click some of the submenus under \"Distance\" to
                 view some graphs!")
      ),
      tabItem(tabName = "dis-sp",
              fluidRow(
                valueBoxOutput("dis_species", width = 4),
                valueBoxOutput("dis_sampling", width = 4),
                valueBoxOutput("dis_bcr", width = 4)
              ),
              fluidRow(
                column(width = 7,
                       box(leafletOutput("dis_coverage_map"),
                           width = NULL)
                ),
                column(width = 5,
                       tabBox(
                         side = "left",
                         tabPanel("Forest Coverage",
                                  plotOutput("dis_forest_hist")),
                         tabPanel("Roadside Status",
                                  plotOutput("dis_road_bar")),
                         width = NULL
                         )
                      )
              )
              ),
      tabItem(tabName = "q",
              fluidRow(
                column(width = 8,
                       box(plotOutput("q_curve_plotter"),
                           width = NULL)
                       ),
                column(width = 4,
                       selectInput(inputId = "q_mod", 
                                  label = "Model",
                                  choices = dis_models,
                                  selected = dis_models[1]),
                       h2("How to Interpret"),
                       "The plot on the left displays the conditional probability that 
                       a bird is perceived, provided it gives a cue (\"q\", y-axis), 
                       modelled by survey radius (x-axis). NA-POPS considers 5 candidate 
                       models for deriving q and EDR that model different combinations of roadside 
                       status (on- vs. off-road) and forest coverage (forest vs. non-forest). 
                       The q curve for each of these models can be viewed with the tabs above 
                       the plot.
                       
                       The table below ranks the models for this particular species based 
                       on AIC from most parsimonious to least parsimonious.",
                       tableOutput("q_aic"))
                )
              ),
      
      tabItem(tabName = "edr",
              fluidRow(
                column(width = 8,
                       box(plotOutput("edr_plotter"),
                           width = NULL)
                  ),
                column(width = 4,
                       selectInput(inputId = "edr_mod", 
                                   label = "Model",
                                   choices = dis_models,
                                   selected = dis_models[1]),
                       h2("How to Interpret"),
                       "The plot on the left displays the effective detection radius
                       (\"EDR\", y-axis). NA-POPS considers 5 candidate 
                       models for deriving q and EDR that model different combinations of roadside 
                       status (on- vs. off-road) and forest coverage (forest vs. non-forest). 
                       
                       The table below ranks the models for this particular species based 
                       on AIC from most parsimonious to least parsimonious.",
                       tableOutput("edr_aic"))
                )
              )
              
    )
  )
)

####### Server Function ###########################

server <- function(input, output) { 
  
  ################ Project Overview Functions ############################
  
  project_coverage[which(project_coverage$ncounts == 0), "ncounts"] <- NA
  #project_coverage <- st_transform(project_coverage, crs = laea)
  
  output$project_coverage_map <- renderLeaflet({
    
    pal <- colorNumeric(palette = rev(viridis(10)),
                        domain = NULL, 
                        na.color = "#808080")
    
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
  
  ################ Distance Species Overview Functions ##########
  
  output$dis_species <- renderValueBox({
    valueBox(value = input$sp,
             subtitle = "Species",
             icon = icon("crow"),
             color = "olive")
  })
  
  output$dis_sampling <- renderValueBox({
    valueBox(value = nrow(covariates_distance(project = FALSE,
                                              species = input$sp)),
             subtitle = "Sampling Events",
             icon = icon("clipboard"),
             color = "olive")
  })

  output$dis_bcr <- renderValueBox({
    valueBox(value = length(which(spatial_coverage(model = "dis",
                                                   species = input$sp)$ncounts > 0)),
             subtitle = "BCRs",
             icon = icon("map"),
             color = "olive")    
  })

  output$dis_coverage_map <- renderLeaflet({
    
    pal <- colorNumeric(palette = rev(viridis(10)),
                        domain = NULL, 
                        na.color = "#808080")
    to_plot <- spatial_coverage(model = "dis",
                                species = input$sp)
    to_plot[which(to_plot$ncounts == 0), "ncounts"] <-NA
    
    leaflet(to_plot) %>%
      addPolygons(stroke = FALSE,
                  fillColor = ~pal(ncounts),
                  layerId = ~BCR,
                  fillOpacity = 1.0) %>%
      addLegend(position = 'bottomright', pal = pal, 
                values = to_plot$ncounts, title = 'Sampling Events')
    
  })
  
  output$dis_forest_hist <- renderPlot({
    ggplot(data = covariates_distance(project = FALSE,
                                      species = input$sp)) +
      geom_histogram(bins = 25, aes(x = Forest)) +
      xlab("Forest Coverage") +
      ylab("Sampling Events") +
      NULL
  })
  
  output$dis_road_bar <- renderPlot({
    ggplot(data = covariates_distance(project = FALSE,
                                      species = input$sp)) +
      geom_bar(aes(x = ifelse(Road == 1, "On-Road", "Off-Road"))) +
      xlab("Roadside Status") +
      ylab("Sampling Events") +
      NULL
  })
  
  ################ Distance Perceptibility Functions ############
  
  output$q_aic <- output$edr_aic <- renderTable(dis_aic[[input$sp]],
                                                striped = TRUE,
                                                bordered = TRUE,
                                                hover = TRUE)
  
  output$q_curve_plotter <- renderPlot({
    if (input$q_mod == dis_models[1])
    {
      dis_values <- seq(25,400, by = 25)
      
      i <- 1
      sim_data <- vector(mode = "list", length = length(dis_values))
      for (dis in dis_values)
      {
        sim_data[[i]] <- percept(species = input$sp,
                                 model = 1,
                                 road = c(TRUE, FALSE),
                                 forest = c(0,1),
                                 distance = dis,
                                 quantiles = c(0.025, 0.975))
        sim_data[[i]]$Radius <- dis
        
        i <- i + 1
      }
      sim_data <- do.call(rbind, sim_data)
      
      ggplot(data = sim_data) +
        geom_line(aes(x = Radius, y = q_est)) +
        geom_ribbon(aes(x = Radius, ymin = q_2.5, ymax = q_97.5),
                    alpha = 0.25) +
        ylim(0, 1) +
        theme(legend.position = "none") +
        ggtitle(paste0("Species: ", input$sp)) +
        ylab("Perceptibility (q)") +
        NULL
    }else if (input$q_mod == dis_models[2])
    {
      dis_values <- seq(25,400, by = 25)
      
      i <- 1
      sim_data <- vector(mode = "list", length = length(dis_values))
      for (dis in dis_values)
      {
        sim_data[[i]] <- percept(species = input$sp,
                                 model = 2,
                                 road = c(TRUE, FALSE),
                                 forest = c(0,1),
                                 distance = dis,
                                 quantiles = c(0.025, 0.975))
        sim_data[[i]]$Radius <- dis
        
        i <- i + 1
      }
      sim_data <- do.call(rbind, sim_data)
      
      sim_data$Roadside_Status <- ifelse(sim_data$Road == 1, "On-Road", "Off-road")
      
      ggplot(data = sim_data[sim_data$Forest == 1, ]) +
        geom_line(aes(x = Radius, y = q_est, color = Roadside_Status)) +
        geom_ribbon(aes(x = Radius, ymin = q_2.5, ymax = q_97.5, color = Roadside_Status),
                    alpha = 0.25) +
        ylim(0, 1) +
        #theme(legend.position = "none") +
        ggtitle(paste0("Species: ", input$sp)) +
        ylab("Perceptibility (q)") +
        NULL
    }else if (input$q_mod == dis_models[3])
    {
      dis_values <- seq(25,400, by = 25)
      
      i <- 1
      sim_data <- vector(mode = "list", length = length(dis_values))
      for (dis in dis_values)
      {
        sim_data[[i]] <- percept(species = input$sp,
                                 model = 3,
                                 road = c(TRUE, FALSE),
                                 forest = c(0,1),
                                 distance = dis,
                                 quantiles = c(0.025, 0.975))
        sim_data[[i]]$Radius <- dis
        
        i <- i + 1
      }
      sim_data <- do.call(rbind, sim_data)
      
      sim_data$Forest_Coverage <- ifelse(sim_data$Forest == 1.0, "Forest", "Non-forest")
      
      ggplot(data = sim_data) +
        geom_line(aes(x = Radius, y = q_est, color = Forest_Coverage)) +
        geom_ribbon(aes(x = Radius, ymin = q_2.5, ymax = q_97.5, color = Forest_Coverage),
                    alpha = 0.25) +
        ylim(0, 1) +
        #theme(legend.position = "none") +
        ggtitle(paste0("Species: ", input$sp)) +
        ylab("Perceptibility (q)") +
        NULL
    }else if (input$q_mod == dis_models[4])
    {
      dis_values <- seq(25,400, by = 25)
      
      i <- 1
      sim_data <- vector(mode = "list", length = length(dis_values))
      for (dis in dis_values)
      {
        sim_data[[i]] <- percept(species = input$sp,
                                 model = 4,
                                 road = c(TRUE, FALSE),
                                 forest = c(0,1),
                                 distance = dis,
                                 quantiles = c(0.025, 0.975))
        sim_data[[i]]$Radius <- dis
        
        i <- i + 1
      }
      sim_data <- do.call(rbind, sim_data)
      
      sim_data$Roadside_Status <- ifelse(sim_data$Road == 1, "On-Road", "Off-road")
      
      distance_plot_list <- vector(mode = "list", length = length(forest_level))
      i <- 1
      
      for (fc in c(1.0, 0.0))
      {
        distance_plot_list[[i]] <- 
          ggplot(data = sim_data[which(sim_data$Forest == fc),]) +
          geom_line(aes(x = Radius, y = q_est, color = Roadside_Status)) +
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
    }else if (input$q_mod == dis_models[5])
    {
      dis_values <- seq(25,400, by = 25)
      
      i <- 1
      sim_data <- vector(mode = "list", length = length(dis_values))
      for (dis in dis_values)
      {
        sim_data[[i]] <- percept(species = input$sp,
                                 model = 5,
                                 road = c(TRUE, FALSE),
                                 forest = c(0,1),
                                 distance = dis,
                                 quantiles = c(0.025, 0.975))
        sim_data[[i]]$Radius <- dis
        
        i <- i + 1
      }
      sim_data <- do.call(rbind, sim_data)
      
      sim_data$Roadside_Status <- ifelse(sim_data$Road == 1, "On-Road", "Off-road")
      
      distance_plot_list <- vector(mode = "list", length = length(forest_level))
      i <- 1
      
      for (fc in c(1.0, 0.0))
      {
        distance_plot_list[[i]] <- 
          ggplot(data = sim_data[which(sim_data$Forest == fc),]) +
          geom_line(aes(x = Radius, y = q_est, color = Roadside_Status)) +
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
    }
  })
  
  ################ Distance EDR Functions #######################
  
  output$edr_plotter <- renderPlot({
    if (input$edr_mod == dis_models[1])
    {
      to_plot <- edr(species = input$sp,
                     model = 1,
                     road = c(TRUE, FALSE),
                     forest = c(0,1),
                     quantiles = c(0.025, 0.975))
      to_plot <- cbind(data.frame(X = 1), to_plot)
      
      ggplot(data = to_plot[1,]) +
        geom_errorbar(aes(x = X,
                          ymin = EDR_2.5,
                          ymax = EDR_97.5,
                          width = 0.1)) +
        geom_point(aes(x = X, y = EDR_est, size = 2)) +
        geom_text(aes(x = X, y = EDR_est, label = EDR_est)) +
        theme(legend.position = "none") +
        ggtitle(paste0("Species: ", input$sp)) +
        xlab(" ") +
        ylab("EDR (m)") +
        NULL 
    }else if (input$edr_mod == dis_models[2])
    {
      to_plot <- edr(species = input$sp,
                     model = 2,
                     road = c(TRUE, FALSE),
                     forest = 1,
                     quantiles = c(0.025, 0.975))
      to_plot <- cbind(data.frame(X = 1), to_plot)
      to_plot$Roadside_Status <- ifelse(to_plot$Road == 1, "On-Road", "Off-road")
      to_plot <- to_plot[!duplicated(to_plot$Roadside_Status), ]
      
      ggplot(data = to_plot) +
        geom_errorbar(aes(x = Roadside_Status,
                          ymin = EDR_2.5,
                          ymax = EDR_97.5,
                          width = 0.2)) +
        geom_point(aes(x = Roadside_Status, y = EDR_est, size = 2)) +
        geom_text(aes(x = Roadside_Status, y = EDR_est, label = EDR_est)) +
        theme(legend.position = "none") +
        ggtitle(paste0("Species: ", input$sp)) +
        ylab("EDR (m)") +
        NULL 
    }else if (input$edr_mod == dis_models[3])
    {
      to_plot <- edr(species = input$sp,
                     model = 3,
                     road = TRUE,
                     forest = c(0,1),
                     quantiles = c(0.025, 0.975))
      to_plot <- cbind(data.frame(X = 1), to_plot)
      
      to_plot$Forest_Coverage <- ifelse(to_plot$Forest == 1.0, "Forest", "Non-forest")
      to_plot <- to_plot[!duplicated(to_plot$Forest_Coverage), ]
      
      ggplot(data = to_plot) +
        geom_errorbar(aes(x = Forest_Coverage,
                          ymin = EDR_2.5,
                          ymax = EDR_97.5,
                          width = 0.2)) +
        geom_point(aes(x = Forest_Coverage, y = EDR_est, size = 2)) +
        geom_text(aes(x = Forest_Coverage, y = EDR_est, label = EDR_est)) +
        theme(legend.position = "none") +
        ggtitle(paste0("Species: ", input$sp)) +
        ylab("EDR (m)") +        
        NULL 
    }else if (input$edr_mod == dis_models[4])
    {
      to_plot <- edr(species = input$sp,
                     model = 4,
                     road = c(TRUE, FALSE),
                     forest = c(0,1),
                     quantiles = c(0.025, 0.975))
      to_plot <- cbind(data.frame(X = 1), to_plot)
      to_plot$Roadside_Status <- ifelse(to_plot$Road == 1, "On-Road", "Off-road")
      
      min_y <- min(to_plot$EDR_2.5) - 10
      max_y <- max(to_plot$EDR_97.5) + 10
      
      distance_plot_list <- vector(mode = "list", length = length(forest_level))
      i <- 1
      
      for (fc in c(1.0, 0.0))
      {
        distance_plot_list[[i]] <- 
          ggplot(data = to_plot[which(to_plot$Forest == fc),]) +
          geom_errorbar(aes(x = Roadside_Status,
                            ymin = EDR_2.5,
                            ymax = EDR_97.5,
                            width = 0.2)) +
          geom_point(aes(x = Roadside_Status, y = EDR_est, size = 2)) +
          geom_text(aes(x = Roadside_Status, y = EDR_est, label = EDR_est)) +
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
    }else if (input$edr_mod == dis_models[5])
    {
      to_plot <- edr(species = input$sp,
                     model = 5,
                     road = c(TRUE, FALSE),
                     forest = c(0,1),
                     quantiles = c(0.025, 0.975))
      to_plot <- cbind(data.frame(X = 1), to_plot)
      to_plot$Roadside_Status <- ifelse(to_plot$Road == 1, "On-Road", "Off-road")
      
      min_y <- min(to_plot$EDR_2.5) - 10
      max_y <- max(to_plot$EDR_97.5) + 10
      
      distance_plot_list <- vector(mode = "list", length = length(forest_level))
      i <- 1
      
      for (fc in c(1.0, 0.0))
      {
        distance_plot_list[[i]] <- 
          ggplot(data = to_plot[which(to_plot$Forest == fc),]) +
          geom_errorbar(aes(x = Roadside_Status,
                            ymin = EDR_2.5,
                            ymax = EDR_97.5,
                            width = 0.2)) +
          geom_point(aes(x = Roadside_Status, y = EDR_est, size = 2)) +
          geom_text(aes(x = Roadside_Status, y = EDR_est, label = EDR_est)) +
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
    }
  })
  
  ################ Removal Species Overview Functions ###########
  
  output$rem_species <- renderValueBox({
    valueBox(value = input$sp,
             subtitle = "Species",
             icon = icon("crow"),
             color = "olive")
  })
  
  output$rem_sampling <- renderValueBox({
    valueBox(value = nrow(rem_species_summary[[input$sp]]),
             subtitle = "Sampling Events",
             icon = icon("clipboard"),
             color = "olive")
  })
  output$rem_bcr <- renderValueBox({
    valueBox(value = length(which(bcr_rem_coverage[[input$sp]]$ncounts > 0)),
             subtitle = "BCRs",
             icon = icon("map"),
             color = "olive")    
  })
  
  output$rem_coverage_map <- renderLeaflet({
    
    pal <- colorNumeric(palette = rev(viridis(10)),
                        domain = NULL, 
                        na.color = "#808080")
    bcr_rem_coverage[[input$sp]][which(bcr_rem_coverage[[input$sp]]$ncounts == 0), "ncounts"] <- NA
    
    
    leaflet(bcr_rem_coverage[[input$sp]]) %>%
      addPolygons(stroke = FALSE,
                  fillColor = ~pal(ncounts),
                  layerId = ~ST_12,
                  fillOpacity = 1.0) %>%
      addLegend(position = 'bottomright', pal = pal, 
                values = bcr_rem_coverage[[input$sp]]$ncounts, title = 'Sampling Events')
    
  })
  
  output$rem_od_hist <- renderPlot({
    ggplot(data = rem_species_summary[[input$sp]]) +
      geom_histogram(aes(x = (OD))) +
      xlab("Ordinal Day") +
      ylab("Sampling Events") +
      NULL
  })
  
  output$rem_tssr_hist <- renderPlot({
    ggplot(data = rem_species_summary[[input$sp]]) +
      geom_histogram(aes(x = (TSSR))) +
      xlab("Time Since Local Sunrise") +
      ylab("Sampling Events") +
      NULL
  })
  
  ################ Removal Availability Functions ############### 
  
  output$p_aic <- renderTable(rem_aic[[input$sp]],
                              striped = TRUE,
                              bordered = TRUE,
                              hover = TRUE)
  
  output$tssr_curve <- renderPlot({
    # Empty plot list
    tssr_plot_list <- vector(mode = "list", length = length(time_values))
    
    i <- 1
    
    mod <- which(rem_models == input$p_mod)
    phi <- eval(parse(text = paste0("phi_", mod)))
    for (tv in time_values)
    {
      tssr_plot_list[[i]] <- 
        ggplot(data = phi[which(phi$OD == input$od & 
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
  
  
  output$od_curve <- renderPlot({
    
    # Empty plot list
    od_plot_list <- vector(mode = "list", length = length(time_values))
    
    i <- 1
    
    mod <- which(rem_models == input$p_mod)
    phi <- eval(parse(text = paste0("phi_", mod)))
    for (tv in time_values)
    {
      od_plot_list[[i]] <- 
        ggplot(data = phi[which(phi$TSSR == input$tssr & 
                                       phi$Time == tv &
                                       phi$Species == input$sp),]) +
        geom_line(aes(x = OD, y = p)) +
        geom_ribbon(aes(x = OD, ymin = p_2.5, ymax = p_97.5),
                    alpha = 0.25) +
        #stat_summary(aes(x = OD, y = p), fun = mean, geom = "smooth", size = 1.25) +
        ylim(0, 1) +
        theme(legend.position = "none")
      i <- i + 1
    }
    
    ggmatrix(
      od_plot_list,
      ncol = length(time_values),
      nrow = 1,
      xAxisLabels = c("1 min", "3 min", "5 min", "10 min"),
      title = paste0("Species ",
                     input$sp)
    )
  })
  
}

####### Run the App! ##############################

shinyApp(ui, server)
