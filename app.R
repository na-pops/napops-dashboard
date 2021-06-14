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

# Load quantitative summary files
load("../results/quant-summary/summary_statistics.rda")
load("../results/quant-summary/dis_species_summary.rda")
load("../results/quant-summary/rem_species_summary.rda")

# Load spatial summary files
load("../results/spatial-summary/project_coverage_bcr_state.rda")
load("../results/spatial-summary/project_coverage_bcr.rda")
load("../results/spatial-summary/project_coverage_state.rda")
load("../results/spatial-summary/dis_coverage_bcr.rda")
load("../results/spatial-summary/rem_coverage_bcr.rda")

# Load files related to distance sampling and set constants
load("../results/aic/dis_aic.rda")
tau_files <- list.files(path = "../results/simulations/tau")
for (f in tau_files)
{
  load(paste0("../results/simulations/tau/", f))
}
dis_models <- c("(1) Null Model", "(2) Road Model", "(3) Forest Coverage Model",
                "(4) Road AND Forest Model (Additive)", "(5) Road AND Forest Model (Interaction)")
forest_level <- c(1.0, 0.0)

# Load files related to removal sampling and set constants
load("../results/aic/rem_aic.rda")
phi_files <- list.files(path = "../results/simulations/phi")
for (f in phi_files)
{
  load(paste0("../results/simulations/phi/", f))
}
rem_models <- c("(1) Null Model", "(2) Time-since-sunrise (TSSR) Model",
                "(3) Julian Day (JD) Model", "(4) TSSR + TSSR^2 Model",
                "(5) JD + JD^2 Model", "(6) TSSR + JD Model",
                "(7) TSSR + TSSR^2 + JD Model",
                "(8) TSSR + JD + JD^2 Model",
                "(9) TSSR + TSSR^2 + JD + JD^2 Model")
time_values <- c(1, 3, 5, 10)

project_coverage <- bcr_coverage

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "NA-POPS Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Project Overview", tabName = "overview"),
      menuItem("Removal *", tabName = "removal"),
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
      h5("Models Run: 18 March 2021"),
      h5("* Removal results are as of"),
      h5("14 January 2021 run")
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
                         tabPanel("Julian Day",
                                  plotOutput("rem_jd_hist")),
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
                column(width = 4,
                       selectInput(inputId = "p_mod", 
                                   label = "Model",
                                   choices = rem_models,
                                   selected = rem_models[1]),
                       h2("How to Interpret"),
                       "The plots on the left display the probability that a bird
                       gives a cue (availability, p), modelled by Julian Day (JD) and Time-since-local-sunrise (TSSR). 
                       Use the sliders to see how the availability curve changes with different 
                       values of JD and TSSR.
                       
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
  
  ################ Distance Species Overview Functions ##########
  
  output$dis_species <- renderValueBox({
    valueBox(value = input$sp,
             subtitle = "Species",
             icon = icon("crow"),
             color = "olive")
  })
  
  output$dis_sampling <- renderValueBox({
    valueBox(value = nrow(dis_species_summary[[input$sp]]),
             subtitle = "Sampling Events",
             icon = icon("clipboard"),
             color = "olive")
  })
  output$dis_bcr <- renderValueBox({
    valueBox(value = length(which(bcr_dis_coverage[[input$sp]]$ncounts > 0)),
             subtitle = "BCRs",
             icon = icon("map"),
             color = "olive")    
  })

  output$dis_coverage_map <- renderLeaflet({
    
    pal <- colorNumeric(viridis(10), NULL)
    
    leaflet(bcr_dis_coverage[[input$sp]]) %>%
      addPolygons(stroke = FALSE,
                  fillColor = ~pal(ncounts),
                  layerId = ~ST_12,
                  fillOpacity = 1.0) %>%
      addLegend(position = 'bottomright', pal = pal, 
                values = bcr_dis_coverage[[input$sp]]$ncounts, title = 'Sampling Events')
    
  })
  
  output$dis_forest_hist <- renderPlot({
    ggplot(data = dis_species_summary[[input$sp]]) +
      geom_histogram(aes(x = ForestOnly_5x5)) +
      xlab("Forest Coverage") +
      ylab("Sampling Events") +
      NULL
  })
  
  output$dis_road_bar <- renderPlot({
    ggplot(data = dis_species_summary[[input$sp]]) +
      geom_bar(aes(x = ifelse(roadside == 1, "On-Road", "Off-Road"))) +
      xlab("Roadside Status") +
      ylab("Sampling Events") +
      NULL
  })
  
  ################ Distance Perceptability Functions ############
  
  output$q_aic <- output$edr_aic <- renderTable(dis_aic[[input$sp]],
                                                striped = TRUE,
                                                bordered = TRUE,
                                                hover = TRUE)
  
  output$q_curve_plotter <- renderPlot({
    if (input$q_mod == dis_models[1])
    {
      ggplot(data = tau_1[which(tau_1$Species == input$sp),]) +
        geom_line(aes(x = Radius, y = q)) +
        geom_ribbon(aes(x = Radius, ymin = q_2.5, ymax = q_97.5),
                    alpha = 0.25) +
        ylim(0, 1) +
        theme(legend.position = "none") +
        ggtitle(paste0("Species: ", input$sp)) +
        NULL
    }else if (input$q_mod == dis_models[2])
    {
      tau_2$Roadside_Status <- ifelse(tau_2$Roadside == 1, "On-Road", "Off-road")
      
      ggplot(data = tau_2[which(tau_2$Species == input$sp),]) +
        geom_line(aes(x = Radius, y = q, color = Roadside_Status)) +
        geom_ribbon(aes(x = Radius, ymin = q_2.5, ymax = q_97.5, color = Roadside_Status),
                    alpha = 0.25) +
        ylim(0, 1) +
        #theme(legend.position = "none") +
        ggtitle(paste0("Species: ", input$sp)) +
        NULL
    }else if (input$q_mod == dis_models[3])
    {
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
    }else if (input$q_mod == dis_models[4])
    {
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
    }else if (input$q_mod == dis_models[5])
    {
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
    }
  })
  
  ################ Distance EDR Functions #######################
  
  output$edr_plotter <- renderPlot({
    if (input$edr_mod == dis_models[1])
    {
      to_plot <- tau_1[which(tau_1$Species == input$sp), c("tau", "tau_2.5", "tau_97.5")]
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
    }else if (input$edr_mod == dis_models[2])
    {
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
    }else if (input$edr_mod == dis_models[3])
    {
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
    }else if (input$edr_mod == dis_models[4])
    {
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
    }else if (input$edr_mod == dis_models[5])
    {
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
    
    pal <- colorNumeric(viridis(10), NULL)
    
    leaflet(bcr_rem_coverage[[input$sp]]) %>%
      addPolygons(stroke = FALSE,
                  fillColor = ~pal(ncounts),
                  layerId = ~ST_12,
                  fillOpacity = 1.0) %>%
      addLegend(position = 'bottomright', pal = pal, 
                values = bcr_rem_coverage[[input$sp]]$ncounts, title = 'Sampling Events')
    
  })
  
  output$rem_jd_hist <- renderPlot({
    ggplot(data = rem_species_summary[[input$sp]]) +
      geom_histogram(aes(x = (JD*365))) +
      xlab("Julian Day") +
      ylab("Sampling Events") +
      NULL
  })
  
  output$rem_tssr_hist <- renderPlot({
    ggplot(data = rem_species_summary[[input$sp]]) +
      geom_histogram(aes(x = (TSSR*24))) +
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
    
    mod <- which(rem_models == input$p_mod)
    phi <- eval(parse(text = paste0("phi_", mod)))
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
