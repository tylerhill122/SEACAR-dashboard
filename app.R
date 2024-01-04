library(rstudioapi)
library(shiny)
library(dplyr)
library(leaflet)
library(data.table)
library(stringr)
library(sf)
library(tidyr)
library(mapview)
library(magick)
library(bslib)
library(bsicons)
library(ggplot2)
library(plotly)

source("load_shape_samples.R")
source("seacar_data_location.R")

# load in habitat files
files <- list.files(seacar_data_location)
habitat_files <- str_subset(files, "All_")

# file_loc function to return full filepath name
file_loc <- function(habitat){return(paste0(seacar_data_location, str_subset(habitat_files, habitat)))}

oyster <- fread(file_loc("Oyster"))
oyster <- oyster[Include == 1 & MADup==1, ] 

sav <- fread(file_loc("SAV"))
sav <- sav[Include == 1 & MADup==1, ] 

# coral <- fread(file_loc("CORAL")) 
# cw <- fread(file_loc("CW"))
# nekton <- fread(file_loc("NEKTON"))

# Functions ----
# returns number of programs for each parameter
program_param_plot <- function(data=oyster, ret="plot"){
  
  program_params <- data %>%
    group_by(ParameterName) %>%
    summarise(n = length(unique(ProgramID)))
  
  plot <- ggplot(program_params, aes(x=0, xend=n, y=ParameterName, yend=ParameterName)) +
    geom_segment(linewidth=14, colour="#4472C4") +
    geom_text(aes(x=n, label=n, hjust=-0.3), color="black") + 
    labs(title="Number of Programs for each Indicator",
         x="Number of Programs",
         y="Indicator")
  
  if (ret == "plot"){
    return(plot)
  } else if(ret == "list"){
    return(program_params$ParameterName)
  }
}

# returns number of years of data for each ProgramID
program_years_plot <- function(data=oyster, pal){
  
  program_years <- data %>%
    group_by(ProgramID) %>%
    summarise(MinYear = min(Year),
              MaxYear = max(Year))
  
  program_years$ProgramID <- as.factor(program_years$ProgramID)
  
  program_years_plot <- ggplot(program_years, aes(x=MinYear-0.1, xend=MaxYear+0.1, y=ProgramID, yend=ProgramID)) +
    geom_segment(linewidth=4, colour=pal(program_years$ProgramID)) +
    labs(title="Years of data for each Program ID",
         x="Years",
         y="Program ID")
  
  return(program_years_plot)
  
}

# Mapping ----
# create map objects at start to make maps more efficiently within dashboard
# function to create map dataframes for each habitat
map_dataframes <- function(habitat){
  if (habitat == "Oyster"){
    data <- oyster
  } else if (habitat == "Submerged Aquatic Vegetation"){
    data <- sav
  }
  
  programs <- unique(data$ProgramID)
  
  ma_data <- data %>%
    group_by(ProgramLocationID, ProgramID, ProgramName, LocationID) %>%
    summarise(n_data = n(), years = list(sort(unique(Year))), params = list(unique(ParameterName)))
  
  df <- point %>% filter(ProgramID %in% programs)
  
  combined_df <- merge(x=ma_data, y=df, by.x=c("ProgramLocationID", "ProgramID", "LocationID"), by.y=c("ProgramLoc","ProgramID","LocationID"))
  
  combined_df <- combined_df %>%
    mutate(popup = paste("ProgramID: ", ProgramID, "<br> ProgramName: ", ProgramName, "<br> LocID: ", LocationID, "<br> ProgLocID: ", ProgramLocationID, 
                         "<br> N_Data: ", n_data, "<br> Years: ", years, "<br> params: ", params),
           label = paste0(ProgramID, ": ", ProgramName, " - LocID: ", LocationID))
  
  return(combined_df)
}

# assign map dataframes for each habitat for access within sample_map function
oyster_map_data <- map_dataframes("Oyster")
sav_map_data <- map_dataframes("Submerged Aquatic Vegetation")

# produce leaflet map of sample locations by program
sample_map <- function(habitat="Oyster", pal){
  
  if (habitat == "Oyster"){
    data <- oyster_map_data
  } else if (habitat == "Submerged Aquatic Vegetation"){
    data <- sav_map_data
  }
  
  rad <- sqrt(data$n_data)/5
  
  map <- leaflet(data) %>%
    addTiles(group="Default") %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels, group = "Positron by CartoDB") %>%
    # addLegend(title = "Program ID",
    #           pal=pal,
    #           position="bottomleft",
    #           values=~ProgramID,
    #           labFormat=labelFormat(prefix="Program "))  %>%
    addMapPane("background", zIndex = 400) %>%
    addMapPane("foreground", zIndex = 500) %>%
    addPolygons(data=orcp_shp, color="#4e809c", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.2,
                group="ORCP Boundaries", options = pathOptions(pane = "background")) %>%
    addLayersControl(
      overlayGroups = c("ORCP Boundaries"),
      baseGroups = c("Default", "Positron by CartoDB"),
      options = layersControlOptions(collapsed = TRUE)) %>%
    addCircleMarkers(lat=~Latitude_D, lng=~Longitude_, color=~pal(ProgramID),
                     weight=1, radius=rad, fillOpacity=0.4,
                     popup = ~popup,
                     label = ~label,
                     options = pathOptions(pane = "foreground"))
  
  return(map %>% hideGroup(c("ORCP Boundaries")))
}

habitats <- c("Oyster","Submerged Aquatic Vegetation")

ui <- fluidPage(
  titlePanel("SEACAR Dashboard"),
  fluidRow(
    column(4,
           wellPanel(
             selectInput(inputId = "habitatSelect",
                         label = "Select Habitat to view",
                         choices = habitats,
                         selected = "Oyster"),
             plotOutput("param_plot"),
             selectInput(inputId = "programSelect",
                         label = "Select Program to Preview Summary Statistics",
                         choices = "Choose a Program")
           ),
    ),
    column(4, 
           plotOutput("program_plot")),
    column(4, 
           leafletOutput("leaflet_map"))
  ),
  fluidRow(
    column(12,
           tableOutput("output_table"))
  )
)

server <- function(input, output, session){
  
  pal <- reactive({
    if (input$habitatSelect == "Oyster"){
      colorFactor("plasma", oyster_map_data$ProgramID)
    } else if(input$habitatSelect == "Submerged Aquatic Vegetation"){
      colorFactor("plasma", sav_map_data$ProgramID)
    }
  })
  
  data <- reactive({
    if (input$habitatSelect == "Oyster"){
      oyster
    } else if(input$habitatSelect == "Submerged Aquatic Vegetation"){
      sav
    }
  })
  
  data_summary <- reactive({
    if (input$habitatSelect == "Oyster"){
      oyster[ProgramID==input$programSelect, ]
    } else if(input$habitatSelect == "Submerged Aquatic Vegetation"){
      sav[ProgramID==input$programSelect, ]
    }
  })
  
  output$param_plot <- renderPlot(program_param_plot(data=data(), ret="plot"))
  
  output$program_plot <- renderPlot(program_years_plot(data=data(), pal()))
  
  output$leaflet_map <- renderLeaflet(sample_map(habitat=input$habitatSelect, pal()))
  
  output$output_table <- renderTable(head(data_summary()))
  
  observe({
    
    included_programs <- as.integer(unique(data()$ProgramID))
    
    updateSelectInput(session = session, "programSelect", 
                      label = "Select Program to Preview Summary Statistics", 
                      choices = included_programs,
                      selected = head(included_programs,1))
    
  })
  
}

shinyApp(ui = ui, server = server)