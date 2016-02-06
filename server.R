# # # # # # # # # # # # # # # # # # # # # # # # # #
#                REQUIRED PACKAGES                #
# # # # # # # # # # # # # # # # # # # # # # # # # #
library(magrittr)
library(shiny)
library(dplyr)
library(reshape2)
library(leaflet)
library(scales)
library(RColorBrewer)
library(ggplot2)


# # # # # # # # # # # # # # # # # # # # # # # # # #
#               Load Required Data                #
# # # # # # # # # # # # # # # # # # # # # # # # # #
HPOs        <- read.csv("Data/HPO.csv") %>% select(-X)
TX_Counties <- readRDS("Data/TX_Counties.RDS")


# # # # # # # # # # # # # # # # # # # # # # # # # #
#                  SHINY SERVER                   #
# # # # # # # # # # # # # # # # # # # # # # # # # #
shinyServer(function(input, output) {
  
  # --- # --- # --- # --- # --- # --- # --- # --- #
  #                  REACTIVE FXN                 #
  #   Filter by 'measurement' & 'HP_type' input   #
  # --- # --- # --- # --- # --- # --- # --- # --- #
  filteredData <- reactive({
    ## Reactive expression for the data subsetted based
    ## on the user input. Specifically, this looks at 
    ## the 'measurement' input
    
    
    # If user selected 'per100k', then find that rate & save as a 
    # data.frame named 'df'
    if(input$measurement=="per100k") {
      df <- HPOs %>%
        melt(id.vars = c("County", "Population", "sqmi")) %>%
        mutate(value = round(value / (Population/100000), 2)) %>%
        dcast(County+Population+sqmi ~ variable) 
    } 
    else(
      df <- HPOs     # Otherwise just save 'HPOs' as the data.frame 
    )
    
    # This line of code sets the selected 'HP_type' as the 'Value' 
    df$Value <- df[,c(input$HP_type)]
    
    # Finally, merge the data.frame with 'TX_Counties'
    tigris::geo_join(
      spatial_data = TX_Counties, 
      data_frame   = df, 
      by_sp        = "NAME", 
      by_df        = "County")
  })
  
  
  # --- # --- # --- # --- # --- # --- # --- # --- #
  #                  REACTIVE FXN                 #
  #  Define the palette to be used with the map   #
  # --- # --- # --- # --- # --- # --- # --- # --- #
  pal <- reactive({
    ## This reactive expression represents the palette function,
    ## which changes as the user makes selections in UI.
    colorNumeric(input$colors, domain = NULL)
  })
  
  
  # --- # --- # --- # --- # --- # --- # --- # --- #
  #              OUTPUT: leaf_map                 #
  #      Output the static portion of the map     #
  # --- # --- # --- # --- # --- # --- # --- # --- #
  output$leaf_map <- renderLeaflet({
    ## Use leaflet() here, and only include aspects of the map that
    ## won't need to change dynamically (at least, not unless the
    ## entire map is being torn down and recreated).
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addPolygons(data = TX_Counties)
  })
  
  
  # --- # --- # --- # --- # --- # --- # --- # --- #
  #               OBSERVER FUNCTION               #
  # Change fill color & legand based on 'HP_type' #
  # --- # --- # --- # --- # --- # --- # --- # --- #
  observe({
    ## Incremental changes to the map (in this case, replacing the
    ## fill color when a new 'HP_type' is chosen) should be performed
    ## an observer. Each independent set of things that can change
    ## in should be managed in its own observer
    
    
    # Call the palette function which is reactive
    pal <- pal()
    
    # Get merged shapefile + data.frame
    merged_df <- filteredData()
    
    # Define pop-up
    popup <- paste0(
      "County: <b>",                    merged_df$County, "</b><br>", 
      "Citizen Population: ",     comma(merged_df$Population),  "<hr>", 
      
      "Veterinarians: <code>",           merged_df$DVM,    "</code><br>",
      "Pharmacists: <code>",             merged_df$PharmD, "</code><br>",
      "Social Workers: <code>",          merged_df$SWs,    "</code><br>",
      "Physical Therapists: <code>",     merged_df$PTs,    "</code><br>",
      "Physician Assistants: <code>",    merged_df$PAs,    "</code><br>",
      "Primary Care Physicians: <code>", merged_df$PCPs,   "</code><br>",
      "Psychiatrists: <code>",           merged_df$Psych,  "</code><br>",
      "Family Medicine: <code>",         merged_df$FM,     "</code><br>",
      "General Practice: <code>",        merged_df$GPs,    "</code><br>",
      "Geriatrics: <code>",              merged_df$Gers,   "</code><br>",
      "Internal Medicine: <code>",       merged_df$IM,     "</code><br>",
      "Women's Health: <code>",          merged_df$OB.GYN, "</code><br>",
      "Pediatrics: <code>",              merged_df$Peds,   "</code><br>")
    
    # Call the 'leaf_map' that was outputted earlier, and make adjustments
    leafletProxy("leaf_map") %>%
      
      ## Clear any old controls
      clearControls() %>%   
      
      ## Clear any old polygons
      clearShapes() %>%     
      
      ## Add the counties polygon based on the merged (filtered) data 
      addPolygons(data = merged_df,
                  fillColor = ~pal(Value), popup=popup,
                  color="#b2aeae", fillOpacity=0.5, weight=1, smoothFactor=0.2) %>%
      
      ## Add a new legend
      addLegend(pal = pal, values = merged_df$Value, position="bottomright")
  })
  
  
  # --- # --- # --- # --- # --- # --- # --- # --- #
  #                OUTPUT: plot                   #
  #  Output plot that fits in the control pannel  #
  # --- # --- # --- # --- # --- # --- # --- # --- #
  output$plot <- renderPlot({
    merged_df <- filteredData()
    
    merged_df@data %>%
      dplyr::select(County:Peds) %>%
      melt(id.vars = c("County", "Population")) %>%
      filter(variable==input$HP_type) %>%
      qplot(data=., x=value) + labs(y="Number of counties")
      # layer_points()
    
    
  })
  
  
  # --- # --- # --- # --- # --- # --- # --- # --- #
  #                OUTPUT: table                  #
  #       Output an interactive data table        #
  # --- # --- # --- # --- # --- # --- # --- # --- #
  output$table <- DT::renderDataTable({
    HPOs %>%
      filter(County %in% input$County) %>%
      melt(id.vars = c("County", "Population")) %>%
      filter(variable %in% input$profession) %>%
      dcast(County+Population ~ variable) %>%
      DT::datatable()
  })
})
