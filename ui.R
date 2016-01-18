# # # # # # # # # # # # # # # # # # # # # # # # # #
#                REQUIRED PACKAGES                #
# # # # # # # # # # # # # # # # # # # # # # # # # #
library(shiny)
library(dplyr)
library(RColorBrewer)
library(magrittr)
library(leaflet)


# # # # # # # # # # # # # # # # # # # # # # # # # #
#               Load Required Data                #
# # # # # # # # # # # # # # # # # # # # # # # # # #
HPOs <- read.csv("Data/HPO.csv") %>% select(-X)


# # # # # # # # # # # # # # # # # # # # # # # # # #
#                    SHINY UI                     #
# # # # # # # # # # # # # # # # # # # # # # # # # #
shinyUI(
  navbarPage(
    title="Texas Health Professions", id="nav",
    tabPanel(
      "Interactive map by county", 
      div(
        class="outer",
        
        # Include our custom CSS
        tags$head(includeCSS("styles.css")),
        
        # Show a plot of the map
        leafletOutput("leaf_map", width="100%", height="100%"),
        
        absolutePanel(
          id = "controls", class = "panel panel-default", fixed = TRUE,
          draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
          width = 330, height = "auto",
          
          h2("Controls"),
                      
          # Select the color scheme to use
          selectInput("colors", "Color Scheme", selected = "Spectral",
                      rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
          # Type of measurement to use
          radioButtons(inputId  = "measurement",
                       label    = "Type of data to show:",
                       choices  = c("Per 100,000 citizens"="per100k", "Total Count"="total")),
          
          # Health profession selected
          selectizeInput(inputId = "HP_type",
                         label   = "Health Profession:",
                         choices = c(
                           "Veterinarians"="DVM", "Pharmacists"="PharmD", 
                           "Social Workers"="SWs", "Physical Therapists"="PTs", 
                           "Physician Assistants"="PAs", "Primary Care Physicians"="PCPs",
                           "Psychiatrists"="Psych", "Family Medicine"="FM",
                           "General Practice"="GPs", "Geriatrics"="Gers", 
                           "Internal Medicine"="IM", "Women's Health"="OB.GYN",
                           "Pediatrics"="Peds", "Citizen Population"="Population"),
                         selected = c("Women's Health"="OB.GYN")),
          plotOutput("plot",  height = 200),
          p("@HunterRatliff1 :: © 2016 Hunter Ratliff")
        )
      )
    ),
    tabPanel(
      "Data Table", 
      
      fluidRow(  # Create a new Row in the UI for selectInputs
        
        # Option to select a county
        column(6, selectInput(
          inputId = "County", label   = "County:",
          selected = c("Travis", "Bastrop", "Burnet", "Blanco",
                       "Hays", "Caldwell", "Lee", "Milam", "Bell"),
          multiple = TRUE, width = "100%",
          choices = unique(as.character(HPOs$County)))),
        
        # Health profession selected
        column(6, selectInput(
          inputId = "profession", label   = "Health Profession:",
          multiple = TRUE, width = "100%",
          selected = c("Veterinarians"="DVM", "Social Workers"="SWs",
          "Physical Therapists"="PTs", "Physician Assistants"="PAs",
          "Primary Care Physicians"="PCPs", "Psychiatrists"="Psych", 
          "Geriatrics"="Gers", "Women's Health"="OB.GYN",
          "Pediatrics"="Peds"),
          choices = c(
            "Veterinarians"="DVM", "Pharmacists"="PharmD", 
            "Social Workers"="SWs", "Physical Therapists"="PTs", 
            "Physician Assistants"="PAs", "Primary Care Physicians"="PCPs",
            "Psychiatrists"="Psych", "Family Medicine"="FM",
            "General Practice"="GPs", "Geriatrics"="Gers", 
            "Internal Medicine"="IM", "Women's Health"="OB.GYN",
            "Pediatrics"="Peds", "Citizen Population"="Population")))
      ),
      
      
      
      # Create a new row for the table
      fluidRow(
        DT::dataTableOutput("table")
      ),
      fluidRow(
        hr(),
        p(
          strong("Source: "),
          a("State of Texas Department of State Health Services", 
            href="http://www.dshs.state.tx.us/chs/hprc/health.shtm")),
        
        p(
          a("@HunterRatliff1", href="https://twitter.com/HunterRatliff1"),
          strong(" | "),
          "© 2016 Hunter Ratliff",
          strong(" | "),
          a("Source on Github", href="https://github.com/hunterratliff1/Texas_Health_Professions")
        )
      )
    )
  )
)


# shinyUI(fluidPage(
# 
#   # Application title
#   titlePanel("Texas Health Professions"),
# 
#   # Sidebar with a slider input for number of bins
#   sidebarLayout(
#     sidebarPanel(
#       # Select the color scheme to use
#       selectInput("colors", "Color Scheme", selected = "Spectral",
#                   rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
#       
#       # Type of measurement to use
#       radioButtons(inputId  = "measurement",
#                    label    = "Type of data to show:",
#                    choices  = c("Per 100,000 citizens"="per100k", "Total Count"="total")),
#       
#       # Health profession selected
#       selectizeInput(inputId = "HP_type",
#                      label   = "Health Profession:",
#                      choices = c(
#                        "Veterinarians"="DVM", "Pharmacists"="PharmD", 
#                        "Social Workers"="SWs", "Physical Therapists"="PTs", 
#                        "Physician Assistants"="PAs", "Primary Care Physicians"="PCPs",
#                        "Psychiatrists"="Psych", "Family Medicine"="FM",
#                        "General Practice"="GPs", "Geriatrics"="Gers", 
#                        "Internal Medicine"="IM", "Women's Health"="OB.GYN",
#                        "Pediatrics"="Peds", "Citizen Population"="Population"),
#                      selected = c("Women's Health"="OB.GYN"))),
#     
#     # Show a plot of the map
#     mainPanel(
#       leafletOutput("leaf_map")
#     )
#   )
# ))
