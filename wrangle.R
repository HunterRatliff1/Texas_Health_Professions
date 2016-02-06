## Load the required packages, and name 
## a few globals

require(dplyr)
require(maptools) # required for rgdal to work correctly
require(tigris)
require(stringr)
require(tidyr)
require(googlesheets)
require(reshape2)
require(leaflet)
# require(acs)


# SOURCE: http://healthdata.dshs.texas.gov/Population
Population <- gs_title("Texas Medical Specialties") %>% 
  gs_read_csv("Population") %>% 
  
  # Rename some columns
  rename(
    RE=Race..Ethnicity, 
    MSA=Metropolitan.Statistical.Area, 
    PHR=Public.Health.Region,
    County = Counties) %>%
  
  # Melt the data.frame
  melt(id.vars = c("FIPS", "County", "RE", "MSA", "PHR", "Year", "Sex"),
       variable.name="Age", value.name="Count") %>%
  
  # Clean up the age category vector
  mutate(Age = gsub("AGE_", "", Age)) %>%
  
  # Split the age category
  separate(Age, into=c("Age.lower", "Age.upper"), sep="\\.", remove=F) %>%
  mutate(Age = gsub("\\.", "-", Age)) %>%
  tbl_df()

# Make the Age groups ordered factors
Population$Age <- factor(Population$Age, ordered = T,
       levels = c("00-01", "01-04", "05-09","10-14", "15-19", "20-24", "25-29", 
                  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
                  "60-64", "65-69", "70-74", "75-79", "80-84", "85-100"))

# Save the population as a RDS & CSV file
Population %>% saveRDS("Data/Population-full.RDS")
Population %>% write.csv("Data/Population-full.csv")

Population %>%
  filter(Year==2013) %>%
  group_by(Age, Sex) %>% summarise(Count = sum(Count)) %>%
  ggplot(aes(x=Age, y=Count, fill=Sex)) + geom_bar(stat="identity")

Population %>%
  filter(Year==2013, Age != "00-01") %>%
  group_by(Age, County) %>% summarise(Count = sum(Count)) %>%
  ggplot(aes(x=Age, y=Count)) + geom_path(aes(group=County)) + coord_flip()

# Get shapefiles of all Texas Counties
TX_Counties <- counties(state = "TX", cb = T)
saveRDS(TX_Counties, "Data/TX_Counties.RDS") # save as RDS file



# Pull the Texas Medical Specialties data from the Google Sheet
gs_title("Texas Medical Specialties") %>% 
  gs_read_csv("2015") %>%
  melt(id.vars = c("County", "Population"), variable.name="HPO", value.name="Count") %>%
  mutate(Per100k = round(Count / (Population/100000), 2)) %>%
  tbl_df()
  select(-RNs) %>%
  write.csv(file = "Data/HPO.csv")

# Find as a rate of HPO per 100,000 people
HPOs_per100k <- HPOs %>%
  melt(id.vars = c("County", "Population")) %>%
  mutate(value = round(value / (Population/100000), 2)) %>%
  dcast(County+Population ~ variable) %>%
  tbl_df()

HPOs %>%
  melt(id.vars = c("County", "Population")) %>%
  group_by(variable) %>%
  mutate(
    value  = value / sum(value),
    value = round(cume_dist(value), 2)) %>% ungroup() %>%
  dcast(County+Population ~ variable) %>%
  tbl_df()

# Join 'HPOs' data.frame to the TX_Counties shapefile
HPOs <- geo_join(
  spatial_data = TX_Counties, 
  data_frame   = HPOs, 
  by_sp        = "NAME", 
  by_df        = "County")

# Join 'HPOs_per100k' data.frame to the TX_Counties shapefile
HPOs_per100k <- geo_join(
  spatial_data = TX_Counties, 
  data_frame   = HPOs_per100k, 
  by_sp        = "NAME", 
  by_df        = "County")


# Define palette
pal_abs <- colorNumeric(palette = "YlGnBu", domain = NULL)

# # Define pop-up
# popup <- paste0(
#   "County: ", data_frame$County, "<hr>", 
#   "GEOID: ",  data_frame$GEOID,  "<br>", 
#   "Percentage of people ", age_group, " with: <br>",
#   "Private health insurance: <code>",          round(data_frame$Private,2), "% </code><br>",
#   "Public health insurance: <code>",           round(data_frame$Public,2),  "% </code><br>",
#   "Private & public health insurance: <code>", round(data_frame$Both,2),    "% </code><br>",
#   "No health insurance: <code>",               round(data_frame$Neither,2), "% </code><br>")

# Define the leaflet map
leaflet() %>%
  
  ## Add the base tiles
  addProviderTiles("CartoDB.Positron") %>%
  
  ## Add the four polygons (i.e. types of coverage)
  addPolygons(data = HPOs_per100k, 
              group = "Veterinarians", fillColor = ~pal_abs(DVM), 
              # popup=popup,
              color="#b2aeae", fillOpacity=0.5, weight=1, smoothFactor=0.2) %>%
  addPolygons(data = HPOs_per100k, 
              group = "Pharmacists", fillColor = ~pal_abs(PharmD), 
              color="#b2aeae", fillOpacity=0.5, weight=1, smoothFactor=0.2) %>%
  addPolygons(data = HPOs_per100k, 
              group = "Social Workers", fillColor = ~pal_abs(SWs), 
              color="#b2aeae", fillOpacity=0.5, weight=1, smoothFactor=0.2) %>%
  addPolygons(data = HPOs_per100k, 
              group = "PTs", fillColor = ~pal_abs(PTs), 
              color="#b2aeae", fillOpacity=0.5, weight=1, smoothFactor=0.2) %>%
  addPolygons(data = HPOs_per100k, 
              group = "PAs", fillColor = ~pal_abs(PAs), 
              color="#b2aeae", fillOpacity=0.5, weight=1, smoothFactor=0.2) %>%
  addPolygons(data = HPOs_per100k, 
              group = "Primary Care Physicians", fillColor = ~pal_abs(PCPs), 
              color="#b2aeae", fillOpacity=0.5, weight=1, smoothFactor=0.2) %>%
  addPolygons(data = HPOs_per100k, 
              group = "Psychiatrists", fillColor = ~pal_abs(Psychiatrist), 
              color="#b2aeae", fillOpacity=0.5, weight=1, smoothFactor=0.2) %>%
  addPolygons(data = HPOs_per100k, 
              group = "Family Medicine", fillColor = ~pal_abs(FM), 
              color="#b2aeae", fillOpacity=0.5, weight=1, smoothFactor=0.2) %>%
  addPolygons(data = HPOs_per100k, 
              group = "General Practice", fillColor = ~pal_abs(General.Practice), 
              color="#b2aeae", fillOpacity=0.5, weight=1, smoothFactor=0.2) %>%
  addPolygons(data = HPOs_per100k, 
              group = "Geriatrics", fillColor = ~pal_abs(Geriatrics), 
              color="#b2aeae", fillOpacity=0.5, weight=1, smoothFactor=0.2) %>%
  addPolygons(data = HPOs_per100k, 
              group = "Internal Medicine", fillColor = ~pal_abs(IM), 
              color="#b2aeae", fillOpacity=0.5, weight=1, smoothFactor=0.2) %>%
  addPolygons(data = HPOs_per100k, 
              group = "Women's Health", fillColor = ~pal_abs(OB.GYN), 
              color="#b2aeae", fillOpacity=0.5, weight=1, smoothFactor=0.2) %>%
  addPolygons(data = HPOs_per100k, 
              group = "Pediatrics", fillColor = ~pal_abs(Pediatrics), 
              color="#b2aeae", fillOpacity=0.5, weight=1, smoothFactor=0.2) %>%
  
#   ## Add the legand
#   addLegend(pal = pal_abs, values = c(0,100), 
#             title = paste0("Precentage (", age_group, ")"), 
#             position="bottomright", labFormat=labelFormat(suffix = "%")) %>%
  
  ## Add layers control
  addLayersControl(
    baseGroups = c("Veterinarians", "Pharmacists", "Social Workers", "PTs", "PAs", 
                   "Primary Care Physicians", "Psychiatrists", "Family Medicine", 
                   "General Practice", "Geriatrics", "Internal Medicine", 
                   "Women's Health", "Pediatrics"),
                   options = layersControlOptions(collapsed = FALSE))



x@data %>%
  select(County:Peds) %>%
  melt(id.vars = c("County", "Population")) %>%
  filter(variable==input$HP_type) %>%
  ggvis(~County, ~value) %>%
  layer_points()


x <- bind_rows(
  (gs_title("TX Births") %>% gs_read_csv("Table10")),
  (gs_title("TX Births") %>% gs_read_csv("Table11"))) %>%
  filter(!grepl("REGION", Location)) %>%
  mutate(Location = str_to_title(Location)) %>%
  rename(County=Location) %>%
  melt(measure.vars=c("X2013", "X2012", "X2011", "X2010", "X2009", 
                      "X2008", "X2007", "X2006", "X2005", "X2004"),
       variable.name="Year") %>%
  mutate(Year  = as.numeric(gsub("X", "", Year))) %>%
  mutate(value = as.numeric(value)) %>%
  tbl_df() 

x %>%
  filter(Statistic=="Mothers 17 Years of Age and Younger") %>%
  ggplot(aes(x=Year, y=value, group=County, color=Race.Ethnicity)) +
  geom_line() + geom_point()

  
  ggplot(aes(x=Location, y=value, size=Year, color=Race.Ethnicity, shape=Statistic)) +
  geom_point()

