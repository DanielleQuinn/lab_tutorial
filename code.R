# ---- Prepare Workspace ----
# Load packages
library(dplyr) # Data manipulation
library(stringr) # Strings
library(lubridate) # Dates and times
library(leaflet) # Interactive maps
library(rvest) # Web scraping

# Set working directory
setwd("C:/Users/Danielle/OneDrive/DQ_Documents/R/leaflet_tutorial")

# ---- Import Data ----
# The original data is found at https://open.canada.ca/data/en/dataset/4cedd37e-0023-41fe-8eff-bea45385e469
# The step-by-step data cleaning process is found at: <TBA>
# The data used for this tutorial is found at: https://github.com/DanielleQuinn/lab_tutorial
clean <- read.csv("clean_earthquake.csv")

# ---- Filter Data ----
# Only consider earthquakes recorded in 2019
recent <- clean %>%
  filter(year(date) == 2019)

# ---- Mapping: Basic World Map ----
# leaflet works similiar to ggplot; build the figure in layers
# but use a pipe (%>%) rather than a plus (+)
leaflet() %>%
  addTiles()

# ---- Mapping: Add Points ----
# Other marker types, but circles are the simplest to use
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = recent)

# ---- Mapping: Define Aesthetics ----
# Use ?addCircleMarkers to see other arguments and related functions
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = recent,
                   fillColor = "grey",
                   fillOpacity = 0.8,
                   color = "black",
                   weight = 1,
                   radius = 3)

# ---- Mapping: Add Color ----
# Match values of a colour palette to values in your data
pal <- colorNumeric("OrRd", domain = recent$magnitude)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = recent,
                   fillColor = ~pal(magnitude),
                   fillOpacity = 0.8,
                   color = "black",
                   weight = 1,
                   radius = 5) %>%
  addLegend(data = recent,
            pal = pal,
            values = ~magnitude,
            title = "Magnitude")

# ---- Mapping: Add Scale Bar ----
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = recent,
                   fillColor = ~pal(magnitude),
                   fillOpacity = 0.8,
                   color = "black",
                   weight = 1,
                   radius = 5) %>%
  addLegend(data = recent,
            pal = pal,
            values = ~magnitude,
            title = "Magnitude") %>%
  addScaleBar(position = "bottomleft")

# ---- Mapping: Add Labels ----
# Labels appear when you hover over a point
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = recent,
                   fillColor = ~pal(magnitude),
                   fillOpacity = 0.8,
                   color = "black",
                   weight = 1,
                   radius = 5,
                   label = ~magnitude.type) %>%
  addLegend(data = recent,
            pal = pal,
            values = ~magnitude,
            title = "Magnitude") %>%
  addScaleBar(position = "bottomleft")

# ---- Mapping: Add Popups ----
# Popups appear when you click on a point
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = recent,
                   fillColor = ~pal(magnitude),
                   fillOpacity = 0.8,
                   color = "black",
                   weight = 1,
                   radius = 5,
                   popup = ~magnitude.type) %>%
  addLegend(data = recent,
            pal = pal,
            values = ~magnitude,
            title = "Magnitude") %>%
  addScaleBar(position = "bottomleft")

# ---- Mapping: Add Custom Popups ----
# Custom pop ups need to be written in html!
# <b>Hello</b>     Hello, in bold text
# <br>     adds a line break
recent <- recent %>%
  mutate(mypopup = paste0("<b>Date: </b>", date, "<br>",
                          "<b>Magnitude: </b>",
                          magnitude, " ", magnitude.type, "<br>",
                          "<b>Depth: </b>", depth))
head(recent)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = recent,
                   fillColor = ~pal(magnitude),
                   fillOpacity = 0.8,
                   color = "black",
                   weight = 1,
                   radius = 5,
                   popup = ~mypopup) %>%
  addLegend(data = recent,
            pal = pal,
            values = ~magnitude,
            title = "Magnitude") %>%
  addScaleBar(position = "bottomleft")

# ---- Web Scraping ----
# We want to add the location of all monitoring staions with seismograms in Canada
# Luckily, this is all stored in a table at:
myURL <- "https://earthquakescanada.nrcan.gc.ca/stndon/wf-fo/index-en.php"

stations <- read_html(myURL) %>%
  html_nodes("table") %>%
  html_nodes("td") %>%
  html_text() %>%
  str_subset(pattern = "\n", negate = TRUE) %>%
  str_subset(pattern = "Loading", negate = TRUE) %>%
  matrix(ncol = 4, byrow = TRUE) %>%
  data.frame() %>%
  rename("station_id" = 1,
         "location" = 2,
         "latitude" = 3,
         "longitude" = 4)

head(stations)
str(stations)

# We see that latitude and longitude are being stored as factors; let's convert those to numeric
# Two step process; first to character, then to numeric
stations <- stations %>%
  mutate(across(c(latitude, longitude), fns = as.character),
         across(c(latitude, longitude), fns = as.numeric))
         
# Let's add a popup that includes a link to the real-time seismogram for each station

# Station ID: XXX
# See the shaking! <- link to website

# The link is found by adding ?channel= and the station id to the URL
# and wrapping it in some html code
# <a href = 'www.google.com'>Go To Google!</a>

stations <- stations %>%
  mutate(link = paste0(myURL, "?channel=", station_id),
         mypopup = paste0("<b>Station ID: </b>", station_id, "<br>",
                          "<a href='", link, "'>See the shaking!</a>"))

head(stations)

# We can create a map and store it in an object
station_map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = stations,
                   stroke = FALSE,
                   fillColor = "black",
                   fillOpacity = 1,
                   radius = 3,
                   popup = ~mypopup) %>%
  addScaleBar(position = "bottomleft")

station_map

# ---- Mapping: Overlay Points ----
# Add the earthquake records!
station_map %>%
  addCircleMarkers(data = recent,
                   fillColor = ~pal(magnitude),
                   fillOpacity = 0.8,
                   color = "black",
                   weight = 1,
                   radius = 5,
                   popup = ~mypopup) %>%
  addLegend(data = recent,
            pal = pal,
            values = ~magnitude,
            title = "Magnitude")
