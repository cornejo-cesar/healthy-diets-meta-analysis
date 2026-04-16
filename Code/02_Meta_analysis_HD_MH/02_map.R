##########################
# 1.  LOAD LIBRARIES     #
##########################

# Install 's2' package binary (required for spatial operations)
renv::install("s2", type = "binary")


# Data loading and manipulation
library(readxl)
library(dplyr)
library(tidyr)   

# Spatial data
library(sf)

# Visualization
library(ggplot2)
library(patchwork)

##########################
# 2.  PREPARE THE DATA   #
##########################

# Load data
countries <- read_excel(file.path(data_path, "Countries.xlsx"))
countries <- countries %>%
  filter(!is.na(country)) 

# Load data map 
world <- st_read(file.path(data_map, "ne_50m_admin_0_countries.shp"))

# Remove Antarctica
world <- world %>%
  filter(ADMIN != "Antarctica")

# Transform coordinate reference system
world <-  st_transform(world, crs = "WGS84")

# Count number of studies per country
country_studies <- countries %>%
  group_by(country) %>%
  summarise(studies_count = n(), .groups = "drop")

# Check for countries that do not match names in the map data
setdiff(country_studies$country, world$ADMIN)

# Check specific country naming conventions in the 'world' dataset
unique(world$ADMIN[grepl("Czech", world$ADMIN)])  
unique(world$ADMIN[grepl("Korea", world$ADMIN)]) 
unique(world$ADMIN[grepl("Netherlands", world$ADMIN)])  
unique(world$ADMIN[grepl("Emirates", world$ADMIN)])  
unique(world$ADMIN[grepl("Tanzania", world$ADMIN)])  
unique(world$ADMIN[grepl("United", world$ADMIN)])

# Ensure country names match those in the `world` dataset (Standardization)
country_studies <- country_studies %>%
  mutate(country = recode(country, 
                          "Czech Republic" = "Czechia",
                          "Korea - South (Republic of Korea)" = "South Korea",
                          "Netherlands, The" = "Netherlands",
                          "United Arab Emirates (UAE)" = "United Arab Emirates",
                          "Tanzania" = "United Republic of Tanzania"))  

# Merge with geographic layer and replace NAs with 0
world_data <- world %>%
  left_join(country_studies, by = c("ADMIN" = "country")) %>%
  mutate(studies_count = replace_na(studies_count, 0))  # Assign 0 to NAs

# Define categories (bins) from 0 to 60
world_data <- world_data %>%
  mutate(studies_category = cut(studies_count,
                                breaks = c(0, 1, 2, 5, 10, 20, 30, 40, 50, 60),  
                                labels = c("0", "1-2", "3-5", "6-10", "11-20", "21-30", 
                                           "31-40", "41-50", "51+"),
                                include.lowest = TRUE, right = FALSE)) 


# Create subset for countries with at least 1 study (for labeling purposes)
label_data <- world_data %>%
  filter(studies_count > 0) %>%  # is.na() removed as NAs were already replaced
  mutate(centroid = st_centroid(geometry),
         coords = st_coordinates(centroid),
         X = coords[,1], 
         Y = coords[,2])


##########################################
# 3. PLOT THE MAP
##########################################

# Map with new categories
mapa_plot <- ggplot(data = world_data) +
  geom_sf(aes(fill = studies_category), 
          color = "white",  
          size = 0.3) +    
  
  # 2. Color scale in blue and red tones (similar to The Economist style)
  scale_fill_manual(
    values = c("0"     = "#C91D42",   # Dark red for countries with 0 studies
               "1-2"   = "#F9D2DB",   # Medium red
               "3-5"   = "#EBEDFA",   # Light red
               "6-10"  = "#D6DBF5",   # Light green/blue tone
               "11-20" = "#475ED1",   # Strong green/blue tone
               "21-30" = "#2E45B8",   # Strong blue
               "31-40" = "#1F2E7A",   # Darker blue
               "41-50" = "#141F52",   # Almost black blue
               "51+"   = "#0A1F5B",   # Intense blue
               "No data" = "#E1DFD0"), # Grey for countries with no data
    guide = "none",  
    na.value = "black" 
  ) +
  
  theme_void() +  # Theme without axes or text
  theme(
    panel.grid = element_blank(),
    plot.background = element_blank()
  ) +
  
  coord_sf(expand = FALSE)

# Save the plot without additional elements
ggsave(file.path(output_path, "Maps/world_map.png"), width = 8, height = 6, dpi = 600)

