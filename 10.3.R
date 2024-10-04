
library(ggplot2)
library(dplyr)
library(knitr)
library(tidyverse)
library(dplyr)
library(readtext)
library(tidyverse)
library(kableExtra)
library(rmarkdown)
library(tidycensus)




c_tract <- get_acs(geography = "tract", 
                   variables = B25034_002, 
                   state = "DC", 
                   county = "District of Columbia", 
                   geometry = TRUE,
                   year = 2022, 
                   tract = "11001000100") 

# Plot for the specific tract
ggplot(data = dc_tract) +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c() +
  
# Get data for all of D.C.
dc_all <- get_acs(geography = "tract", 
                  variables = B25034_002, 
                  state = "DC", 
                  county = "District of Columbia", 
                  geometry = TRUE,
                  year = 2022)


# Plot for all of D.C.
ggplot(data = dc_all) +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c() +
  labs(title = "Median Household Income in All of D.C.",
       fill = "Income") +
  theme_minimal()

# Code to highlight certain tracts in DC

dfw_tracts <- dfw_tracts %>%
  mutate(hotspot = case_when(
    localG >= 2.576 ~ "High cluster",
    localG <= -2.576 ~ "Low cluster",
    TRUE ~ "Not significant"
  ))

ggplot(dfw_tracts) + 
  geom_sf(aes(fill = hotspot), color = "grey90", size = 0.1) + 
  scale_fill_manual(values = c("red", "blue", "grey")) + 
  theme_void()