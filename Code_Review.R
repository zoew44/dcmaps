# Visualization (Maps)

##Poverty

pov2022
statepov22 <- pov2022 %>% 
  select(Name, Percent.in.Poverty) %>% 
  mutate(povper = Percent.in.Poverty) %>% 
  as_tibble()

statepov22
statepov22 %>% 
  ggplot(aes(x = povper, y = reorder(Name, povper))) +
  geom_point() + 
  xlab(Poverty Rate) + ylab(State)
labs(title = "Poverty Rates by US State", subtitle = "2022", caption = "Based on US Decennial Census Data")

total_data <- totalpop_ga %>%
  rename(total_population = estimate) %>% 
  select(GEOID,
         NAME,
         total_population) %>% 
  left_join(totalpov_ga %>% 
              rename(total_poverty = estimate) %>% 
              select(GEOID,
                     NAME,
                     total_poverty),
            by = c("GEOID",
                   "NAME")) %>%
  mutate(percent_poverty = total_poverty / total_population) %>% 
  filter(percent_poverty >= 0.35)

total_data

###Tract level population  by poverty

totalpov_ga <- map_df(us, function(x) {
  get_acs(geography = "tract",
          variables = "B17001_002",
          state = "GA")
})

## Population (total)

totalpop


totalpop_ga <- map_df(us, function(x) {
  get_acs(geography = "tract",
          variables = "B01003_001",
          state = "GA")
  
### Summary Stats + Visualization
  
  summary_stats <- clean_census_data %>%
    group_by(year) %>%
    summarize(
      total_population = sum(population),
      avg_population = mean(population),
      median_population = median(population)
    )
  
  ggplot(summary_stats, aes(x = year, y = total_population)) +
    geom_line() +
    labs(title = "US Population Growth Over Time",
         x = "Year", y = "Total Population")
  
### State Level population change + Visualization
  
  state_change <- clean_census_data %>%
    group_by(state) %>%
    summarize(
      pop_1800 = population[year == 1800],
      pop_2020 = population[year == 2020],
      percent_change = (pop_2020 - pop_1800) / pop_1800 * 100
    )
  
  us_map <- map_data("state")
  state_map <- left_join(us_map, state_change, by = c("region" = "state"))
  
  ggplot(state_map, aes(long, lat, group = group, fill = percent_change)) +
    geom_polygon(color = "white") +
    scale_fill_viridis_c() +
    labs(title = "Population Change by State (1800-2020)",
         fill = "Percent Change")
  
### Demographic Trend + Visualization + Regression analysis
  
 demographic_trends <- clean_census_data %>%
    group_by(year) %>%
    summarize(
      percent_urban = sum(urban_population) / sum(population) * 100,
      percent_rural = 100 - percent_urban
    )
  
  ggplot(demographic_trends, aes(x = year)) +
    geom_line(aes(y = percent_urban, color = "Urban")) +
    geom_line(aes(y = percent_rural, color = "Rural")) +
    labs(title = "Urban vs. Rural Population Trends",
         x = "Year", y = "Percentage", color = "Population Type")

  population_model <- lm(population ~ year + region, data = clean_census_data)
  summary(population_model)
  

##Demographics
### Age (Decennial)
age2020 <- get_decennial(geography = "state",
                         variables = "P13_001N",
                         year = 2020,
                         sumfile = "dhc")
age2020
head(age2020)
tail(age2020)


age2020 %>% 
  ggplot(aes(x = value, y = reorder(NAME, value))) + 
  geom_point()

children_0_18_acs1 <- get_acs(geography = "state",
                              variables = "B19013B_001",
                              year = 2020)


###Race (ACS)

sc_acs_data <- get_acs(
  geography = "tract",
  variables = c(
    white = "B03002_003",
    black = "B03002_004",
    asian = "B03002_006",
    hispanic = "B03002_012"
  ), 
  state = "SC", # FIPS code is 45
  geometry = TRUE,
  year = 2020
#### Race demographics of a county

sc_urban_data %>%
  filter(variable %in% c("white", "hispanic")) %>%
  group_by(urban_name) %>%
  group_modify(~
                 dissimilarity(.x,
                               group = "variable",
                               unit = "GEOID",
                               weight = "estimate"
                 )
  ) %>% 
  arrange(desc(est))

mutual_within(
  data = ca_urban_data,
  group = "variable",
  unit = "GEOID",
  weight = "estimate",
  within = "urban_name",
  wide = TRUE
)

#### Race of Tracts in Counties

black_fulton_ga <- get_acs(geography = "tract", 
                           state = "GA", 
                           county = "Fulton", 
                           variable = "B02001_003", # Black or AA alone or in combination with one or more races
                           geometry = TRUE)
plot(black_fulton_ga)

#### Race of counties in state

ga_black_county_2020 <- get_acs(geography = "county",
                                variables = "B02001_003",
                                year = 2020,
                                survey = "acs5",
                                state = "13", # FIPS code 13 for state GA
                                geometry = TRUE) 

plot(ga_black_county_2020)

### Race and Segregation of a county

georgetown_race_estimate <- df %>% 
  filter(NAME == "Census Tract 9205.02, Georgetown County, South Carolina")
georgetown

df %>%
  as_tibble() %>% 
  mutate(as.numeric(df$estimate)) %>% 
  sum(df$estimate)

sc_local_seg <- df %>%
  filter(NAME == "Census Tract 9205.10, Georgetown County, South Carolina") %>%
  mutual_local(
    group = "variable",
    unit = "GEOID",
    weight = "estimate", 
    wide = TRUE
  )

sc_tracts_seg <- tracts("SC", cb = TRUE, year = 2020) %>%
  inner_join(sc_local_seg, by = "GEOID") 

la_tracts_seg %>%
  ggplot(aes(fill = ls)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26946) + 
  scale_fill_viridis_c(option = "inferno") + 
  theme_void() + 
  labs(fill = "Local\nsegregation index")

### Race in a County

black <- get_acs(geography = "tract", state = "GA", 
                 county = "Fulton", 
                 variable = "B02009_001", 
                 geometry = TRUE)


black_fulton_ga <- get_acs(geography = "tract", state = "GA", 
                           county = "Fulton", 
                           variable = "B02001_003",
                           geometry = TRUE)

plot(black_fulton_ga["estimate"])

### Race demographic extraction

race_vars = c("B03002_003", "B03002_004", "B03002_006", "B03002_012")

sfStates = get_acs(
  geography = "state", variables = race_vars, year = 2017, 
  output = "wide", geometry = TRUE
)
sfStates = select(sfStates, state = GEOID, name = NAME, white = B03002_003E, 
                  black = B03002_004E, asian = B03002_006E, hispanic = B03002_012E)
dfTracts = get_acs(
  geography = "tract", vwhite_fulton <- get_acs(geography = "tract", state = "GA", # State/FIPS code: 13
                                                ariables = race_vars, year = 2017, 
                                                output = "wide", state = sfStates$state
  )
  dfTracts = transmute(
    dfTracts, state = substr(GEOID, 1, 2), tract = GEOID, 
    name = NAME, white = B03002_003E, black = B03002_004E, asian = B03002_006E, 
    hispanic = B03002_012E
  )



## Economic Factors

blackmedianincome <- get_acs(geography = "state",
                             variables = "B19013B_001",
                             year = 2020)
nc_county_income = get_acs(
  geography = "county",
  state = "NC",
  table = "B19001")

median_housing_value <- get_acs(geography = "tract", state = "GA", 
                                county = "Fulton", 
                                variable = "B25077_001", 
                                geometry = TRUE)
plot(median_housing_value["estimate"])

# Calculate X

## X = poverty
total_data <- totalpop_ga %>%
  rename(total_population = estimate) %>%
  left_join(totalpov_ga %>% rename(total_poverty = estimate), by = c("GEOID", "NAME")) %>%
  mutate(percent_poverty = total_poverty / total_population) %>%
  filter(percent_poverty >= 0.35)