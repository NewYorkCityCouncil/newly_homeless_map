library(sf)
library(tidyverse)
library(leaflet)
library(janitor)

last_known_raw <- read_csv('Associated_Address_by_Borough_and_Community_District.csv') %>% 
  clean_names()


last_known_raw$report_date <- as.POSIXct(last_known_raw$report_date, format="%m/%d/%Y")


unkown_cd <- last_known_raw[last_known_raw$community_district %in% 'No CD',]

last_known <- last_known_raw %>% filter(str_detect(community_district, '[0-9]+'))
last_known$community_district <- as.numeric(last_known$community_district)


agg_last_known <- aggregate(list(cases = last_known$cases,
                                 people = last_known$individuals),
                            by = list(boro_cd = last_known$community_district),
                            function(x) {round(sum(x)/12,0)})


comdist <- read_sf('Community Districts/geo_export_717a4fd2-0fdf-4875-add5-968bee9ff0da.shp') %>% 
  clean_names()


homeless <- left_join(comdist, agg_last_known)
homeless <- homeless %>% st_transform(., crs = '+proj=longlat +datum=WGS84')

homeless_clean <- filter(homeless, !is.na(cases), !is.na(people))


pal_cases <- colorBin(
  palette = 'Oranges',
  domain = homeless_clean$cases
)

pal_individuals <- colorBin(
  palette = 'Oranges',
  domain = homeless_clean$people
)

pop_cases <- paste0('Community District: ', homeless_clean$boro_cd, '<br>',
              'Average Monthly New Homeless Cases, 1yr: ', homeless_clean$cases)

pop_people <- paste0('Community District: ', homeless_clean$boro_cd, '<br>',
                    'Average Monthly Newly Homeless People, 1yr: ', homeless_clean$people)



leaflet(homeless_clean) %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addPolygons(fillColor = ~pal_cases(homeless_clean$cases),
              weight = 1,
              group = 'Cases',
              fillOpacity = .9,
              popup = pop_cases) %>% 
  addPolygons(fillColor = ~pal_individuals(homeless_clean$people),
              weight = 1,
              group = 'Individuals',
              fillOpacity = .9,
              popup = pop_people) %>%
  addLayersControl(baseGroups = c('Individuals', 'Cases'),
                   options = layersControlOptions(collapsed = FALSE),
                   position = 'bottomright')

