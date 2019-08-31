library(sf)
library(tidyverse)
library(leaflet)
library(janitor)
library(tidycensus)



# Read and clean community district data ----------------------------------



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



# Map raw numeric data ----------------------------------------------------



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



# Load puma data ----------------------------------------------------------

v17 <- load_variables(2017, "acs5", cache = TRUE)

puma <- get_acs(geography = 'public use microdata area', 
                state = "NY", 
                variables = 'B01003_001', 
                year = 2017,
                key = 'd74c1b2b7e13feac45f63d4b5f2e8789338fc6fb') %>% 
  janitor::clean_names()

#clean name column in two different columns, so it only represents community districts
puma_nyc <- puma %>% filter(., grepl('NYC', name))
split_name <- puma_nyc$name
rexp <- "^(\\w+\\-\\w+)\\s?(.*)$"
output_names <- data.frame(boro=sub(rexp,"\\1",split_name), cd=sub(rexp,"\\2",split_name))
output_names$geoid <- puma_nyc$geoid
puma_nyc <- left_join(puma_nyc, output_names)

puma_nyc$cd <- gsub("[A-Za-z\\,\\;\\ \\.\\'\\-]", '', puma_nyc$cd)
puma_nyc$cd <- gsub("[//&]", " ", puma_nyc$cd)

#make separate row for each community district, in preparation for join
puma_nyc <- separate_rows(puma_nyc, cd)
puma_nyc$cd <- trimws(puma_nyc$cd)
puma_nyc <- puma_nyc %>% filter(., grepl("[0-9]", cd))
puma_nyc$cd <- as.numeric(puma_nyc$cd)


#reclassify community boards numerically
puma_nyc$boro <- gsub('NYC-Manhattan', '100', puma_nyc$boro)
puma_nyc$boro <- gsub('NYC-Brooklyn', '300', puma_nyc$boro)
puma_nyc$boro <- gsub('NYC-Bronx', '200', puma_nyc$boro)
puma_nyc$boro <- gsub('NYC-Staten', '500', puma_nyc$boro)
puma_nyc$boro <- gsub('NYC-Queens', '400', puma_nyc$boro)
puma_nyc$boro <- as.numeric(puma_nyc$boro)

#combine boro and cd
puma_nyc$community_district <- puma_nyc$boro+puma_nyc$cd


#scrap unnecessary columns
puma_nyc_final <- puma_nyc %>% select(.,c(geoid, estimate,community_district))



# Join estimates and homelessness data ------------------------------------

# NOTE: there will be some issues, as certain geoid's have been estimated as the sum total of multiple cd's
# for this reason, I've left the geoid's in, so we can look to figure out the issue later.

homeless_clean <- left_join(homeless_clean, puma_nyc_final, by = c("boro_cd" = "community_district"))
