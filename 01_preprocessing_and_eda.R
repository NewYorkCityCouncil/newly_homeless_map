library(sf)
library(tidyverse)
library(leaflet)
library(janitor)
library(tidycensus)



# Read and clean community district data ----------------------------------



last_known_raw <- read_csv('https://data.cityofnewyork.us/resource/ur7y-ziyb.csv?$limit=9999999') %>% 
  clean_names()


last_known_raw$report_date <- as.POSIXct(last_known_raw$report_date, format="%m/%d/%Y")


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

last_known_raw$case_type <- sapply(tolower(last_known_raw$case_type), simpleCap)
last_known_raw$case_type <- gsub(' ', '_', last_known_raw$case_type)

unkown_cd <- last_known_raw[last_known_raw$community_district %in% 'No CD',]

last_known <- last_known_raw %>% filter(str_detect(community_district, '[0-9]+'))
last_known$community_district <- as.numeric(last_known$community_district)

#calculating average cases and people per month. 
agg_last_known <- aggregate(list(cases = last_known$cases,
                                 people = last_known$individuals),
                            by = list(boro_cd = last_known$community_district,
                                      case_type = last_known$case_type),
                            function(x) {round(sum(x)/length(unique(last_known$report_date)),0)})

agg_last_known_families_w_children <- agg_last_known %>% 
  filter(case_type == "Family_With_Children") %>% 
  arrange(desc(people))

agg_last_known_families_w_children$total_people <- sum(agg_last_known_families_w_children$people)
agg_last_known_families_w_children$percent <- round(100*agg_last_known_families_w_children$people/agg_last_known_families_w_children$total_people, 2)

agg_last_known_single <- agg_last_known %>% 
  filter(case_type == "Single_Adult") %>% 
  arrange(desc(people))

agg_last_known_single$total_people <- sum(agg_last_known_single$people)
agg_last_known_single$percent <- round(100*agg_last_known_single$people/agg_last_known_single$total_people, 2)












# comdist <- read_sf('Community Districts/geo_export_717a4fd2-0fdf-4875-add5-968bee9ff0da.shp') %>% 
#   clean_names()
# 
# 
# homeless <- left_join(comdist, last_known, by = c('boro_cd' = 'community_district'))
# homeless <- homeless %>% st_transform(., crs = '+proj=longlat +datum=WGS84')

homeless_clean <- filter(last_known, !is.na(cases), !is.na(individuals))






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




# test <- read_csv('ACS_17_5YR_S1701/ACS_17_5YR_S1701_with_ann.csv')


# Join estimates and homelessness data ------------------------------------

# NOTE: there will be some issues, as certain geoid's have been estimated as the sum total of multiple cd's
# for this reason, I've left the geoid's in, so we can look to figure out the issue later.

# UPDATE: Based on the lack of up-to-date information regarding population at the CD level,
# the analysis will have to be conducted at the PUMA level.

homeless_clean <- left_join(homeless_clean, puma_nyc_final)

# 2018 only
homeless_18 <- homeless_clean %>% 
  mutate(year = as.numeric(format(report_date, '%Y'))) %>% 
  filter(year == 2018)

#aggregate total cases to PUMA level

puma_agg_18 <- aggregate(list(cases = homeless_18$cases,
                           people = homeless_18$individuals),
                      by = list(puma = homeless_18$geoid,
                                pop_est = homeless_18$estimate),
                      function(x) {sum(x)})

puma_agg_18 <- puma_agg_18 %>% 
  mutate(cases_norm = round(cases/pop_est, 4)*100) %>% 
  mutate(people_norm = round(people/pop_est, 4)*100)



# load in PUMA shapefile

puma_shape <- read_sf('Public Use Microdata Areas (PUMA)/geo_export_d66ecf93-5ed7-4d78-819e-4fd2b0f4c16a.shp') %>% 
  select(puma, geometry)


#join to homelessness file

puma_agg_18$puma <- gsub('^360', '', puma_agg_18$puma)

puma_agg_18 <- left_join(puma_shape, puma_agg_18)

puma_agg_18 <- puma_agg_18 %>% 
  st_as_sf(wkt = puma_agg_18$geometry, crs = 4326)

st_write(puma_agg_18, 'puma_18_agg.shp', delete_layer = TRUE)

puma_pop <- paste0('PUMA: ', puma_agg_18$puma, '<br>',
                   "% Pop Newly Homeless: ", puma_agg_18$people_norm, '<br>',
                   'Number of Cases: ', puma_agg_18$cases)

pal_puma <- colorBin(
  palette = 'Blues',
  domain = puma_agg_18$people_norm)

leaflet(puma_agg_18) %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addPolygons(weight = 1,
              fillOpacity = .5,
              fillColor = ~pal_puma(puma_agg_18$cases_norm),
              popup = puma_pop)





# Prepare data for Shiny Mapping  ---------------------------------------------

newhom_spread <- homeless_clean %>% 
  gather(variable, value, -(report_date:case_type)) %>% 
  unite(temp, case_type, variable) %>% 
  spread(temp, value)

newhom_spread$case_total = newhom_spread$Adult_Family_cases + newhom_spread$Family_With_Children_cases + newhom_spread$Single_Adult_cases
newhom_spread$ppl_total = newhom_spread$Adult_Family_individuals + newhom_spread$Family_With_Children_individuals + newhom_spread$Single_Adult_individuals
  
newhom_spread <- left_join(newhom_spread, puma_nyc_final) %>% 
  rename(puma = geoid)

newhom_spread$puma <- gsub('^360', '', newhom_spread$puma)

geo_newhom_spread <- left_join(puma_shape, newhom_spread)

st_write(geo_newhom_spread, 'geo_newhom_spread/geo.shp', delete_layer = TRUE)




# CDs back to character $ combined, as they'll end up in popup 

homeless_clean <- homeless_clean %>% 
  mutate(community_district = as.character(community_district)) %>% 
  mutate(community_district = ifelse(community_district == "101" | community_district == "102",
                                     '101 & 102',
                                     ifelse(community_district == "104" | community_district == "105",
                                            '104 & 105',
                                            ifelse(community_district == "501" | community_district == "502",
                                                   '501 & 502',
                                                   ifelse(community_district == "503" | community_district == "506",
                                                          '503 & 506',
                                                          community_district)))))

# Now retotal rows to combine values that are currently listed across multiple CD's that exist in the same PUMA

homeless_for_shiny <- aggregate(list(cases = homeless_clean$cases,
                                       individuals = homeless_clean$individuals),
                                  by = list(report_date = homeless_clean$report_date, 
                                            borough = homeless_clean$borough, 
                                            community_district = homeless_clean$community_district, 
                                            case_type = homeless_clean$case_type, 
                                            geoid = homeless_clean$geoid, 
                                            estimate = homeless_clean$estimate),
                                  function(x) {sum(x)}) %>% 
  rename(puma = geoid)

homeless_for_shiny$puma <- gsub('^360', '', homeless_for_shiny$puma)

# Create new agg dataset of totals, to be joined into homeless_for_shiny df
hom_for_shiny_totals_agg <- aggregate(list(cases = homeless_for_shiny$cases,
                                           individuals = homeless_for_shiny$individuals),
                                      by = list(report_date = homeless_for_shiny$report_date, 
                                                borough = homeless_for_shiny$borough, 
                                                community_district = homeless_for_shiny$community_district, 
                                                puma = homeless_for_shiny$puma, 
                                                estimate = homeless_for_shiny$estimate),
                                      function(x) {sum(x)}) %>% 
  mutate(case_type = 'total')

homeless_for_shiny_final <- rbind(homeless_for_shiny, hom_for_shiny_totals_agg)

geo_hom <- left_join(puma_shape, homeless_for_shiny_final) %>% 
  mutate(#new_hom_percent = round((individuals/estimate)*100, 2),
         report_date = as.character(report_date)) %>% 
  select(puma, community_district, geometry, report_date, case_type, cases, individuals)


geo_hom_final <- gather(geo_hom, 'measure', 'count', -c(puma, community_district, geometry, report_date, case_type))

#geo_hom <- gather(geo_hom, 'measure', 'count', -c(puma, geometry, report_date))

geo_hom_final$case_type <- ifelse(geo_hom_final$case_type == "Adult_Family",
                                  "Adult Family",
                                  ifelse(geo_hom_final$case_type == "Family_With_Children",
                                         "Family with Children",
                                         ifelse(geo_hom_final$case_type == "Single_Adult",
                                                "Single Adult",
                                                "Total")))


st_write(geo_hom_final, 'geo_hom/geo.shp', delete_layer = TRUE)




ggplot(cd_shape_agg, aes(x = total)) + geom_histogram()

