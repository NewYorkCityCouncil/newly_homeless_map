library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(rjson)
library(ggplot2)
library(classInt)


## NOTE: If you've gone through 01_preprocessing_and_eda, then much of this first part is unnecessary. The first couple of sections are mostly repurposed if you want to skip EDA

last_known_raw <- read_csv('https://data.cityofnewyork.us/resource/ur7y-ziyb.csv?$limit=9999999') %>% 
  janitor::clean_names()

#report date to posixct
last_known_raw$report_date <- as.POSIXct(last_known_raw$report_date, format="%m/%d/%Y")

#funciton to capitalize first letter in each word
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#capitalize just first letter in each word, replace 
last_known_raw$case_type <- sapply(tolower(last_known_raw$case_type), simpleCap)
last_known_raw$case_type <- gsub(' ', '_', last_known_raw$case_type)

# remove any rows with no CD, convert to numeric
last_known <- last_known_raw %>% 
  filter(str_detect(community_district, '[0-9]+')) %>% 
  mutate(borough = ifelse(borough == 'Manhattan',        # turns borough into numbers
                          "1",
                          ifelse(borough == "Bronx",
                                 "2",
                                 ifelse(borough == "Brooklyn",
                                        "3",
                                        ifelse(borough == "Queens",
                                               "4", "5")))),
         community_district = ifelse(nchar(community_district) == 1,    # turns CD into borough num + cd num
                                     as.numeric(paste0(borough, "0", community_district)),
                                     as.numeric(paste0(borough, community_district))))


#pull just the last rolling year of data
last_known <- last_known %>% 
  filter(report_date <= max(report_date) & report_date > seq(as.POSIXct(cut(max(last_known$report_date), "month")), length=2, by="-11 month")[2]-1)

homeless_clean <- filter(last_known, !is.na(individuals))




# Average Monthly ------------------------------------

cd_agg <- aggregate(list(cases_monthly = homeless_clean$cases,
                              people_monthly = homeless_clean$individuals),
                         by = list(boro_cd = homeless_clean$community_district,
                                   case_type = homeless_clean$case_type),
                         function(x) {round(sum(x)/length(unique(last_known$report_date)),0)})



# Spread Data for Mapping -------------------------------------------------
## NO LONGER RELEVANT BECAUSE THEY CHANGED THE DATA SOURCE
# cd_agg_spread <- cd_agg %>% 
#   select(-cases_monthly) %>% 
#   spread(case_type, people_monthly) 
# 
# cd_agg_spread$total <- rowSums(cd_agg_spread[,c('Adult_Family','Family_With_Children','Single_Adult')])


# Load in CD shapefile, joine to homelessness file  --------------------------------------------------

# Note: csv/json files from internet were not playing nicely. If you've downloaded this from our GitHub page, 
# you'll have to download the data from here: https://data.cityofnewyork.us/City-Government/Community-Districts/yfnk-k7r4

cd_shape <- read_sf("cds/geo_export_f7012547-fda3-49de-9ed1-fd2dfb36a141.shp") %>% 
  select(geometry, boro_cd) %>% 
  mutate(boro_cd = as.numeric(boro_cd))

cd_shape_agg <- left_join(cd_shape, cd_agg) %>% 
  filter(!is.na(people_monthly)) %>% 
  st_transform(crs = 4326)



# Load in Homeless Shelters File ------------------------------------------

# get only the latest report date for each CD, replace na with 0
shelters <- read_csv("https://data.cityofnewyork.us/resource/3qem-6v3v.csv") %>% 
  group_by(community_district) %>%
  slice(which.max(report_date)) %>% 
  select(-report_date, -borough) %>% 
  replace(is.na(.), 0)

# sum total shelters
shelters$total_shelters <- rowSums(shelters[,2:8])

#joining to final data

cd_shape_final <- left_join(cd_shape_agg, shelters, by = c("boro_cd" = "community_district"))


# Mapping -----------------------------------------------


cd_tot <- paste0('Community District: ', cd_shape_final$boro_cd, '<br>',
                 "Avg Monthly Shelter Entries: ", cd_shape_final$people_monthly, '<br>',
                 "Total Shelters: ", cd_shape_final$total_shelters)


## Total Map

classes_tot <- classInt::classIntervals(cd_shape_final$people_monthly, n = 5, style = "fisher")

pal_tot <- colorBin(
  palette = 'Blues',
  bins = round(classes_tot$brks,0),
  domain = cd_shape_final$people_monthly)

total_map <- leaflet(cd_shape_final, options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addPolygons(weight = 1,
              fillOpacity = .5,
              fillColor = pal_tot(cd_shape_final$people_monthly),
              popup = ~cd_tot,
              group = "Total") %>% 
  addLegend('topleft', pal = pal_tot,
            values = cd_shape_final$people_monthly,
            group = "Total",
            title = "Monthly Average DHS Shelter Entries"#,
            #labFormat = labelFormat(suffix = " people")
            )



total_map

## Single Adults Map

classes_sing <- classInt::classIntervals(cd_shape_final$Single_Adult, n = 5, style = "fisher")

pal_sing <- colorBin(
  palette = 'Blues',
  bins = round(classes_sing$brks,0),
  domain = cd_shape_final$Single_Adult)

sing_ad_map <- leaflet(cd_shape_final) %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addPolygons(weight = 1,
              fillOpacity = .5,
              fillColor = ~pal_sing(cd_shape_final$Single_Adult),
              popup = cd_tot,
              group = "Single Adults") %>% 
  addLegend('topleft', pal = pal_sing,
            values = cd_shape_final$Single_Adult,
            group = "Single Adults",
            title = "Monthly Avg Shelter Entries - Single Adults")




## Adult Families Map

classes_ad_fam <- classInt::classIntervals(cd_shape_final$Adult_Family, n = 5, style = "fisher")

pal_ad_fam <- colorBin(
  palette = 'Blues',
  bins = round(classes_ad_fam$brks,0),
  domain = cd_shape_final$Adult_Family)


ad_fam_map <- leaflet(cd_shape_final) %>% 
  addProviderTiles('CartoDB.Positron') %>%     
  addPolygons(weight = 1,
              fillOpacity = .5,
              fillColor = ~pal_ad_fam(cd_shape_final$Adult_Family),
              popup = cd_tot,
              group = "Adult Families") %>% 
  addLegend('topleft', pal = pal_ad_fam,
            values = cd_shape_final$Adult_Family,
            group = "Adult Families",
            title = "Monthly Avg Shelter Entries - Adult Families")






## Families with Children Map

classes_fam_kids <- classInt::classIntervals(cd_shape_final$Family_With_Children, n = 5, style = "fisher")

pal_fam_kids <- colorBin(
  palette = 'Blues',
  bins = round(classes_fam_kids$brks,0),
  domain = cd_shape_final$Family_With_Children)

fam_kids_map <- leaflet(cd_shape_final) %>% 
  addProviderTiles('CartoDB.Positron') %>%   
  addPolygons(weight = 1,
              fillOpacity = .5,
              fillColor = ~pal_fam_kids(cd_shape_final$Family_With_Children),
              popup = cd_tot,
              group = "Families with Children") %>% 
  addLegend('topleft', pal = pal_fam_kids,
            values = cd_shape_final$Family_With_Children,
            group = "Families with Children",
            title = "Monthly Average Shelter Entries - Families")


total_map





# Shelters One-Off Map ----------------------------------------------------

## Join shelters and CD shapefile

shelters_shape <- left_join(cd_shape, shelters, by = c('boro_cd' = "community_district")) %>% 
  replace(is.na(.), 0) %>% 
  st_transform(crs = 4326)

# Plot shelter distribution

ggplot(shelters_shape, aes(x=total_shelters)) + 
  geom_histogram(binwidth=1)


natural_shelters_interval = classIntervals(shelters$total_shelters, n = 6, style = 'jenks')$brks
ggplot(shelters_shape, aes(x=total_shelters)) + geom_vline(aes(xintercept=total_shelters), color='red') + geom_histogram(breaks=natural_shelters_interval)

pop_shelters <- paste0('Community District: ', shelters_shape$boro_cd, '<br>',
                       "Total Shelters: ", shelters_shape$total_shelters, '<br>',
                       "Shelters for Single Adults: ", shelters_shape$adult_shelter+shelters_shape$adult_shelter_comm_hotel, '<br>',
                       "Shelters for Adult Families: ", shelters_shape$adult_family_shelter+shelters_shape$adult_family_comm_hotel,  '<br>',
                       "Shelters for Families with Children: ", shelters_shape$fwc_shelter+shelters_shape$fwc_comm_hotel+shelters_shape$fwc_cluster)

pal_shelters <- colorBin(
  palette = 'Blues',
  bins = round(natural_shelters_interval,0),
  domain = shelters_shape$total_shelters)

shelters_map <- leaflet(shelters_shape) %>% 
  addProviderTiles('CartoDB.Positron') %>%   
  addPolygons(weight = 1,
              fillOpacity = .5,
              fillColor = ~pal_shelters(shelters_shape$total_shelters),
              popup = pop_shelters) %>% 
  addLegend('topleft', pal = pal_shelters,
            values = shelters_shape$total_shelters,
            title = "Shelters per Community District")


total_map
shelters_map
