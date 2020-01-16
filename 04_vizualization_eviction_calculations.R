library(tidyverse)

#data pulled from Furman Center Core Data application
# http://app.coredata.nyc/?mlb=false&ntii=&ntr=&mz=14&vtl=https%3A%2F%2Fthefurmancenter.carto.com%2Fu%2Fnyufc%2Fapi%2Fv2%2Fviz%2F98d1f16e-95fd-4e52-a2b1-b7abaf634828%2Fviz.json&mln=false&mlp=true&mlat=40.718&ptsb=&nty=&mb=roadmap&pf=%7B%22subsidies%22%3Atrue%7D&md=map&mlv=false&mlng=-73.996&btl=Borough&atp=properties

evic <- read_csv('communitydistrict-privateevictionfilings.csv') %>% 
  janitor::clean_names() %>% 
  select(community_district, x2017) %>% 
  rename(evictions = x2017)

evic_1000 <- read_csv('communitydistrict-privateevictionfilingsper1000privaterentalunits.csv') %>% 
  janitor::clean_names() %>% 
  select(community_district, x2017) %>% 
  rename(per_1000 = x2017)

evic <- left_join(evic, evic_1000)

#calculate units per cb
# x = evic. per 1,000
# y = total evic.
# z = total units

# x/1000 = y/z
# z = (y*1000)/x

evic$units <- (evic$evictions * 1000)/evic$per_1000

# calculate rate for nyc

evic$city <- 'nyc'

nyc_evictions <- aggregate(list(units = evic$units,
                                evictions  = evic$evictions),
                           by = list(city = evic$city),
                           function(x) {sum(x)})

#calculate eviction rate per 1,000
# x/1000 = y/z
# x = (y * 1000)/z

nyc_evictions$per_1000 <- (nyc_evictions$evictions * 1000)/nyc_evictions$units
