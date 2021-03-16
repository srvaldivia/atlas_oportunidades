library(tidyverse)
library(sf)
library(haven)

geo_psu_2009 <- read_dta("data_raw/Geo_PSU_2009_rank_child_father_v1.dta")
glimpse(geo_psu_2009)


# no duplicated ID (id_atlas)
geo_psu_2009 %>% 
  count(id_atlas) %>% 
  filter(n > 1)


geo_psu_2009 %>% 
  count(nom_com_rbd) %>% 
  view()


# 226454 NA coordinates
# 22919 non-NA coordinates (10.2%)
geo_psu_2009 %>% 
  summarise(sum(is.na(latitud_geo1)))

# check unvalid coordinates (ALL non-NA ARE VALID - 10.2%)
geo_psu_2009 %>% 
  filter(!is.na(latitud_geo1)) %>%
  nrow()
  select(latitud_geo1, longitud_geo1) %>% 
  summary()

geo_psu_2009 %>% 
  filter(!is.na(latitud_geo1)) %>%
  count(nom_com_rbd) %>% 
  view()

geo_psu_2009 %>% 
  filter(is.na(latitud_geo1)) %>%
  count(nom_com_rbd) %>% 
  view()


geo_psu_2009 %>% 
  filter(!is.na(latitud_geo1)) %>%
  st_as_sf(
    coords = c("longitud_geo1", "latitud_geo1"),
    crs = 4326
    ) %>% 
  # select(nom_com_alu) %>% 
  # plot(max.plot = 1)
  write_sf("shapes_output/geo_psu_2009.shp")

warnings()


