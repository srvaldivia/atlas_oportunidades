library(tidyverse)
library(sf)
library(haven)
library(readxl)
library(labelled)



## import data ----------------
# point data
geo_psu_2009 <- read_dta("data_raw/Geo_PSU_2009_rank_child_father_v1.dta")

school_addresses_2004 <- read_dta("data_raw/school_addresses_2004-06_v10_all_traveltimes_all_fixed.dta")

rbd_geo2010 <- read_dta("data_raw/RBD_Geo2010v2explicitGeoVarNames.dta")


# census tracts
manzanas <- read_sf("shapes_raw/INE.gdb", "Variables_C17_Manzana", quiet = FALSE) %>% 
  st_transform(crs = 4326) %>% 
  janitor::clean_names()


entidades <- read_sf("shapes_raw/INE.gdb", "Variables_C17_Entidad", quiet = FALSE) %>% 
  st_transform(crs = 4326) %>% 
  janitor::clean_names() %>% 
  st_cast("MULTIPOLYGON")


# directorio_mineduc_2004 <- read_csv2("data_raw/directorio_oficial_EE_2004_Oficial.csv",
#                                      na = "[NULL]")


# mineduc_2020 <- read_excel("data_raw/mineduc_2020.xlsx") %>% 
#   janitor::clean_names() %>% 
#   mutate(direccion_full = paste(direccion, numero, sep = ", "))




# skimr::skim(geo_psu_2009)
# visdat::vis_dat(geo_psu_2009, warn_large_dat = FALSE)


## check data ----------------
# geo_psu_2009


# no duplicated ID (id_atlas)
geo_psu_2009 %>% 
  count(id_atlas) %>% 
  filter(n > 1)


geo_psu_2009 %>% 
  count(nom_com_rbd)


# 226454 NA coordinates
# 22919 non-NA coordinates (10.2%)
geo_psu_2009 %>% 
  summarise(sum(is.na(latitud_geo1)))

# check unvalid coordinates (ALL non-NA ARE VALID - 10.2%)
# all coords have valid values
geo_psu_2009 %>% 
  ## filter(!is.na(latitud_geo1)) %>%
  ## nrow()
  select(latitud_geo1, longitud_geo1) %>%
  summary()



# create sf object
geo_psu_2009_sf <- geo_psu_2009 %>% 
  filter(!is.na(latitud_geo1)) %>%
  st_as_sf(
    coords = c("longitud_geo1", "latitud_geo1"),
    crs = 4326
  )


# shared CRS?
st_crs(geo_psu_2009_sf)$epsg == st_crs(manzanas)$epsg





## check data ----------------
# school_addresses_2004

# no na-coordinates    
school_addresses_2004 %>% 
  summarise(sum(is.na(lat_updated)))

school_addresses_2004 %>% 
  summarise(sum(is.na(long_updated)))


# observation with coordinates == 0: 1
school_addresses_2004 %>% 
  select(lat_updated, long_updated) %>% 
  summary()

school_addresses_2004 %>% 
  filter(lat_updated == 0)

school_addresses_2004 %>% 
  filter(long_updated == 0)




# create sf object
school_addresses_2004_sf <- school_addresses_2004 %>% 
  filter(!long_updated == 0) %>% 
  st_as_sf(
    coords = c("long_updated", "lat_updated"),
    crs = 4326
    )


# shared CRS?
st_crs(school_addresses_2004_sf)$epsg == st_crs(manzanas)$epsg

st_geometry_type(school_addresses_2004_sf, by_geometry = FALSE) ==
  st_geometry_type(geo_psu_2009_sf, by_geometry = FALSE)


## check data ----------------
# rbd_geo2010


# no na-coordinates    
rbd_geo2010 %>% 
  summarise(sum(is.na(school04lat)))

rbd_geo2010 %>% 
  summarise(sum(is.na(school04lon)))


# observation with coordinates == 0: 1
rbd_geo2010 %>% 
  select(school04lat, school04lon) %>% 
  summary()

rbd_geo2010 %>% 
  filter(school04lat == 0)

rbd_geo2010 %>% 
  filter(school04lon == 0)


# fix easter island coordinate
rbd_geo2010 <- rbd_geo2010 %>% 
  mutate(school04lat = case_when(school04lat > 100 ~ school04lat * -1,
                                 TRUE ~ school04lat)) %>% 
  arrange((school04lat))

#change xy fields + labels
rbd_geo2010 <- rbd_geo2010 %>% 
  rename(school04lon_2 = school04lat,
         school04lat = school04lon) %>% 
  rename(school04lon = school04lon_2) %>% 
  set_variable_labels(
    school04lat = "School 2004 latitude",
    school04lon = "School 2004 longitude"
  )
  

# create sf object
rbd_geo2010_sf <- rbd_geo2010 %>% 
  st_as_sf(
    coords = c("school04lon", "school04lat"),
    crs = 4326
  )
# add xy-coordinate columns
# bind_cols(rbd_geo2010, as_tibble(st_coordinates(rbd_geo2010_sf)))


rbd_geo2010_sf %>% 
  write_sf("shapes_output/rbd_geo2010.shp", delete_layer = TRUE)


## data analysis

# spatial join geo_psu_2009
geo_psu_2009_SJ <- geo_psu_2009_sf %>% 
  st_join(
    entidades,
    left = TRUE
    ) %>% 
  mutate(manzana_censal_residencia = manzent_i,
         intersect = case_when(!is.na(manzana_censal_residencia) ~ TRUE,
                               TRUE ~ FALSE),
         area = case_when(!is.na(manzana_censal_residencia) ~ "rural",
                          TRUE ~ "urbano")) %>%
  select(rbd2006:id_atlas, manzana_censal_residencia, intersect, area) %>% 
  st_join(
    manzanas,
    left = TRUE
  ) %>%
  mutate(intersect = case_when(!is.na(manzent_i) ~ TRUE,
                               TRUE ~ intersect)) %>%
  select(rbd2006:id_atlas, manzana_censal_residencia, intersect, area) %>%
  st_join(
      manzanas,
      left = TRUE,
      join = st_nearest_feature
    ) %>%
  mutate(manzana_censal_residencia = case_when(area == "urbano" ~ manzent_i,
                                                 TRUE ~ manzana_censal_residencia)) %>%
  select(rbd2006:id_atlas, manzana_censal_residencia, intersect, area) %>%
  mutate(comuna_id = str_sub(manzana_censal_residencia, start = 1, end = 5),
         zona_censal_residencia = as.double(str_sub(manzana_censal_residencia, start = 1, end = 11)))

  # write_sf("shapes_output/geo_psu_SJ.shp", delete_layer = TRUE)



# # spatial join schools_2004
# school_addresses_2004_SJ <- school_addresses_2004_sf %>% 
#   st_join(entidades,
#     left = TRUE) %>% 
#   mutate(manzana_censal_colegio = manzent_i,
#          intersect = case_when(!is.na(manzana_censal_colegio) ~ TRUE,
#                                TRUE ~ FALSE),
#          area = case_when(!is.na(manzana_censal_colegio) ~ "rural",
#                           TRUE ~ "urbano")) %>% 
#   select(rbd:traveltime_dist12, manzana_censal_colegio, intersect, area) %>% 
#   # view()
#   st_join(manzanas,
#           left = TRUE) %>% 
#   mutate(intersect = case_when(!is.na(manzent_i) ~ TRUE,
#                                TRUE ~ intersect)) %>% 
#   select(rbd:traveltime_dist12, manzana_censal_colegio, intersect, area) %>%
#   # view()
#   st_join(manzanas,
#           left = TRUE,
#           join = st_nearest_feature) %>% 
#   # view()
#   mutate(manzana_censal_colegio = case_when(area == "urbano" ~ manzent_i,
#                                             TRUE ~ manzana_censal_colegio)) %>% 
#   select(rbd:traveltime_dist12, manzana_censal_colegio, intersect, area) %>% 
#   # view()
#   mutate(comuna_id = str_sub(manzana_censal_colegio, start = 1, end = 5)) 
#   # write_sf("shapes_output/schools_2004_SJ.shp", delete_layer = TRUE)

  


# spatial join rbd_geo2010

rbd_geo2010_SJ <- rbd_geo2010_sf %>% 
  st_join(entidades,
          left = TRUE) %>% 
  mutate(manzana_censal_colegio = manzent_i,
         intersect = case_when(!is.na(manzana_censal_colegio) ~ TRUE,
                               TRUE ~ FALSE),
         area = case_when(!is.na(manzana_censal_colegio) ~ "rural",
                          TRUE ~ "urbano")) %>% 
  select(rbd2004:schooladdress, manzana_censal_colegio, intersect, area) %>% 
  st_join(manzanas,
          left = TRUE) %>% 
  mutate(intersect = case_when(!is.na(manzent_i) ~ TRUE,
                               TRUE ~ intersect)) %>% 
  select(rbd2004:schooladdress, manzana_censal_colegio, intersect, area) %>%
  st_join(manzanas,
          left = TRUE,
          join = st_nearest_feature) %>% 
  mutate(manzana_censal_colegio = case_when(area == "urbano" ~ manzent_i,
                                            TRUE ~ manzana_censal_colegio)) %>% 
  select(rbd2004:schooladdress, manzana_censal_colegio, intersect, area) %>% 
  mutate(comuna_id = str_sub(manzana_censal_colegio, start = 1, end = 5)) %>%
  filter(schoolregion == 13)
  # write_sf("shapes_output/rbd_geo2010_SJ.shp", delete_layer = TRUE)




 # write.table(x, "clipboard", sep="\t")
 

# conteo de casos por comuna
geo_psu_2009_SJ %>% 
  st_drop_geometry() %>% 
  count(comuna_id) %>% 
  arrange((n)) %>%
  mutate(criterio = case_when(n > 20 ~ "Sí",
                              TRUE ~ "No")) %>% 
  # write_csv2("conteo_comunas.csv")
  # filter(criterio == "No") %>% 
  # nrow()
  
  # select(n) %>% 
  # summary()
  # %>% 
  # view()
  ggplot() +
  geom_histogram(aes(n, fill = criterio), bins = 45) +   #"#66CDAA"
  labs(title = "Comuna", x = "domicilios por comuna") +
  #geom_vline(xintercept = 20) +
  scale_fill_discrete(name = "> 20")


geo_psu_2009_SJ %>% 
  st_drop_geometry() %>% 
  count(zona_censal_residencia) %>% 
  arrange((n)) %>% 
  mutate(criterio = case_when(n > 20 ~ "Sí",
                              TRUE ~ "No")) %>% 
  # write_csv2("conteo_zonas.csv")
  # filter(criterio == "No") %>% 
  # nrow()
  
  # filter(n < 20) %>% 
  # write_csv2("conteo_zona_menor_20.csv")
  
  # select(n) %>%
  # summary()
  ggplot() +
  geom_histogram(aes(n, fill = criterio), bins = 35) +     # "#3A5FCD"
  labs(title = "Zona censal", x = "domicilios por zona censal") +
  scale_fill_discrete(name = "> 20")



geo_psu_2009_SJ %>% 
  st_drop_geometry() %>% 
  count(manzana_censal_residencia) %>% 
  arrange(desc(n)) %>% 
  mutate(criterio = case_when(n > 20 ~ "Sí",
                              TRUE ~ "No")) %>%
  # write_csv2("conteo_manzanas.csv")
  # filter(criterio == "No") %>% 
  # nrow()
  
  # filter(n < 20) %>% 
  # write_csv2("conteo_manz_menor_20.csv")
  
  # select(n) %>%
  # summary()

  ggplot() +
  geom_histogram(aes(n, fill = criterio)) +       # "#CD4F39"
  labs(title = "Manzana censal", x = "domicilios por manzana censal") +
  scale_fill_discrete(name = "> 20")




# join de código manzanas (colegios) a alumnos (geo_psu) WIP
geo_psu_wip <- geo_psu_2009_SJ %>% 
  st_drop_geometry() %>% 
  tibble()

# schools_wip <- school_addresses_2004_SJ %>%
#   st_drop_geometry() %>% 
#   select(rbd, dir_loca04, com_cod04, com_nom04, comuna_id, manzana_censal_colegio,
#          intersect_schl = intersect,
#          area_schl = area)

schools_wip <- rbd_geo2010_SJ %>%
  st_drop_geometry() %>% 
  rename(intersect_schl = intersect,
         area_schl = area)



geo_psu_wip %>% 
  tibble() %>% 
  rename(rbd_geo_psu = rbd2004) %>% 
  left_join(schools_wip %>% 
              rename(rbd_schools = rbd), by = c("rbd_geo_psu" = "rbd_schools")) %>% 
  mutate(has_rbd = case_when(!is.na(dir_loca04) ~ TRUE,
                             TRUE ~ FALSE)) %>%
  # view()
  # count(has_rbd)
  left_join(mineduc_2020 %>%
              select(rbd_2020 = rbd, nom_rbd, direccion_full, nom_com),
            by = c("rbd_geo_psu" = "rbd_2020")) %>% 
  filter(!is.na(direccion_full)) %>% 
  view()

mineduc_2020 %>% 
  count(agno)
  





