library(sf)
library(ggplot2)
library(dplyr)
library(lwgeom)


# Read geometric files for Divisions
SubDiv<-st_read("./data/input/Sectoren/DVW_SECTOREN_BASIS.shp") %>%
        st_transform(4326)


Districts <- SubDiv %>%
  st_make_valid() %>%
  group_by(DSTRCT) %>%
  summarize(across(where(is.character), first), 
            geometry = st_union(geometry), .groups = 'drop') %>%
  select(-c(SCTR_NR, SCTR, SCTR_NM, SCTR_GSM, LNKWACHT, TYPE)) %>%
  st_write("./data/output/Districts/Districts.shp")

Sector_extended <- SubDiv %>%
  st_make_valid() %>%
  group_by(SCTR) %>%
  summarize(across(where(is.character), first), 
            geometry = st_union(geometry), .groups = 'drop') %>%
  mutate(TYPE= "extended") %>%
  select(c(SCTR, SCTR_NM, SCTR_GSM, DSTRCT, DSTRCT_NM, DSTRCT_GSM, AFD, TYPE)) %>%
  st_write("./data/output/Sector_extended/Sector_extended.shp")

Sector_core <- SubDiv %>%
  st_make_valid() %>%
  filter(TYPE=="KERN")%>%
  group_by(SCTR) %>%
  summarize(across(where(is.character), first), 
            geometry = st_union(geometry), .groups = 'drop') %>%
  mutate(TYPE= "core") %>%
    select(c(SCTR, SCTR_NM, SCTR_GSM, DSTRCT, DSTRCT_NM, DSTRCT_GSM, AFD, TYPE))  %>%
  st_write("./data/output/Sector_core/Sector_core.shp")


#Parcels par sector
Parcels<-st_read("./data/Percelen/arW_arC.shp") %>%
        st_transform(4326) %>%
        st_make_valid()

joined_data <- st_join(Parcels, Sector_extended, left = TRUE) %>%
                filter(!is.na(SCTR))

merged_parcels <- joined_data %>%
  group_by(SCTR) %>%
  summarize(across(where(is.character), first), 
            geometry = st_union(geometry), .groups = 'drop') %>%
  mutate(TYPE= "parcels") %>%
  select(c(SCTR, SCTR_NM, SCTR_GSM, DSTRCT, DSTRCT_NM, DSTRCT_GSM, AFD, TYPE)) %>%
  st_write("./data/output/Sector_parcels/Sector_parcels.shp")


