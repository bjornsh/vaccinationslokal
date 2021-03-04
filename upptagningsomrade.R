library(dplyr)
library(tidyr)
library(readxl)
library(sf)
library(mapview)


# städa
rm(list = ls())
invisible(gc())

# ingen scientific notation
options(scipen=999)


# Hämta data
lokal = read_xlsx("vardlokaler.xlsx") # vaccinationslokaler
pop = read_xlsx("koordinater_distinct_nattbefolkning_uppsalalan.xlsx") # unika xy bostadskoordinater på individnivå


# konvertera lokal till sf objekt för geodatahantering (koordinater för vårdlokaler är i WGS84 )
lokal_sf = st_as_sf(lokal, coords = c("y", "x"), crs = 4326)


# konvertera projektion från WGS84 till SWEREF 99TM (en metrisk projektion)
lokal_sweref = st_transform(lokal_sf, 3006)


# skapa dataframe
lokal_sweref_koord = lokal_sweref %>% 
  st_coordinates() %>% 
  as.data.frame() %>%
  rename(lokal_x = 1, lokal_y = 2)


# slå ihop xy koordinater med lokalmetadata (namn, kommun, adress)
lokal_for_merge = lokal %>% 
  bind_cols(., lokal_sweref_koord) %>%
  select(-x, -y)


# förbereda bostadsdata
pop = pop %>% 
  rename(bostad_x = 1, bostad_y = 2) %>%
  mutate_if(is.character,as.numeric) %>% 
  mutate(bostad_x = round(bostad_x, 0),
         bostad_y = round(bostad_y, 0)) %>%
  distinct()


# skapa alla kombinationer av bostads och lokal koordinater
merged = merge(pop, lokal_for_merge)


# beräkna distans mellan alla bostäder och alla vaccinationslokaler
# identifiera bostad-lokal kombination med kortaste raka linjen
distans = merged %>% 
  mutate(diff_x = abs(bostad_x - lokal_y), # X i bostadsdata motsvarar Y i vårdlokaldata
         diff_y = abs(bostad_y - lokal_x),
         dist = round(sqrt(diff_x^2 + diff_y^2) / 1000, 3)) %>% # Pythagoras a^2 + b^2 = c^2
  group_by(bostad_x, bostad_y) %>% # för varje bostadskoordinat, filtrera bort alla rader som inte är min distans
  filter(dist == min(dist)) %>%
  as.data.frame()


# spara tabellen
write.csv2(distans, "bostadskoordinater_vaccinationslokal.csv", row.names = FALSE)


# skapa karta
distans_sf = st_as_sf(distans, coords = c("bostad_y", "bostad_x"), crs = 3006)

map = mapview(distans_sf, zcol = "kommun", layer.name = "Upptagningsområde")

map = map + mapview(lokal_sweref, col.regions = "red", layer.name = "Vaccinationslokal")


# spara kartan
mapshot(map, url = "map_bostadskoordinater.html")

