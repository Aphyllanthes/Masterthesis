if(!("gsheet" %in% installed.packages()[,"Package"])) devtools::install_github("maxconway/gsheet")

source("~/Documents/Uni/Master/Masterarbeit/Masterthesis/links.R")

library(gsheet)
Daten <- gsheet2tbl(link1, sheetid = 4)
Standorte <- gsheet2tbl(link2, sheetid = 2)


library(dplyr)
library(tidyr)
library(ggplot2)


############### standorttabelle ########
# Standorte <- Standorte %>% 
#   separate(Neststandort, c("lat", "lon"), sep = "([\\,])")
#write.csv2(Standorte[,c(1,2,8,9)], "standorte.csv", row.names = F)
Standorte <- read.csv("~/Documents/Uni/Master/Masterarbeit/Daten/Standorte_all_coords.csv",
                      colClasses = "character" )

versiegelung <- read.csv2("~/Documents/Uni/Master/Masterarbeit/Daten/standorte_versiegelung.csv", sep = ",", dec = ".") %>% 
  tidyr::unite("geometry", geometry:geometry2, sep = ",") %>% 
  mutate(ID = tolower(ID),
         imp = as.numeric(imp))

versiegelung2000 <- read.csv2("~/Documents/Uni/Master/Masterarbeit/Daten/standorte_versiegelung2000.csv", sep = ",", dec = ".") %>% 
  tidyr::unite("geometry", geometry:geometry2, sep = ",") %>% 
  mutate(ID = tolower(ID),
         imp = as.numeric(imp))

versiegelung <- versiegelung %>%
  bind_rows(versiegelung2000) %>%
  mutate(radius_m = as.factor(radius_m))

############# 1. Daten bereinigen ################
# Daten bereinigen und alles zu kleinbuchstaben damit es einheitlicher wird.
Daten <- Daten %>% 
  select(id:f3) %>% 
  filter(!is.na(location)) %>% 
  filter(location != "\n") %>% 
  mutate(location = tolower(location),
         host_bwo = tolower(host_bwo),
         morphotype_host = tolower(morphotype_host),
         #morphotype_new = tolower(morphotype_host),
         Host_species_if_known = tolower(Host_species_if_known),
         Length = as.numeric(Length)) %>% 
  mutate(Host_species_if_known = ifelse(is.na(Host_species_if_known), 
                                        morphotype_host, Host_species_if_known))

## ?? IH_1600, IH_1653

Bees <- Daten %>% 
  filter(host_bwo == "bee") %>% 
  filter(!Host_species_if_known %in% c("ancistrocerus", "trypoxylon", "discoelius zonalis", "passaloecus", "symmorphus", "eumenidae") )
Wasps <- Daten %>% 
  filter(host_bwo == "wasp")


nests <- Bees %>% 
  select(id:hatched, hatched_1, dead, para, sample_sex, pinned)

meta <- Bees %>% 
  select(c(id, trashed, notes, Bearbeiter, morphotype_host)) %>% 
  mutate(
    trashed = ifelse(
      trashed %in% c("yes", "y", "yes\n"), "yes", NA
    ))

Chalcidoidea <- c("coelopencyrtus", "Chalcididae", "Chalcidoidea",  "kleine Erzwespen", "melittobia", "melittobia acasta", "chalcid wasp", "Chalcid wasp", "Melittobia", "Melittobia acasta", "Coelopencyrtus", "Erzwespe", "Eulophidae", "Kleine Wespen", "Eurytoma", "euritoma", "Monodontomerus sp")
Cacoxenus <- c("diptera", "cacox", "cacoxenus?", "cacoxenus, mites", "cacoxenus\n" , "Cacoxenus\n", "Cocaxenus", "Cacoxenenus\n", "Cacoxenus Indagator", "Cocaxenus Indagator", "Cacoxenus (fly)", "Cacoxenus" , "cacoxenus", "Cacoxenus indagator", "ci", "CI")
NAs <- c("?", "??", "na", "na\n", "white larvae", "xylocopa violacea", "small white larvae", "gelb schwarzer matsch in zelle", "Auplopus carbonarius", "Caterpillar", "Eumenidae", "dermaptera", "Dermaptera")
Kaefer <- c("beetle", "Beetle", "Holzkäfer\n", "Holzkäfer") ## Trichodes, Speckkäfer = Dermestidae (Museumskäfer)
Dermestidae <- c("Museumskäfer", "Megatoma", "Megatoma undata", "Dermastidae", "Dermastid")
Chaetodactylus <- c("Chaetodactylis", "Chaetodactylus", "Chetodactylis", "Kuglemilbe", "Chaetodactylus rund")
Ichneumonidae <- c("Schlupfwespe", "Ichneumonidae?","Ichneumonidae", "Ichneumonid")
Sapygidae <- c("Sapyga", "Sapygidae")
Gasteruption <- c("Wespe dicke Hinterbeine", "Wespe verdickte Hinterbeine")


species <- Bees %>% 
  select(c(id, location, host_bwo, morphotype_host, sample_sex, Host_species_if_known:f3)) %>% 
  rename(S0 = Host_species_if_known,
         S1 = `1st_parasitoid_morphotype`,
         S2 = `2st_parasitoid_morphotype`,
         S3 = `3rd_parasit`,
         m0 = male, m1 = m1,
         f0 = fem, f1 = f1,
  ) %>% 
  mutate(m1 = as.character(m1),
         m2 = as.character(m2),
         m3 = as.character(m3),
         f2 = as.character(f2), 
         f3 = as.character(f3)) %>% 
  select(id, location, host_bwo, sample_sex, S0, S1, S2, m0, m1, m2, m3, f0, f1, f2, f3) %>% # f0, f1
  pivot_longer(cols = !id:sample_sex, #
               #Host_species_if_known, `1st_parasitoid_morphotype`, male, female, m1, f1
               names_to = c(".value", "species_type"),
               names_pattern = "(.)(.)",
               values_drop_na = T) %>% 
  mutate(species_type = ifelse(species_type == 0, "Host", "Parasitoid")) %>% 
  # alle Chalcidoidea zusammenfassen:
  mutate(S = ifelse(S %in% Chalcidoidea, "Chalcidoidea", S)) %>% 
  # alle Käfer schreibfehler korrigieren:
  mutate(S = ifelse(S %in% Kaefer, "Beetle", S)) %>% 
  # alle Dermestidae schreibfehler korrigieren:
  mutate(S = ifelse(S %in% Dermestidae, "Dermestidae", S)) %>%  
  # alle Cacoxenus schreibfehler korrigieren:
  mutate(S = ifelse(S %in% Cacoxenus, "Cacoxenus indagator", S)) %>%
  # alle Ichneumonidae schreibfehler korrigieren:
  mutate(S = ifelse(S %in%  Ichneumonidae, "Ichneumonidae", S)) %>%  
  # alle Chaetodactylus schreibfehler korrigieren:
  mutate(S = ifelse(S %in% Chaetodactylus, "Chaetodactylus", S))%>%  
  # alle Sapygidae schreibfehler korrigieren:
  mutate(S = ifelse(S %in% Sapygidae, "Sapygidae", S)) %>% 
  # alle Gasteruption schreibfehler korrigieren:
  mutate(S = ifelse(S %in% Gasteruption, "Gasteruption", S)) %>%  
  # alle NAs korrigieren und unnötiges entfernen:
  mutate(S = ifelse(S %in% NAs, NA, S)) %>%  # paste0("Parasitoid_", species_type)
  # in die Spalte species_det werden alle bestimmten Arten geschrieben, von denen es ein sample gibt.
  group_by(location) %>% 
  mutate(species_det = tolower(ifelse(species_type == "Host",
                                      ifelse(
                                        stringr::str_detect(sample_sex, "m") | stringr::str_detect(sample_sex, "f"),
                                        S, NA), 
                                      ifelse(
                                        stringr::str_detect(sample_sex, "p"),
                                        S, NA)
  ))
  ) %>% 
  filter(!is.na(S))

rm(Chalcidoidea, Cacoxenus, NAs, Kaefer, Dermestidae, Chaetodactylus, Ichneumonidae, Sapygidae, Gasteruption)


## 1.1 leere Trapnester:  die zurückgeschickt wurden aber leer waren heraussuchen ###############
library(readxl)
Mappe1 <- read_excel("~/Documents/Uni/Master/Masterarbeit/Daten/Mappe1.xlsx", 
                     range = "A1:B309", col_types = c("text", 
                                                      "text"))
angekommen <- tolower(Mappe1$ID[which(!is.na(Mappe1$`Rücksend. Nisth. 2019`))])
leer <- angekommen[which(!angekommen %in% nests$location)]

rm(angekommen, Mappe1)

## 2. Arttabelle ##########
### für Analysen der Artenanzahl:
# fehlende Arten in Host_species_if_known aufgrund von:
# -  nest trashed
# - sample_sex == 0
# werden ersetzt durch 1) Host_species_if_known aus selben trapnest wenn:
# - identischer morphotype_host für nur eine Art im selben trapnest
# durch 2) bei mehreren möglichen Arten im selben trapnest nur durch morphotype-host bzw. gattung, was jedoch für die Berechnung der Artanzahl dann ausgeklammert werden muss.
# durch 3) gattung/morphotype_host wenn kein identisches Nest_material und morphotype_host im selben trapnest, was bei Analysen zur ß-diversität/species-turnover ausgeklammert werden muss (?)

arttabelle <- readr::read_csv2("~/Documents/Uni/Master/Masterarbeit/Daten/Arttabelle.csv")
Artdet <- species %>% 
  mutate(S = tolower(S)) %>% 
  left_join(arttabelle) %>% 
  left_join(arttabelle, by = c("S"= "species_det")) %>% 
  mutate(Gattung = ifelse(is.na(Gattung.x), Gattung.y, Gattung.x)) %>% 
  select(-Gattung.x, -Gattung.y) 

sub_Artx <- Artdet %>% 
  select(location, Art.x) %>% 
  filter(!is.na(Art.x)) %>% 
  group_by(location, Art.x) %>% 
  summarise(anzahl = n()) %>% 
  ungroup()

sub_Gattung <- Artdet %>% 
  select(location, Art.x, Gattung, species_type) %>% 
  filter(!is.na(Art.x)) %>% 
  group_by(location, Gattung, species_type) %>% 
  mutate(Artenzahl_gattung = n_distinct(Art.x, na.rm = T)) %>% 
  ungroup() %>% 
  rename(Art_unique = Art.x)

# wenn es von Gattung und Trapnest nur eine ARt gibt:
sub_Arty <- Artdet %>% 
  filter(!is.na(Art.y)) %>% 
  select(location, Art.y, Gattung, species_type) %>% 
  group_by(location, Gattung, species_type) %>% 
  summarise(anzahl_y = n_distinct(Art.y, na.rm = T)) %>% 
  ungroup()

# 22609_hemzeau ?
Artenzahl <- Artdet %>% 
  left_join(sub_Artx, by = c("location", "Art.y" = "Art.x")) %>% 
  left_join(distinct(filter(sub_Gattung, Artenzahl_gattung == 1)), by = c("location", "Gattung", "species_type")) %>% 
  left_join(sub_Arty, by = c("location", "Gattung", "species_type")) %>% 
  ## Alle Arten in Art.x wurden richtig bestimmt.
  ## Für alle weiteren Arten wird die Art nur in die Art-spalte geschrieben, wenn im IH bereits die selbe Art vorkommt oder noch keine Art der Gattung vorkommt. Wenn keine Weitere Art der Gattung vorkommt wird die Gattung als weitere Art verwendet.
  mutate(
    Art = case_when(
      !is.na(Art.x) ~ Art.x,
      is.na(Art.x) & !is.na(anzahl) ~ Art.y,
      is.na(Art.x) & is.na(anzahl) & Artenzahl_gattung == 1 ~ Art_unique,
      is.na(Art.x) & is.na(anzahl) & Artenzahl_gattung == 1 & is.na(Art_unique) ~ Gattung,
      #is.na(Art.x) & is.na(anzahl) & Artenzahl_gattung > 1 ~ NA,
      is.na(Art.x) & is.na(anzahl) & is.na(Artenzahl_gattung) & anzahl_y == 1 ~ Art.y,
      is.na(Art.x) & is.na(anzahl) & is.na(Artenzahl_gattung) & is.na(anzahl_y) ~ Gattung,
    )
  ) %>% 
  select(-Artenzahl_gattung, -anzahl, -anzahl_y) %>% ungroup()
rm(sub_Artx, sub_Arty, sub_Gattung)

# Art-spalte ist für die Berechnung der Artenzahl geeignet.

Artenzahl_Host <- Artenzahl %>% 
  group_by(species_type, location) %>% 
  summarise(Artenanzahl_det = n_distinct(Art, na.rm = T)) %>% 
  filter(species_type == "Host") %>% ungroup() %>% 
  bind_rows(data.frame(species_type = "Host", 
                       location = leer,
                       Artenanzahl_det = 0))

Abundanz_Ob <- nests %>% 
  select(id, nb_cells) %>% 
  left_join(Artenzahl, by = "id") %>% 
  filter(Art == "osmia bicornis") %>% 
  group_by(location) %>% 
  summarise(Abundanz_Ob = sum(nb_cells, na.rm = T) ,
            nests_ob = n()) %>% 
  bind_rows(data.frame(location = leer,
                       Abundanz_Ob = 0,
                       nests_ob = 0))

No_Ob <- unique(Artenzahl$location)[which(! unique(Artenzahl$location) %in% Abundanz_Ob$location)]

Abundanz_Ob <- Abundanz_Ob %>% 
  bind_rows(data.frame(location = No_Ob,
                       Abundanz_Ob = 0,
                       nests_ob = 0))


Abundanz <- nests %>% 
  group_by(location) %>% 
  summarise(Abundanz = sum(nb_cells, na.rm = T)) %>% ungroup() %>% 
  bind_rows(data.frame(location = leer,
                       Abundanz = 0))

Abundanz_Host <- Artenzahl_Host %>% 
  left_join(Abundanz, by = "location")

Artenzahl_Par <- Artenzahl %>% 
  group_by(species_type, location, .drop=FALSE) %>% 
  summarise(Artenanzahl_det = n_distinct(Art, na.rm = T)) %>% 
  pivot_wider(values_from = Artenanzahl_det, names_from = species_type, values_fill = 0 ) %>% 
  ungroup() %>% 
  bind_rows(data.frame(location = leer,
                       Host = 0,
                       Parasitoid = 0))

Artenzahl_all <- Artenzahl %>% 
  group_by(location) %>% 
  summarise(Artenanzahl_det = n_distinct(Art, na.rm = T))%>% 
  bind_rows(data.frame(location = leer,
                       Artenanzahl_det = 0))

# 2.1 einzelne Arten: Welche Arten sind als einzelne Arten in den trapnestern? ########
TN_1nest <- Artenzahl %>% filter(species_type == "Host") %>% 
  select(location, Art) %>% 
  distinct() %>% 
  group_by(location) %>% 
  filter(n() == 1) %>% 
  left_join(Artenzahl_Host, by = c("location"))

## 3. Praedis ##########
# dazu kommen als prädiktoren: -imp 500, imp 2000, clc500, clc_500_edgelength, clc_500_nr_classes, temp, percip, altitude

plz <- function(adress){
  return(as.numeric(unlist(stringr::str_split(adress, ","))[2]))
}

pr1_2 <- versiegelung %>% select(-geometry) %>% 
  pivot_wider(names_from = radius_m, values_from = imp) %>% 
  group_by(ID) %>% 
  mutate(postal_code = plz(Adresse)) %>% 
  ungroup() %>% 
  rename(imp500 = `500`,
         imp2000 = `2000`,
         imp1000 = `1000`,
         imp250 = `250`,
         imp100 = `100`)

pr3 <- read.csv("~/Documents/Uni/Master/Masterarbeit/Daten/clc500.csv") %>% select(-X) %>% 
  mutate_at(vars(matches("X")), list(~ replace_na(., 0))) %>% 
  mutate(schulid = tolower(schulid))
pr4 <- read.csv2("~/Documents/Uni/Master/Masterarbeit/Daten/clc_500_edgelength.csv") %>% 
  mutate(schulid = tolower(schulid))
pr5 <- read.csv2("~/Documents/Uni/Master/Masterarbeit/Daten/clc_500_nr_classes.csv")%>% 
  mutate(schulid = tolower(schulid))
pr6_8 <- read.csv2("~/Documents/Uni/Master/Masterarbeit/Daten/climate_data.csv") %>% 
  distinct(postal_code, .keep_all = TRUE)
pr_insektenhotel <- read.csv2("~/Documents/Uni/Master/Masterarbeit/Daten/Standortinfos20201115.csv") %>% 
  select(-X) %>% mutate(Kenncode = tolower(Kenncode)) %>% 
  select(-AngeInsektenhaus) %>% 
  mutate(Isnsektenhaus = case_when(Isnsektenhaus == "Ja" ~ 1,
                                   Isnsektenhaus == "Nein"~ 0))
library(readxl)
add_insektenhotel <- read_excel("~/Documents/Uni/Master/Masterarbeit/Daten/2020_01_08_contact form data.xlsx") %>% 
  filter(Date > as.POSIXct(as.Date("2020.11.15", "%Y.%m.%d"))) %>% 
  select(Kenncode, DistInsektenhaus, Isnsektenhaus) %>% 
  mutate(Kenncode = tolower(Kenncode),
         DistInsektenhaus = as.integer(DistInsektenhaus),
         Isnsektenhaus = case_when(Isnsektenhaus == "Ja" ~ 1,
                                   Isnsektenhaus == "Nein"~ 0))

add_insektenhotel <- pr_insektenhotel %>% filter(!Kenncode %in% add_insektenhotel$Kenncode) %>% 
  bind_rows(add_insektenhotel[-c(8,12,40),])

# allse Prs zusammenmergen
Praedis <- pr1_2 %>% 
  left_join(pr6_8, by= "postal_code") %>% 
  select(ID, imp100, imp250, imp500, imp1000, imp2000, mean_annual_temp_celsius, annual_precipitation_mm, altitude_town_m) %>% 
  left_join(pr4, by = c("ID" = "schulid")) %>% 
  left_join(pr5, by = c("ID" = "schulid")) %>% 
  left_join(pr3, by = c("ID" = "schulid")) %>% 
  mutate(ID = tolower(ID)) %>% 
  left_join(add_insektenhotel, by = c("ID" = "Kenncode"))
rm(pr1_2, pr3, pr4, pr5, pr6_8, pr_insektenhotel)

## Prädiktoren, die mehr als 90% aus Nullen bestehen werden entfernt:

nullpraedis <- Praedis %>% 
  summarize_all(function(x){sum(x == 0, na.rm = T)}) %>% 
  pivot_longer(cols = everything()) %>% 
  filter(value < nrow(Praedis)*0.75)
# entfernt: "X140" "X510" "X520" "X324" "X420" "X242" "X410" "X243" "X221" "X222" "X321"

Praedis <- Praedis %>% select(any_of(nullpraedis$name)) %>% 
  rename(Forest = X310,
         Urban = X100,
         Arable = X210,
         Pastures = X231)
rm(nullpraedis)

set.seed(1)
A1 <- Abundanz_Host %>% 
  left_join(Praedis,  by = c("location"= "ID")) %>% 
  select(-species_type) %>% 
  mutate(test1 = scale(runif(nrow(.)))[,1],
         test2 = scale(runif(nrow(.)))[,1],
         test3 = scale(runif(nrow(.)))[,1])

A1_IH_factor <- A1 %>% 
  mutate(Insektenhaus = as.factor(Isnsektenhaus)) %>% 
  select(-Isnsektenhaus) 

A2 <- Artenzahl_Par %>% 
  left_join(Praedis,  by = c("location"= "ID"))  %>% 
  mutate(test1 = scale(runif(nrow(.)))[,1],
         test2 = scale(runif(nrow(.)))[,1],
         test3 = scale(runif(nrow(.)))[,1])

Osmia_bicornis <- Abundanz_Ob %>% 
  left_join(Praedis,  by = c("location"= "ID")) %>% 
  #select( -location) %>% 
  mutate(Insektenhaus = as.factor(Isnsektenhaus)) %>% 
  select(-Isnsektenhaus) %>% 
  mutate(test1 = scale(runif(nrow(.)))[,1],
         test2 = scale(runif(nrow(.)))[,1],
         test3 = scale(runif(nrow(.)))[,1])

## 4. koords: Standorte ###########
koords <- Standorte %>% 
  select(ID, lat, lon) %>% 
  mutate(ID = tolower(ID)) 

A1_koords <- A1 %>% 
  left_join(koords, by = c("location" = "ID")) 

Artenzahl_autocor <- A1_koords %>% 
  # mutate(ID = tolower(ID),
  #        lat = as.numeric(gsub("\\.", "", lat)),#as.numeric(gsub(",", "", y))
  #        lon = as.numeric(gsub("\\.", "", lon))) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% ## 3857
  sf::st_transform(crs = 3035) %>% 
  mutate(lon = sf::st_coordinates(geometry)[,1],
         lat = sf::st_coordinates(geometry)[,2]) %>% 
  sf::st_set_geometry(NULL)


