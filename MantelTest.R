# Prueba mantel

library(tidyverse)
library(readxl)

# Limpieza data aves

BirdData <- read_excel("Data/Data_birds.xls", sheet = 1)
BirdData2 <- read_excel("Data/Data_birds.xls", sheet = 2)
BirdData3 <- read_excel("Data/Data_birds.xls", sheet = 3)


BirdData <- BirdData %>% 
  select(`Sitio nombre`, `Sitio #`, Especies) %>% 
  mutate(Visita = "Uno")

BirdData2 <- BirdData2 %>% 
  select(`Sitio nombre`, `Sitio #...2`  , Especies) %>% 
  mutate(Visita = "Dos")

BirdData3 <- BirdData3 %>% 
  select(`Sitio nombre`, `Sitio #...2`  , Especies) %>% 
  mutate(Visita = "Tres")



colnames(BirdData) <-c( "SitioNombre", "SitioCod", "Especies", "Visita")
colnames(BirdData2) <-c( "SitioNombre", "SitioCod", "Especies", "Visita")
colnames(BirdData3) <-c( "SitioNombre", "SitioCod", "Especies", "Visita")

BirdFull <- rbind(BirdData, BirdData2, BirdData3)


BirdWider <- BirdFull %>% 
  group_by(SitioNombre, SitioCod, Especies) %>% 
  summarise(Abundance = max(n())) %>% 
  pivot_wider(names_from = Especies, values_from = Abundance,
              values_fill = 0) %>% 
  separate(SitioCod, into = c("Sitio", "Sistema"), sep = "(?<=[0-9])(?=[A-Za-z])" )

str(BirdData)
# Ingresar ubicacion por sitio

library(geosphere)

Sitios <- read_excel("Data/Data_birds.xls", sheet = "PTS")

str(Sitios)

# Unir base de datos

DataBirdLoc <- Sitios %>% 
  mutate(ID = as.character(ID)) %>% 
  rename("Sitio" = ID,
         "SitioNombre" = SiteName) %>% 
  full_join(BirdWider, by = c("Sitio", "SitioNombre"))

rownames(DataBirdLoc) <- DataBirdLoc$Sitio

DataBirdLoc <- DataBirdLoc %>% 
  na.omit()

# Calcular distancia en kilometros para sitios

coords <- as.matrix(DataBirdLoc[, c("LONG", "LAT")])

distance_matrix <- distm(coords, fun = distHaversine)
distance_matrix_km <- distance_matrix / 1000

colnames(distance_matrix_km) <- DataBirdLoc$Sitio


# Calcular distancia bray-curtis para sitios


bird_bc <- as.matrix(distance(DataBirdLoc[,15:150], "bray-curtis"))

dim(bird_bc)           
dim(distance_matrix_km)


vegan::mantel(bird_bc, distance_matrix_km, method = "spearman")


bird_bc_dist <- as.dist(bird_bc)
plot(hclust(bird_bc_dist))
