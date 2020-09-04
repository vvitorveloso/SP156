library(tidyverse)
library(readr)

if(!file.exists("dados-do-sp156---1o-tri-2020.csv.csv")){
  download.file("http://dados.prefeitura.sp.gov.br/dataset/0aecfa2b-aa3a-40d4-8183-0d4351b7fd0a/resource/f02562af-23dd-45a3-943b-6a6b32bc5799/download/dados-do-sp156---1o-tri-2020.csv", "dados-do-sp156---1o-tri-2020.csv.csv")
}

SP156 <- read.csv2("~/Documents/CPM/dados-do-sp156---1o-tri-2020.csv", sep=";", fileEncoding = "Latin1")
calçadas <- unique(SP156$Serviço) %>% grep("alçada",.,value = TRUE)
calc <- as.data.frame(SP156 %>% filter(Serviço %in% calçadas))
buraco <- as.data.frame(SP156 %>% filter(Serviço %in% "Tapa-buraco"
))
servs <- as.data.frame(unique(SP156$Serviço) )

unique(calc$Logradouro)
unique(calc$Número)
unique(calc$cep)
nrow(calc)

##########ARQUIVO ESIC

SP156 <- read.csv2("~/Documents/CPM/49087_CALCADAS_ESIC (1).CSV", sep=";", fileEncoding = "Latin1")
calçadas <- unique(SP156$Serviço) %>% grep("alçada",.,value = TRUE)
calc <- as.data.frame(SP156 %>% filter(Serviço %in% calçadas))
buraco <- as.data.frame(SP156 %>% filter(Serviço %in% "apa-buraco"
))
servs <- as.data.frame(unique(SP156$Serviço) )

unique(calc$Logradouro)
calc[unique(calc$Número)]

#list address
#x <- calc[!is.na(calc$Número),] %>% select(Serviço, Logradouro,Número) %>% group_by(Serviço)  %>% unique(Logradouro && Número)# %>% plot(Serviço,Logradouro)

x_dup <-calc[!is.na(calc$Número),] %>% select(Serviço, Logradouro,Número,Longitude,Latitude) %>% add_count(Logradouro,Número) %>% 
  filter(n > 1)

x <-calc[!is.na(calc$Número),] %>% select(Serviço, Logradouro,Número) %>% distinct() # && Número)

unique(calc$cep)
nrow(calc)
x_dup$Latitude

###################################################
library(maps) #mapas simples, eixos, escala, cidades 
library(mapdata) #base de dados WorldHires e rios
library(rworldmap) #outra base de dados de mapas do mundo
library(maptools) #Ler ESRI shapefiles 
library(mapproj) #Projeções e grids
library(ggmap) #Gmaps, OSM + mapas baseados em ggplot2
library(rgdal)
library(maps)


#?map
#map.cities(country = "Brazil",minpop = 2000000,pch=19, cex=1.2)# pacote maps
subs <- readShapePoly("~/Documents/CPM/LAYER_DISTRITO/DEINFO_DISTRITO.shp")

head(subs)
names(subs)
plot(subs, col="grey70")
summary(subs@data)

subs 

map <- ggplot() + geom_polygon(data = subs, aes(x = long, y = lat, group = group, fill = id), colour = "black") + theme_void()

?geom_polygon

#install.packages("sf")
library(sf)
subs <- read_sf("~/Documents/CPM/LAYER_DISTRITO/DEINFO_DISTRITO.shp")
ggplot(subs) + geom_sf(aes(fill=COD_SUB)) + geom_sf(data=x_dup)#, aes(x = Longitude, y = Latitude), size = 1, shape = 1, fill = "darkred") #+  coord_sf(xlim = c(-81, -79), ylim = c(34, 36))
x_dup2 <- x_dup %>% select(Longitude,Latitude) 

x_dup2 <- st_transform(x_dup, crs = target_crs)
  ?st_transform
  
ggplot(subs) + geom_sf(data=subs) + geom_point(data = x_dup, aes(x = Longitude, y = Latitude)) +coord_sf(xlim = c(-20, 45), ylim = c(-26, -46), expand = TRUE)
  #geom_sf_label(aes(label = NOME_DIST))
?geom_sf
map 
#################FUNCIONAAAAAAA PORRRAAAAA!!!!
install.packages("spData")
my <- sf::st_read("~/Documents/CPM/LAYER_DISTRITO/DEINFO_DISTRITO.shp") %>%  st_transform(4326) 
#nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#world3 <- sf::st_read(system.file("shapes/world.gpkg", package = "spData"))
ggplot(my) +
  geom_sf()
ggplot(nc) +
  geom_sf()

x_dup %>% st_transform(4326)
ggplot(my) +
  geom_sf(data=my) +
  geom_point(data = x_dup, aes(x = Longitude, y = Latitude))

####################
#SP <- cidades[cidades@data$NAME_1=="São Paulo",] ##seleciona e cria um novo shapefile
#plot(SP[SP@data$NAME_2=="São Paulo",], add=T, col="grey70")

#plot(SP)

#proj4string(cidades)

#?map.cities
##########################################################NEW


library(tidyverse)
library(sf)
devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
library(geobr) 
library(foreign)
library(readxl)
#https://stackoverflow.com/questions/57415464/label-kml-features-using-st-write
county_polygons <- st_read(system.file("shape/nc.shp", package="sf")) %>% 
  st_transform(4326) %>% # just because wgs84...
  select(Name = NAME) # see https://gdal.org/drivers/vector/kml.html#creation-options


st_write(county_polygons, "test.kml", driver = "kml", delete_dsn = TRUE)

# aplicando ao meu banco (DEU CERTO!!!):
setores_selec_kml <- setores_selec %>% 
  st_transform(4326) %>% 
  select(Name = code_tract,
         name_district)

sf::st_write(setores_selec_kml, "setores selecionados s?o paulo3.kml",
             driver = "kml",
             delete_dsn = TRUE)

help(st_write)
?sf