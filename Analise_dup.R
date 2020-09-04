library(tidyverse)
library(sf)

if(!file.exists("dados-do-sp156---1o-tri-2020.csv.csv")){
  download.file("http://dados.prefeitura.sp.gov.br/dataset/0aecfa2b-aa3a-40d4-8183-0d4351b7fd0a/resource/f02562af-23dd-45a3-943b-6a6b32bc5799/download/dados-do-sp156---1o-tri-2020.csv", "dados-do-sp156---1o-tri-2020.csv.csv")
}

#Lê arquivo
SP156 <- as.data.frame(read.csv2("dados-do-sp156---1o-tri-2020.csv.csv", sep=";", fileEncoding = "Latin1"))
###########################################################
#Carrega calçadas

SP156_CAL <- read.csv2("~/Documents/CPM/49087_CALCADAS_ESIC (1).CSV", sep=";", fileEncoding = "Latin1")

#Filtra non NA, Seleciona dados, seleciona duplicidades
casos_cal_dup <-SP156_CAL[!is.na(SP156_CAL$Número),] %>% 
  select(Serviço, Logradouro,Número,Longitude,Latitude) %>% 
  add_count(Logradouro,Número) %>%  
  filter(n > 1) %>% 
  unique()

#calçadas <- unique(SP156$Serviço) %>% grep("alçada",.,value = TRUE)
#calc <- as.data.frame(SP156 %>% filter(Serviço %in% calçadas))
##########################################################

#Filtra non NA, Seleciona dados, seleciona duplicidades
casos_dup <-SP156[!is.na(SP156$Número),] %>% 
  select(Serviço, Logradouro,Número,Longitude,Latitude) %>% 
  add_count(Logradouro,Número) %>%  
  filter(n > 1) %>% 
  unique()


#Converte casos para localização geométrica
pontos_sf <- st_as_sf(casos_cal_dup, coords = c( 'Longitude','Latitude'), crs = st_crs(mapa_cidade)) %>%  st_transform(4326)

casos <-  pontos_sf %>% mutate(
  intersection = as.integer(st_intersects(geometry, mapa_cidade))
     , COD_SUB = if_else(is.na(intersection)
     , ''
     , mapa_cidade$COD_SUB[intersection])
) 

#pontos_sf %>% as.integer(st_intersects(.$geometry, mapa_cidade))

#pontos_sf %>% mutate(
#  intersection = as.integer(st_intersects(geometry, mapa_cidade))
#  , COD_SUB = if_else(is.na(intersection)
#                   , ''
#                   , mapa_cidade$COD_SUB[intersection])
#) 

#st_snap(pontos_sf$geometry,mapa_cidade)

#st_contains(pontos_sf$geometry,mapa_cidade)
#st_contains(mapa_cidade$geometry,pontos_sf$geometry)
#st_combine(pontos_sf$geometry,mapa_cidade)
#st_union(pontos_sf$geometry, mapa_cidade)


#st_intersects(pontos_sf$geometry, mapa_cidade)
#st_within(pontos_sf,mapa_cidade)
#?st_within

#sf_as_st
pontos_sf <- st_as_sf(casos_cal_dup, coords = c( 'Longitude','Latitude'), crs = st_crs(mapa_cidade))
##################
#Junta mesma sub

casos_by_mapa_cidade <- 
  casos %>%  as.data.frame() %>% 
  #seleciona 
    select(n,COD_SUB) %>% 
  #soma
    aggregate( . ~ COD_SUB, .,FUN = sum )


# nao serve para esse caso #tapply(x$n,INDEX=x$area,FUM=sum)

# Merge não adianta , mais de 1 dist por sub, monguei. #
x<- merge(mapa_cidade,casos_by_mapa_cidade,by.x="COD_SUB" ,by.y = "COD_SUB",all = TRUE)

####################PAREI AQUI

##############MUITO UTIL MERGE
casos

#x<- merge(mapa_cidade,as(casos,"Spatial"),by.x="COD_SUB" ,by.y = "area")
#################

ggplot(mapa_cidade) +
  geom_sf()+
  geom_sf(data=casos,aes(geometry = geometry, size = n), show.legend = "point")#  geom_sf(data=casos, fill = casos$intersection, color = gray(.5))

################################MAPAS
#Carrega mapa
mapa_cidade <- sf::st_read("~/Documents/CPM/LAYER_DISTRITO/DEINFO_DISTRITO.shp") %>%  st_transform(4326) 

#Cria mapa de densidade

ggplot(mapa_cidade) +
  geom_sf(data=mapa_cidade) +
  geom_density2d(data=casos_dup,aes(x=Longitude,y=Latitude), bins=30) +
  stat_density2d(data=casos_dup,aes(x=Longitude,y=Latitude,fill=..level.., alpha=..level..), geom='polygon') 
#####################
#Cria mapa das mapa_cidade
ggplot(mapa_cidade) +
  geom_sf(data=mapa_cidade) +
  #geom_point(data = casos_cal_dup, aes(x = Longitude, y = Latitude))+
  geom_sf(aes(fill = COD_SUB))
#####################
#Plota pontos em sobreposição
ggplot(mapa_cidade) +
  geom_sf(data=mapa_cidade) +
  geom_point(data = casos_dup, aes(x = Longitude, y = Latitude),alpha=.05,size=.5)
  #geom_point(data = casos_cal_dup, aes(x = Longitude, y = Latitude))
#####################
#Cria mapa por mapa_cidade com tamanho de acordo com N???
ggplot(mapa_cidade) +
  geom_sf() +
  geom_sf(data=casos,aes(geometry = geometry, size = n), show.legend = "point")#  geom_sf(data=casos, fill = casos$intersection, color = gray(.5))

#####################
#Cria mapa por 
ggplot(x) +
#  geom_sf(data=mapa_cidade, color="red",fill="transparent") +
  geom_sf(data=x,color="red", aes(fill=n,color=n,))#+
  geom_sf(data=state)
  
####################FAAAAAAAZZZZ SUUUUUBS FAZ MAPA mapa_cidade  
x %>%  
  group_by(COD_SUB) %>% 
  summarise() %>% 
  ggplot(aes(fill=COD_SUB))  +
  geom_sf()
###################
  #  geom_sf(data=mapa_cidade, color="red",fill="transparent") +
########PRECISA CRIAR UM SEGUNDO MAPA PARA DELIMITAR AS SUBS
#https://stackoverflow.com/questions/54658616/make-a-map-with-a-group-of-subregions-with-geom-sf
mapa_subs <- mapa_cidade %>%  
  group_by(COD_SUB) %>% 
  summarise()
#################
##MAPA COM CASOS POR SUBS
install.packages("viridis")
library(viridis)

ggplot(x) +
  #  geom_sf(data=mapa_cidade, color="red",fill="transparent") +
  geom_sf(data=x,color="transparent", aes(fill=n,colour = white))+
  geom_sf(color="black",data=mapa_subs, fill="transparent")+
  scale_fill_gradient(low = "gray", high = "red")
  
  scale_fill_viridis(option="magma")
######################################################################
#TEST SPACE

  pontos_sf <- st_as_sf(casos_dup, coords = c( 'Longitude','Latitude'), crs = st_crs(mapa_cidade)) %>%  st_transform(4326)
  
  casos <-  pontos_sf %>% mutate(
    intersection = as.integer(st_intersects(geometry, mapa_cidade))
    , COD_SUB = if_else(is.na(intersection)
                        , ''
                        , mapa_cidade$COD_SUB[intersection])
  ) 

  
  casos_by_mapa_cidade <- 
    casos %>%  as.data.frame() %>% 
    #seleciona 
    select(n,COD_SUB) %>% 
    #soma
    aggregate( . ~ COD_SUB, .,FUN = sum )
  
  
    
  x<- merge(mapa_cidade,casos_by_mapa_cidade,by.x="COD_SUB" ,by.y = "COD_SUB",all = TRUE)
  
  ggplot(x) +
    #  geom_sf(data=mapa_cidade, color="red",fill="transparent") +
    geom_sf(data=x,color="transparent", aes(fill=n,colour = white))+
    geom_sf(color="black",data=mapa_subs, fill="transparent")+
#    geom_sf_label(data=x,aes(label = SIGLA_DIST),label.size = 0.1,size=5)+
    scale_fill_gradient(low = "blue", high = "red")
  

#?subset
#st_bbox(x)
#subset(counties, grepl("florida", counties$ID))
#subset(x, grepl("*", x$COD_SUB), drop = TRUE)
  #  geom_sf(data=casos,aes(geometry = geometry, size = n), show.legend = "point")#  geom_sf(data=casos, fill = casos$intersection, color = gray(.5))

#z <- merge(mapa_cidade,casos_by_mapa_cidade,by.x="COD_SUB" ,by.y = "COD_SUB",all = FALSE)


#########################################################################
###################################################
#Checagem e testes

?geom_sf()
?stat_count

st_crs(mapa_cidade)
mapa_cidade
casos
mapa_cidade$geometry
pontos_sf %>% st_intersects(geometry, mapa_cidade)
