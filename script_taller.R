

rm(list=ls())
gc()

setwd("/Users/pfernandezn/Desktop/")

# "C:/XXXXXXXXXXXX/Desktop" para windows

require("openxlsx")

datos<-read.xlsx("res/2023-12-13_resultado_geo_ciber.xlsx")

datos_ad<-read.xlsx("res/2023-12-13_resultado_geo_ciber_adicional.xlsx")

table(datos$geo_ciber_score)

table(datos_ad$comb_score, datos_ad$combinaciones)


# QC Scores

check_score<-which(datos_ad$"combinaciones"==2)

datos_ad[check_score,c("ID","NUM","portalNumber_carto",
                       "comb_score","combinaciones")]

datos_ad[check_score,c("TIPO_VIA","DIRECCION","MUNICIPIO",
                       "PROVINCIA","province_carto")]


delete_reg<-which(datos_ad$"combinaciones"==2)

datos<-datos[-delete_reg,]
datos_ad<-datos_ad[-delete_reg,]

row.names(datos)<-NULL
row.names(datos_ad)<-NULL

dim(datos)
dim(datos_ad)


require(sf)
require(rgdal)

EPSG <- make_EPSG()
EPSG[EPSG$"note"%in%"WGS 84",]


# st_as_sf

datos_sf <- sf::st_as_sf(datos,
                         coords = c("long_geo_ciber","lat_geo_ciber"),
                         crs = 4326)

navalmoral.shp <- st_read("datos/mapa_naval.shp")

navalmoral.shp<-st_transform(navalmoral.shp, 4326)

plot(st_geometry(navalmoral.shp))
plot(st_geometry(datos_sf),pch=3,col="red",add=T)

st_write(datos_sf, "res/datos_geocodificados.kml", 
         driver = "kml", delete_dsn = TRUE)
st_write(navalmoral.shp, "res/mapa_navalmoral.kml", 
         driver = "kml", delete_dsn = TRUE)


indice<-unlist(st_intersects(datos_sf, navalmoral.shp))

datos_sf$CUSEC<-navalmoral.shp$CUSEC[indice]



datos.epi<-read.xlsx("datos/datos.epi.xlsx")

head(datos.epi)

datos_sf<-merge(datos_sf,datos.epi,by="ID")


pob<-read.xlsx("datos/poblacion_cusec_caceres.xlsx")
pob$ambos<-pob$Hombre+pob$Mujer

navalmoral.shp<-merge(navalmoral.shp,pob,by="CUSEC")


casos<-datos_sf

save(navalmoral.shp,casos,file="datos/datos_procesados.RData")






