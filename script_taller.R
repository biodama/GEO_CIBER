

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



require(tmaptools)

st_bbox(casos)

navalmoral <- read_osm(casos, ext=1.2)

navalmoral

require(tmap)
mapa <- tm_shape(navalmoral) + tm_rgb(saturation=0.2) #saturation=0: mapa en blanco y negro
mapa

mapa + tm_shape(casos) + tm_dots(alpha=.5, col="red3", size=.25)

mapa +
  tm_shape(casos) + tm_dots(alpha=.5, col="red3", size=.25) +
  tm_facets("sexo", ncol=1,free.coords = FALSE)


mapa +
  tm_shape(casos) + tm_dots(alpha=.5, col="red3", size=.25) +
  tm_facets("sexo", ncol=2)


mapa +
  tm_shape(casos) + tm_dots(alpha=.5, col="red3", size=.25) +
  tm_facets("edad.gr", ncol=2)


tmap_mode("view") #tmap_mode("plot") para mapas estaticos
tm_shape(casos) + tm_dots(alpha=.5, col="red3")


tmap_mode("view") #tmap_mode("plot") para mapas estaticos
tm_shape(casos) + tm_dots(alpha=.5, col="sexo")


tmap_mode("view")
  tm_shape(casos) + tm_dots(alpha=.5, col="sexo", size=.25) +
  tm_facets("sexo", ncol=2)


casos.sec=aggregate(ID~CUSEC, data=casos, FUN =length) #calcula numero de casos por sc
casos.sec

navalmoral.shp[, c("CUSEC","ambos")]


tmap_mode("plot") # vuelta al modo estático
mapa + tm_shape(navalmoral.shp) + tm_borders("blue3",lwd=2)


casos.shp = merge(navalmoral.shp,casos.sec,by="CUSEC",all.x=T) # Cuidado hay una sección sin datos
casos.shp$ID[is.na(casos.shp$ID)]<-0


casos.shp$tasa = casos.shp$ID / (casos.shp$ambos*5) * 100000 # tasa por 100.000 personas año
casos.shp[,c("CUSEC","ambos","tasa")]


tmap_mode("plot") #vuelta a la versión estatica
mapa + tm_shape(casos.shp) + tm_fill("tasa",alpha=.5, title="Incidencia (por 100.000)")



tmap_mode("plot") #vuelta a la versión estatica
mapa + tm_shape(casos.shp) + tm_fill("tasa",alpha=.5, title="Incidencia (por 100.000)")+
  tm_borders(col="grey")





