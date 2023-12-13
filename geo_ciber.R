##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################



########################################################################################################################
########################################################################################################################
# 
#    1. LIBRERIAS
#
########################################################################################################################
########################################################################################################################


rm(list=ls())
gc()


setwd("C:/Users/biodama/AppData/Local/Temp/VICA/app")  # Sustituir el usuario por el que corresponda

########################################################################################################################
########################################################################################################################
# 
#    2. IMPORTACION DATOS: Campos obligatorios: TIPO_VIA,  DIRECCION, NUM, CP, MUNICIPIO ,PROVINCIA  
#
########################################################################################################################
########################################################################################################################
scriptEntero <- function() {

library("openxlsx")
library("gdata")
library("caRtociudad")
library("stringi")
library("rgdal")
library("parallel")
library("plyr")

fileEntrada <- file("./data/ficheroentrada.txt") #jgp
ficheroentrada <- readLines(con = fileEntrada, n = 3) #jgp
datos.orig<-read.xlsx(ficheroentrada[1], sheet=1) #jgp

datos<-datos.orig
datos$id<-seq(1,dim(datos)[1],1)



########################################################################################################################
########################################################################################################################
# 
#    3. NORMALIZACION INICIAL DE VARIABLES  
#
########################################################################################################################
########################################################################################################################

datos$TIPO_VIA<-trim(toupper(datos$TIPO_VIA))
datos$TIPO_VIA_carto<-datos$TIPO_VIA
datos$TIPO_VIA_carto[datos$TIPO_VIA_carto%in%""]<-"999999"
datos$TIPO_VIA_carto[is.na(datos$TIPO_VIA_carto)]<-"999999"
datos$TIPO_VIA_carto<-gsub(","," ",datos$TIPO_VIA_carto)
datos$TIPO_VIA_carto<-gsub("     "," ",datos$TIPO_VIA_carto)
datos$TIPO_VIA_carto<-gsub("    "," ",datos$TIPO_VIA_carto)
datos$TIPO_VIA_carto<-gsub("   "," ",datos$TIPO_VIA_carto)
datos$TIPO_VIA_carto<-gsub("  "," ",datos$TIPO_VIA_carto)


datos$DIRECCION<-trim(toupper(datos$DIRECCION))
datos$DIRECCION_carto<-datos$DIRECCION
datos$DIRECCION_carto[is.na(datos$DIRECCION_carto)]<-"999999"
datos$DIRECCION_carto[datos$DIRECCION_carto%in%""]<-"999999"

datos$PROVINCIA<-trim(toupper(datos$PROVINCIA))
datos$PROVINCIA_carto<-datos$PROVINCIA
datos$PROVINCIA_carto[is.na(datos$PROVINCIA_carto)]<-"999999"
datos$PROVINCIA_carto[datos$PROVINCIA_carto%in%""]<-"999999"

datos$pais<-"SPAIN"

datos$NUM<-trim(toupper(datos$NUM))
datos$numero<-as.numeric(datos$NUM)
datos$NUM[is.na(datos$numero)]<-""
datos$NUM_carto<-datos$NUM
datos$NUM_carto[is.na(datos$NUM_carto)]<-"999999"
datos$NUM_carto[datos$NUM_cartoo%in%""]<-"999999"

datos$MUNICIPIO<-trim(toupper(datos$MUNICIPIO))
mirar<-strsplit(datos$MUNICIPIO,split="/")
mirar2<-lapply(mirar,function(x) x[1])
datos$MUNICIPIO<-unlist(mirar2)
rm(mirar,mirar2)
datos$MUNICIPIO_carto<-datos$MUNICIPIO
datos$MUNICIPIO_carto[is.na(datos$MUNICIPIO_carto)]<-"999999"
datos$MUNICIPIO_carto[datos$MUNICIPIO_carto%in%""]<-"999999"


if(exists("datos$CP")) {
  datos$CP<-trim(toupper(datos$CP))
  datos$cp.new<-as.numeric(datos$CP)
  datos$CP[is.na(datos$cp.new)]<-""
  datos$CP_carto<-datos$CP

  for(m in 1:dim(datos)[1]){
	
	  longitud.mirar<-nchar(datos$CP_carto[m])
	
	  if(longitud.mirar<5 & datos$CP_carto[m]!="" & !is.na(datos$CP_carto[m])){
		
		  datos$CP_carto[m]<-paste(rep("0",5-nchar(datos$CP_carto[m])),datos$CP_carto[m],sep="")
			
	  }
		
  }
  datos$CP_carto[is.na(datos$CP_carto)]<-"999999"
  datos$CP_carto[datos$CP_carto%in%""]<-"999999"
}

########################################################################################################################
########################################################################################################################
# 
#    4. GEOCODIFICACION CON CARTOCIUDAD
#
########################################################################################################################
########################################################################################################################


################################################
# CALLES A BUSCAR EN CARTOCIUDAD
################################################

# Con todo
datos$calle.carto1<-tolower(paste(trim(paste(datos$TIPO_VIA_carto,datos$DIRECCION_carto,datos$NUM_carto,sep=" ")),
datos$MUNICIPIO_carto,sep=","))


datos$calle.carto1[datos$MUNICIPIO_carto%in%"999999"]<-tolower(paste(trim(paste(trim(paste(datos$TIPO_VIA_carto[datos$MUNICIPIO_carto%in%"999999"],
datos$DIRECCION_carto[datos$MUNICIPIO_carto%in%"999999"],
    datos$NUM_carto[datos$MUNICIPIO_carto%in%"999999"],sep=" ")))),sep=","))
datos$calle.carto1<-gsub(",999999,",",",datos$calle.carto1)
datos$calle.carto1<-trim(gsub("999999","",datos$calle.carto1))


# Sin tipo via

datos$calle.carto2<-tolower(paste(trim(paste(datos$DIRECCION_carto,datos$NUM_carto,sep=" ")),
datos$MUNICIPIO_carto,sep=","))

datos$calle.carto2[datos$MUNICIPIO_carto%in%"999999"]<-tolower(paste(trim(paste(trim(paste(datos$DIRECCION_carto[datos$MUNICIPIO_carto%in%"999999"],
    datos$NUM_carto[datos$MUNICIPIO_carto%in%"999999"],sep=" ")))),sep=","))
datos$calle.carto2<-gsub(",999999,",",",datos$calle.carto2)
datos$calle.carto2<-trim(gsub("999999","",datos$calle.carto2))



############################################################
# FUNCION PARA GEOCODIFICAR USANDO CARTOCIUDAD
############################################################

busqueda.carto<-function(datos.buscar, long.ini, long.total) {   
    
    library("caRtociudad")
 
  aux <- datos.buscar  #jgp
  s<-strsplit(aux,split="@@@") #jgp
  datos.buscar<- s[[1]][[2]] #jgp
  fila<-s[[1]][[1]] #jgp
  x.progress <- (as.numeric(fila) * 22) / long.total #jgp
  i.progress <- long.ini + (round(x.progress,digits=0)) #jgp

    inicio<-Sys.time()
    datos.carto.mirar<-datos.buscar
           lat<-NA
    long<-NA
    state<-NA
    stateMsg<-NA
    geom_res<-NA
    portalNumber<-NA
    province<-NA
          
        # print(i)
        calle.buscar<-as.character(datos.carto.mirar)
        
        res <- try(cartociudad_geocode(full_address = calle.buscar),silent=T)

        if(class(res)%in%"try-error"){
        
            lat<-NA
            long<-NA
            state<- 999999
            stateMsg<-"REVISA LA CALLE"
            geom_res<-NA
            portalNumber<-NA
            province<-NA
        }

        if(class(res)!="try-error"){
       		  if(is.null(res$"portalNumber")){
                
                portalNumber<-NA
                
            }
            if(!is.null(res$"portalNumber")){
                
                portalNumber<-res$"portalNumber"
                
            }
            
            lat<-res$lat
            long<-res$lng
            state<- res$state
            stateMsg<-res$stateMsg
            geom_res<-unlist(strsplit(res$"geom",split="[(]"))[[1]]
            #portalNumber<-res$"portalNumber"
            province<-res$"province"
        }
    
    
    
    
    resultado<-data.frame(lat_carto=lat,long_carto=long,state=state,
        stateMsg=stateMsg,geom=geom_res,portalNumber=portalNumber,
        province=province,stringsAsFactors=F)

    resultado


}


############################################################
# GEOCODIFICACION CARTOCIUDAD
############################################################

numWorkers <- detectCores()-1
cl <- makeCluster(numWorkers)

aux<-datos #jgp
#aux$fila<-row.names(datos) #jgp
aux$fila<-c(1:dim(datos)[1]) #jgp
aux$calle.carto1 <- paste0(aux$fila,"@@@",aux$calle.carto1) #jgp
aux.long <- length(aux$calle.carto1) #jgp
#close(fileConn) #jgp

#res1<-ldply(clusterApply(cl = cl, x=datos$calle.carto1, fun=busqueda.carto))
res1<-ldply(clusterApply(cl = cl, x=aux$calle.carto1, fun=busqueda.carto, long.ini=11 , long.total=aux.long)) #jgp
stopCluster(cl) 

aux$calle.carto2 <- paste0(aux$fila,"@@@",aux$calle.carto2) #jgp
aux.long <- length(aux$calle.carto2) #jgp

numWorkers <- detectCores()-1
cl <- makeCluster(numWorkers)
#res2<-ldply(clusterApply(cl = cl, x=datos$calle.carto2, fun=busqueda.carto))
res2<-ldply(clusterApply(cl = cl, x=aux$calle.carto2, fun=busqueda.carto, long.ini=33 , long.total=aux.long)) #jgp
stopCluster(cl) 

############################################################
# PROCESADO RESULTADO GEOCODIFICACION CARTOCIUDAD
############################################################

datos1<-cbind(datos,res1)

datos_carto<-datos1

datos_carto$lat_carto[is.na(datos_carto$lat_carto)]<-res2$lat[is.na(datos_carto$lat)]
datos_carto$long_carto[is.na(datos_carto$long_carto)]<-res2$long[is.na(datos_carto$long)]
datos_carto$geom[is.na(datos_carto$geom)]<-res2$geom[is.na(datos_carto$geom)]


datos_carto$score<-0
datos_carto$score[!is.na(datos_carto$geom)]<-1
datos_carto$score[datos_carto$state%in%999999]<-0
datos_carto$score[datos_carto$geom%in%"POINT"]<-2


rm(datos1,res1,res2)



########################################################################################################################
########################################################################################################################
#
#  5. GEOCODIFICACION CON BING
#
########################################################################################################################
########################################################################################################################


################################################
# CALLES A BUSCAR EN BING
################################################

calles_para_bing<-datos$DIRECCION_carto
calles_para_bing<-gsub(","," ",calles_para_bing)
calles_para_bing<-gsub("     "," ",calles_para_bing)
calles_para_bing<-gsub("    "," ",calles_para_bing)
calles_para_bing<-gsub("   "," ",calles_para_bing)
calles_para_bing<-gsub("  "," ",calles_para_bing)
calles_para_bing<-gsub("#","",calles_para_bing)
calles_para_bing<-gsub("?","",calles_para_bing)
calles_para_bing<-gsub("¿","",calles_para_bing)
calles_para_bing<-gsub("/","",calles_para_bing)
calles_para_bing<-gsub("}","",calles_para_bing)
calles_para_bing<-gsub("[{]","",calles_para_bing)
calles_para_bing<-gsub("@","",calles_para_bing)
calles_para_bing<-gsub("º","",calles_para_bing)
calles_para_bing<-gsub("ª","",calles_para_bing)


datos$calle.bing1<-NA 
if (exists("datos$CP_carto")) {
  datos$calle.bing1<-tolower(trim(paste(
  	trim(paste(datos$TIPO_VIA_carto,calles_para_bing,sep=" ")),
  	trim(datos$NUM_carto),
  	trim(datos$CP_carto),
  	trim(datos$MUNICIPIO_carto),
  	trim(datos$PROVINCIA_carto),
  	"Spain",
  	sep=",")))
} else {
  datos$calle.bing1<-tolower(trim(paste(
    trim(paste(datos$TIPO_VIA_carto,calles_para_bing,sep=" ")),
    trim(datos$NUM_carto),
    trim(datos$MUNICIPIO_carto),
    trim(datos$PROVINCIA_carto),
    "Spain",
    sep=",")))
}
datos$calle.bing1<-gsub(",999999,",",,",datos$calle.bing1)
datos$calle.bing1<-trim(gsub("999999","",datos$calle.bing1))
 
############################################################
# FUNCION PARA GEOCODIFICAR USANDO BING
############################################################

#bing_geo <- function(address){
 bing_geo <- function(address, long.total){  # jgp
   
    require("httr")
    require("rjson")

    aux <- address  #jgp
    s<-strsplit(aux,split="@@@") #jgp
    address<- s[[1]][[2]] #jgp
    fila<-s[[1]][[1]] #jgp      
    

    
	# Keys Grupo BIODAMA    	
    	BingMapsKey<-"MnXnuga2REfclQ8lyBvW~IrrQWqWpZE9xk9yCGjsZsw~AidVghOsp810nUgDWYeeCCdacpTyH5cLN58MbVHnhtcP6KqZFoyddP8iz5Tn9EQM" # Definitiva
    	#BingMapsKey<-"dK56fXJBKy5vF5ZWZIg3~i8_r6ZnExmgMdxwMo2pacw~Ar9o4v0ft1zJ5eIA3qhFc4UrIlp1fNvlawFZ5-A_gOA7VOQ8l9k4XH18bYub3zYG"  # Pruebas
    	BingMapsKey2<-"Ao6yngQpBmP5wjocVFrFRupaazay_DyWdKZVzZG4hE8PxYuurdT96TWsOjsu9HVc"
    
    	url      <- URLencode(paste0("http://dev.virtualearth.net/REST/v1/Locations?q=", address, "&maxResults=1&key=", BingMapsKey))
    
    	
   	    response <- GET(url,query=list(sensor="FALSE",address=address))
    	
    	json <- fromJSON(content(response,type="text"))
    
    
            
	sal1<-json$resourceSets[[1]]$resources[[1]]$address$countryRegion 
		
		
	#print(sal1)
    	
	inicio<-Sys.time()
	tiempo<-0
	while(sal1%in%c("Spain","España")==FALSE & tiempo<=10){
		
		# Keys Grupo BIODAMA
    	BingMapsKey<-"MnXnuga2REfclQ8lyBvW~IrrQWqWpZE9xk9yCGjsZsw~AidVghOsp810nUgDWYeeCCdacpTyH5cLN58MbVHnhtcP6KqZFoyddP8iz5Tn9EQM" # Definitiva
    	#BingMapsKey<-"dK56fXJBKy5vF5ZWZIg3~i8_r6ZnExmgMdxwMo2pacw~Ar9o4v0ft1zJ5eIA3qhFc4UrIlp1fNvlawFZ5-A_gOA7VOQ8l9k4XH18bYub3zYG"  # Pruebas
	    BingMapsKey2<-"Ao6yngQpBmP5wjocVFrFRupaazay_DyWdKZVzZG4hE8PxYuurdT96TWsOjsu9HVc"
	    url      <- URLencode(paste0("http://dev.virtualearth.net/REST/v1/Locations?q=", address, "&maxResults=1&key=", BingMapsKey))
	   	response <- GET(url,query=list(sensor="FALSE",address=address))
	    json <- fromJSON(content(response,type="text"))
    
		sal1<-json$resourceSets[[1]]$resources[[1]]$address$countryRegion 
	 
    	
		final<-Sys.time()
		tiempo<-difftime(final,inicio,units="secs")
		print(sal1)
		print(tiempo)
		
	}
    
	sal2<-json$resourceSets[[1]]$resources[[1]]$confidence 
	sal3<-paste(json$resourceSets[[1]]$resources[[1]]$matchCodes,collapse="_") 
	sal4<-json$statusCode 
	sal5<-json$statusDescription  
	sal6<-json$resourceSets[[1]]$resources[[1]]$point$coordinates
	mirar.longitud<-length(json$resourceSets[[1]]$resources[[1]]$geocodePoints)
	sal7<-json$resourceSets[[1]]$resources[[1]]$address$formattedAddress
	
	salida_type<-list()
	salida_coordenadas<-list()
	salida_calculationMethod<-list()
	salida_usageTypes<-list()
	   
    
	for(j in 1:mirar.longitud){
		
		salida_type[[j]]<-json$resourceSets[[1]]$resources[[1]]$geocodePoints[[j]]$type
		salida_coordenadas[[j]]<-json$resourceSets[[1]]$resources[[1]]$geocodePoints[[j]]$coordinates
		salida_calculationMethod[[j]]<-json$resourceSets[[1]]$resources[[1]]$geocodePoints[[j]]$calculationMethod
		salida_usageTypes[[j]]<-json$resourceSets[[1]]$resources[[1]]$geocodePoints[[j]]$usageTypes
	
     	}
     

	resultado<-data.frame(address=address,address_bing=sal7,lat=sal6[1],long=sal6[2],
		country=sal1,confidence=sal2,match_code=sal3,statusCode=sal4,Description=sal5,stringsAsFactors=F)
	
	
	
	for(p in 1:mirar.longitud){
		
		
		datos.meter<-data.frame(type=salida_type[[p]],
			coor_1=salida_coordenadas[[p]][1],coor_2=salida_coordenadas[[p]][2],
			calculationMethod=salida_calculationMethod[[p]],
			usageTypes=salida_usageTypes[[p]],stringsAsFactors=F)
		names(datos.meter)<-paste(names(datos.meter),"_",p,sep="")	
		resultado<-cbind(resultado,datos.meter)
		rm(datos.meter)
		
		
	}	
	
	
	resultado$country[resultado$country%in%"España"]<-"Spain"
	    
    
	
	if(resultado$country!="Spain"){
		
	    calle.nueva<-unlist(strsplit(address,split=","))	
	      
	    address_1<-calle.nueva
	    address_1[3]<-""
	    address_1<-paste(address_1,collapse=",")
	    address_2<-calle.nueva
	    address_2[4]<-""
	    address_2<-paste(address_2,collapse=",")
	  	   
        	    url1<- URLencode(paste0("http://dev.virtualearth.net/REST/v1/Locations?q=", address_1, "&maxResults=1&key=", BingMapsKey))
       	    response1 <- GET(url1,query=list(sensor="FALSE",address=address_1))
        	    json1 <- fromJSON(content(response1,type="text"))
	
	
        	    url2<- URLencode(paste0("http://dev.virtualearth.net/REST/v1/Locations?q=", address_2, "&maxResults=1&key=", BingMapsKey))
       	    response2 <- GET(url2,query=list(sensor="FALSE",address=address_2))
        	    json2 <- fromJSON(content(response2,type="text"))

	    salida.mirar<-list()
	    salida.mirar[[1]]<-json1 
	    salida.mirar[[2]]<-json2
	    countries<-c(json1$resourceSets[[1]]$resources[[1]]$address$countryRegion,json2$resourceSets[[1]]$resources[[1]]$address$countryRegion)
	    

	    if(sum(countries=="Spain")==0){
		    
	        	    url_prov<- URLencode(paste0("http://dev.virtualearth.net/REST/v1/Locations?q=", paste(calle.nueva[5],"Spain",sep=","), "&maxResults=1&key=", BingMapsKey))
	       	        response_prov <- GET(url_prov,query=list(sensor="FALSE",address= paste(calle.nueva[5],"Spain",sep=",")))
	        	    json_prov <- fromJSON(content(response_prov,type="text"))
	
		    resultado[1,]<-rep(NA,dim(resultado)[2])
		    resultado$country<-"Estimated by province"
		    resultado$lat<-json_prov$resourceSets[[1]]$resources[[1]]$point$coordinates[1]
		    resultado$long<-json_prov$resourceSets[[1]]$resources[[1]]$point$coordinates[2]
		    
	    }	
	    
	    
	    if(sum(countries=="Spain")>0){
	    	    
    
		    country.selec<-which(countries%in%"Spain")[1]    
		    json.selec<-salida.mirar[[country.selec]]
	    	    sal1<-paste(json.selec$resourceSets[[1]]$resources[[1]]$address$countryRegion,"REVISAR",
		    json.selec$resourceSets[[1]]$resources[[1]]$address$formattedAddress,sep="_")  
	    	    sal2<-json.selec$resourceSets[[1]]$resources[[1]]$confidence # High
	    	    sal3<-paste(json.selec$resourceSets[[1]]$resources[[1]]$matchCodes,collapse="_") 
	    	    sal4<-json.selec$statusCode 
	    	    sal5<-json.selec$statusDescription 
	    	    sal6<-json.selec$resourceSets[[1]]$resources[[1]]$point$coordinates
	    	    sal7<-json.selec$resourceSets[[1]]$resources[[1]]$address$formattedAddress
		    mirar.longitud<-length(json.selec$resourceSets[[1]]$resources[[1]]$geocodePoints)
	
	    	    salida_type<-list()
	    	    salida_coordenadas<-list()
	    	    salida_calculationMethod<-list()
	    	    salida_usageTypes<-list()
    
    
	    	    for(j in 1:mirar.longitud){
		
	    		salida_type[[j]]<-json.selec$resourceSets[[1]]$resources[[1]]$geocodePoints[[j]]$type
	    		salida_coordenadas[[j]]<-json.selec$resourceSets[[1]]$resources[[1]]$geocodePoints[[j]]$coordinates
	    		salida_calculationMethod[[j]]<-json.selec$resourceSets[[1]]$resources[[1]]$geocodePoints[[j]]$calculationMethod
	    		salida_usageTypes[[j]]<-json.selec$resourceSets[[1]]$resources[[1]]$geocodePoints[[j]]$usageTypes
	
	         	     }
     

	    	     resultado<-data.frame(address=address,address_bing=sal7,lat=sal6[1],long=sal6[2],
	    		country=sal1,confidence=sal2,match_code=sal3,statusCode=sal4,Description=sal5,stringsAsFactors=F)
	
	    	     for(p in 1:mirar.longitud){
		
		
	    		datos.meter<-data.frame(type=salida_type[[p]],
	    			coor_1=salida_coordenadas[[p]][1],coor_2=salida_coordenadas[[p]][2],
	    			calculationMethod=salida_calculationMethod[[p]],
	    			usageTypes=salida_usageTypes[[p]],stringsAsFactors=F)
	    		names(datos.meter)<-paste(names(datos.meter),"_",p,sep="")	
	    		resultado<-cbind(resultado,datos.meter)
	    		rm(datos.meter)
		
		
	    	      }
	
	    	     resultado
	
	    }		
		
	 	
	}
	
 
	return(resultado)
        
 }	


############################################################
# GEOCODIFICACION BING
############################################################

numWorkers <- detectCores()-1
cl <- makeCluster(numWorkers)
aux<-datos #jgp
#aux$fila<-row.names(datos) #jgp
aux$fila<-c(1:dim(datos)[1]) #jgp
aux$calle.bing1 <- paste0(aux$fila,"@@@",aux$calle.bing1) #jgp
aux.long <- length(aux$calle.bing1) #jgp
#close(fileConn) #jgp

result<-clusterApply(cl = cl, x=aux$calle.bing1, fun= bing_geo, long.total=aux.long) #jgp
#result<-clusterApply(cl = cl, x=datos$calle.bing1, fun= bing_geo)
stopCluster(cl)   
    
  
############################################################
# PROCESADO RESULTADO GEOCODIFICACION BING
############################################################

mirar <- as.data.frame(t(stringi::stri_list2matrix(result)),stringsAsFactors=F)
posibles.nombres<-c("address","address_bing","lat_bing","long_bing","country","confidence","match_code","statusCode","Description",
"type_a","lat_1_bing_a","long_1_bing_a","calculationMethod_a","usageTypes_a",
"type_b","lat_1_bing_b","long_1_bing_b","calculationMethod_b","usageTypes_b",
"type_c","lat_1_bing_c","long_1_bing_c","calculationMethod_c","usageTypes_c",
"type_d","lat_1_bing_d","long_1_bing_d","calculationMethod_d","usageTypes_d",
"type_e","lat_1_bing_e","long_1_bing_e","calculationMethod_e","usageTypes_e")
posibles.nombres.meter<-posibles.nombres[1:dim(mirar)[2]]
names(mirar)<-posibles.nombres.meter

rm(result,posibles.nombres,posibles.nombres.meter)

mirar$confidence_new<-0
mirar$confidence_new[mirar$confidence%in%"High"]<-2
mirar$confidence_new[mirar$confidence%in%"Medium"]<-1
mirar$confidence_new[mirar$confidence%in%"Low"]<-0

mirar$match_code.new<-0
mirar$match_code.new[mirar$match_code%in%"Good"]<-1

mirar$statusCode.new<-0
mirar$statusCode.new[mirar$statusCode%in%"200"]<-1

mirar$Description.new<-0
mirar$Description.new[mirar$Description%in%"OK"]<-1

mirar$country.new<-0
mirar$country.new[mirar$country%in%"Spain"]<-2
mirar$country.new[grep("Spain_REVISAR",mirar$country)]<-1

mirar$score<-c(mirar$confidence_new + mirar$match_code.new + mirar$statusCode.new + mirar$Description.new + mirar$country.new)

mirar$score[mirar$country%in%"Estimated by province"]<-0

datos_bing<-cbind(datos[,c(1:11)],mirar)

rm(mirar)



########################################################################################################################
########################################################################################################################
#
#  6. UNION GEOCODIFICACION CON PROCESO DE VALIDACION DEL ESTUDIO PILOTO VICA
#
########################################################################################################################
########################################################################################################################

datos_finales<-datos[,names(datos.orig)]


datos_meter_carto<-datos_carto[,c("lat_carto","long_carto","geom","stateMsg","portalNumber","province","score")]

names(datos_meter_carto)[-which(names(datos_meter_carto)%in%c("lat_carto","long_carto"))]<-paste(names(datos_meter_carto)[-which(names(datos_meter_carto)%in%c("lat_carto","long_carto"))],"_carto",sep="")


datos_meter_bing<-datos_bing[,c("lat_bing","long_bing",          
"country","confidence","match_code","statusCode","Description",        
"type_a","lat_1_bing_a","long_1_bing_a","calculationMethod_a","usageTypes_a",       
"type_b","lat_1_bing_b","long_1_bing_b","calculationMethod_b","usageTypes_b","score")]
names(datos_meter_bing)[c(3:8,11:13,16:18)]<-paste(names(datos_meter_bing)[c(3:8,11:13,16:18)],"_bing",sep="")

datos_finales<-cbind(datos_finales,datos_meter_carto)
datos_finales<-cbind(datos_finales,datos_meter_bing)

datos_finales$"score_carto.new"<-datos_finales$"score_carto" #

datos_finales$"score_bing.new"<-NA
datos_finales$"score_bing.new"[datos_finales$"score_bing"%in%7]<-1
datos_finales$"score_bing.new"[datos_finales$"score_bing"<7]<-0

datos_finales$"comb_score"<-paste(as.character(datos_finales$"score_carto.new"),as.character(datos_finales$"score_bing.new"),sep="_")

datos_finales$"combinaciones"<-NA

########################################################################################################################
# Asignamos un codigo a la combinacion para que el orden sea el adecuado conforme a los resultados del estudio piloto
########################################################################################################################
datos_finales$combinaciones[datos_finales$"comb_score"%in%c("2_1","2_0")]<-3
datos_finales$combinaciones[datos_finales$"comb_score"%in%c("1_1","0_1")]<-2
datos_finales$combinaciones[datos_finales$"comb_score"%in%c("1_0")]<-1
datos_finales$combinaciones[datos_finales$"comb_score"%in%c("0_0")]<-0


########################################################################################################################
# Resultados del estudio piloto (copiados de los resultados de la validacion en el estudio piloto)
########################################################################################################################

cal.comb<-data.frame(combinaciones=c(3:0),selection=c("CARTO","BING","CARTO","BING"),stringsAsFactors=F)

datos_finales$"indice_orden"<-seq(1,dim(datos_finales)[1],1)
datos_finales<-merge(datos_finales,cal.comb,by="combinaciones")
datos_finales<-datos_finales[order(datos_finales$"indice_orden"),]
datos_finales<-datos_finales[,-which(names(datos_finales)%in%"indice_orden")]

datos_finales$"lat_geo_ciber"<-NA
datos_finales$"long_geo_ciber"<-NA

datos_finales$"lat_geo_ciber"[datos_finales$"selection"%in%"BING"]<-datos_finales$"lat_bing"[datos_finales$"selection"%in%"BING"]
datos_finales$"long_geo_ciber"[datos_finales$"selection"%in%"BING"]<-datos_finales$"long_bing"[datos_finales$"selection"%in%"BING"]

datos_finales$"lat_geo_ciber"[datos_finales$"selection"%in%"CARTO"]<-datos_finales$"lat_carto"[datos_finales$"selection"%in%"CARTO"]
datos_finales$"long_geo_ciber"[datos_finales$"selection"%in%"CARTO"]<-datos_finales$"long_carto"[datos_finales$"selection"%in%"CARTO"]


res_geo_ciber<-datos_finales[,c(names(datos.orig),"lat_geo_ciber","long_geo_ciber","combinaciones")]
names(res_geo_ciber)[dim(res_geo_ciber)[2]]<-"geo_ciber_score"

res_geo_ciber_adicional<-datos_finales[,c(names(datos.orig),"lat_carto","long_carto","geom_carto","stateMsg_carto","portalNumber_carto", "province_carto",
"lat_bing","long_bing","country_bing","confidence_bing","match_code_bing","statusCode_bing","Description_bing",         
"type_a_bing","lat_1_bing_a","long_1_bing_a","calculationMethod_a_bing","usageTypes_a_bing","type_b_bing","lat_1_bing_b","long_1_bing_b",
"comb_score","combinaciones")]


rm(list=ls()[-which(ls()%in%c("res_geo_ciber","res_geo_ciber_adicional","datos.orig", "fileConn","ficheroentrada"))])
gc()

########################################################################################################################
########################################################################################################################
#
#  7. EXPORTACION DE RESULTADOS
#
########################################################################################################################
########################################################################################################################

res_geo_ciber<-cbind(datos.orig,res_geo_ciber[,-c(1:dim(datos.orig)[2])])

date_analysis<-Sys.Date()

#openxlsx::write.xlsx(res_geo_ciber,file=paste("/Users/tatum/Desktop/resultado_geo_ciber_",date_analysis,".xlsx",sep=""))
openxlsx::write.xlsx(res_geo_ciber,file=paste("./result/",date_analysis,"_resultado_geo_ciber",".xlsx",sep=""),overwrite = TRUE)

########################################################################################################################
# opcionales
########################################################################################################################

#openxlsx::write.xlsx(res_geo_ciber_adicional,file=paste("/Users/tatum/Desktop/resultado_geo_ciber_adicional_",date_analysis,".xlsx",sep=""))
## jgp - cojo del fichero, si se ha marcado el check de 'Generar datos adicionales'
if (ficheroentrada[2]=="DATOSADICIONALES_YES") {
  openxlsx::write.xlsx(res_geo_ciber_adicional,file=paste("./result/",date_analysis,"_resultado_geo_ciber_adicional",".xlsx",sep=""),overwrite = TRUE)
}

registros<-res_geo_ciber
registros_data<-as.data.frame(registros)
registros$lat_geo_ciber<-as.numeric(registros$lat_geo_ciber)
registros$long_geo_ciber<-as.numeric(registros$long_geo_ciber)
coordinates(registros) <- c("long_geo_ciber","lat_geo_ciber")
proj4string(registros)<-CRS("+proj=longlat +datum=WGS84") 
description <- paste("<b>",names(datos.orig)[1],":</b>", as.character(registros_data[[names(datos.orig)[1]]]), 
"<br><b>",names(datos.orig)[2],":</b>", as.character(registros_data[[names(datos.orig)[2]]]),sep="")
for(i in 3:length(names(datos.orig))){
	
	description<-paste(description,"<br><b>",names(datos.orig)[i],":</b>", as.character(registros_data[[names(datos.orig)[i]]]),sep="")
	
}
description<-paste(description,
	"<br><b>geo_ciber_score:</b>", registros_data$geo_ciber_score,sep="")
#registros$id<-seq(1,dim(registros)[1],1)
#maptools::kmlPoints(registros, kmlfile = paste("/Users/tatum/Desktop/resultado_geo_ciber_",date_analysis,".kml",sep=""),name = registros$"ID",
#description=description)

if (ficheroentrada[3]=="KML_YES") {
  maptools::kmlPoints(registros, kmlfile = paste("./result/",date_analysis,"_resultado_geo_ciber",".kml",sep=""),name = registros$"ID",
                      description=description)
}

rm(list=ls())
gc()
}

#jgp - para controlar si hay un error en cualquier parte del script
tryCatch( scriptEntero()
, error = function(e){ # jgp - para que el java compruebe si ha habido algún error en el proceso
   	i.progress <- -1 # jgp
    fileConnErr<-file("./progress/progress.txt") #jgp
  	writeLines(as.character(i.progress), fileConnErr) # jgp
	close(fileConnErr) #jgp
	#q() #jgp
  }
) #fin del tryCatch

close(fileConn) #jgp


#q() #jgp

##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################