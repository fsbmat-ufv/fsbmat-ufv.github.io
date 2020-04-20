# Carregando os pacotes----

library(maptools)     
library(spdep)          
library(cartography)    
library(tmap)           
library(leaflet)        
library(dplyr)
library(rgdal)
library(dplyr)
library(RColorBrewer) 
library(data.table)
shp <- readOGR("Mapa\\.", "BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")
data <- fread("covid19.csv", encoding = "UTF-8")
data$deaths[is.na(data$deaths)] <- 0
data$date <- as.Date(data$date)
data <- data[order(data$date) , ]
data$deaths_100k_inhabitants <- ((data$deaths)/(data$estimated_population_2019))*100000

data <- data %>%
  dplyr::filter(place_type == "state") %>%
  dplyr::group_by(state,date, confirmed,deaths) %>% 
  select(date, state, confirmed, deaths, estimated_population_2019,confirmed_per_100k_inhabitants, city_ibge_code)
names(data) <- c("date", "state", "confirmed", "deaths", "Pop", "quant_confirmed_100k_hab", "ibge" )

# pg <- read.csv("Dados\\ClassificacaoPontosCorridos.csv", header=T,sep=";")
# 
# pg <- pg %>% group_by(Estado) %>% mutate(cumsum = cumsum(PG))
# pg <- pg %>%
#   group_by(Estado) %>%
#   summarise(Score= max(Score))
# 
# pg <- as.data.frame(pg)
# 
# 
#pg <- merge(data,ibge, by.x = "Estado", by.y = "UF")


brasileiropg <- merge(shp, data, by.x = "CD_GEOCUF", by.y = "ibge", duplicateGeoms=TRUE)
proj4string(brasileiropg) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

Encoding(brasileiropg$NM_ESTADO) <- "UTF-8"

#brasileiropg$Score[is.na(brasileiropg$Score)] <- 0

display.brewer.all()

pal <- colorBin("Reds",domain = NULL,n=5) #cores do mapa


state_popup <- paste0("<strong>Estado: </strong>", 
                      brasileiropg$NM_ESTADO, 
                      "<br><strong>Confirmados: </strong>", 
                      brasileiropg$confirmed)
leaflet(data = brasileiropg) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(brasileiropg$confirmed), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup) %>%
  addLegend("bottomright", pal = pal, values = ~brasileiropg$confirmed,
            title = "Número de Confirmados",
            opacity = 1)
