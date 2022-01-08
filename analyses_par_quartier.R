#offre et demande
library(leaflet)
library(sp)
library(sf)
library(rgdal)
library(leaflet)
library(tidyverse)
library(htmltools)
library(htmlwidgets)
library(data.table)
library(R.utils)
library(RColorBrewer)

# Import des listings
data_listings_2021 <- data.table::fread("http://data.insideairbnb.com/france/ile-de-france/paris/2021-10-09/data/listings.csv.gz",encoding="UTF-8")
dh <- data_listings_2021

#on remet en forme le dataset
dh <- dh %>%
      filter(accommodates!=0,price!="$0.00") %>%
      rename('c_ar'=neighbourhood_cleansed)%>%
      select(id,name,property_type,room_type,c_ar,latitude,longitude,price) %>%
      rename(x=latitude,y=longitude)

dh$price<-gsub("[$]","",dh$price)
dh$price<-gsub("[,]","",dh$price)
dh <- dh%>%mutate(prix_euro=round(as.numeric(price)*0.84,2))

# Import des donnÈes gÈographiques des quartiers et arrondissements
quartier<-readOGR(dsn = "./quartier_paris",layer="quartier_paris",encoding="UTF-8",use_iconv=TRUE)
arrondissements<-readOGR(dsn = "./arrondissements",layer="arrondissements",encoding="UTF-8",use_iconv=TRUE)
quartier_sf<-st_as_sf(quartier)

dh_sf <- st_as_sf(dh, coords = c('y', 'x'), crs = st_crs(quartier_sf))


#obtention des quartiers des locations
dh_quartier <- dh_sf %>%
  mutate(
    intersection = as.integer(st_intersects(geometry, quartier_sf)),
    quartier = if_else(is.na(intersection), '', quartier_sf$l_qu[intersection])
  ) 


#obtention des quartiers auxquels appartiennent les locations
dh_quartier <- dh_sf %>%
  mutate(
    intersection = as.integer(st_intersects(geometry, quartier_sf)),
    quartier = if_else(is.na(intersection), '', quartier_sf$l_qu[intersection])
  ) 

#Prix moyen par quartier
dh_prix_moyen<-dh_quartier%>%
  filter(quartier!='')%>%
  group_by(quartier)%>%
  summarise(prix_moyen=round(mean(prix_euro,na.rm=TRUE),2))

#Nombre de listings par quartier
dh_nb_listings<-dh_quartier%>%
  filter(quartier!='')%>%
  count(quartier)


#Moficiation des SpatialPolygonsDataframe
quartiern <- quartier
quartier@data<-left_join(quartier@data,dh_prix_moyen,by=c("l_qu"="quartier"))
quartiern@data<-left_join(quartiern@data,dh_nb_listings,by=c("l_qu"="quartier"))

qpal <- colorNumeric(palette = "YlOrRd",domain = quartier@data$prix_moyen)

#carte des prix moyen par quartier
map1<-leaflet(options = leafletOptions(dragging = TRUE,minZoom = 11,maxZoom = 18)) %>%
  addProviderTiles("CartoDB") %>%
  addPolylines(data=arrondissements,color="#798081")%>%
  addPolygons(data=quartier,
              fillColor = ~qpal(prix_moyen),
              weight = 1,
              color="black",
              opacity=1,
              fillOpacity = 0.7,
              dashArray="5",
              label = ~paste0(c_ar,"Ëme",l_qu," : ",prix_moyen," ‚Ç¨"),
              highlight = highlightOptions(weight = 2,color = "black",bringToFront = TRUE,dashArray="5")) %>%
  addLegend(data=quartier,position=c("topright"),values=~prix_moyen,pal=qpal,title="Prix moyen par jour",opacity=0.8,
            labFormat=labelFormat(suffix="???"))%>%
  setView(lng = 2.348, lat = 48.862725, zoom = 8) 

map1

#exporter map1 en html
saveWidget(map1, file="map1_prix_moyen_quartier.html")




#Nombre de listings pas quartier

#On ajoute les lieux d'attracttion : salles de spectacle, Ètablissements du supÈrieur et grands states


# Charger le dataset des salles de spectacle
spectacles <- read.csv("lieux_spectacles.csv",sep=";",encoding="UTF-8")

#DÈfinir les coordonnÈes gÈographiques en latitude et longitude
spectacles <- spectacles %>%  separate(col = "WGS84", into = c("latitude","longitude"), sep = ",")

spectacles$latitude<-as.numeric(spectacles$latitude)
spectacles$longitude<-as.numeric(spectacles$longitude)


#Ajout de salles absentes

Nom<-c("Palais des Sports","Le Grand Rex","Maison de la Mutualit√©")
c<-colnames(spectacles)
for (i in 2:10){
  assign(c[i],c(NA,NA,NA))
}
latitude<-c(48.83986866303423,48.87113099041575,48.8488753614441)
longitude<-c(2.2917182855130367,2.3486919069292864,2.350677913655472)

salles_absentes<-data.frame(mget(c))

spectacles<-rbind.data.frame(spectacles,salles_absentes)

grandes_salles<-spectacles%>%
  select(Nom,latitude,longitude)%>%
  filter(Nom %in% c("Palais Omnisport de Paris Bercy",
                    "Parc de la Villette",
                    "Palais des Sports",
                    "Palais des Congr√®s",
                    "Le Grand Rex",
                    "Salle Pleyel",
                    "Cit√© de la Musique",
                    "Le Casino de Paris",
                    "L'Olympia",
                    "Maison de la Mutualit√©",
                    "Les Folies Berg√®re",
                    "Th√©√¢tre Mogador",
                    "Le Bataclan",
                    "La Cigale",
                    "√âlys√©e Montmartre"))

# Dataframe des grands stades
noms<-c("Stade Charl√©ty","Stade Jean Bouin","Stade Rolland Garros","Parc des Princes")
la<-c(48.818808774723216,48.84392728401238,48.84611782841797,48.841583862823406 )
lo<-c(2.3469981631511034,2.2529888575984147,2.2535953422549913, 2.2530701557471384)

stades<-data.frame(noms,la,lo)
colnames(stades)<-c('Nom',"latitude","longitude")

#grandes Ècoles
ecoles<-readOGR(dsn = "fr-esr-parcoursup",
                layer="fr-esr-parcoursup",
                encoding="UTF-8",use_iconv=TRUE)

ecolesp<-remove.duplicates(ecoles,zero=0.2)

ecoles_data<-st_as_sf(ecolesp)

#filtre sur les Ècoles situÈes ‡ Paris
ecoles_data<-ecoles_data%>%rename('academie'=acad_mies,"libelle"=g_ea_lib_vx)%>%filter(academie=="Paris")%>%select(academie,libelle)


#DÈfinition des icÙnes sur la carte

spectaclesIcon <- makeIcon(
  iconUrl = "https://www.flaticon.com/fr/premium-icon/icons/svg/1175/1175400.svg",
  iconWidth = 30, iconHeight = 30,
  # iconAnchorX = 22, iconAnchorY = 94,
  # shadowWidth = 50, shadowHeight = 64,
  # shadowAnchorX = 40, shadowAnchorY = 62
)

ecoleIcon<-makeIcon(
  iconUrl = "school-outline.svg",
  iconWidth = 30, iconHeight = 30,
  # iconAnchorX = 22, iconAnchorY = 94,
  # shadowWidth = 50, shadowHeight = 64,
  # shadowAnchorX = 40, shadowAnchorY = 62
)


stadesIcon <- makeIcon(
  iconUrl = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSX7OPjZMDnxiEuSTWv76Q74nuPY1QbcVYNjQ&usqp=CAU",
  iconWidth = 30, iconHeight = 30,
  # iconAnchorX = 22, iconAnchorY = 94,
  # shadowWidth = 50, shadowHeight = 64,
  # shadowAnchorX = 40, shadowAnchorY = 62
)





qpal <- colorNumeric(palette = "Blues",domain = quartiern@data$n)

#carte du nombre de listings par quartier avec les attractions
map2<-leaflet(options = leafletOptions(dragging = TRUE,minZoom = 11,maxZoom = 18)) %>% 
  addProviderTiles("CartoDB") %>% 
  addPolylines(data=arrondissements,color="#798081")%>%
  addPolygons(data=quartiern,
              fillColor = ~qpal(n),
              weight = 1,
              color="black",
              opacity=1,
              fillOpacity = 0.7,
              dashArray="5",
              label = ~paste0(c_ar,"Ëme",l_qu," : ",n),
              highlight = highlightOptions(weight = 2,color = "black",bringToFront = TRUE,dashArray="5"))%>%
  addMarkers(data =stades, label = ~htmlEscape(stades$Nom),group = "Grands stades",
             popup=~paste0(stades$Nom),icon=stadesIcon)%>%
  addMarkers(data=grandes_salles,label=~grandes_salles$Nom,group="Grandes salles",icon=spectaclesIcon)%>%
  addMarkers(data=ecoles_data,label=~ecoles_data$libelle,group="Etablissements du supÈrieur",
             icon=ecoleIcon)%>%
  addLayersControl(overlayGroups= c("Grandes salles","Grands stades","Etablissements du supÈrieur"))%>%
  setView(lng = 2.348, lat = 48.862725, zoom = 10)

map2

#exporter map2 en html
saveWidget(map2, file="map2nb_listings_quartier.html")


#On souhaite trouver les taux d'occupation moyen par quartier
data_calendar_2021<- data.table::fread("http://data.insideairbnb.com/france/ile-de-france/paris/2021-10-09/data/calendar.csv.gz",encoding="UTF-8")

date_1 <- as.Date(min(data_calendar_2021$date))
date_2 <- as.Date(max(data_calendar_2021$date))
days <- length(seq(from = date_1, to = date_2, by = 'day')) - 1

#Calcul du taux d'occupation de chaque logement
tx_ocup<-data_calendar_2021%>%filter(available=="f")%>%count(listing_id)%>%mutate(tx_oc_pourcent=n*100/days)

#Jointure entre les listings et leurs taux d'occupation
df<-data_listings_2021%>%select(id,name,room_type,property_type,longitude,latitude,neighbourhood_cleansed,description,availability_365)
listingsto<-inner_join(df,tx_ocup,by=c("id"="listing_id"))
listingsto<-listingsto%>%rename(x=latitude,y=longitude)

listingsto_sf <- st_as_sf(listingsto, coords = c('y', 'x'), crs = st_crs(quartier_sf))
dlto_quartier <- listingsto_sf %>%
  mutate(
    intersection = as.integer(st_intersects(geometry, quartier_sf)),
    quartier = if_else(is.na(intersection), '', quartier_sf$l_qu[intersection])
  ) 


dlto_quartier<-dlto_quartier%>%
  filter(quartier!="")%>%
  group_by(quartier) %>%
  summarise(tx_moyen=mean(tx_oc_pourcent))

quartier@data<-left_join(quartier@data,dlto_quartier,by=c("l_qu"="quartier"))


qpal <- colorNumeric(palette = "YlOrRd",domain = quartier@data$n)



m3<-leaflet(options = leafletOptions(dragging = TRUE,minZoom = 11,maxZoom = 18))%>%
  addProviderTiles("CartoDB")%>%
  addPolygons(data=arrondissements,color='green') %>%
  addPolygons(data=quartier,fillColor = ~qpal(tx_moyen),weight = 1,color="black",opacity=1,fillOpacity = 0.7,
              dashArray="5",
              label = ~htmlEscape(paste0(c_ar,"Ëme",l_qu," : ",tx_moyen)),
              highlight = highlightOptions(weight = 2,color = "black",bringToFront = TRUE,dashArray="5"))%>%
  addLegend(data=quartier,values=~tx_moyen,pal=qpal,title="Taux moyen de <br> non disponibilitÈ <br> des locations <br>",
            opacity = 0.7,labFormat=labelFormat(suffix="%")) %>%
  setView(lng = 2.348, lat = 48.862725, zoom = 10) 

m3
saveWidget(m3, file="map3_taux_non_dispo_quartier.html")


