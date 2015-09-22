setwd("Carte scolaire/")
library(rgdal)
library(spatstat)
library(rgeos)
library(sp)
library(deldir)
source("utils.R")

## 1. Recharche et préparation des données
etablissements<-read.csv("etablissements_Paris.csv")
maternelles<-read.csv("carte_scolaire_maternelles.csv")
primaires<-read.csv("carte_scolaire_primaires.csv")
colleges<-read.csv("carte_scolaire_colleges.csv")
adresses<-read.csv("adresses_paris_codees.csv")
adresses<-adresses[,c("C_AR","N_SQ_AD","Geometry.X.Y")]
adresses$latitude<-as.numeric(gsub(pattern = "^(.*), (.*)$",x = adresses$Geometry.X.Y,replacement = "\\1"))
adresses$longitude<-as.numeric(gsub(pattern = "^(.*), (.*)$",x = adresses$Geometry.X.Y,replacement = "\\2"))
adresses$Geometry.X.Y<-NULL

## 2. Lecture du fonds de carte de Paris, qui servira de fenêtrage par Iris
# Lecture du fichier des IRIS
contours_iris <- readOGR(dsn="CONTOURS-IRIS/1_DONNEES_LIVRAISON_2014-08-00168/CONTOURS-IRIS_1-0_SHP_LAMB93_D75-2013",layer="CONTOURS-IRIS75")
# Lecture du fichier des contours d'arrondissement
contours_arrondissements <- readOGR(dsn="../Géographie/COMMUNE",layer="COMMUNE")
contours_arrondissements<-contours_arrondissements[contours_arrondissements$CODE_DEPT==75,]
contours_arrondissements$arrondissement<-as.numeric(substr(contours_arrondissements$INSEE_COM,start = 4,stop = 5))
projection_utilisee <-contours_iris@proj4string
contours_arrondissements<-spTransform(x = contours_arrondissements, CRSobj = projection_utilisee)
contours_arrondissements<-contours_arrondissements[order(contours_arrondissements$INSEE_COM),]

# Transformation de la base adresse avec la même projection que les IRIS
coordinates(adresses) <- c("longitude", "latitude")
proj4string(adresses) <- CRS("+proj=longlat")
adresses <- spTransform(adresses, projection_utilisee)

# les contours des IRIS 
fenetre_iris <- lapply(contours_iris@polygons, function(x) owin(poly=apply(x@Polygons[[1]]@coords,2,rev)))
# le contour de Paris
fenetre_arrondissements<-lapply(contours_arrondissements@polygons, function(x) owin(poly=apply(x@Polygons[[1]]@coords,2,rev)))
fenetre_paris <- owin(poly=lapply(contours_arrondissements@polygons, function(x) apply(x@Polygons[[1]]@coords,2,rev)))
# NB : il ne faut surtout pas chercher à comprendre le code ci-dessous : soit je suis un pied, soit
# les structures de données du package spatstat ne sont pas idéales...
# le contour des arrondissements
fenetre_paris_iris <- owin(poly=lapply(contours_iris@polygons, function(x) apply(x@Polygons[[1]]@coords,2,rev)))

# codage des adresses dans les IRIS
numeros_IRIS<-contours_iris$DCOMIRIS
for (i in 1:length(contours_iris)) {
  adresses@data[inside.owin(adresses@coords[,1], adresses@coords[,2], w = fenetre_iris[[i]]),"DCOMIRIS"]<-numeros_IRIS[i]
}

# on a un problème : un millier d'adresses valables (dans la carte scolaire) sont hors des contours 
# des arrondissements dont ils font normalemenet partie. Or il est probable que cette mauvaise localisation
# se retrouve au niveau des IRIS, et fausse le voronoï de la carte scolaire par arrondissement.
# on crée donc une enveloppe large qui inclue les points de l'arrondissement hors périmètre, pour calculer
# les voronoï. A posteriori, on les restreindra aux contours des arrondissements.

# quelles adresses ne sont pas dans les contours d'arrondissement
# on prend une marge
fenetre_arrondissements_larges<-lapply(1:20,function(i) 
  gBuffer(owin2SP(fenetre_arrondissements[[i]],projection_utilisee),width = 400))
adresses<-adresses[adresses@data$C_AR!=0,]
hors_arrondissements<-adresses[sapply(
  1:nrow(adresses@data),
  function(i) 
    !inside.owin(adresses@coords[i,1],
                 adresses@coords[i,2],
                 fenetre_arrondissements_larges[[adresses@data[i,"C_AR"]]])),]
  
## 3. Géocodage (lat,long) des cartes scolaires
maternelles.sp<-unique(merge(x=maternelles[2:3],y=adresses,by.x = "N_SQ_AD", by.y="N_SQ_AD"))
coordinates(maternelles.sp) <- c("longitude", "latitude")
proj4string(maternelles.sp)<-projection_utilisee

primaires.sp<-unique(merge(x=primaires[2:3],y=adresses,by.x = "N_SQ_AD", by.y="N_SQ_AD"))
coordinates(primaires.sp) <- c("longitude", "latitude")
proj4string(primaires.sp)<-projection_utilisee

colleges.sp<-unique(merge(x=colleges[2:3],y=adresses,by.x = "N_SQ_AD", by.y="N_SQ_AD"))
coordinates(colleges.sp) <- c("longitude", "latitude")
proj4string(colleges.sp)<-projection_utilisee

etablissements.sp<-unique(merge(x=etablissements[3:9],y=adresses,by.x = "N_SQ_AD", by.y="N_SQ_AD"))
coordinates(etablissements.sp) <- c("longitude", "latitude")
proj4string(etablissements.sp)<-projection_utilisee

## 4. Calcul des Voronoï
# Soit on dispose de temps ou  de puissance de calcul, soit non.
# L'algorithme de calcul des Voronoï ne semble pas être linéaire (et ce n'est pas étonnant),
# donc il y a deux stratégies : 1. subdiviser par arrondissement, pour garder un temps de calcul
# raisonnable, mais risquer des effets de bords en limite d'arrondissement (même si en théorie
# la carte scolaire ne doit pas chevaucher plusieurs arrondissements), soit 2. faire un calcul
# sur tout Paris.
# 1. Calcul par arrondissement
# maternelles_voronoi<-lapply(1:20,function(i) dirichlet(
#  as.ppp(X = maternelles.sp[maternelles.sp@data$C_AR==i,]@coords,
#         W = fenetre_arrondissements_larges[[i]])))
# primaires_voronoi<-lapply(1:20,function(i) dirichlet(
#   as.ppp(X = primaires.sp[primaires.sp@data$C_AR==i,]@coords,
#          W = fenetre_arrondissements_larges[[i]])))
# colleges_voronoi<-lapply(1:20,function(i) dirichlet(
#   as.ppp(X = colleges.sp[colleges.sp@data$C_AR==i,]@coords,
#          W = fenetre_arrondissements_larges[[i]])))
# etablissements_voronoi<-lapply(1:20,function(i) dirichlet(
#   as.ppp(X = etablissements.sp[etablissements.sp@data$C_AR==i,]@coords,
#          W = fenetre_arrondissements_larges[[i]])))
# 
# save(maternelles_voronoi,primaires_voronoi,colleges_voronoi,etablissements_voronoi,file="voronoi_arrond.RData")

# 4. calcul des Voronoï sur tout Paris
fenetre_paris_elargie <-gBuffer(owin2SP(fenetre_paris,projection_utilisee),width = 400)
fenetre_paris_elargie <- owin(poly=apply(fenetre_paris_elargie@polygons[[1]]@Polygons[[1]]@coords,2,rev))
maternelles.duplicated<-duplicatedxy(maternelles.sp@coords[,1],maternelles.sp@coords[,2])
maternelles_voronoi_paris<-dirichlet(as.ppp(X = maternelles.sp@coords,W = fenetre_paris_elargie))
primaires_voronoi_paris<-dirichlet(as.ppp(X = primaires.sp@coords,W = fenetre_paris_elargie))
colleges_voronoi_paris<-dirichlet(as.ppp(X = colleges.sp@coords,W = fenetre_paris_elargie))
etablissements_voronoi_paris<-dirichlet(as.ppp(X = etablissements.sp@coords,W = fenetre_paris_elargie))

save(maternelles_voronoi_paris,primaires_voronoi_paris,colleges_voronoi_paris,etablissements_voronoi_paris,file="voronoi_paris.RData")

# 3. fusion des Voronoïs par établissement
# Attention: certains points sont sur deux cartes scolaires, en tout cas pour les maternelles (résultat
# des warnings obtenus lors du calcul. Le résultat des fusions donnera donc des zones arrières d'établissement
# qui se recoupent.
etablissements.nombre<-max(primaires.sp$primaire)
primaires.voronoi.unifies<-lapply(1:etablissements.nombre,function(i) owin(plot=NULL))
for (i in 1:nrow(primaires.sp)) {
  etabl<-primaires.sp@data[i,"primaire"]
  primaires.voronoi.unifies[[etabl]]<-union.owin(primaires.voronoi.unifies[[etabl]],primaires_voronoi_paris$tiles[[i]])
}

etablissements.nombre<-max(colleges.sp$college)
colleges.voronoi.unifies<-lapply(1:etablissements.nombre,function(i) owin(poly=NULL))
for (i in 1:nrow(colleges.sp)) {
  etabl<-colleges.sp@data[i,"college"]
  colleges.voronoi.unifies[[etabl]]<-union.owin(colleges.voronoi.unifies[[etabl]],colleges_voronoi_paris$tiles[[i]])
}

maternelles.duplicated<-duplicatedxy(maternelles.sp@coords[,1],maternelles.sp@coords[,2])
maternelle.surjection<-1:maternelles_voronoi_paris$n
for (i in 1:nrow(maternelles.sp)) if (maternelles.duplicated[i]) {
  maternelle.surjection[i]<-maternelle.surjection[match(x = maternelles.sp@coords[i,],table = maternelles.sp@coords[!maternelles.duplicated,])[[1]]]
}

etablissements.nombre<-max(maternelles.sp$maternelle)
maternelles.voronoi.unifies<-lapply(1:etablissements.nombre,function(i) owin(plot=NULL))
for (i in 1:nrow(maternelles.sp)) {
  etabl<-maternelles.sp@data[i,"maternelle"]
  v<-maternelles_voronoi_paris$tiles[[maternelle.surjection[i]]]
  maternelles.voronoi.unifies[[etabl]]<-union.owin(maternelles.voronoi.unifies[[etabl]],v)
}
