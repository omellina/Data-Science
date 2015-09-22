setwd("Carte scolaire")
etablissements<-read.csv("etablissements_Paris.csv")
adresses<-read.csv("adresses_paris_codees.csv")

### A. Géolocalisation des établissements
## 1. détermination de la rue et de l'arrondissement
adresses$L_NVOIE<-as.character(adresses$L_NVOIE)
adresses$L_ADR<-as.character(adresses$L_ADR)
adresses$libelles.rues<-substr(x = adresses$L_ADR,start = nchar(adresses$L_NVOIE)+2,stop=nchar(adresses$L_ADR))
etablissements$rue<-as.character(etablissements$rue)
etablissements$numero<-as.character(etablissements$numero)
# correction d'un "B" ou "BIS" accolé au nom de rue plutôt qu'au numéro
index_rectif <-grepl(x = etablissements$rue, pattern = "B .*")
etablissements[index_rectif,"numero"]<-paste(etablissements[index_rectif,"numero"],"B", sep="")
etablissements[index_rectif,"rue"]<-gsub(x=etablissements[index_rectif,"rue"],pattern = "^B (.*)$", replacement = "\\1")
index_rectif <-grepl(x = etablissements$rue, pattern = "BIS .*")
etablissements[index_rectif,"numero"]<-paste(etablissements[index_rectif,"numero"],"BIS", sep="")
etablissements[index_rectif,"rue"]<-gsub(x=etablissements[index_rectif,"rue"],pattern = "^BIS (.*)$", replacement = "\\1")

# mise au format du nom du type de voie
etablissements$rue<-gsub(x = etablissements$rue,pattern = "^BOULEVARD ",replacement = "BD ")
etablissements$rue<-gsub(x = etablissements$rue,pattern = "^AVENUE ",replacement = "AV ")
etablissements$rue<-gsub(x = etablissements$rue,pattern = "^PASSAGE ",replacement = "PAS ")
etablissements$rue<-gsub(x = etablissements$rue,pattern = "^PLACE ",replacement = "PL ")
etablissements$rue<-gsub(x = etablissements$rue,pattern = "^ALLEE ",replacement = "ALL ")
etablissements$rue<-gsub(x = etablissements$rue,pattern = "^SQUARE ",replacement = "SQ ")
etablissements$rue<-gsub(x = etablissements$rue,pattern = "^IMPASSE ",replacement = "IMP ")
etablissements$rue<-gsub(x = etablissements$rue,pattern = "^QUAI ",replacement = "QU ")
etablissements$rue<-gsub(x = etablissements$rue,pattern = "^COURS ",replacement = "CRS ")
etablissements$rue<-gsub(x = etablissements$rue,pattern = "^BLD ",replacement = "BD ")
etablissements$rue<-gsub(x = etablissements$rue,pattern = "^R ",replacement = "RUE ")
etablissements$rue<-gsub(x = etablissements$rue,pattern = "44",replacement = "QUARANTE-QUATRE")
etablissements$rue<-gsub(x = etablissements$rue,pattern = " FBG ",replacement = " FAUBOURG ")
etablissements$rue<-gsub(x = etablissements$rue,pattern = "AV PORTE ",replacement = "AV DE LA PORTE ")

rues_paris<-unique(adresses[,c("N_SQ_VO","libelles.rues","C_AR")])

# on cherche par arrondissement
for (arrondissement in 1:20) {
  indices_ets<-which(etablissements$arrondissement==arrondissement)
  indices_ets_paris<-which(rues_paris$C_AR==arrondissement)
  etablissements[indices_ets,"N_SQ_VO"]<-
    rues_paris[indices_ets_paris[sapply(etablissements[indices_ets,"rue"],
                                             function (s) which.min(adist(s,rues_paris[indices_ets_paris,"libelles.rues"])))],"N_SQ_VO"]
}
rm(rues_paris)

## 2. détermination du numéro dans la bonne rue
etablissements$N_SQ_AD<-NA
for (i in 1:nrow(etablissements)) {
  index<-which(adresses$N_SQ_VO==etablissements[i,"N_SQ_VO"] & 
                 adresses$L_NVOIE==gsub(x = etablissements[i,"numero"], pattern="^([0-9]+)[^0-9]+.*$",replacement = "\\1"))
  if (length(index)>0) {
    etablissements[i,"N_SQ_AD"]<- adresses[index[1],"N_SQ_AD"]
  }
}
etablissements$N_SQ_VO<-NULL
write.csv(etablissements,file="etablissements_Paris.csv")