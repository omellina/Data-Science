library("stringi")

##############################################################################################
# Ce code rapproche la base de données des adresses de Paris du site Open Data Paris         #
# avec le fichier des codes de voies des Paris (la version la plus à jour semblant être      #
# celle fournie avec le lien dit "bureaux de vote".                                          #
##############################################################################################

### A. EXTRACTION DU FICHIER OPEN SOURCE DE LA MAIRIE DE PARIS DES NOMS DE RUE
## 1. Récupération des noms de rue de Paris, avec leurs codes
# Allez savoir pourquoi, on trouve ça de façon propre dans la liste "liste des bureaux de vote 2013"
# et pas dans les autres base en date d'avril 2015

# Version en ligne :
# rues_paris <- read.csv("http://parisdata.opendatasoft.com/explore/dataset/liste_des_bureaux_de_vote_2013_voies_de_paris/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true",header = TRUE,sep=";")

# Version locale (extraction 12 avril 2015) :
rues_paris <- read.csv("Open Data Paris/liste_des_bureaux_de_vote_2013_voies_de_paris.csv",header = TRUE,sep=";")
rues_paris<-rues_paris[!grepl(pattern = "ligne",x=rues_paris$VOIE_ID),] # suppression d'une ligne parasite
rues_paris$LibellÃ..voie.long<-as.character(rues_paris$LibellÃ..voie.long)

## 2. Récupération de la base adresse de tous les numéros de Paris

# Version en ligne :
# adresses_paris<-read.csv("http://parisdata.opendatasoft.com/explore/dataset/adresse_paris/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true",header = TRUE,sep=";")

# Version locale (extraction 12 avril 2015) :
adresses_paris<-read.csv("Open Data Paris//adresse_paris.csv",header = TRUE,sep=";")

# on ne garde que les adresses dans le département 75
adresses_paris<-adresses_paris[adresses_paris$B_HORS75=="N",]
adresses_paris$L_NVOIE<-as.character(adresses_paris$L_NVOIE)
adresses_paris$L_ADR<-as.character(adresses_paris$L_ADR)

# Construction des noms de rues : on retire du libellé les numéros
adresses_paris$libelles.rues<-substr(x = adresses_paris$L_ADR,start = nchar(adresses_paris$L_NVOIE)+2,stop=nchar(adresses_paris$L_ADR))

## 2. Correction des noms de voies
# Ces traitements visent à homogénéiser les noms de rues entre les deux bases
# Notamment, la base adresse comporte des abbréviation de toutes les dénominations de voie
adresses_paris$libelles.rues<-stri_trans_toupper(adresses_paris$libelles.rues) # tout en majuscule
adresses_paris$libelles.rues<-gsub(pattern = "^ALL ",  x = adresses_paris$libelles.rues,replacement = "ALLEE ")
adresses_paris$libelles.rues<-gsub(pattern = "^AV ",   x = adresses_paris$libelles.rues,replacement = "AVENUE ")
adresses_paris$libelles.rues<-gsub(pattern = "^CHEM ", x = adresses_paris$libelles.rues,replacement = "CHEMIN ")
adresses_paris$libelles.rues<-gsub(pattern = "^CAR ",  x = adresses_paris$libelles.rues,replacement = "CARREFOUR ")
adresses_paris$libelles.rues<-gsub(pattern = "^ESPL ", x = adresses_paris$libelles.rues,replacement = "ESPLANADE ")
adresses_paris$libelles.rues<-gsub(pattern = "^HAM ",  x = adresses_paris$libelles.rues,replacement = "HAMEAU ")
adresses_paris$libelles.rues<-gsub(pattern = "^IMP ",  x = adresses_paris$libelles.rues,replacement = "IMPASSE ")
adresses_paris$libelles.rues<-gsub(pattern = "^PAS ",  x = adresses_paris$libelles.rues,replacement = "PASSAGE ")
adresses_paris$libelles.rues<-gsub(pattern = "^PL ",   x = adresses_paris$libelles.rues,replacement = "PLACE ")
adresses_paris$libelles.rues<-gsub(pattern = "^RLE ",  x = adresses_paris$libelles.rues,replacement = "RUELLE ")
adresses_paris$libelles.rues<-gsub(pattern = "^RTE ",  x = adresses_paris$libelles.rues,replacement = "ROUTE ")
adresses_paris$libelles.rues<-gsub(pattern = "^SQ ",   x = adresses_paris$libelles.rues,replacement = "SQUARE ")
adresses_paris$libelles.rues<-gsub(pattern = "^TERR ", x = adresses_paris$libelles.rues,replacement = "TERRASSE ")
adresses_paris$libelles.rues<-gsub(pattern = "^BD ",   x = adresses_paris$libelles.rues,replacement = "BOULEVARD ")
adresses_paris$libelles.rues<-gsub(pattern = "^GAL ",  x = adresses_paris$libelles.rues,replacement = "GALERIE ")
adresses_paris$libelles.rues<-gsub(pattern = "^QU ",   x = adresses_paris$libelles.rues,replacement = "QUAI ")
adresses_paris$libelles.rues<-gsub(pattern = "^CRS ",  x = adresses_paris$libelles.rues,replacement = "COURS ")
adresses_paris$libelles.rues<-gsub(pattern = "^VLA ",  x = adresses_paris$libelles.rues,replacement = "VILLA ")
adresses_paris$libelles.rues<-gsub(pattern = "^SENT ", x = adresses_paris$libelles.rues,replacement = "SENTIER ")
adresses_paris$libelles.rues<-gsub(pattern = "^SENTE ",x = adresses_paris$libelles.rues,replacement = "SENTIER ")
adresses_paris$libelles.rues<-gsub(pattern = "^PER ",  x = adresses_paris$libelles.rues,replacement = "PERISTYLE ")
adresses_paris$libelles.rues<-gsub(pattern = "^PRT ",  x = adresses_paris$libelles.rues,replacement = "PORT ")
adresses_paris$libelles.rues<-gsub(pattern = "^PARV ", x = adresses_paris$libelles.rues,replacement = "PARVIS ")
adresses_paris$libelles.rues<-gsub(pattern = "^CHAU ", x = adresses_paris$libelles.rues,replacement = "CHAUSSEE ")
adresses_paris$libelles.rues<-gsub(pattern = "^PROM ", x = adresses_paris$libelles.rues,replacement = "PROMENADE ")
adresses_paris$libelles.rues<-gsub(pattern = "^RPT ",  x = adresses_paris$libelles.rues,replacement = "ROND POINT ")
adresses_paris$libelles.rues<-gsub(pattern = "^SOUT ", x = adresses_paris$libelles.rues,replacement = "SOUTERRAIN ")
adresses_paris$libelles.rues<-gsub(pattern = "^(.+ )*([A-Z]{1,2})/([0-9]{1,2})( .+)*$",x=adresses_paris$libelles.rues,replacement = "\\2/\\3")
adresses_paris$libelles.rues<-gsub(pattern = "[^A-Za-z0-9/]",x=adresses_paris$libelles.rues,replacement = "")

# Le fichier dit "bureau de vote" est plus propre : il nécessite moins de retraitement
rues_paris$libelles.rues<-gsub(x=rues_paris$LibellÃ..voie.long,pattern = "NON DENOMMEE",replacement = "VOIE")
rues_paris$libelles.rues<-gsub(x=rues_paris$libelles.rues,pattern = "^(.+ )*([A-Z]{1,2})/([0-9]{1,2})( .+)*$",replacement = "\\2/\\3")
rues_paris$libelles.rues<-gsub(x=rues_paris$libelles.rues,pattern = "SENTE ",replacement = "SENTIER")
rues_paris$libelles.rues<-gsub(x=rues_paris$libelles.rues,pattern = "[^A-Za-z0-9/]",replacement = "")

stopifnot(!duplicated(rues_paris$arrond.rue)) # si jamais nos simplifications venaient à créer un
# doublon dans la base des rues de la Mairie, ça voudrait dire qu'elles sont trop drastiques

## 4. Création d'une base des noms de rues extraite de la base adresse
# Les rues sont codées avec le champ N_SQ_VO
adresses_paris$N_SQ_VO<-as.factor(adresses_paris$N_SQ_VO)
rues_adresses<-levels(adresses_paris$N_SQ_VO)

## 5. Création dans les deux bases d'un libellé comparable
# On utilisera un rapprochement par nom de rue. Mais on souhaite également différentier les mêmes rues
# d'arrondissements différents. On crée donc un champ d'identifiant unique concaténant les deux
adresses_paris$arrondissement.rue<-paste(
  as.numeric(adresses_paris$N_SQ_AR)%%100,
  adresses_paris$libelles.rues,sep=".")
rues_paris$arrondissement.rue<-paste(
  as.numeric(rues_paris$Arrondissement)%%100,
  rues_paris$libelles.rues,sep=".")

### C. Rapprochement des deux bases
## 1. croisement des noms de rues sur la base d'intitullés (et d'arrondissement identiques)
# On fusionne simplement les deux bases
table_rue<-merge(x=rues_paris,
              y=adresses_paris,
              by.x="arrondissement.rue",
              by.y="arrondissement.rue")

# simplification de la fusion
table_rue<-table_rue[,c("N_SQ_VO","N_SQ_AD","VOIE_ID")]

# identification des adresses non rapprochées
adresses_manquantes<-adresses_paris[is.na(match(x = adresses_paris$N_SQ_AD,table=table_rue$N_SQ_AD)),]
rues_paris_manquantes<-rues_paris[is.na(match(x = rues_paris$VOIE_ID,table=table_rue$VOIE_ID)),]
rues_manquantes_adresses<-unique(adresses_manquantes[,c("N_SQ_VO","arrondissement.rue","C_AR")])

# pour les correspondances trouvées, on code la base adresse
table_rue<-unique(table_rue[,c("N_SQ_VO","VOIE_ID")])

# taux de réussite de 99.48%
sprintf("Adresses codées à %1.2f%%",
        100*(1-(length(adresses_manquantes$N_SQ_AD)/length(adresses_paris$N_SQ_AD))))

## 2. Rapprochement des noms de rues similaires (à une orthographe près)
# on calcule des distances relatives entre chaînes de caractère, pour rattraper les différences
# d'orthographe éventuelles (ou oubli d'un prénom, etc.)

for (i in 1:nrow(rues_manquantes_adresses)) {
  index <- which.min(adist(rues_manquantes_adresses[i,"arrondissement.rue"],rues_paris_manquantes$arrondissement.rue))
  rues_manquantes_adresses[i,"VOIE_ID"]<-rues_paris_manquantes[index,"VOIE_ID"]
  rues_manquantes_adresses[i,"min.distance"]<-adist(rues_manquantes_adresses[i,"arrondissement.rue"],rues_paris_manquantes[index,"arrondissement.rue"])/min(nchar(rues_manquantes_adresses[i,"arrondissement.rue"]),nchar(rues_paris_manquantes[index,"arrondissement.rue"]))
}

# on ne corrige que les proximités lexicographiques de <0.8 (vérifié à l'oeil nu)
table_rue<-rbind(table_rue,rues_manquantes_adresses[rues_manquantes_adresses$min.distance<0.8,c("N_SQ_VO","VOIE_ID")])
# deux exceptions manuelles
table_rue[table_rue$N_SQ_VO=="750000586","VOIE_ID"]<-"1450"
# deux autres rues manuellement rajoutées
table_rue[nrow(table_rue)+1,]<-c("750007714","1334")
table_rue<-unique(table_rue)

### D. Recodage de la base à partir des correspondances des identifiants de rue
adresses_paris_codee<-merge(x=adresses_paris,y=table_rue,by.x="N_SQ_VO", by.y="N_SQ_VO")
adresses_paris_codee<-unique(adresses_paris_codee)

# Taux de réussite : 100%
sprintf("Adresses codées à %1.2f%%",
        100*(length(adresses_paris_codee$N_SQ_AD)/length(adresses_paris$N_SQ_AD)))

# Sauvegarde du résultat dans un fichier complet
adresses_paris_codee<-adresses_paris_codee[-c(19:20)]
write.csv(adresses_paris_codee,file="adresses_paris_codees.csv")