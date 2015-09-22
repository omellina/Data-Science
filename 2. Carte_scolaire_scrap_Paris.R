library("stringi")
library("XML")

###################################################################################################
## Préalable : la carte scolaire est chargée pour une série d'adresses de Paris.                 ##
## On utilise pour ce faire la base adresse fournie par la Mairie, qui a été préalablement codée ##
## avec l'identifiant rue utilisé dans la carte scolaire, et ce en suivant les étapes du fichier ##
## codage_adresses_rues_Paris_vf.R                                                               ##
###################################################################################################

# Lecture des adresses de Paris codées avec le code rue utilisé par la carte scolaire
adresses<-read.csv(file="adresses_paris_codees.csv")

# Préparation des structures de données
etablissements<-NULL # recevra la liste de tous les établissements scolaires de Paris
carte_scolaire<-as.data.frame(cbind(N_SQ_AD=adresses$N_SQ_AD,maternelle=NA,primaire=NA,college=NA))

### A. FONCTIONS UTILISEES DANS L'ALGORITHME
## 1. Vérification et ajout d'un établissement
# quand on charge un établissement scolaire du site, cette fonction vérifie s'il est présent
# dans la base des établissements. Sinon, il l'ajouter. Dans tous les cas, l'indice de la
# liste est renvoyé

numero_etablissement <-function (ecole) {
  index<-match(x=ecole,table=etablissements)
  if (is.na(index)) {
    index<-length(etablissements)+1
    etablissements[index]<<-ecole
  }
  return(index)
}

## 2. Récupération des établissements pour un numéro d'une rue (code Mairie de Paris)
# Fonction de base de récupération des établissements
# elle renvoie la liste des trois établissements (maternelle, primaire, collège)
# dont dépend une adresse. Elle renvoie l'indice de ces établissements dans la base
# des établissements

charge<-function (code_rue,numero) {
  #appel de la bonne page
  htmllink<-paste(
    "http://perimsco.paris.fr/perimsco/jsp/site/RunStandaloneApp.jsp?page=schoolsearch&action_select=true&school_year=17&",
    "street_number=",numero,
    "&number_suffix=",  #visiblement utile
    "&street_code=",code_rue,
    sep="")
  print(htmllink)
  page<- readLines(htmllink) #téléchargement
  doc <- htmlTreeParse(
    htmllink,
    useInternal=TRUE)
  body<-xmlRoot(doc)[["body"]]  #on va cherche le corps de la page
  ul<-getNodeSet(body,"//ul")   #puis les listes
  if (length(ul)<4) { #pas la même structure de donnée, probablement hors carte scolaire
    c(NA,NA,NA)
  } else {
    c(maternelle = numero_etablissement(xmlValue(ul[[2]])),
      primaire = numero_etablissement(xmlValue(ul[[3]])),
      college = numero_etablissement(xmlValue(ul[[4]])))
    # Remarques :
    # - on n'analyse pas la chaîne de caractère à ce stade
    # - on ne gère pas, à ce stades, les multiples établissements : ils seront tous énumérés dans la même case
  }
}

## 3. Récupération des établissements pour un numéro d'une rue (entrée de la base adresse)
# fonction intermédiaire : elle appelle la fonction de base pour une entrée dans la
# base adresse de Paris et cette ligne avec le résultat

charge_carte_adresse<-function(indice) { #indice dans la liste adresses
  ecole<-charge(
    code_rue = as.character(adresses[indice,"VOIE_ID"]),
    numero = as.character(adresses[indice,"N_VOIE"])
    )
  carte_scolaire[indice,"maternelle"]<<-ecole[1]
  carte_scolaire[indice,"primaire"]<<-ecole[2]
  carte_scolaire[indice,"college"]<<-ecole[3]
  ecole #le résultat est également renvoyé
}

## 4. Parcours des rues par dichotomie pour charger la carte scolaire

# sous-fonction de dichotomie par récurrence
# permet de traiter le cas où deux numéros de rue ne sont pas sur le même secteur
segmente <- function (liste_indice, ecole0, ecole1) {
  l<-length(liste_indice)
  if (l<=2) { # plus secable, donc plus rien à faire
  } else { #il y a au moins un point intermédiaire
    if (identical(ecole0,ecole1)) { # on suppose que le segment entier est dans le même secteur
      j<-liste_indice[2:(l-1)]
      carte_scolaire[j,"maternelle"]<<-ecole0[1]
      carte_scolaire[j,"primaire"]<<-ecole0[2]
      carte_scolaire[j,"college"]<<-ecole0[3]
    } else { # il faut trouver par récurrence la limite du secteur
      indice_intermediaire<-trunc((l+1)/2)
      ecole_intermediaire<-charge_carte_adresse(indice = liste_indice[indice_intermediaire])
      # on traite les deux sous-segments
      segmente(liste_indice[1:indice_intermediaire],ecole0,ecole_intermediaire)
      segmente(liste_indice[indice_intermediaire:l],ecole_intermediaire,ecole1)
    }
  }
}


## 5. Parcours des rues pour charger la carte scolaire

# fonction déterminant la carte scolaire de tous les numéros de liste_indice
traite_rue <-function(liste_indice) {
  l<-length(liste_indice)
  ecole0<-charge_carte_adresse(liste_indice[1])
  if (l==1) {
    # un seul numéro dans la rue : plus rien à faire
  } else {
    ecole1<-charge_carte_adresse(liste_indice[l])
    segmente(liste_indice = liste_indice,ecole0,ecole1)
  }
} 

### B. Scraping de la carte proprement dite
## La méthode consiste à récupérer la carte scolaire par un bon échantillonnage des numéros
## de rue. Avec 5482 rues, environ 11000 coupes (rues,pair/impair) et 130000 numéros,
## on ne peut pas se permettre de solliciter le serveur sur tous les numéros de rue.
## La stratégie consiste donc à appeler le serveur pour les deux numéros extrêmes d'un même 
## côté de rue ; s'ils dépendent des mêmes établissements, on suppose qu'il en est de même
## pour tous les numéros intermédiaires. Sinon, on cherche la limite des secteurs par dichotomie.

## 1. Liste des rues

# pour chaque rue codée de Paris
# remarque : cette version du code n'est pas protégée des erreurs de connexion Internet
# elle nécessite soit une relance régulière avec surveillance manuelle, soit une V2 pour
# temporiser automatiquement quand un problème Internet est constaté

for (code_rue in unique(adresses$VOIE_ID)) {
  liste_indice<-which(adresses$VOIE_ID==code_rue & adresses$N_VOIE%%2 ==0) # numeros pairs
  liste_indice<-liste_indice[order(adresses[liste_indice,"N_VOIE"],adresses[liste_indice,"L_NVOIE"])] #triés par ordre croissant
  if (length(liste_indice)>0) traite_rue(liste_indice)
  liste_indice<-which(adresses$VOIE_ID==code_rue & adresses$N_VOIE%%2 ==1) # numeros impairs
  liste_indice<-liste_indice[order(adresses[liste_indice,"N_VOIE"],adresses[liste_indice,"L_NVOIE"])] #triés par ordre croissant
  if (length(liste_indice)>0) traite_rue(liste_indice)
}

# Proportion des adresses de Paris non inclues dans la carte scolaire : 10,76%
# Ce sont souvent des adresses d'impasses, dont l'adresse des bâtis se situe sur les rues
# adjacentes. On y trouve aussi des adresses de zones industrielles.
sum(is.na(carte_scolaire$maternelle))/nrow(carte_scolaire)

# sauvegarde intermédiaire
write.csv(carte_scolaire,file="carte_scolaire_brute.csv")
save(list = c("etablissements","carte_scolaire"),file = "carte_scolaire_brute.RData")

### B. NETTOYAGE
## 1. Dédoublonnage des établissements scolaires
# il s'avère que certaines adresses sont rattachées à plusieurs établissements différents
# dans la base récupérée, cela correspond à des chaînes de caractères comportant plusieurs école
# il faut donc les dédoublonner.
# On crée une liste d'établissements uniques et une matrice de correspondance de la première liste
# à la seconde

etablissements<-stri_conv(etablissements,from = "UTF-8", to = "LATIN1")
etablissements_dedoubles<-unique(unlist(strsplit(unique(unlist(strsplit(etablissements,split="Ecole "))),split = "Collège ")))
etablissements_dedoubles<-gsub(x=etablissements_dedoubles,pattern = ".*Votre adresse n'est pas.*$",replacement = "NA")
etablissements_dedoubles<-etablissements_dedoubles[!(grepl(x=etablissements_dedoubles,pattern = "^$"))]
etablissements_dedoubles<-gsub(pattern = "\r|\t|\n",etablissements_dedoubles,replacement = "")
etablissements_dedoubles<-gsub(pattern = "  ",etablissements_dedoubles,replacement = " ")
etablissements_dedoubles<-unique(etablissements_dedoubles)
correspondance<-as.data.frame(NULL)

for (i in 1:length(etablissements)) {
  liste_ecole<-etablissements[i]
  for (prefixe in c("Ecole ", "Collège ")) {
    ecoles<-unlist(strsplit(as.character(liste_ecole),split = prefixe))
    if (length(ecoles)>1) { # Au moins un établissement
      for (ecole in ecoles[2:length(ecoles)]) {
        ecole<-gsub(pattern = "\r|\t|\n",ecole,replacement = "")
        ecole<-gsub(pattern = "  ",ecole,replacement = " ")
        correspondance[length(correspondance$index_doublonne)+1,"index_doublonne"]<-i
        correspondance[length(correspondance$index_doublonne),"index_dedoublonne"]<-
          match(x = ecole, table = etablissements_dedoubles)
      }
    }
  }
}

## 2. Recodage de la carte scolaire par établissement
# on crée trois couches, correspondant aux trois niveaux (maternelles, primaires, colleges)
maternelles<-merge(x = carte_scolaire[,c(1:2)], y = correspondance, by.x = "maternelle", by.y = "index_doublonne")
primaires<-merge(x = carte_scolaire[,c(1:3)], y = correspondance, by.x = "primaire", by.y = "index_doublonne")
colleges<-merge(x = carte_scolaire[,c(1:4)], y = correspondance, by.x = "college", by.y = "index_doublonne")

# tri des colonnes utilisées pour la fusion
maternelles<-maternelles[,c(2,3)]
primaires<-primaires[,c(2,3)]
colleges<-colleges[,c(2,3)]
colnames(maternelles)<-c("N_SQ_AD","maternelle")
colnames(primaires)<-c("N_SQ_AD","primaire")
colnames(colleges)<-c("N_SQ_AD","college")

write.csv(maternelles,file="carte_scolaire_maternelles.csv")
write.csv(primaires,file="carte_scolaire_primaires.csv")
write.csv(colleges,file="carte_scolaire_colleges.csv")

etablissements_dedoubles<-as.data.frame(cbind("libelle.complet" = etablissements_dedoubles))
etablissements_dedoubles$index<-row(etablissements_dedoubles)
etablissements_dedoubles<-etablissements_dedoubles[unique(correspondance$index_dedoublonne),]
  # revient à supprimer la ligne "NA" disparu avec le mode de fusion choisi

## 3. Retraitement des noms d'établissement
etablissements_dedoubles$libelle.complet<-gsub(x = etablissements_dedoubles$libelle.complet,pattern="(maternelle|élémentaire)",replacement = "Ecole \\1")
indices_colleges<-!grepl(x = etablissements_dedoubles$libelle.complet,pattern = "^Ecole ")
etablissements_dedoubles[indices_colleges,"libelle.complet"]<-
  gsub(x=etablissements_dedoubles[indices_colleges,"libelle.complet"],pattern = "^(.*)$",replacement = "Collège \\1")

# retraitement pour retomber sur des libellés normés
etablissements_dedoubles$libelle.complet<-stri_trans_toupper(etablissements_dedoubles$libelle.complet)
etablissements_dedoubles$libelle.complet<-gsub(x = etablissements_dedoubles$libelle.complet,pattern="([0-9]+) - ([0-9]+)",replacement = "\\1-\\2")
etablissements_dedoubles$libelle<-gsub(x = etablissements_dedoubles$libelle.complet,pattern="^([^0-9]*) ([0-9][^ ]*) (.+) - PARIS ([0-9]{2})$",replacement = "\\1")
etablissements_dedoubles$numero<-gsub(x = etablissements_dedoubles$libelle.complet,pattern="^([^0-9]*) ([0-9][^ ]*) (.+) - PARIS ([0-9]{2})$",replacement = "\\2")
etablissements_dedoubles$rue<-gsub(x = etablissements_dedoubles$libelle.complet,pattern="^([^0-9]*) ([0-9][^ ]*) (.+) - PARIS ([0-9]{2})$",replacement = "\\3")
etablissements_dedoubles$arrondissement<-gsub(x = etablissements_dedoubles$libelle.complet,pattern="^([^0-9]*) ([0-9][^ ]*) (.+) - PARIS ([0-9]{2})$",replacement = "\\4")

## 4. Sauvegarde de la liste des établissements
write.csv(etablissements_dedoubles,file="etablissements_Paris.csv")