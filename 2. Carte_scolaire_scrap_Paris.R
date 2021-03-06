library("stringi")
library("XML")

###################################################################################################
## Pr�alable : la carte scolaire est charg�e pour une s�rie d'adresses de Paris.                 ##
## On utilise pour ce faire la base adresse fournie par la Mairie, qui a �t� pr�alablement cod�e ##
## avec l'identifiant rue utilis� dans la carte scolaire, et ce en suivant les �tapes du fichier ##
## codage_adresses_rues_Paris_vf.R                                                               ##
###################################################################################################

# Lecture des adresses de Paris cod�es avec le code rue utilis� par la carte scolaire
adresses<-read.csv(file="adresses_paris_codees.csv")

# Pr�paration des structures de donn�es
etablissements<-NULL # recevra la liste de tous les �tablissements scolaires de Paris
carte_scolaire<-as.data.frame(cbind(N_SQ_AD=adresses$N_SQ_AD,maternelle=NA,primaire=NA,college=NA))

### A. FONCTIONS UTILISEES DANS L'ALGORITHME
## 1. V�rification et ajout d'un �tablissement
# quand on charge un �tablissement scolaire du site, cette fonction v�rifie s'il est pr�sent
# dans la base des �tablissements. Sinon, il l'ajouter. Dans tous les cas, l'indice de la
# liste est renvoy�

numero_etablissement <-function (ecole) {
  index<-match(x=ecole,table=etablissements)
  if (is.na(index)) {
    index<-length(etablissements)+1
    etablissements[index]<<-ecole
  }
  return(index)
}

## 2. R�cup�ration des �tablissements pour un num�ro d'une rue (code Mairie de Paris)
# Fonction de base de r�cup�ration des �tablissements
# elle renvoie la liste des trois �tablissements (maternelle, primaire, coll�ge)
# dont d�pend une adresse. Elle renvoie l'indice de ces �tablissements dans la base
# des �tablissements

charge<-function (code_rue,numero) {
  #appel de la bonne page
  htmllink<-paste(
    "http://perimsco.paris.fr/perimsco/jsp/site/RunStandaloneApp.jsp?page=schoolsearch&action_select=true&school_year=17&",
    "street_number=",numero,
    "&number_suffix=",  #visiblement utile
    "&street_code=",code_rue,
    sep="")
  print(htmllink)
  page<- readLines(htmllink) #t�l�chargement
  doc <- htmlTreeParse(
    htmllink,
    useInternal=TRUE)
  body<-xmlRoot(doc)[["body"]]  #on va cherche le corps de la page
  ul<-getNodeSet(body,"//ul")   #puis les listes
  if (length(ul)<4) { #pas la m�me structure de donn�e, probablement hors carte scolaire
    c(NA,NA,NA)
  } else {
    c(maternelle = numero_etablissement(xmlValue(ul[[2]])),
      primaire = numero_etablissement(xmlValue(ul[[3]])),
      college = numero_etablissement(xmlValue(ul[[4]])))
    # Remarques :
    # - on n'analyse pas la cha�ne de caract�re � ce stade
    # - on ne g�re pas, � ce stades, les multiples �tablissements : ils seront tous �num�r�s dans la m�me case
  }
}

## 3. R�cup�ration des �tablissements pour un num�ro d'une rue (entr�e de la base adresse)
# fonction interm�diaire : elle appelle la fonction de base pour une entr�e dans la
# base adresse de Paris et cette ligne avec le r�sultat

charge_carte_adresse<-function(indice) { #indice dans la liste adresses
  ecole<-charge(
    code_rue = as.character(adresses[indice,"VOIE_ID"]),
    numero = as.character(adresses[indice,"N_VOIE"])
    )
  carte_scolaire[indice,"maternelle"]<<-ecole[1]
  carte_scolaire[indice,"primaire"]<<-ecole[2]
  carte_scolaire[indice,"college"]<<-ecole[3]
  ecole #le r�sultat est �galement renvoy�
}

## 4. Parcours des rues par dichotomie pour charger la carte scolaire

# sous-fonction de dichotomie par r�currence
# permet de traiter le cas o� deux num�ros de rue ne sont pas sur le m�me secteur
segmente <- function (liste_indice, ecole0, ecole1) {
  l<-length(liste_indice)
  if (l<=2) { # plus secable, donc plus rien � faire
  } else { #il y a au moins un point interm�diaire
    if (identical(ecole0,ecole1)) { # on suppose que le segment entier est dans le m�me secteur
      j<-liste_indice[2:(l-1)]
      carte_scolaire[j,"maternelle"]<<-ecole0[1]
      carte_scolaire[j,"primaire"]<<-ecole0[2]
      carte_scolaire[j,"college"]<<-ecole0[3]
    } else { # il faut trouver par r�currence la limite du secteur
      indice_intermediaire<-trunc((l+1)/2)
      ecole_intermediaire<-charge_carte_adresse(indice = liste_indice[indice_intermediaire])
      # on traite les deux sous-segments
      segmente(liste_indice[1:indice_intermediaire],ecole0,ecole_intermediaire)
      segmente(liste_indice[indice_intermediaire:l],ecole_intermediaire,ecole1)
    }
  }
}


## 5. Parcours des rues pour charger la carte scolaire

# fonction d�terminant la carte scolaire de tous les num�ros de liste_indice
traite_rue <-function(liste_indice) {
  l<-length(liste_indice)
  ecole0<-charge_carte_adresse(liste_indice[1])
  if (l==1) {
    # un seul num�ro dans la rue : plus rien � faire
  } else {
    ecole1<-charge_carte_adresse(liste_indice[l])
    segmente(liste_indice = liste_indice,ecole0,ecole1)
  }
} 

### B. Scraping de la carte proprement dite
## La m�thode consiste � r�cup�rer la carte scolaire par un bon �chantillonnage des num�ros
## de rue. Avec 5482 rues, environ 11000 coupes (rues,pair/impair) et 130000 num�ros,
## on ne peut pas se permettre de solliciter le serveur sur tous les num�ros de rue.
## La strat�gie consiste donc � appeler le serveur pour les deux num�ros extr�mes d'un m�me 
## c�t� de rue ; s'ils d�pendent des m�mes �tablissements, on suppose qu'il en est de m�me
## pour tous les num�ros interm�diaires. Sinon, on cherche la limite des secteurs par dichotomie.

## 1. Liste des rues

# pour chaque rue cod�e de Paris
# remarque : cette version du code n'est pas prot�g�e des erreurs de connexion Internet
# elle n�cessite soit une relance r�guli�re avec surveillance manuelle, soit une V2 pour
# temporiser automatiquement quand un probl�me Internet est constat�

for (code_rue in unique(adresses$VOIE_ID)) {
  liste_indice<-which(adresses$VOIE_ID==code_rue & adresses$N_VOIE%%2 ==0) # numeros pairs
  liste_indice<-liste_indice[order(adresses[liste_indice,"N_VOIE"],adresses[liste_indice,"L_NVOIE"])] #tri�s par ordre croissant
  if (length(liste_indice)>0) traite_rue(liste_indice)
  liste_indice<-which(adresses$VOIE_ID==code_rue & adresses$N_VOIE%%2 ==1) # numeros impairs
  liste_indice<-liste_indice[order(adresses[liste_indice,"N_VOIE"],adresses[liste_indice,"L_NVOIE"])] #tri�s par ordre croissant
  if (length(liste_indice)>0) traite_rue(liste_indice)
}

# Proportion des adresses de Paris non inclues dans la carte scolaire : 10,76%
# Ce sont souvent des adresses d'impasses, dont l'adresse des b�tis se situe sur les rues
# adjacentes. On y trouve aussi des adresses de zones industrielles.
sum(is.na(carte_scolaire$maternelle))/nrow(carte_scolaire)

# sauvegarde interm�diaire
write.csv(carte_scolaire,file="carte_scolaire_brute.csv")
save(list = c("etablissements","carte_scolaire"),file = "carte_scolaire_brute.RData")

### B. NETTOYAGE
## 1. D�doublonnage des �tablissements scolaires
# il s'av�re que certaines adresses sont rattach�es � plusieurs �tablissements diff�rents
# dans la base r�cup�r�e, cela correspond � des cha�nes de caract�res comportant plusieurs �cole
# il faut donc les d�doublonner.
# On cr�e une liste d'�tablissements uniques et une matrice de correspondance de la premi�re liste
# � la seconde

etablissements<-stri_conv(etablissements,from = "UTF-8", to = "LATIN1")
etablissements_dedoubles<-unique(unlist(strsplit(unique(unlist(strsplit(etablissements,split="Ecole "))),split = "Coll�ge ")))
etablissements_dedoubles<-gsub(x=etablissements_dedoubles,pattern = ".*Votre adresse n'est pas.*$",replacement = "NA")
etablissements_dedoubles<-etablissements_dedoubles[!(grepl(x=etablissements_dedoubles,pattern = "^$"))]
etablissements_dedoubles<-gsub(pattern = "\r|\t|\n",etablissements_dedoubles,replacement = "")
etablissements_dedoubles<-gsub(pattern = "  ",etablissements_dedoubles,replacement = " ")
etablissements_dedoubles<-unique(etablissements_dedoubles)
correspondance<-as.data.frame(NULL)

for (i in 1:length(etablissements)) {
  liste_ecole<-etablissements[i]
  for (prefixe in c("Ecole ", "Coll�ge ")) {
    ecoles<-unlist(strsplit(as.character(liste_ecole),split = prefixe))
    if (length(ecoles)>1) { # Au moins un �tablissement
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

## 2. Recodage de la carte scolaire par �tablissement
# on cr�e trois couches, correspondant aux trois niveaux (maternelles, primaires, colleges)
maternelles<-merge(x = carte_scolaire[,c(1:2)], y = correspondance, by.x = "maternelle", by.y = "index_doublonne")
primaires<-merge(x = carte_scolaire[,c(1:3)], y = correspondance, by.x = "primaire", by.y = "index_doublonne")
colleges<-merge(x = carte_scolaire[,c(1:4)], y = correspondance, by.x = "college", by.y = "index_doublonne")

# tri des colonnes utilis�es pour la fusion
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
  # revient � supprimer la ligne "NA" disparu avec le mode de fusion choisi

## 3. Retraitement des noms d'�tablissement
etablissements_dedoubles$libelle.complet<-gsub(x = etablissements_dedoubles$libelle.complet,pattern="(maternelle|�l�mentaire)",replacement = "Ecole \\1")
indices_colleges<-!grepl(x = etablissements_dedoubles$libelle.complet,pattern = "^Ecole ")
etablissements_dedoubles[indices_colleges,"libelle.complet"]<-
  gsub(x=etablissements_dedoubles[indices_colleges,"libelle.complet"],pattern = "^(.*)$",replacement = "Coll�ge \\1")

# retraitement pour retomber sur des libell�s norm�s
etablissements_dedoubles$libelle.complet<-stri_trans_toupper(etablissements_dedoubles$libelle.complet)
etablissements_dedoubles$libelle.complet<-gsub(x = etablissements_dedoubles$libelle.complet,pattern="([0-9]+) - ([0-9]+)",replacement = "\\1-\\2")
etablissements_dedoubles$libelle<-gsub(x = etablissements_dedoubles$libelle.complet,pattern="^([^0-9]*) ([0-9][^ ]*) (.+) - PARIS ([0-9]{2})$",replacement = "\\1")
etablissements_dedoubles$numero<-gsub(x = etablissements_dedoubles$libelle.complet,pattern="^([^0-9]*) ([0-9][^ ]*) (.+) - PARIS ([0-9]{2})$",replacement = "\\2")
etablissements_dedoubles$rue<-gsub(x = etablissements_dedoubles$libelle.complet,pattern="^([^0-9]*) ([0-9][^ ]*) (.+) - PARIS ([0-9]{2})$",replacement = "\\3")
etablissements_dedoubles$arrondissement<-gsub(x = etablissements_dedoubles$libelle.complet,pattern="^([^0-9]*) ([0-9][^ ]*) (.+) - PARIS ([0-9]{2})$",replacement = "\\4")

## 4. Sauvegarde de la liste des �tablissements
write.csv(etablissements_dedoubles,file="etablissements_Paris.csv")