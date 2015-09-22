setwd("Carte scolaire/")
library("XML")
content <- readLines("Sectorisation 2015-2016.csv")
content <- content[c(-1,-2)]
colleges<-read.csv(textConnection(content),header = TRUE,sep = ",",stringsAsFactors=FALSE, quote = "\"")
colleges<-colleges[1:12]
colleges[,5]<-as.numeric(colleges[,5])
colleges[is.na(colleges[,5]),5]<-9999 # il y a une valeur NA
colleges[,7]<-as.numeric(colleges[,7])
colleges[colleges[,9]=="I",9]<-"i"
colleges[colleges[,9]=="P",9]<-"p"

#on va faire secteur par secteur

for (i in 1:length(data[,1])) {
  #alla mano
  numero_debut <-colleges[i,5] #numéro de début exclus
  if ((colleges[i,9]=="i")|(colleges[i,9]=="I")) {
    street_number<-numero_debut+2-(numero_debut%%2==0) #prochain numéro impair
  } else {
    street_number<-numero_debut+1+(numero_debut%%2==0) #prochain numéro pair
  }
  #appel de la bonne page
  htmllink<-paste(
    "http://perimsco.paris.fr/perimsco/jsp/site/RunStandaloneApp.jsp?page=schoolsearch&action_select=true&school_year=17&",
    "street_number=",street_number,
    "&number_suffix=",  #visiblement utile
    "&street_code=",colleges[i,1],
    sep="")
  page<- readLines(htmllink)
  doc <- htmlTreeParse(
    htmllink,
    useInternal=TRUE)
  body<-xmlRoot(doc)[["body"]]  #on va cherche le corps de la page
  ul<-getNodeSet(body,"//ul")   #puis les listes
  if (length(ul)<4) { #pas la même structure de donnée, probablement hors carte scolaire
    colleges[i,"maternelle"]<-""
    colleges[i,"primaire"]<-""
    colleges[i,"college"]<-""    
  } else {
    maternelle<-ul[[2]]           #les maternelles sont les 2e dans la liste
    primaire<-ul[[3]]
    college<-ul[[4]]
    # Remarques :
    # - on ne raffine pas la chaîne de caractère à ce stade
    # - on ne gère pas, à ce stades, les multiples établissements : ils seront tous énumérés dans la même case
    
    colleges[i,"maternelle"]<-xmlValue(maternelle)
    colleges[i,"primaire"]<-xmlValue(primaire)
    colleges[i,"college"]<-xmlValue(college)
  }
}
write.csv(colleges,file="carte2015-2016.csv")