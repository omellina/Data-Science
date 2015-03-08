data<-read.csv("carte2015-2016.csv",header = TRUE,sep = ",",stringsAsFactors=FALSE, quote = "\"")
primaire<-data.frame(t(c(NA,NA,NA,NA,NA,NA,NA,NA,NA)))
maternelle<-data.frame(t(c(NA,NA,NA,NA,NA,NA,NA,NA,NA)))
colnames(primaire)<-c(
  "Code_voie",
  "Type.de.voie",
  "Nom.de.la.voie",
  "N..Début.exclu.sauf.0.",
  "Bis.Ter.Quat.",
  "N..fin.inclus",
  "Bis.Ter.Quat..1",
  "Pair..P..Impair..I.",
  "Ecole")
colnames(maternelle)<-colnames(primaire)
i_m<-1 #manuellement les indices du data frame des écoles maternelles
i_p<-1 #manuellement les indices du data frame des écoles primaires

for (i in 1:length(data[,1])) {
  for (ecole_m in strsplit(data[i,14],"Ecole ")[[1]]) {
    if(ecole_m!="") {
      maternelle[i_m,1:8]<-data[i,c(2:4,6:10)]
        #on réintroduit le mot "Ecole" enlevé par le split des chaînes et on enlève les retours chariot
      maternelle[i_m,9]=gsub(pattern = "\r|\t|\n",paste("Ecole",ecole_m),replacement = "")
      i_m<-i_m+1
    }
  }
  for (ecole_p in strsplit(data[i,15],"Ecole ")[[1]]) {
    if(ecole_p!="") {
      primaire[i_p,1:8]<-data[i,c(2:4,6:10)]
        #on réintroduit le mot "Ecole" enlevé par le split des chaînes et on enlève les retours chariot
      primaire[i_p,9]=gsub(pattern = "\r|\t|\n",paste("Ecole",ecole_p),replacement = "")
      i_p<-i_p+1
    }
  }
}
write.csv(maternelle,file="maternelle 2015-2016.csv")
write.csv(primaire,file="primaire 2015-2016.csv")