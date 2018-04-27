# Transformation de la table visprem #
#---- Chargement de la table visprem et des fonctions ----#
setwd(".")
visprem0<-read.table("visprem.txt",header=T)

#---- Regroupement de modalit?es ----#
visprem0[,"PCSPQ"]<-factor(visprem0[,"PCSPQ"],
levels=c("Pagr","Part","Pcad","Pint","Pemp","Pouv","Pret","Psan","Pinc"),
labels=c("Pint","Pint","Pcad","Pint","Pemp","Pouv","Psan","Psan","Psan")) # duplicated 

visprem0[,"FAMIQ"]<-factor(visprem0[,"FAMIQ"],
levels=c("Fcel","Fdiv","Finc","Fmar","Fsep","Fuli","Fveu"),
labels=c("Fseu","Fseu","Finc","Fcou","Fseu","Fcou","Fseu"))

#---- Modifications ----#
# Compl?eter la variable ROCNB
# les NA ont ?et?e automatiquement remplac?e par 0 mais sont encore consid?er?ees comme NA.
NArocnb<-row.names(subset(visprem0,is.na(visprem0$ROCNB)))
visprem0[NArocnb,"ROCNB"]<-0
# Correction des plus grosses erreurs de RELAT
sup600<-row.names(subset(visprem0,visprem0$RELAT>600))
visprem0[sup600,"RELAT"]<-visprem0[sup600,"RELAT"]-720
visprem0<-subset(visprem0,is.na(visprem0[,"DMVTP"])=="FALSE")
#---- Transformations des variables quantitatives ----#
visprem0<-transform(visprem0,OPGNBL=log(1+OPGNB))
visprem0<-transform(visprem0,MOYRVL=log(1+MOYRV))
visprem0<-transform(visprem0,TAVEPL=log(1+TAVEP))
visprem0<-transform(visprem0,ENDETL=log(1+ENDET))
visprem0<-transform(visprem0,GAGETL=log(1+GAGET))
visprem0<-transform(visprem0,GAGECL=log(1+GAGEC))
visprem0<-transform(visprem0,GAGEML=log(1+GAGEM))
visprem0<-transform(visprem0,QCREDL=log(1+QCRED))
visprem0<-transform(visprem0,DMVTPL=log(1+DMVTP))
visprem0<-transform(visprem0,BOPPNL=log(1+BOPPN))
visprem0<-transform(visprem0,FACANL=log(1+FACAN))
visprem0<-transform(visprem0,LGAGTL=log(1+LGAGT))
visprem0<-transform(visprem0,VIEMTL=log(1+VIEMT))
visprem0<-transform(visprem0,XLGMTL=log(1+XLGMT))
visprem0<-transform(visprem0,YLVMTL=log(1+YLVMT))
visprem0<-transform(visprem0,ITAVCL=log(1+ITAVC))
visprem0<-transform(visprem0,HAVEFL=log(1+HAVEF))
visprem0<-transform(visprem0,JNBJDL=log(1+JNBJD))
#---- Construction de la table finale ----#
var<-c("SEXEQ","FAMIQ","PCSPQ",
"RELAT","AGER","OPGNBL","MOYRVL","TAVEPL","ENDETL","GAGETL",
"GAGECL","GAGEML","KVUNB","QSMOY","QCREDL","DMVTPL","BOPPNL",
"FACANL","LGAGTL","VIENB","VIEMTL","UEMNB","XLGNB",
"XLGMTL","YLVNB","YLVMTL","ROCNB","NPTAG","ITAVCL",
"HAVEFL","JNBJDL","CARVP")
vispremt<-visprem0[,var]
#-- Enregistrement de la table
write.table(vispremt,"vispremt.txt")
write.table(vispremt,"vtR.txt")