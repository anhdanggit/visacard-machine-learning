###      Recodage des variables       ###
vispremt<-read.table("vispremt.txt")
vispremv<-vispremt

inc<-row.names(subset(vispremt,vispremt$FAMIQ=="Finc"))
unif<-runif(length(inc))<0.45

vispremt[inc,"FAMIQ"]<-factor(unif,levels=c("TRUE","FALSE"),labels=c("Fseu","Fcou"))

famiq<-as.vector(vispremt[,"FAMIQ"])    # Pour aléatoirement le niveau "Finc"
vispremt[,"FAMIQ"]<-factor(famiq)
vispremv<-vispremt

vispremv[,"familr"]<-2-as.numeric(vispremt[,"FAMIQ"]) # Fseu:0 Fcou:1

vispremv[,"sexer"]<-2-as.numeric(vispremt[,"SEXEQ"]) # H:0 F:1

vispremv[,"kvunbq"]<-cut(vispremv$KVUNB, 
			breaks=c(min(vispremv$KVUNB),1.5,max(vispremv$KVUNB)), 
			labels = c("K0","K1"),include.lowest = TRUE)

vispremv[,"vienbq"]<-cut(vispremv$VIENB, 
			breaks=c(min(vispremv$VIENB),0.5,max(vispremv$VIENB)), 
			labels = c("V0","V1"),include.lowest = TRUE)

vispremv[,"uemnbq"]<-cut(vispremv$UEMNB, 
			breaks=c(min(vispremv$UEMNB),0.5,1.5,max(vispremv$UEMNB)), 
			labels = c("U0","U1","U2"),include.lowest = TRUE)

vispremv[,"xlgnbq"]<-cut(vispremv$XLGNB, 
			breaks=c(min(vispremv$XLGNB),0.5,1.5,max(vispremv$XLGNB)), 
			labels = c("X0","X1","X2"),include.lowest = TRUE)

vispremv[,"ylvnbq"]<-cut(vispremv$YLVNB, 
			breaks=c(min(vispremv$YLVNB),0.5,1.5,max(vispremv$YLVNB)), 
			labels = c("Y0","Y1","Y2"),include.lowest = TRUE)

vispremv[,"rocnbq"]<-cut(vispremv$ROCNB, 
			breaks=c(min(vispremv$ROCNB),0.5,max(vispremv$ROCNB)), 
			labels = c("R0","R1"),include.lowest = TRUE)

vispremv[,"nptagq"]<-cut(vispremv$NPTAG, 
			breaks=c(min(vispremv$NPTAG),0.5,max(vispremv$NPTAG)), 
			labels = c("N0","N1"),include.lowest = TRUE)
vispremv[,"endetq"]<-cut(ceiling(vispremv$ENDETL), 
			breaks=c(min(ceiling(vispremv$ENDETL)),0.5,max(ceiling(vispremv$ENDETL))), 
			labels = c("E0","E1"),include.lowest = TRUE)
vispremv[,"gagetq"]<-cut(ceiling(vispremv$GAGETL), 
			breaks=c(min(ceiling(vispremv$GAGETL)),0.5,max(ceiling(vispremv$GAGETL))), 
			labels = c("G0","G1"),include.lowest = TRUE)
vispremv[,"facanq"]<-cut(ceiling(vispremv$FACANL), 
			breaks=c(min(ceiling(vispremv$FACANL)),0.5,max(ceiling(vispremv$FACANL))), 
			labels = c("F0","F1"),include.lowest = TRUE)
vispremv[,"lgagtq"]<-cut(ceiling(vispremv$LGAGTL), 
			breaks=c(min(ceiling(vispremv$LGAGTL)),0.5,max(ceiling(vispremv$LGAGTL))), 
			labels = c("L0","L1"),include.lowest = TRUE)
vispremv[,"havefq"]<-cut(ceiling(vispremv$HAVEFL), 
			breaks=c(min(ceiling(vispremv$HAVEFL)),0.5,max(ceiling(vispremv$HAVEFL))), 
			labels = c("H0","H1"),include.lowest = TRUE)
vispremv[,"ageq"]<-cut(vispremv$AGER,breaks=quantile(vispremv[,"AGER"], probs = seq(0, 1, 1/3)),
      labels = c("A0","A1","A2"),include.lowest = TRUE)
vispremv[,"relatq"]<-cut(vispremv$RELAT,breaks=quantile(vispremv[,"RELAT"], probs = seq(0, 1, 1/3)),
      labels = c("R0","R1","R2"),include.lowest = TRUE)
vispremv[,"qsmoyq"]<-cut(vispremv$QSMOY,breaks=quantile(vispremv[,"QSMOY"], probs = seq(0, 1, 1/3)),
      labels = c("Q0","Q1","Q2"),include.lowest = TRUE)
vispremv[,"opgnbq"]<-cut(vispremv$OPGNBL,
      breaks=c(0,0.0000001,0.7,3.367296),
      labels = c("O0","O1","O2"),include.lowest = TRUE)

	# à la main (pb de quantiles)
vispremv[,"moyrvq"]<-cut(vispremv$MOYRVL,breaks=quantile(vispremv[,"MOYRVL"], probs = seq(0, 1, 1/3)),
      labels = c("M0","M1","M2"),include.lowest = TRUE)

vispremv[,"dmvtpq"]<-cut(vispremv$DMVTPL,breaks=quantile(vispremv[,"DMVTPL"], probs = seq(0, 1, 1/3)),
      labels = c("D0","D1","D2"),include.lowest = TRUE)

vispremv[,"boppnq"]<-cut(vispremv$BOPPNL,breaks=quantile(vispremv[,"BOPPNL"], probs = seq(0, 1, 1/3)),
      labels = c("B0","B1","B2"),include.lowest = TRUE)
vispremv[,"jnbjdq"]<-cut(vispremv$JNBJDL,
      breaks=c(0,0.0001,2.8,5),
      labels = c("J0","J1","J2"),include.lowest = TRUE)

	# à la main (pb de quantiles)
vispremv[,"itavcq"]<-cut(vispremv$ITAVCL,breaks=quantile(vispremv[,"ITAVCL"], probs = seq(0, 1, 1/3)),
      labels = c("I0","I1","I2"),include.lowest = TRUE)

#-- Enregistrement de la table      
# Réorganisation de la table
# la table vispremv aura la forme suivante : 
      # les variables quantitatives
      # les variables qualitatives (d'origine et transformées)
      # la variable CARVP
# var<-c(names(vispremv)[33:35],names(vispremv)[4:31],
#      names(vispremv)[1:3],names(vispremv)[36:56],
#      names(vispremv)[32])       

var<-c(names(vispremv)[33:34],names(vispremv)[4:31], # numeric/quantities var
      names(vispremv)[1:3],names(vispremv)[35:55], # qualitative var (factor, and numeric -. factors)
      names(vispremv)[32]) # CARVP Var   
vispremv<-vispremv[,var]    

write.table(vispremv,"vispremv.txt")

