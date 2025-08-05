
rm(list=ls())

setwd('Documentos/AdaptaBrasil/ANA/')

#Bibliotecas
library('sf') ; sf_use_s2(FALSE) # Desativa o uso de geometria esferica
library('openxlsx')
library('dplyr')

# Funções

fun_cat<-function(X,Y1,Y2,W1,W2){
	# X: 	objeto sf
	# Y1:	labels modelos
	# Y2: 	labels referencia
	# W1:	classes modelos
	# W2: 	classes referencia

	X1<- X %>% st_drop_geometry()
	X2<-X1 ; X3<-X1 ; X4<-X1 ; X5<-X
	# Categorizando
	for(i in 2:(ncol(X1)-1)){
		X2[,i]<-as.numeric(as.character(cut(X1[,i],breaks=W1,include.lowest=TRUE,right=FALSE,labels=Y1)))
	}
	X2[,ncol(X1)]<-as.numeric(as.character(cut(X1[,ncol(X1)],breaks=W2,include.lowest=TRUE,right=FALSE,labels=Y2)))

	#for(i in 2:ncol(X2)){ X2[which(X2[,i]==6),i]<-0 }

	# Calculando Indice
	for(i in 2:(ncol(X1)-1)){ X3[,i]<-X2[,i]+X2[,ncol(X2)] }
	
	X3[,ncol(X1)]<-X2[,ncol(X1)]

	break_end<-c(0,2,4,6,8,11)

	X5$GFDL_ESM4		<-X3[,2]
	X5$INM_CM5		<-X3[,3]
	X5$MPI_ESM1_2_HR 	<-X3[,4]
	X5$MRI_ESM2_0 		<-X3[,5]
	X5$NORESM2_MM 		<-X3[,6]
	X5$OBS 			<-X3[,7]
		
	return(X5)
}


Mode<-function(x) {
	x 	<-x[!is.na(x)]	
	ux	<-sort(unique(x))
  	ux[which.max(tabulate(match(sort(x),ux)))]
}

# Leitura de shapefiles
map_bio<-st_read('/home/usuario/Documentos/Shapefiles/Biomas_250mil/lm_bioma_250.shp')
map0<-st_read('/home/usuario/Documentos/Shapefiles/Br_2019/br_municipios/BR_Municipios_2019.shp')

# Leitura do dado de referencia

dado_obs<-read.csv('2-Input/geopackages_adaptabrasil/bho_baseline_1980_2014_estudo_mc_ana.csv',header=T,sep=',',dec='.')

dado_obs$Qmlt_calc<-dado_obs$dispq95/dado_obs$relq95qmlt
dado_obs$Qmlt_calc<-ifelse(dado_obs$relq95qmlt==0 & !is.na(dado_obs$relq95qmlt),0,dado_obs$Qmlt_calc)

# Leitura dos dados de modelo

input_ref<-st_read('2-Input/geopackages_adaptabrasil/bho_budyko_mc_periodo1_deltaqrel_ssp245.gpkg')%>% 
			select(cobacia=cobacia)

# Indexar Qmlt aos trechos de rio
tmp_ref<-input_ref %>% st_drop_geometry()
tmp_ref[,1]<-as.numeric(tmp_ref[,1])
tmp_ref<-left_join(tmp_ref,dado_obs[,c(1,12)],by='cobacia')
tmp_ref[,2]<-as.numeric(tmp_ref[,2])
input_ref$Qmlt_obs<-tmp_ref[,2]

# --------------------- ------------- ----------
input_p1_ssp245<-st_read('2-Input/geopackages_adaptabrasil/bho_budyko_mc_periodo1_deltaqrel_ssp245.gpkg')%>% 
			select(COBACIA=cobacia,
			GFDL_ESM4=gfdl_esm4,
			INM_CM5=inm_cm5_0,
			MPI_ESM1_2_HR=mpi_esm1_2_hr,
			MRI_ESM2_0=mri_esm2_0,
			NORESM2_MM=noresm2_mm) 

input_p2_ssp245<-st_read('2-Input/geopackages_adaptabrasil/bho_budyko_mc_periodo2_deltaqrel_ssp245.gpkg')%>% 
			select(COBACIA=cobacia,
			GFDL_ESM4=gfdl_esm4,
			INM_CM5=inm_cm5_0,
			MPI_ESM1_2_HR=mpi_esm1_2_hr,
			MRI_ESM2_0=mri_esm2_0,
			NORESM2_MM=noresm2_mm)

input_p3_ssp245<-st_read('2-Input/geopackages_adaptabrasil/bho_budyko_mc_periodo3_deltaqrel_ssp245.gpkg')%>% 
			select(COBACIA=cobacia,
			GFDL_ESM4=gfdl_esm4,
			INM_CM5=inm_cm5_0,
			MPI_ESM1_2_HR=mpi_esm1_2_hr,
			MRI_ESM2_0=mri_esm2_0,
			NORESM2_MM=noresm2_mm)

input_p1_ssp585<-st_read('2-Input/geopackages_adaptabrasil/bho_budyko_mc_periodo1_deltaqrel_ssp585.gpkg')%>% 
			select(COBACIA=cobacia,
			GFDL_ESM4=gfdl_esm4,
			INM_CM5=inm_cm5_0,
			MPI_ESM1_2_HR=mpi_esm1_2_hr,
			MRI_ESM2_0=mri_esm2_0,
			NORESM2_MM=noresm2_mm) 

input_p2_ssp585<-st_read('2-Input/geopackages_adaptabrasil/bho_budyko_mc_periodo2_deltaqrel_ssp585.gpkg')%>% 
			select(COBACIA=cobacia,
			GFDL_ESM4=gfdl_esm4,
			INM_CM5=inm_cm5_0,
			MPI_ESM1_2_HR=mpi_esm1_2_hr,
			MRI_ESM2_0=mri_esm2_0,
			NORESM2_MM=noresm2_mm) 

input_p3_ssp585<-st_read('2-Input/geopackages_adaptabrasil/bho_budyko_mc_periodo3_deltaqrel_ssp585.gpkg')%>% 
			select(COBACIA=cobacia,
			GFDL_ESM4=gfdl_esm4,
			INM_CM5=inm_cm5_0,
			MPI_ESM1_2_HR=mpi_esm1_2_hr,
			MRI_ESM2_0=mri_esm2_0,
			NORESM2_MM=noresm2_mm) 

lab_mod<-c(6:0)
lab_ana<-c(6:1)
lab_obs<-c(5:1)

lim_bioma_obs<-data.frame(ID=seq(0,1,by=0.2),AMZ=NA,CTG=NA,CRD=NA,MAT=NA,PMP=NA,PTN=NA)

# Recortar a area  de interesse
Bioma<-c('Amazônia','Caatinga','Cerrado','Mata Atlântica','Pampa','Pantanal')

for(i in Bioma){
	out_lim<-data.frame(ID=c('Min','P0','P125','P25','P375','P5',0,'Max'),Lim_ANA=NA,Lim_85=NA,Lim_FU=NA)
	bioma_sel<-subset(map_bio,Bioma %in% i)
	rec_obs<-input_ref[bioma_sel,crop=T]
	break_obs<-round(quantile(unique(rec_obs$Qmlt_obs),probs=seq(0,1,by=0.2),na.rm=T),5)
	lim_bioma_obs[,(which(Bioma==i)+1)]<-break_obs
	rec_p1_ssp45<-input_p1_ssp245[bioma_sel,crop=T]
	rec_p2_ssp45<-input_p2_ssp245[bioma_sel,crop=T]
	rec_p3_ssp45<-input_p3_ssp245[bioma_sel,crop=T]
	rec_p1_ssp85<-input_p1_ssp585[bioma_sel,crop=T]
	rec_p2_ssp85<-input_p2_ssp585[bioma_sel,crop=T]
	rec_p3_ssp85<-input_p3_ssp585[bioma_sel,crop=T]
	rec_p1_ssp45$OBS<-rec_obs$Qmlt_obs ; rec_p2_ssp45$OBS<-rec_obs$Qmlt_obs
	rec_p3_ssp45$OBS<-rec_obs$Qmlt_obs ; rec_p1_ssp85$OBS<-rec_obs$Qmlt_obs
	rec_p2_ssp85$OBS<-rec_obs$Qmlt_obs ; rec_p3_ssp85$OBS<-rec_obs$Qmlt_obs
	SSP45<-as.numeric(unlist(rec_p1_ssp45[,2:6]),unlist(rec_p2_ssp45[,2:6]),unlist(rec_p3_ssp45[,2:6]))
	SSP85<-as.numeric(unlist(rec_p1_ssp85[,2:6]),unlist(rec_p2_ssp85[,2:6]),unlist(rec_p3_ssp85[,2:6]))
	SSPFU<-c(SSP45,SSP85)
	# Identificar os limiares de corte
	break_ana<-c(min(SSPFU,na.rm=T),rev(seq(0,-0.2,length.out=5)),max(SSPFU,na.rm=T))
	break_085<-round(c(min(SSP85,na.rm=T),as.numeric(quantile(SSP85[which(SSP85>=(-0.2) & SSP85<0)],probs=seq(0,0.5,length.out=5),na.rm=T)),0,max(SSP85,na.rm=T)),3)
	break_FUL<-round(c(min(SSPFU,na.rm=T),as.numeric(quantile(SSPFU[which(SSPFU>=(-0.2) & SSPFU<0)],probs=seq(0,0.5,length.out=5),na.rm=T)),0,max(SSPFU,na.rm=T)),3)
	out_lim[,3]<-break_085 ; out_lim[,4]<-break_FUL ; out_lim[-7,2]<-break_ana
	write.xlsx(out_lim,file=paste0('4-Output/Limiares_Bioma_',i,'.xlsx'))
	# Classificar os trechos de rio
	# Cenário SSP245 
	p1_45_cat_ana<-fun_cat(rec_p1_ssp45,lab_ana,lab_obs,break_ana,break_obs)
	p1_45_cat_085<-fun_cat(rec_p1_ssp45,lab_mod,lab_obs,break_085,break_obs)
	p1_45_cat_FUL<-fun_cat(rec_p1_ssp45,lab_mod,lab_obs,break_FUL,break_obs)
	p2_45_cat_ana<-fun_cat(rec_p2_ssp45,lab_ana,lab_obs,break_ana,break_obs)
	p2_45_cat_085<-fun_cat(rec_p2_ssp45,lab_mod,lab_obs,break_085,break_obs)
	p2_45_cat_FUL<-fun_cat(rec_p2_ssp45,lab_mod,lab_obs,break_FUL,break_obs)
	p3_45_cat_ana<-fun_cat(rec_p3_ssp45,lab_ana,lab_obs,break_ana,break_obs)
	p3_45_cat_085<-fun_cat(rec_p3_ssp45,lab_mod,lab_obs,break_085,break_obs)
	p3_45_cat_FUL<-fun_cat(rec_p3_ssp45,lab_mod,lab_obs,break_FUL,break_obs)
	# Cenário SSP585
	p1_85_cat_ana<-fun_cat(rec_p1_ssp85,lab_ana,lab_obs,break_ana,break_obs)
	p1_85_cat_085<-fun_cat(rec_p1_ssp85,lab_mod,lab_obs,break_085,break_obs)
	p1_85_cat_FUL<-fun_cat(rec_p1_ssp85,lab_mod,lab_obs,break_FUL,break_obs)
	p2_85_cat_ana<-fun_cat(rec_p2_ssp85,lab_ana,lab_obs,break_ana,break_obs)
	p2_85_cat_085<-fun_cat(rec_p2_ssp85,lab_mod,lab_obs,break_085,break_obs)
	p2_85_cat_FUL<-fun_cat(rec_p2_ssp85,lab_mod,lab_obs,break_FUL,break_obs)
	p3_85_cat_ana<-fun_cat(rec_p3_ssp85,lab_ana,lab_obs,break_ana,break_obs)
	p3_85_cat_085<-fun_cat(rec_p3_ssp85,lab_mod,lab_obs,break_085,break_obs)
	p3_85_cat_FUL<-fun_cat(rec_p3_ssp85,lab_mod,lab_obs,break_FUL,break_obs)
	# Equalizar os trechos de rio com a matriz de correspondência
	p1_45_cat_ana %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p1_45_ana_bioma_',i,'_.gpkg'))
	p1_45_cat_085 %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p1_45_085_bioma_',i,'_.gpkg'))
	p1_45_cat_FUL %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p1_45_FUL_bioma_',i,'_.gpkg'))
	p2_45_cat_ana %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p2_45_ana_bioma_',i,'_.gpkg'))
	p2_45_cat_085 %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p2_45_085_bioma_',i,'_.gpkg'))
	p2_45_cat_FUL %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p2_45_FUL_bioma_',i,'_.gpkg'))
	p3_45_cat_ana %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p3_45_ana_bioma_',i,'_.gpkg'))
	p3_45_cat_085 %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p3_45_085_bioma_',i,'_.gpkg'))
	p3_45_cat_FUL %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p3_45_FUL_bioma_',i,'_.gpkg'))
	p1_85_cat_ana %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p1_85_ana_bioma_',i,'_.gpkg'))
	p1_85_cat_085 %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p1_85_085_bioma_',i,'_.gpkg'))
	p1_85_cat_FUL %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p1_85_FUL_bioma_',i,'_.gpkg'))
	p2_85_cat_ana %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p2_85_ana_bioma_',i,'_.gpkg'))
	p2_85_cat_085 %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p2_85_085_bioma_',i,'_.gpkg'))
	p2_85_cat_FUL %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p2_85_FUL_bioma_',i,'_.gpkg'))
	p3_85_cat_ana %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p3_85_ana_bioma_',i,'_.gpkg'))
	p3_85_cat_085 %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p3_85_085_bioma_',i,'_.gpkg'))
	p3_85_cat_FUL %>% st_write(paste0('4-Output/rec_bioma2/Vazao2_cat_p3_85_FUL_bioma_',i,'_.gpkg'))	
	# 
	MyColours<-c('darkgreen','green2','khaki1','darkorange','red')	
	classes<-c('Muito Baixo','Baixo','Moderado','Alto','Muito Alto')
	MODELS<-c('GFDL_ESM4','INM_CM5','MPI_ESM1_2_HR','MRI_ESM2_0','NORESM2_MM')
	for(j in 2:6){
		A1<-p1_45_cat_ana %>% st_drop_geometry()
		name1<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias ANA - P1 - SSP245')
		File1<-paste0('5-Figures/ANA/P1_45_cat_ana_',MODELS[(j-1)],'_',i,'.png')
		png(File1,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p1_45_cat_ana[,j],col=MyColours[findInterval(A1[,j],c(1:6),all.inside=TRUE)],main=name1)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
		A2<-p1_45_cat_085 %>% st_drop_geometry()
		name2<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias 85 - P1 - SSP245')
		File2<-paste0('5-Figures/85/P1_45_cat_085_',MODELS[(j-1)],'_',i,'.png')
		png(File2,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p1_45_cat_085[,j],col=MyColours[findInterval(A2[,j],c(1:6),all.inside=TRUE)],main=name2)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
		A3<-p1_45_cat_FUL %>% st_drop_geometry()
		name3<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias FULL - P1 - SSP245')
		File3<-paste0('5-Figures/FUL/P1_45_cat_FUL_',MODELS[(j-1)],'_',i,'.png')
		png(File3,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p1_45_cat_FUL[,j],col=MyColours[findInterval(A3[,j],c(1:6),all.inside=TRUE)],main=name3)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
		B1<-p1_85_cat_ana %>% st_drop_geometry()
		name1<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias ANA - P1 - SSP585')
		File1<-paste0('5-Figures/ANA/P1_85_cat_ana_',MODELS[(j-1)],'_',i,'.png')
		png(File1,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p1_85_cat_ana[,j],col=MyColours[findInterval(B1[,j],c(1:6),all.inside=TRUE)],main=name1)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
		B2<-p1_85_cat_085 %>% st_drop_geometry()
		name2<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias 85 - P1 - SSP585')
		File2<-paste0('5-Figures/85/P1_85_cat_085_',MODELS[(j-1)],'_',i,'.png')
		png(File2,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p1_85_cat_085[,j],col=MyColours[findInterval(B2[,j],c(1:6),all.inside=TRUE)],main=name2)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
		B3<-p1_85_cat_FUL %>% st_drop_geometry()
		name3<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias FULL - P1 - SSP585')
		File3<-paste0('5-Figures/FUL/P1_85_cat_FUL_',MODELS[(j-1)],'_',i,'.png')
		png(File3,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p1_85_cat_FUL[,j],col=MyColours[findInterval(B3[,j],c(1:6),all.inside=TRUE)],main=name3)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
	}
	for(j in 2:6){
		A1<-p2_45_cat_ana %>% st_drop_geometry()
		name1<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias ANA - P2 - SSP245')
		File1<-paste0('5-Figures/ANA/P2_45_cat_ana_',MODELS[(j-1)],'_',i,'.png')
		png(File1,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p2_45_cat_ana[,j],col=MyColours[findInterval(A1[,j],c(1:6),all.inside=TRUE)],main=name1)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
		A2<-p2_45_cat_085 %>% st_drop_geometry()
		name2<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias 85 - P2 - SSP245')
		File2<-paste0('5-Figures/85/P2_45_cat_085_',MODELS[(j-1)],'_',i,'.png')
		png(File2,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p2_45_cat_085[,j],col=MyColours[findInterval(A2[,j],c(1:6),all.inside=TRUE)],main=name2)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
		A3<-p2_45_cat_FUL %>% st_drop_geometry()
		name3<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias FULL - P2 - SSP245')
		File3<-paste0('5-Figures/FUL/P2_45_cat_FUL_',MODELS[(j-1)],'_',i,'.png')
		png(File3,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p2_45_cat_FUL[,j],col=MyColours[findInterval(A3[,j],c(1:6),all.inside=TRUE)],main=name3)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
		B1<-p2_85_cat_ana %>% st_drop_geometry()
		name1<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias ANA - P2 - SSP585')
		File1<-paste0('5-Figures/ANA/P2_85_cat_ana_',MODELS[(j-1)],'_',i,'.png')
		png(File1,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p2_85_cat_ana[,j],col=MyColours[findInterval(B1[,j],c(1:6),all.inside=TRUE)],main=name1)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
		B2<-p2_85_cat_085 %>% st_drop_geometry()
		name2<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias 85 - P2 - SSP585')
		File2<-paste0('5-Figures/85/P2_85_cat_085_',MODELS[(j-1)],'_',i,'.png')
		png(File2,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p2_85_cat_085[,j],col=MyColours[findInterval(B2[,j],c(1:6),all.inside=TRUE)],main=name2)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
		B3<-p2_85_cat_FUL %>% st_drop_geometry()
		name3<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias FULL - P2 - SSP585')
		File3<-paste0('5-Figures/FUL/P2_85_cat_FUL_',MODELS[(j-1)],'_',i,'.png')
		png(File3,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p2_85_cat_FUL[,j],col=MyColours[findInterval(B3[,j],c(1:6),all.inside=TRUE)],main=name3)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
	}
	for(j in 2:6){
		A1<-p3_45_cat_ana %>% st_drop_geometry()
		name1<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias ANA - P3 - SSP245')
		File1<-paste0('5-Figures/ANA/P3_45_cat_ana_',MODELS[(j-1)],'_',i,'.png')
		png(File1,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p3_45_cat_ana[,j],col=MyColours[findInterval(A1[,j],c(1:6),all.inside=TRUE)],main=name1)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
		A2<-p3_45_cat_085 %>% st_drop_geometry()
		name2<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias 85 - P3 - SSP245')
		File2<-paste0('5-Figures/85/P3_45_cat_085_',MODELS[(j-1)],'_',i,'.png')
		png(File2,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p3_45_cat_085[,j],col=MyColours[findInterval(A2[,j],c(1:6),all.inside=TRUE)],main=name2)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
		A3<-p3_45_cat_FUL %>% st_drop_geometry()
		name3<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias FULL - P3 - SSP245')
		File3<-paste0('5-Figures/FUL/P3_45_cat_FUL_',MODELS[(j-1)],'_',i,'.png')
		png(File3,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p3_45_cat_FUL[,j],col=MyColours[findInterval(A3[,j],c(1:6),all.inside=TRUE)],main=name3)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
		B1<-p3_85_cat_ana %>% st_drop_geometry()
		name1<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias ANA - P3 - SSP585')
		File1<-paste0('5-Figures/ANA/P3_85_cat_ana_',MODELS[(j-1)],'_',i,'.png')
		png(File1,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p3_85_cat_ana[,j],col=MyColours[findInterval(B1[,j],c(1:6),all.inside=TRUE)],main=name1)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
		B2<-p3_85_cat_085 %>% st_drop_geometry()
		name2<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias 85 - P3 - SSP585')
		File2<-paste0('5-Figures/85/P3_85_cat_085_',MODELS[(j-1)],'_',i,'.png')
		png(File2,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p3_85_cat_085[,j],col=MyColours[findInterval(B2[,j],c(1:6),all.inside=TRUE)],main=name2)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
		B3<-p3_85_cat_FUL %>% st_drop_geometry()
		name3<-paste0('Vazão modelo ',MODELS[(j-1)],' Categorias FULL - P3 - SSP585')
		File3<-paste0('5-Figures/FUL/P3_85_cat_FUL_',MODELS[(j-1)],'_',i,'.png')
		png(File3,width=(5*480),height=(5*480),type='cairo',res=400)
		par(mar=c(1,1,8,1),mai=c(.01,0.01,0.1,0.01),oma=c(.1,.1,0.1,.1))
		plot(p3_85_cat_FUL[,j],col=MyColours[findInterval(B3[,j],c(1:6),all.inside=TRUE)],main=name3)
		legend('bottomleft',title='Classes',legend=classes,border=MyColours,fill=MyColours,bty="n",cex=1)
		dev.off()
	}
}

write.xlsx(lim_bioma_obs,file=paste0('4-Output/Limiares_Bioma_observação_20241016.xlsx'))


