
rm(list=ls())

#path<-'/media/hellgate/16D7D0A761E2DB6A/Documentos/ImpactaClima/Atendimentos/AdaptaBrasil/ANA/'
setwd('Documentos/AdaptaBrasil/ANA/')

library('sf') ; library('openxlsx') ; library('dplyr')
sf_use_s2(FALSE) # Desativa o uso de geometria esferica

Mode<-function(x) {
	x 	<-x[!is.na(x)]	
	ux	<-sort(unique(x))
  	ux[which.max(tabulate(match(sort(x),ux)))]
}

# Leitura de shapefiles
map_bio<-st_read('/home/usuario/Documentos/Shapefiles/Biomas_250mil/lm_bioma_250.shp')
map0<-st_read('/home/usuario/Documentos/Shapefiles/Br_2019/br_municipios/BR_Municipios_2019.shp')

Bioma<-c('Amazônia','Caatinga','Cerrado','Mata Atlântica','Pampa','Pantanal')

for(i in Bioma){
	
	p1_45_cat_ana <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p1_45_ana_bioma_',i,'_.gpkg'))
	p1_45_cat_085 <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p1_45_085_bioma_',i,'_.gpkg'))
	p1_45_cat_FUL <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p1_45_FUL_bioma_',i,'_.gpkg'))

	p2_45_cat_ana <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p2_45_ana_bioma_',i,'_.gpkg'))
	p2_45_cat_085 <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p2_45_085_bioma_',i,'_.gpkg'))
	p2_45_cat_FUL <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p2_45_FUL_bioma_',i,'_.gpkg'))

	p3_45_cat_ana <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p3_45_ana_bioma_',i,'_.gpkg'))
	p3_45_cat_085 <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p3_45_085_bioma_',i,'_.gpkg'))
	p3_45_cat_FUL <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p3_45_FUL_bioma_',i,'_.gpkg'))

	p1_85_cat_ana <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p1_85_ana_bioma_',i,'_.gpkg'))
	p1_85_cat_085 <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p1_85_085_bioma_',i,'_.gpkg'))
	p1_85_cat_FUL <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p1_85_FUL_bioma_',i,'_.gpkg'))

	p2_85_cat_ana <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p2_85_ana_bioma_',i,'_.gpkg'))
	p2_85_cat_085 <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p2_85_085_bioma_',i,'_.gpkg'))
	p2_85_cat_FUL <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p2_85_FUL_bioma_',i,'_.gpkg'))

	p3_85_cat_ana <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p3_85_ana_bioma_',i,'_.gpkg'))
	p3_85_cat_085 <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p3_85_085_bioma_',i,'_.gpkg'))
	p3_85_cat_FUL <- st_read(paste0('4-Output/rec_bioma2/Vazao2_cat_p3_85_FUL_bioma_',i,'_.gpkg'))	


	bioma<-subset(map_bio,Bioma %in% i)
	crop_bioma<-map0[bioma,crop=T]
	rec_bioma<- crop_bioma %>% select(GEOCOD=CD_MUN,UF=SIGLA_UF)
	rec_df<-rec_bioma %>% st_drop_geometry()
	rec_p0<-rec_bioma
	rec_p0<-st_transform(rec_p0,crs=st_crs(p1_45_cat_ana))

	source('3-Scripts/script_2.1_convert_linha_municipio.R')


	for(j in 1:nrow(rec_df)){ # START FOR INDEXAÇÃO
		tmp1<-p1_45_ana[which(p1_45_ana[,1]==rec_df[j,1]),-c(1:3)]
		tmp2<-p2_45_ana[which(p2_45_ana[,1]==rec_df[j,1]),-c(1:3)]
		tmp3<-p3_45_ana[which(p3_45_ana[,1]==rec_df[j,1]),-c(1:3)]

		tmp4<-p1_85_ana[which(p1_85_ana[,1]==rec_df[j,1]),-c(1:3)]
		tmp5<-p2_85_ana[which(p2_85_ana[,1]==rec_df[j,1]),-c(1:3)]
		tmp6<-p3_85_ana[which(p3_85_ana[,1]==rec_df[j,1]),-c(1:3)]

		{if(NROW(tmp1)<10)
			{rec_p1_45_ana[j,-c(1:2)]<-apply(tmp1,2,FUN='median')
			rec_p2_45_ana[j,-c(1:2)]<-apply(tmp2,2,FUN='median')
			rec_p3_45_ana[j,-c(1:2)]<-apply(tmp3,2,FUN='median')

			rec_p1_85_ana[j,-c(1:2)]<-apply(tmp4,2,FUN='median')
			rec_p2_85_ana[j,-c(1:2)]<-apply(tmp5,2,FUN='median')
			rec_p3_85_ana[j,-c(1:2)]<-apply(tmp6,2,FUN='median')}
		}
		{if(NROW(tmp1)>=10)
			{rec_p1_45_ana[j,-c(1:2)]<-apply(tmp1,2,FUN=Mode)
			rec_p2_45_ana[j,-c(1:2)]<-apply(tmp2,2,FUN=Mode)
			rec_p3_45_ana[j,-c(1:2)]<-apply(tmp3,2,FUN=Mode)

			rec_p1_85_ana[j,-c(1:2)]<-apply(tmp4,2,FUN=Mode)
			rec_p2_85_ana[j,-c(1:2)]<-apply(tmp5,2,FUN=Mode)
			rec_p3_85_ana[j,-c(1:2)]<-apply(tmp6,2,FUN=Mode)}
		}
	} # END FOR INDEXAÇÂO

	for(j in 1:nrow(rec_df)){ # START FOR INDEXAÇÃO
		tmp1<-p1_45_085[which(p1_45_085[,1]==rec_df[j,1]),-c(1:3)]
		tmp2<-p2_45_085[which(p2_45_085[,1]==rec_df[j,1]),-c(1:3)]
		tmp3<-p3_45_085[which(p3_45_085[,1]==rec_df[j,1]),-c(1:3)]

		tmp4<-p1_85_085[which(p1_85_085[,1]==rec_df[j,1]),-c(1:3)]
		tmp5<-p2_85_085[which(p2_85_085[,1]==rec_df[j,1]),-c(1:3)]
		tmp6<-p3_85_085[which(p3_85_085[,1]==rec_df[j,1]),-c(1:3)]

		{if(NROW(tmp1)<10)
			{rec_p1_45_085[j,-c(1:2)]<-apply(tmp1,2,FUN='median')
			rec_p2_45_085[j,-c(1:2)]<-apply(tmp2,2,FUN='median')
			rec_p3_45_085[j,-c(1:2)]<-apply(tmp3,2,FUN='median')

			rec_p1_85_085[j,-c(1:2)]<-apply(tmp4,2,FUN='median')
			rec_p2_85_085[j,-c(1:2)]<-apply(tmp5,2,FUN='median')
			rec_p3_85_085[j,-c(1:2)]<-apply(tmp6,2,FUN='median')}
		}
		{if(NROW(tmp1)>=10)
			{rec_p1_45_085[j,-c(1:2)]<-apply(tmp1,2,FUN=Mode)
			rec_p2_45_085[j,-c(1:2)]<-apply(tmp2,2,FUN=Mode)
			rec_p3_45_085[j,-c(1:2)]<-apply(tmp3,2,FUN=Mode)

			rec_p1_85_085[j,-c(1:2)]<-apply(tmp4,2,FUN=Mode)
			rec_p2_85_085[j,-c(1:2)]<-apply(tmp5,2,FUN=Mode)
			rec_p3_85_085[j,-c(1:2)]<-apply(tmp6,2,FUN=Mode)}
		}
	} # END FOR INDEXAÇÂO

	for(j in 1:nrow(rec_df)){ # START FOR INDEXAÇÃO
		tmp1<-p1_45_FUL[which(p1_45_FUL[,1]==rec_df[j,1]),-c(1:3)]
		tmp2<-p2_45_FUL[which(p2_45_FUL[,1]==rec_df[j,1]),-c(1:3)]
		tmp3<-p3_45_FUL[which(p3_45_FUL[,1]==rec_df[j,1]),-c(1:3)]

		tmp4<-p1_85_FUL[which(p1_85_FUL[,1]==rec_df[j,1]),-c(1:3)]
		tmp5<-p2_85_FUL[which(p2_85_FUL[,1]==rec_df[j,1]),-c(1:3)]
		tmp6<-p3_85_FUL[which(p3_85_FUL[,1]==rec_df[j,1]),-c(1:3)]

		{if(NROW(tmp1)<10)
			{rec_p1_45_FUL[j,-c(1:2)]<-apply(tmp1,2,FUN='median')
			rec_p2_45_FUL[j,-c(1:2)]<-apply(tmp2,2,FUN='median')
			rec_p3_45_FUL[j,-c(1:2)]<-apply(tmp3,2,FUN='median')

			rec_p1_85_FUL[j,-c(1:2)]<-apply(tmp4,2,FUN='median')
			rec_p2_85_FUL[j,-c(1:2)]<-apply(tmp5,2,FUN='median')
			rec_p3_85_FUL[j,-c(1:2)]<-apply(tmp6,2,FUN='median')}
		}
		{if(NROW(tmp1)>=10)
			{rec_p1_45_FUL[j,-c(1:2)]<-apply(tmp1,2,FUN=Mode)
			rec_p2_45_FUL[j,-c(1:2)]<-apply(tmp2,2,FUN=Mode)
			rec_p3_45_FUL[j,-c(1:2)]<-apply(tmp3,2,FUN=Mode)

			rec_p1_85_FUL[j,-c(1:2)]<-apply(tmp4,2,FUN=Mode)
			rec_p2_85_FUL[j,-c(1:2)]<-apply(tmp5,2,FUN=Mode)
			rec_p3_85_FUL[j,-c(1:2)]<-apply(tmp6,2,FUN=Mode)}
		}
	} # END FOR INDEXAÇÂO


	write.table(rec_p1_45_ana,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p1_ssp245_ana_new.csv'),sep=';',quote=F,col.names=T,row.names=F)
	write.table(rec_p1_45_085,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p1_ssp245_085_new.csv'),sep=';',quote=F,col.names=T,row.names=F)
	write.table(rec_p1_45_FUL,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p1_ssp245_FUL_new.csv'),sep=';',quote=F,col.names=T,row.names=F)

	write.table(rec_p2_45_ana,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p2_ssp245_ana_new.csv'),sep=';',quote=F,col.names=T,row.names=F)
	write.table(rec_p2_45_085,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p2_ssp245_085_new.csv'),sep=';',quote=F,col.names=T,row.names=F)
	write.table(rec_p2_45_FUL,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p2_ssp245_FUL_new.csv'),sep=';',quote=F,col.names=T,row.names=F)

	write.table(rec_p3_45_ana,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p3_ssp245_ana_new.csv'),sep=';',quote=F,col.names=T,row.names=F)
	write.table(rec_p3_45_085,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p3_ssp245_085_new.csv'),sep=';',quote=F,col.names=T,row.names=F)
	write.table(rec_p3_45_FUL,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p3_ssp245_FUL_new.csv'),sep=';',quote=F,col.names=T,row.names=F)

	write.table(rec_p1_85_ana,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p1_ssp585_ana_new.csv'),sep=';',quote=F,col.names=T,row.names=F)
	write.table(rec_p1_85_085,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p1_ssp585_085_new.csv'),sep=';',quote=F,col.names=T,row.names=F)
	write.table(rec_p1_85_FUL,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p1_ssp585_FUL_new.csv'),sep=';',quote=F,col.names=T,row.names=F)

	write.table(rec_p2_85_ana,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p2_ssp585_ana_new.csv'),sep=';',quote=F,col.names=T,row.names=F)
	write.table(rec_p2_85_085,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p2_ssp585_085_new.csv'),sep=';',quote=F,col.names=T,row.names=F)
	write.table(rec_p2_85_FUL,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p2_ssp585_FUL_new.csv'),sep=';',quote=F,col.names=T,row.names=F)

	write.table(rec_p3_85_ana,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p3_ssp585_ana_new.csv'),sep=';',quote=F,col.names=T,row.names=F)
	write.table(rec_p3_85_085,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p3_ssp585_085_new.csv'),sep=';',quote=F,col.names=T,row.names=F)
	write.table(rec_p3_85_FUL,file=paste0('4-Output/rec_bioma2/Municipalizado2/rec_',i,'_p3_ssp585_FUL_new.csv'),sep=';',quote=F,col.names=T,row.names=F)
	
}
