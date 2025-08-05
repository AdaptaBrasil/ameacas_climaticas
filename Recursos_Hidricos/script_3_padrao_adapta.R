
rm(list=ls())

setwd('/media/hellgate/16D7D0A761E2DB6A/Documentos/ImpactaClima/Atendimentos/AdaptaBrasil/ANA/')

library('rgdal') ; library('openxlsx') ; library('dplyr')

map0<-readOGR('/media/hellgate/16D7D0A761E2DB6A/MEGA/Shapefiles/Br_2019/br_municipios/BR_Municipios_2019.shp')
map_bac<-readOGR('/media/hellgate/16D7D0A761E2DB6A/MEGA/Shapefiles/Br_2019/br_unidades_da_federacao/BR_UF_2019.shp')

files<-dir('4-Output/rec_bioma/Municipalizado2/')

files_new<-files[grep('new',files)]

files_245<-files_new[grep('ssp245',files_new)]
files_585<-files_new[grep('ssp585',files_new)]

map_p1_245<-map_p2_245<-map_p3_245<-map0
map_p1_585<-map_p2_585<-map_p3_585<-map0

map_p1_245@data[,5:10]<-map_p2_245@data[,5:10]<-map_p3_245@data[,5:10]<-NA
map_p1_585@data[,5:10]<-map_p2_585@data[,5:10]<-map_p3_585@data[,5:10]<-NA

colnames(map_p1_245@data)<-colnames(map_p2_245@data)<-c(colnames(map0@data),'GFDL_ESM4','INM_CM5','MPI_ESM1_2_HR','MRI_ESM2_0','NORESM2_MM','OBS')
colnames(map_p3_245@data)<-colnames(map_p1_585@data)<-c(colnames(map0@data),'GFDL_ESM4','INM_CM5','MPI_ESM1_2_HR','MRI_ESM2_0','NORESM2_MM','OBS')
colnames(map_p2_585@data)<-colnames(map_p3_585@data)<-c(colnames(map0@data),'GFDL_ESM4','INM_CM5','MPI_ESM1_2_HR','MRI_ESM2_0','NORESM2_MM','OBS')

Make.map<-function(X1,X2,INFO,WIDTH,HEIGHT,RES,TITLE,PRINT=0,MAP=0){
    # X1:       Objeto shapefile com informacoes
    # X2:       Objeto shapefile vazio
    # INFO:     Data Frame contendo duas colunas.
    # WIDTH:    Multiplicador do comprimento/largura
    # HEIGHT:   Multiplicador da altura
    # RES:      numero de DPI

    output<-getwd()

    subDir1<-paste0(output,'/4-Output/Resultado/',TITLE,'/tmp/')
        
    ifelse(!dir.exists(file.path(output,subDir1)),dir.create(file.path(subDir1),showWarnings=FALSE,recursive=TRUE), FALSE)

    col.empty<-4 ; col.val<-ncol(X1@data)
    
    brks1<-seq(0,1,by=0.2)
    
    COD<-colnames(X1@data)

    #Comp.ind<-unique(nchar(INFO[,1]))

    #TOPX<-ceiling(max(X1@bbox[1,])+(max(X1@bbox[1,])-min(X1@bbox[1,]))*0.5)+1
    TOPX<-(-ceiling(max(X2@bbox[1,])-min(X2@bbox[1,]))) + 4
    TOPY<-as.integer(max(X2@bbox[2,]))
    
    for(i in c(5:(col.val))){

        File<-paste0(subDir1,COD[i],'.png')

        IND<-COD[i]

        #L<-INFO[which(substr(IND,1,Comp.ind)==INFO[,1]),3]
        #L<-INFO[which(substr(IND,1,4)==INFO[,1]),3]
        L<-INFO[IND==INFO[,1],3]
        
        {if(L==1)
            {cols<-c('darkgreen','green2','khaki1','darkorange','red')}
        }

        {if(L==0)
            {cols<-rev(c('darkgreen','green2','khaki1','darkorange','red'))}
        }

        # {if(L==1)
        #     {cols<-c('darkblue','lightblue','khaki1','darkorange','red')}
        # }

        # {if(L==0)
        #     {cols<-rev(c('darkblue','lightblue','khaki1','darkorange','red'))}
        # }        
        
        classes<-c('NO DATA','Muito Baixo','Baixo','Moderado','Alto','Muito Alto')
        png(File,width=(WIDTH*480),height=(HEIGHT*480),type='cairo',res=RES)
        par(mar=c(1,1,1,1),mai=c(.01,0.01,0.01,0.01),oma=c(.1,.1,.1,.1))
        plot(X2, col='gray',border=NA,main=' ')
        plot(X1, col=cols[findInterval(X1@data[,i],brks1,all.inside=TRUE)],border=NA,add=T)
        legend('bottomleft',title='Classes',legend=classes,border=c('gray',cols),fill=c('gray',cols),bty="n",cex=0.75)
        if(PRINT==1) {text(TOPX,TOPY,COD[i],cex=0.5)}
        if(MAP==1) {plot(X2,add=T,lwd=0.25)}
        dev.off()
    }
}

Norma_Categoria<-function(X,Y){
    # X: Vetor de dados
    # Y: Vetor de categorias

    B<-ifelse(X>=Y[1] & X<=Y[2] & !is.na(X),1,
        ifelse(X>Y[2] & X<=Y[3] & !is.na(X),2,
        ifelse(X>Y[3] & X<=Y[4] & !is.na(X),3,
        ifelse(X>Y[4] & X<=Y[5] & !is.na(X),4,
        ifelse(X>Y[5] & X<=Y[6] & !is.na(X),5,NA)))))

    Data<-data.frame(Dado=X,Classe=B,Norm=NA)

    Data[,3]<-ifelse(Data[,2]==1,(((Data[,1]-Y[1])*(0.2-0))/(Y[2]-Y[1])),
            ifelse(Data[,2]==2,(((Data[,1]-Y[2])*(0.4-0.2))/(Y[3]-Y[2]))+0.2,
            ifelse(Data[,2]==3,(((Data[,1]-Y[3])*(0.6-0.4))/(Y[4]-Y[3]))+0.4,
            ifelse(Data[,2]==4,(((Data[,1]-Y[4])*(0.8-0.6))/(Y[5]-Y[4]))+0.6,
            ifelse(Data[,2]==5,(((Data[,1]-Y[5])*(1.0-0.8))/(Y[6]-Y[5]))+0.8,NA)))))
    
    A<-Data[,3]
    
    return(A)
} # End Function

Categorias<-c(0,2,4,6,8,11)

descr<-data.frame(ID=c(colnames(map_p1_245@data[,-c(1:4)]),'MEAN','MEDIAN','WORST'),
				Titule=c(colnames(map_p1_245@data[,-c(1:4)]),'MEAN','MEDIAN','WORST'),
				COR=1)

for(i in c('p1','p2','p3')){
	rec_periodo<-files_245[grep(i,files_245)]
	for(j in c('_ana','_085','_FUL')){
		rec_limite<-rec_periodo[grep(j,rec_periodo)]
		input_amz_245<-read.csv(paste0('4-Output/rec_bioma/Municipalizado2/',rec_limite[1]),header=T,sep=';')
		input_ctg_245<-read.csv(paste0('4-Output/rec_bioma/Municipalizado2/',rec_limite[2]),header=T,sep=';')
		input_crd_245<-read.csv(paste0('4-Output/rec_bioma/Municipalizado2/',rec_limite[3]),header=T,sep=';')
		input_mta_245<-read.csv(paste0('4-Output/rec_bioma/Municipalizado2/',rec_limite[4]),header=T,sep=';')
		input_pmp_245<-read.csv(paste0('4-Output/rec_bioma/Municipalizado2/',rec_limite[5]),header=T,sep=';')
		input_ptn_245<-read.csv(paste0('4-Output/rec_bioma/Municipalizado2/',rec_limite[6]),header=T,sep=';')
		
		for(k in 1:nrow(map0@data)){
			pos_amz<-which(map0@data[k,1]==input_amz_245[,1])
			pos_ctg<-which(map0@data[k,1]==input_ctg_245[,1])
			pos_crd<-which(map0@data[k,1]==input_crd_245[,1])
			pos_mta<-which(map0@data[k,1]==input_mta_245[,1])
			pos_pmp<-which(map0@data[k,1]==input_pmp_245[,1])
			pos_ptn<-which(map0@data[k,1]==input_ptn_245[,1])
			
			mat_amz_245<-mat_ctg_245<-mat_crd_245<-matrix(NA,nrow=2,ncol=6)
			mat_mta_245<-mat_pmp_245<-mat_ptn_245<-matrix(NA,nrow=2,ncol=6)

			{if(length(pos_amz)>0)
				{mat_amz_245[1,]<-as.numeric(map_p1_245@data[k,-c(1:4)])
				 mat_amz_245[2,]<-as.numeric(input_amz_245[pos_amz,-c(1:2)])
				 map_p1_245@data[k,-c(1:4)]<-colMeans(mat_amz_245,na.rm=T)}
			}

			{if(length(pos_ctg)>0)
				{mat_ctg_245[1,]<-as.numeric(map_p1_245@data[k,-c(1:4)])
				 mat_ctg_245[2,]<-as.numeric(input_ctg_245[pos_ctg,-c(1:2)])
				 map_p1_245@data[k,-c(1:4)]<-colMeans(mat_ctg_245,na.rm=T)}
			}

			{if(length(pos_crd)>0)
				{mat_crd_245[1,]<-as.numeric(map_p1_245@data[k,-c(1:4)])
				 mat_crd_245[2,]<-as.numeric(input_crd_245[pos_crd,-c(1:2)])
				 map_p1_245@data[k,-c(1:4)]<-colMeans(mat_crd_245,na.rm=T)}
			}

			{if(length(pos_mta)>0)
				{mat_mta_245[1,]<-as.numeric(map_p1_245@data[k,-c(1:4)])
				 mat_mta_245[2,]<-as.numeric(input_mta_245[pos_mta,-c(1:2)])
				 map_p1_245@data[k,-c(1:4)]<-colMeans(mat_mta_245,na.rm=T)}
			}

			{if(length(pos_pmp)>0)
				{mat_pmp_245[1,]<-as.numeric(map_p1_245@data[k,-c(1:4)])
				 mat_pmp_245[2,]<-as.numeric(input_pmp_245[pos_pmp,-c(1:2)])
				 map_p1_245@data[k,-c(1:4)]<-colMeans(mat_pmp_245,na.rm=T)}
			}

			{if(length(pos_ptn)>0)
				{mat_ptn_245[1,]<-as.numeric(map_p1_245@data[k,-c(1:4)])
				 mat_ptn_245[2,]<-as.numeric(input_ptn_245[pos_ptn,-c(1:2)])
				 map_p1_245@data[k,-c(1:4)]<-colMeans(mat_ptn_245,na.rm=T)}
			}

			print(k)
		}

		map_p1_245_value<-map_p1_245

		for(l in 5:ncol(map_p1_245@data)){
			map_p1_245_value@data[,l]<-Norma_Categoria(map_p1_245@data[,l],Categorias)
		}

		map_p1_245_value@data$MEAN<-apply(map_p1_245_value@data[,5:9],1,mean,na.rm=T)
		map_p1_245_value@data$MEDIAN<-apply(map_p1_245_value@data[,5:9],1,median,na.rm=T)
		map_p1_245_value@data$WORST<-apply(map_p1_245_value@data[,5:9],1,max,na.rm=T)
		map_p1_245_value@data[which(is.infinite(map_p1_245_value@data$WORST)),11:13]<-NA

		Make.map(map_p1_245_value,map_bac,descr,5,5,600,paste0('Aval_',i,j,'_245_20241017'),1,1)

		write.xlsx(map_p1_245_value@data[,-4],file=paste0('4-Output/rec_bioma/Brasil/Aval_',i,j,'_245_20241017.xlsx'))
	}
}


for(i in c('p1','p2','p3')){
	rec_periodo<-files_585[grep(i,files_585)]
	for(j in c('_ana','_085','_FUL')){
		rec_limite<-rec_periodo[grep(j,rec_periodo)]
		input_amz_585<-read.csv(paste0('4-Output/rec_bioma/Municipalizado2/',rec_limite[1]),header=T,sep=';')
		input_ctg_585<-read.csv(paste0('4-Output/rec_bioma/Municipalizado2/',rec_limite[2]),header=T,sep=';')
		input_crd_585<-read.csv(paste0('4-Output/rec_bioma/Municipalizado2/',rec_limite[3]),header=T,sep=';')
		input_mta_585<-read.csv(paste0('4-Output/rec_bioma/Municipalizado2/',rec_limite[4]),header=T,sep=';')
		input_pmp_585<-read.csv(paste0('4-Output/rec_bioma/Municipalizado2/',rec_limite[5]),header=T,sep=';')
		input_ptn_585<-read.csv(paste0('4-Output/rec_bioma/Municipalizado2/',rec_limite[6]),header=T,sep=';')
		
		for(k in 1:nrow(map0@data)){
			pos_amz<-which(map0@data[k,1]==input_amz_585[,1])
			pos_ctg<-which(map0@data[k,1]==input_ctg_585[,1])
			pos_crd<-which(map0@data[k,1]==input_crd_585[,1])
			pos_mta<-which(map0@data[k,1]==input_mta_585[,1])
			pos_pmp<-which(map0@data[k,1]==input_pmp_585[,1])
			pos_ptn<-which(map0@data[k,1]==input_ptn_585[,1])
			
			mat_amz_585<-mat_ctg_585<-mat_crd_585<-matrix(NA,nrow=2,ncol=6)
			mat_mta_585<-mat_pmp_585<-mat_ptn_585<-matrix(NA,nrow=2,ncol=6)

			{if(length(pos_amz)>0)
				{mat_amz_585[1,]<-as.numeric(map_p1_585@data[k,-c(1:4)])
				 mat_amz_585[2,]<-as.numeric(input_amz_585[pos_amz,-c(1:2)])
				 map_p1_585@data[k,-c(1:4)]<-colMeans(mat_amz_585,na.rm=T)}
			}

			{if(length(pos_ctg)>0)
				{mat_ctg_585[1,]<-as.numeric(map_p1_585@data[k,-c(1:4)])
				 mat_ctg_585[2,]<-as.numeric(input_ctg_585[pos_ctg,-c(1:2)])
				 map_p1_585@data[k,-c(1:4)]<-colMeans(mat_ctg_585,na.rm=T)}
			}

			{if(length(pos_crd)>0)
				{mat_crd_585[1,]<-as.numeric(map_p1_585@data[k,-c(1:4)])
				 mat_crd_585[2,]<-as.numeric(input_crd_585[pos_crd,-c(1:2)])
				 map_p1_585@data[k,-c(1:4)]<-colMeans(mat_crd_585,na.rm=T)}
			}

			{if(length(pos_mta)>0)
				{mat_mta_585[1,]<-as.numeric(map_p1_585@data[k,-c(1:4)])
				 mat_mta_585[2,]<-as.numeric(input_mta_585[pos_mta,-c(1:2)])
				 map_p1_585@data[k,-c(1:4)]<-colMeans(mat_mta_585,na.rm=T)}
			}

			{if(length(pos_pmp)>0)
				{mat_pmp_585[1,]<-as.numeric(map_p1_585@data[k,-c(1:4)])
				 mat_pmp_585[2,]<-as.numeric(input_pmp_585[pos_pmp,-c(1:2)])
				 map_p1_585@data[k,-c(1:4)]<-colMeans(mat_pmp_585,na.rm=T)}
			}

			{if(length(pos_ptn)>0)
				{mat_ptn_585[1,]<-as.numeric(map_p1_585@data[k,-c(1:4)])
				 mat_ptn_585[2,]<-as.numeric(input_ptn_585[pos_ptn,-c(1:2)])
				 map_p1_585@data[k,-c(1:4)]<-colMeans(mat_ptn_585,na.rm=T)}
			}

			print(k)
		}

		map_p1_585_value<-map_p1_585

		for(l in 5:ncol(map_p1_585@data)){
			map_p1_585_value@data[,l]<-Norma_Categoria(map_p1_585@data[,l],Categorias)
		}

		map_p1_585_value@data$MEAN<-apply(map_p1_585_value@data[,5:9],1,mean,na.rm=T)
		map_p1_585_value@data$MEDIAN<-apply(map_p1_585_value@data[,5:9],1,median,na.rm=T)
		map_p1_585_value@data$WORST<-apply(map_p1_585_value@data[,5:9],1,max,na.rm=T)
		map_p1_585_value@data[which(is.infinite(map_p1_585_value@data$WORST)),11:13]<-NA

		Make.map(map_p1_585_value,map_bac,descr,5,5,600,paste0('Aval_',i,j,'_585_20241017'),1,1)

		write.xlsx(map_p1_585_value@data[,-4],file=paste0('4-Output/rec_bioma/Brasil/Aval_',i,j,'_585_20241017.xlsx'))
	}
}
