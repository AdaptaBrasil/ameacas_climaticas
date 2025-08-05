#-------------------------------------------------------------------#
#																	#
#																	#
#																	#
# Criado: 		01-03-2023											#
# Modificado: 	10-05-2023											#
# R version 4.0.5 (2021-03-31) -- "Shake and Throw" 				#
#-------------------------------------------------------------------#

rm(list=ls())

library('rgdal') ; library('ggplot2')

source('functions.R')

# Leitura de dados

# Planilhas de casos de Leishmaniose e dados climáticos
cas_lta<-read.csv('../2-Input/1-LTA/LTA_incidencia_2001_2005_wide.csv',header=T,sep=';',dec=',')
cli_ind<-read.csv('../2-Input/1-LTA/Ind_Climaticos_Simples_LTA_03_12_2023.csv',header=T,sep=';',dec=',')
cli_bru<-read.csv('../2-Input/1-LTA/Dados_Climaticos_Anuais_LTA.csv',header=T,sep=';',dec='.')
cli_nor<-read.csv('../2-Input/1-LTA/Dados_Climaticos_Anuais_NORM_LTA.csv',header=T,sep=';',dec='.')

# Leitura de Shapefiles

map0 <- readOGR('/media/hellgate/16D7D0A761E2DB6A/MEGA/Shapefiles/Br_2019/br_municipios/BR_Municipios_2019.shp')

map_bac <- readOGR('/media/hellgate/16D7D0A761E2DB6A/MEGA/Shapefiles/Br_uf_5000/uf_5000.shp')


map1<-map2<-map3<-map0

head(cas_lta)

head(cli_ind)

LTA <- as.numeric(c(cas_lta[, 5], cas_lta[, 6], cas_lta[, 7], cas_lta[, 8], cas_lta[, 9]))

# Contagem de municipios com informações válidas em cada ano da amostra
# excluindo valores UNDEF (NA) e zeros

LTA01 <- length(cas_lta[, 5]) - length(which(cas_lta[, 5] == 0 | is.na(cas_lta[, 5]))) # Ano de 2001
LTA02 <- length(cas_lta[, 6]) - length(which(cas_lta[, 6] == 0 | is.na(cas_lta[, 6]))) # Ano de 2002
LTA03 <- length(cas_lta[, 7]) - length(which(cas_lta[, 7] == 0 | is.na(cas_lta[, 7]))) # Ano de 2003
LTA04 <- length(cas_lta[, 8]) - length(which(cas_lta[, 8] == 0 | is.na(cas_lta[, 8]))) # Ano de 2004
LTA05 <- length(cas_lta[, 9]) - length(which(cas_lta[, 9] == 0 | is.na(cas_lta[, 9]))) # Ano de 2005

CLTA <- matrix(NA, ncol = 5, nrow = 1)
dimnames(CLTA)[[1]] <- "Casos"
dimnames(CLTA)[[2]] <- c(2001:2005)
CLTA[1, ] <- c(LTA01, LTA02, LTA03, LTA04, LTA05)

CLTA

# Calculando o perfil mediano do número de casos por município
MED <- function(x) {
    Y <- median(x, na.rm = T)
}

cas_lta[,13] <- apply(cas_lta[, 5:9], 1, MED)

map1@data[,5:9]<-NA

# Inserindo as variaveis climáticas referente ao período histórico e indexando pelo geocódido

for (i in 1:nrow(map0@data)){
    pos1 <- which(map0@data[i, 1] == cas_lta[, 1])
    if(length(pos1)!=0) {map1@data[i, c(5:6)] <- cas_lta[pos1,13]}
    
    pos2 <- which(map0@data[i, 1] == cli_ind[, 1])
    if(length(pos2)!=0) {map1@data[i, c(7:9)] <- cli_ind[pos2,c(3,8,13)]}
}

dimnames(map1@data)[[2]]<-c('CD_MUN','NM_MUN','SIGLA_UF','AREA_KM2','LTA.P','LTA.C','TASMAX','SDII','HURS')

# Aqui foram calculados os quintis, considerando valores acima de zero e 
# diferentes de NA(No Avaliable).
# Esses limiares serão utilizados para a classificação da variável de interesse

QTL <- quantile(map1@data[map1@data[,5]>0,5],probs=c(.2,.4,.6,.8),na.rm=T)

# 20% 40% 60% 80% 
#  1   2   6  18

# Essa classificação indica que valores iguais ou inferiores a 1
# serão considerados como 0 e os superiores a 1.
# Isso será utilizado para apresentar ao modelo locais com 
# maior probabilidade de ocorrência de LTA (1) e locais com menor
# probabilidade (0). As localidades sem informação receberão 0.

#map1@data[,6]<-ifelse(map1@data[,5]<=QTL[1],0,1)
map1@data[,6]<-ifelse(map1@data[,5]>0,1,0)

DADO_MOD<-map1@data[!is.na(map1@data[,6]),]

# Garante a reprodutibilidade do experimento
set.seed(1)

# Uso 70% do conjunto no treinamento e o restante, 30% como conjunto teste
#amostra 	<- sample(c(TRUE, FALSE),nrow(DADO_MOD),replace=TRUE,prob=c(0.7,0.3))
#dado.train 	<- DADO_MOD[amostra, ]
#dado.test 	<- DADO_MOD[!amostra,]  

DADO_MOD1   <-  DADO_MOD[which(DADO_MOD[,6]==1),]
DADO_MOD0   <-  DADO_MOD[which(DADO_MOD[,6]==0),]

# dim(DADO_MOD1)
#[1] 1790    9
# dim(DADO_MOD0)
#[1] 1422    9

amostra1     <- sample(c(TRUE, FALSE),nrow(DADO_MOD0),replace=TRUE,prob=c(0.7,0.3))
amostra2     <- sample(c(TRUE, FALSE),nrow(DADO_MOD0),replace=TRUE,prob=c(0.7,0.3))

dado.train  <- rbind(DADO_MOD1[which(amostra1==T), ],DADO_MOD0[which(amostra2==T),])
dado.test   <- rbind(DADO_MOD1[which(amostra1==F), ],DADO_MOD0[which(amostra2==F),])  

length(which(dado.train[,6]==1))

# Ajustando o modelo de regressão logistíca
model <- glm(LTA.C~TASMAX+SDII+HURS, family="binomial", data=dado.train)

predicted <- predict(model,dado.test, type="response")

OPT.model<-optimalCutoff(actuals=dado.test$LTA.C,predictedScores=predicted)

confusionMatrix(dado.test$LTA.C, predicted, threshold=OPT.model)

sensitivity(dado.test$LTA.C, predicted, threshold=OPT.model)

specificity(dado.test$LTA.C, predicted, threshold=OPT.model)

misClassError(dado.test$LTA.C, predicted, threshold=OPT.model)

png('../5-Figures/plotROC_LTA.png',width=(5*480),height=(5*480),type='cairo',res=600)
par(mar=c(1,1,1,1),mai=c(.01,0.01,0.01,0.01),oma=c(.1,.1,.1,.1))
plotROC(dado.test$LTA.C, predicted)
dev.off()
# =========================================

map2@data[,5:19]<-NA
map3@data[,5:9]<-NA

for (i in 1:nrow(map0@data)){
    pos2 <- which(map0@data[i, 1] == cli_ind[, 1])
    if(length(pos2)!=0) {map2@data[i, 5:19] <- cli_ind[pos2,-c(1:2)]}
}

for(i in 5:9){
    projecao<-data.frame(TASMAX=map2@data[,i],SDII=map2@data[,(i+5)],HURS=map2@data[,(i+10)])
    map3@data[,i]<-predict(model,projecao,type="response")
}

dimnames(map3@data)[[2]]<-c('CD_MUN','NM_MUN','SIGLA_UF','AREA_KM2','PRESENTE','2030-O','2050-O','2030-P','2050-P')

mapa<-function(X1,X2,Y,W,L,SIZE,RES,Z){
    # X1: Objeto shapefile com informacoes
    # X2: Objeto shapefile vazio
    # Y: Coluna de interesse
    # W: Endereco, nome da figura e formato
    # SIZE: tamanho base pra figura
    # RES: numero de DPI
    # Z: Titulo
    # L: Sentido do Indicador
    brks1<-c(0,0.2,0.4,0.6,0.8,1)
    brks2<-c(0.2,0.4,0.6,0.8)
    {if(L==1)
        {cols<-c('darkgreen','green2','khaki1','darkorange','red')}
    }
    {if(L==0)
        {cols<-rev(c('darkgreen','green2','khaki1','darkorange','red'))}
    }
    classes<-c('NO DATA','Muito Baixo','Baixo','Moderado','Alto','Muito Alto')
    png(W,width=(5*SIZE),height=(5*SIZE),type='cairo',res=RES)
    par(mar=c(1,1,1,1),mai=c(.01,0.01,0.01,0.01),oma=c(.1,.1,.1,.1))
    plot(X2, col='gray',border=NA,main=' ')
    plot(X1, col=cols[findInterval(X1@data[,Y], brks1,all.inside=TRUE)],border=NA,add=T)
    legend('bottomright',title='Classes',legend=classes,border=c('gray',cols),fill=c('gray',cols),bty="n",cex=0.75)
    text(-39,2,Z,cex=1)
    dev.off()
}

mapa(map3,map_bac,5,'../5-Figures/LTA_PRESENTE.png',1,480,600,'PRESENTE')

mapa(map3,map_bac,6,'../5-Figures/LTA_2030-O.png',1,480,600,'2030-O')

mapa(map3,map_bac,7,'../5-Figures/LTA_2050-O.png',1,480,600,'2050-O')

mapa(map3,map_bac,8,'../5-Figures/LTA_2030-P.png',1,480,600,'2030-P')

mapa(map3,map_bac,9,'../5-Figures/LTA_2050-P.png',1,480,600,'2050-P')

write.table(map3@data[,-4],file='../4-Output/RESULTADO_LTA.csv',col.names=T,row.names=F,quote=F,sep=';',dec=',')

writeOGR(map3, ".",dsn='../4-Output/RESULTADO_LTA.shp', driver="ESRI Shapefile")
