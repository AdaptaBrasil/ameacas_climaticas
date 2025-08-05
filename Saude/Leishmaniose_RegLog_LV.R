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
cas_lv<-read.csv('../2-Input/2-LV/LV_incidencia_2001_2005_wide.csv',header=T,sep=';',dec=',')
cli_ind<-read.csv('../2-Input/2-LV/Ind_Climaticos_Simples_LV_03_12_2023.csv',header=T,sep=';',dec=',')
cli_bru<-read.csv('../2-Input/2-LV/Dados_Climaticos_Anuais_LV.csv',header=T,sep=';',dec='.')
cli_nor<-read.csv('../2-Input/2-LV/Dados_Climaticos_Anuais_NORM_LV.csv',header=T,sep=';',dec='.')

# Leitura de Shapefiles

map0 <- readOGR('/media/hellgate/16D7D0A761E2DB6A/MEGA/Shapefiles/Br_2019/br_municipios/BR_Municipios_2019.shp')

map_bac <- readOGR('/media/hellgate/16D7D0A761E2DB6A/MEGA/Shapefiles/Br_uf_5000/uf_5000.shp')


map1<-map2<-map3<-map0

head(cas_lv)

head(cli_ind)

LV <- as.numeric(c(cas_lv[, 8], cas_lv[, 9], cas_lv[, 10], cas_lv[, 11], cas_lv[, 12]))

# Contagem de municipios com informações válidas em cada ano da amostra
# excluindo valores UNDEF (NA) e zeros

LV01 <- length(cas_lv[, 8]) - length(which(cas_lv[, 8] == 0 | is.na(cas_lv[, 8]))) # Ano de 2001
LV02 <- length(cas_lv[, 9]) - length(which(cas_lv[, 9] == 0 | is.na(cas_lv[, 9]))) # Ano de 2002
LV03 <- length(cas_lv[, 10]) - length(which(cas_lv[, 10] == 0 | is.na(cas_lv[, 10]))) # Ano de 2003
LV04 <- length(cas_lv[, 11]) - length(which(cas_lv[, 11] == 0 | is.na(cas_lv[, 11]))) # Ano de 2004
LV05 <- length(cas_lv[, 12]) - length(which(cas_lv[, 12] == 0 | is.na(cas_lv[, 12]))) # Ano de 2005

CLV <- matrix(NA, ncol = 5, nrow = 1)
dimnames(CLV)[[1]] <- "Casos"
dimnames(CLV)[[2]] <- c(2001:2005)
CLV[1, ] <- c(LV01, LV02, LV03, LV04, LV05)

CLV

# Calculando o perfil mediano do número de casos por município
MED <- function(x) {
    Y <- median(x, na.rm = T)
}

cas_lv[,14] <- apply(cas_lv[, 8:12], 1, MED)

map1@data[,5:9]<-NA

# Inserindo as variaveis climáticas referente ao período histórico e indexando pelo geocódido

for (i in 1:nrow(map0@data)){
    pos1 <- which(map0@data[i, 1] == cas_lv[, 1])
    if(length(pos1)!=0) {map1@data[i, c(5:6)] <- cas_lv[pos1,14]}
    
    pos2 <- which(map0@data[i, 1] == cli_ind[, 1])
    if(length(pos2)!=0) {map1@data[i, c(7:9)] <- cli_ind[pos2,c(3,8,13)]}
}

dimnames(map1@data)[[2]]<-c('CD_MUN','NM_MUN','SIGLA_UF','AREA_KM2','LV.P','LV.C','TASMIN','SDII','HURS')

# Aqui foram calculados os quintis, considerando valores acima de zero e 
# diferentes de NA(No Avaliable).
# Esses limiares serão utilizados para a classificação da variável de interesse

QTL <- quantile(map1@data[map1@data[,5]>0,5],probs=c(.2,.4,.6,.8),na.rm=T)

# 20% 40% 60% 80% 
#  1   1   2   6

# Essa classificação indica que valores iguais ou inferiores a 1
# serão considerados como 0 e os superiores a 1.
# Isso será utilizado para apresentar ao modelo locais com 
# maior probabilidade de ocorrência de LV (1) e locais com menor
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

amostra1     <- sample(c(TRUE, FALSE),nrow(DADO_MOD1),replace=TRUE,prob=c(0.7,0.3))
amostra2     <- sample(c(TRUE, FALSE),nrow(DADO_MOD1),replace=TRUE,prob=c(0.7,0.3))

dado.train  <- rbind(DADO_MOD1[which(amostra1==T), ],DADO_MOD0[which(amostra2==T),])
dado.test   <- rbind(DADO_MOD1[which(amostra1==F), ],DADO_MOD0[which(amostra2==F),])  

length(which(dado.train[,6]==1))

# Ajustando o modelo de regressão logistíca
model <- glm(LV.C~TASMIN+SDII+HURS, family="binomial", data=dado.train)

predicted <- predict(model,dado.test, type="response")

OPT.model<-optimalCutoff(actuals=dado.test$LV.C,predictedScores=predicted)

confusionMatrix(dado.test$LV.C, predicted, threshold=OPT.model)

# true positive rate
sensitivity(dado.test$LV.C, predicted, threshold=OPT.model)

# true negative rate
specificity(dado.test$LV.C, predicted, threshold=OPT.model)

# percentage of total incorrect
misClassError(dado.test$LV.C, predicted, threshold=OPT.model)

png('../5-Figures/plotROC_LV.png',width=(5*480),height=(5*480),type='cairo',res=600)
par(mar=c(1,1,1,1),mai=c(.01,0.01,0.01,0.01),oma=c(.1,.1,.1,.1))
plotROC(dado.test$LV.C, predicted)
dev.off()
# =========================================

map2@data[,5:19]<-NA
map3@data[,5:9]<-NA

for (i in 1:nrow(map0@data)){
    pos2 <- which(map0@data[i, 1] == cli_ind[, 1])
    if(length(pos2)!=0) {map2@data[i, 5:19] <- cli_ind[pos2,-c(1:2)]}
}

for(i in 5:9){
    projecao<-data.frame(TASMIN=map2@data[,i],SDII=map2@data[,(i+5)],HURS=map2@data[,(i+10)])
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

mapa(map3,map_bac,5,'../5-Figures/LV_PRESENTE.png',1,480,600,'PRESENTE')

mapa(map3,map_bac,6,'../5-Figures/LV_2030-O.png',1,480,600,'2030-O')

mapa(map3,map_bac,7,'../5-Figures/LV_2050-O.png',1,480,600,'2050-O')

mapa(map3,map_bac,8,'../5-Figures/LV_2030-P.png',1,480,600,'2030-P')

mapa(map3,map_bac,9,'../5-Figures/LV_2050-P.png',1,480,600,'2050-P')

write.table(map3@data[,-4],file='../4-Output/RESULVDO_LV.csv',col.names=T,row.names=F,quote=F,sep=';',dec=',')

writeOGR(map3, ".",dsn='../4-Output/RESULVDO_LV.shp', driver="ESRI Shapefile")
