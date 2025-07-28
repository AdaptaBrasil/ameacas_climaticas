#!/usr/bin/env Rscript
# Script: 3-Analise_probabilidades_presente_futuro.R
# Descrição: Análise comparativa de probabilidades climáticas entre períodos históricos e futuros
#            para os cenários SSP2-4.5 e SSP5-8.5, utilizando matriz de correspondência
# Autor: George Ulguim Pedra
# Versão: 1.0

# 1. Configuração Inicial ------------------------------------------------------

# Limpa ambiente e configurações prévias
rm(list = ls())
options(warn = 2) # Transforma warnings em errors

# Carrega pacotes necessários
required_packages <- c("stringr")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# 2. Funções Auxiliares --------------------------------------------------------

#' Carrega dados históricos e organiza em dataframes consolidados
#' 
#' @param dir_hist Diretório contendo os arquivos históricos
#' @return Lista com dois dataframes (P1 e P2) consolidados
load_historical_data <- function(dir_hist) {
  arq_hist <- list.files(path = dir_hist, pattern = "\\.csv$", full.names = TRUE)
  nome_hist <- substr(arq_hist, nchar(dir_hist)+2, nchar(arq_hist)-4)
  
  output_hist_p1 <- output_hist_p2 <- NULL
  
  for(i in seq_along(arq_hist)) {
    input <- read.csv(arq_hist[i], header = TRUE, sep = ';', dec = '.')
    
    if(i == 1) {
      output_hist_p1 <- output_hist_p2 <- data.frame(
        CD_MUN = input[,1],
        NM_MUN = input[,2],
        UF = input[,3]
      )
    }
    
    output_hist_p1[[nome_hist[i]]] <- input[,4]
    output_hist_p2[[nome_hist[i]]] <- input[,5]
  }
  
  return(list(P1 = output_hist_p1, P2 = output_hist_p2))
}

# 3. Carregamento de Dados -----------------------------------------------------

# Carrega função de categorização
source('3-Scripts/nova/0-functions.R')

# Diretório com dados históricos
dir_hist <- '4-Output/Historical'

# Carrega dados históricos
message("Carregando dados históricos...")
historical_data <- load_historical_data(dir_hist)
output_hist_p1 <- historical_data$P1
output_hist_p2 <- historical_data$P2

# Carrega dados futuros
message("Carregando dados futuros...")
source('3-Scripts/3.1-Analise_probabilidades_presente_futuro.R') # SSP2-4.5
source('3-Scripts/3.2-Analise_probabilidades_presente_futuro.R') # SSP5-8.5

# 4. Preparação da Matriz de Informações ---------------------------------------

message("Preparando matriz de informações...")

# Inicializa lista de informações
Info <- list(
  Count_15mm_3days = NULL,
  Count_20mm = NULL,
  Count_50mm = NULL,
  Count_5mm_6days = NULL,
  Count_cwd_10mm_3days = NULL,
  Count_cwd_5mm_6days = NULL
)

# Combina dados de chance para todos os cenários e períodos
for(i in 1:5) {
  Info[[1]] <- c(Info[[1]], 
                chance_ssp245_15_p1[[i]][,4], chance_ssp245_20_p1[[i]][,4],
                chance_ssp585_15_p1[[i]][,4], chance_ssp585_20_p1[[i]][,4], 
                chance_ssp585_30_p1[[i]][,4])
  
  Info[[2]] <- c(Info[[2]], 
                chance_ssp245_15_p1[[i]][,5], chance_ssp245_20_p1[[i]][,5],
                chance_ssp585_15_p1[[i]][,5], chance_ssp585_20_p1[[i]][,5], 
                chance_ssp585_30_p1[[i]][,5])
  
  Info[[3]] <- c(Info[[3]], 
                chance_ssp245_15_p1[[i]][,6], chance_ssp245_20_p1[[i]][,6],
                chance_ssp585_15_p1[[i]][,6], chance_ssp585_20_p1[[i]][,6], 
                chance_ssp585_30_p1[[i]][,6])
  
  Info[[4]] <- c(Info[[4]], 
                chance_ssp245_15_p1[[i]][,7], chance_ssp245_20_p1[[i]][,7],
                chance_ssp585_15_p1[[i]][,7], chance_ssp585_20_p1[[i]][,7], 
                chance_ssp585_30_p1[[i]][,7])
  
  Info[[5]] <- c(Info[[5]], 
                chance_ssp245_15_p1[[i]][,8], chance_ssp245_20_p1[[i]][,8],
                chance_ssp585_15_p1[[i]][,8], chance_ssp585_20_p1[[i]][,8], 
                chance_ssp585_30_p1[[i]][,8])
  
  Info[[6]] <- c(Info[[6]], 
                chance_ssp245_15_p1[[i]][,9], chance_ssp245_20_p1[[i]][,9],
                chance_ssp585_15_p1[[i]][,9], chance_ssp585_20_p1[[i]][,9], 
                chance_ssp585_30_p1[[i]][,9])
}

# 5. Aplicação da Matriz de Correspondência ------------------------------------

message("Aplicando matriz de correspondência...")

# SSP2-4.5
resultados_245_15_p1 <- categorize_climate_probabilities(output_hist_p1, chance_ssp245_15_p1, Info)
resultados_245_20_p1 <- categorize_climate_probabilities(output_hist_p1, chance_ssp245_20_p1, Info)

# SSP5-8.5
resultados_585_15_p1 <- categorize_climate_probabilities(output_hist_p1, chance_ssp585_15_p1, Info)
resultados_585_20_p1 <- categorize_climate_probabilities(output_hist_p1, chance_ssp585_20_p1, Info)
resultados_585_30_p1 <- categorize_climate_probabilities(output_hist_p1, chance_ssp585_30_p1, Info)

message("Processamento concluído com sucesso!")
