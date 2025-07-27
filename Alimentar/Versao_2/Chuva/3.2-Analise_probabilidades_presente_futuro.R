#!/usr/bin/env Rscript
# Script: 3.2-Analise_probabilidades_presente_futuro.R
# Descrição: Análise comparativa de probabilidades climáticas entre períodos históricos e futuros
#            para o cenário SSP5-8.5, com cálculo de chances relativas
# Autor: George Ulguim Pedra
# Versão: 1.0

# 1. Configuração Inicial ------------------------------------------------------

# Carrega pacotes necessários
required_packages <- c("stringr")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# 2. Funções Auxiliares --------------------------------------------------------

#' Carrega e processa dados futuros para um determinado cenário
#' 
#' @param dir_path Diretório base dos dados
#' @param modelos Lista de modelos a processar
#' @param gwl_levels Níveis de aquecimento global a processar
#' @return Lista com dados processados para P1 e P2
load_process_ssp585_data <- function(dir_path, modelos, gwl_levels) {
  todos_arquivos <- list.files(path = dir_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Inicializa estrutura de resultados
  resultados <- list()
  for(level in gwl_levels) {
    resultados[[paste0("GWL_", level)]] <- list(
      P1 = setNames(vector("list", length(modelos)), modelos),
      P2 = setNames(vector("list", length(modelos)), modelos)
    )
  }
  
  for(k in modelos) {
    for(level in gwl_levels) {
      padrao <- paste0("_", level, "_")
      arquivos <- todos_arquivos[str_detect(todos_arquivos, padrao) & 
                                str_detect(todos_arquivos, k)]
      
      nomes <- substr(arquivos, nchar(dir_path)+2, nchar(arquivos)-4)
      
      temp_p1 <- temp_p2 <- NULL
      
      for(i in seq_along(arquivos)) {
        input <- read.csv(arquivos[i], header = TRUE, sep = ';', dec = '.')
        
        if(i == 1) {
          temp_p1 <- temp_p2 <- data.frame(
            CD_MUN = input[,1],
            NM_MUN = input[,2],
            UF = input[,3]
          )
        }
        
        temp_p1[[nomes[i]]] <- input[,4]
        temp_p2[[nomes[i]]] <- input[,5]
      }
      
      resultados[[paste0("GWL_", level)]]$P1[[k]] <- temp_p1
      resultados[[paste0("GWL_", level)]]$P2[[k]] <- temp_p2
    }
  }
  
  return(resultados)
}

#' Calcula chances relativas em relação ao período histórico
#' 
#' @param dados_futuros Lista com dados futuros
#' @param dados_hist Dataframe com dados históricos
#' @param gwl_levels Níveis de aquecimento global processados
#' @return Lista com chances calculadas para P1 e P2
calculate_ssp585_relative_chance <- function(dados_futuros, dados_hist, gwl_levels) {
  modelos <- names(dados_futuros[[1]]$P1)
  
  # Inicializa estrutura de resultados
  resultados <- list()
  for(level in gwl_levels) {
    resultados[[paste0("GWL_", level)]] <- list(
      P1 = setNames(vector("list", length(modelos)), modelos),
      P2 = setNames(vector("list", length(modelos)), modelos)
    )
  }
  
  for(level in gwl_levels) {
    for(k in modelos) {
      # Inicializa dataframes para resultados
      temp_p1 <- temp_p2 <- data.frame(
        CD_MUN = dados_hist[,1],
        NM_MUN = dados_hist[,2],
        UF = dados_hist[,3],
        Count_15mm_3days = NA,
        Count_20mm = NA,
        Count_50mm = NA,
        Count_5mm_6days = NA,
        Count_cwd_10mm_3days = NA,
        Count_cwd_5mm_6days = NA
      )
      
      # Calcula chances para cada variável
      for(j in 4:9) {
        temp_p1[,j] <- ifelse(dados_hist[,j] < 0.01, 1, 
                             dados_futuros[[paste0("GWL_", level)]]$P1[[k]][,j] / dados_hist[,j])
        temp_p2[,j] <- ifelse(dados_hist[,j] < 0.01, 1,
                             dados_futuros[[paste0("GWL_", level)]]$P2[[k]][,j] / dados_hist[,j])
      }
      
      resultados[[paste0("GWL_", level)]]$P1[[k]] <- temp_p1
      resultados[[paste0("GWL_", level)]]$P2[[k]] <- temp_p2
    }
  }
  
  return(resultados)
}

#' Calcula valores máximos para cada variável e GWL
#' 
#' @param chance_data Lista com dados de chance calculados
#' @param gwl_levels Níveis de aquecimento global processados
#' @return Dataframe com valores máximos para P1 e P2
calculate_ssp585_max_values <- function(chance_data, gwl_levels) {
  modelos <- names(chance_data[[1]]$P1)
  
  # Inicializa dataframe de resultados
  max_df <- data.frame(
    GWL = rep(gwl_levels, each = length(modelos)),
    Modelo = rep(modelos, times = length(gwl_levels)),
    Count_15mm_3days = NA,
    Count_20mm = NA,
    Count_50mm = NA,
    Count_5mm_6days = NA,
    Count_cwd_10mm_3days = NA,
    Count_cwd_5mm_6days = NA
  )
  
  row <- 1
  for(level in gwl_levels) {
    for(k in modelos) {
      current_data <- chance_data[[paste0("GWL_", level)]]$P1[[k]]
      max_df[row, 3:8] <- apply(current_data[,-c(1:3)], 2, max, na.rm = TRUE)
      row <- row + 1
    }
  }
  
  return(max_df)
}

# 3. Carregamento e Processamento de Dados -------------------------------------

# Diretório com dados SSP5-8.5
dir_ssp585 <- '4-Output/SSP585'
modelos <- c('GFDL', 'INM', 'MPI', 'MRI', 'NorESM')
gwl_levels <- c(1.5, 2, 3)  # Níveis de aquecimento global

# Carrega dados para todos os GWL levels
message("Processando dados SSP5-8.5 para GWL 1.5, 2.0 e 3.0...")
dados_ssp585 <- load_process_ssp585_data(
  dir_path = dir_ssp585,
  modelos = modelos,
  gwl_levels = gwl_levels
)

# 4. Cálculo das Chances Relativas ---------------------------------------------

# Assume que output_hist_p1 e output_hist_p2 estão disponíveis
if(!exists("output_hist_p1") || !exists("output_hist_p2")) {
  stop("Dados históricos (output_hist_p1 e output_hist_p2) não encontrados!")
}

message("Calculando chances relativas para SSP5-8.5...")
chance_ssp585 <- calculate_ssp585_relative_chance(
  dados_futuros = dados_ssp585,
  dados_hist = output_hist_p1,
  gwl_levels = gwl_levels
)

# Atribui para o ambiente global (para uso em outros scripts)
chance_ssp585_15_p1 <<- chance_ssp585$GWL_1.5$P1
chance_ssp585_15_p2 <<- chance_ssp585$GWL_1.5$P2
chance_ssp585_20_p1 <<- chance_ssp585$GWL_2$P1
chance_ssp585_20_p2 <<- chance_ssp585$GWL_2$P2
chance_ssp585_30_p1 <<- chance_ssp585$GWL_3$P1
chance_ssp585_30_p2 <<- chance_ssp585$GWL_3$P2

# 5. Cálculo de Valores Máximos ------------------------------------------------

message("Calculando valores máximos para SSP5-8.5...")
valores_max_ssp585_p1 <- calculate_ssp585_max_values(chance_ssp585, gwl_levels)
valores_max_ssp585_p2 <- calculate_ssp585_max_values(
  list(
    GWL_1.5 = list(P1 = chance_ssp585_15_p1),
    GWL_2 = list(P1 = chance_ssp585_20_p1),
    GWL_3 = list(P1 = chance_ssp585_30_p1)
  ), 
  gwl_levels
)

message("Processamento SSP5-8.5 concluído com sucesso!")
