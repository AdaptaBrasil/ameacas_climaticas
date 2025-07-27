#!/usr/bin/env Rscript
# Script: 3.1-Analise_probabilidades_presente_futuro.R
# Descrição: Análise comparativa de probabilidades climáticas entre períodos históricos e futuros
#            para o cenário SSP2-4.5, com cálculo de chances relativas
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
#' @param padrao_arquivo Padrão para identificar os arquivos
#' @return Lista com dados processados para P1 e P2
load_process_future_data <- function(dir_path, modelos, padrao_arquivo) {
  todos_arquivos <- list.files(path = dir_path, pattern = "\\.csv$", full.names = TRUE)
  
  output_p1 <- output_p2 <- setNames(vector("list", length(modelos)), modelos)
  
  for(k in modelos) {
    arquivos <- todos_arquivos[str_detect(todos_arquivos, padrao_arquivo) & 
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
    
    output_p1[[k]] <- temp_p1
    output_p2[[k]] <- temp_p2
  }
  
  return(list(P1 = output_p1, P2 = output_p2))
}

#' Calcula chances relativas em relação ao período histórico
#' 
#' @param dados_futuros Lista com dados futuros
#' @param dados_hist Dataframe com dados históricos
#' @return Lista com chances calculadas para P1 e P2
calculate_relative_chance <- function(dados_futuros, dados_hist) {
  chance_p1 <- chance_p2 <- vector("list", length(dados_futuros$P1))
  names(chance_p1) <- names(chance_p2) <- names(dados_futuros$P1)
  
  for(i in seq_along(dados_futuros$P1))) {
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
                           dados_futuros$P1[[i]][,j] / dados_hist[,j])
      temp_p2[,j] <- ifelse(dados_hist[,j] < 0.01, 1,
                           dados_futuros$P2[[i]][,j] / dados_hist[,j])
    }
    
    chance_p1[[i]] <- temp_p1
    chance_p2[[i]] <- temp_p2
  }
  
  return(list(P1 = chance_p1, P2 = chance_p2))
}

# 3. Carregamento e Processamento de Dados -------------------------------------

# Diretório com dados SSP2-4.5
dir_ssp245 <- '4-Output/SSP245'
modelos <- c('GFDL', 'INM', 'MPI', 'MRI', 'NorESM')

# Carrega dados para GWL 1.5
message("Processando dados para GWL 1.5...")
dados_ssp245_15 <- load_process_future_data(
  dir_path = dir_ssp245,
  modelos = modelos,
  padrao_arquivo = "_1.5_"
)

# Carrega dados para GWL 2.0
message("Processando dados para GWL 2.0...")
dados_ssp245_20 <- load_process_future_data(
  dir_path = dir_ssp245,
  modelos = modelos,
  padrao_arquivo = "_2_"
)

# 4. Cálculo das Chances Relativas ---------------------------------------------

# Assume que output_hist_p1 e output_hist_p2 estão disponíveis
if(!exists("output_hist_p1") || !exists("output_hist_p2")) {
  stop("Dados históricos (output_hist_p1 e output_hist_p2) não encontrados!")
}

message("Calculando chances relativas para GWL 1.5...")
chance_ssp245_15 <- calculate_relative_chance(dados_ssp245_15, output_hist_p1)

message("Calculando chances relativas para GWL 2.0...")
chance_ssp245_20 <- calculate_relative_chance(dados_ssp245_20, output_hist_p1)

# Atribui para o ambiente global (para uso em outros scripts)
chance_ssp245_15_p1 <<- chance_ssp245_15$P1
chance_ssp245_15_p2 <<- chance_ssp245_15$P2
chance_ssp245_20_p1 <<- chance_ssp245_20$P1
chance_ssp245_20_p2 <<- chance_ssp245_20$P2

# 5. Cálculo de Valores Máximos ------------------------------------------------

message("Calculando valores máximos...")

# Função para calcular valores máximos
calculate_max_values <- function(chance_data) {
  modelos <- names(chance_data)
  gwl_levels <- c(1.5, 2)
  
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
  for(i in seq_along(modelos)) {
    for(j in seq_along(gwl_levels)) {
      idx <- ifelse(j == 1, "15", "20")
      current_data <- get(paste0("chance_ssp245_", idx, "_p1"))[[modelos[i]]]
      max_df[row, 3:8] <- apply(current_data[,-c(1:3)], 2, max, na.rm = TRUE)
      row <- row + 1
    }
  }
  
  return(max_df)
}

valores_max_ssp245_p1 <- calculate_max_values(chance_ssp245_15_p1)
valores_max_ssp245_p2 <- calculate_max_values(chance_ssp245_15_p1) # Adaptar se necessário

message("Processamento SSP2-4.5 concluído com sucesso!")
