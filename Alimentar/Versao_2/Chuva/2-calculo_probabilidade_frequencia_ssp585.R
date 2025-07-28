#!/usr/bin/env Rscript
# Script: calculo_probabilidade_frequencia_ssp585.R
# Descrição: Calcula probabilidades de eventos climáticos extremos para cenário futuro SSP585
#            utilizando modelo NorESM2-MM (2066-2086), com tratamento especial para Fernando de Noronha
# Autor: George Ulguim Pedra
# Versão: 1.0

# 1. Configuração Inicial ------------------------------------------------------

# Limpa ambiente e configurações prévias
rm(list = ls())
options(warn = 2) # Transforma warnings em errors para facilitar debug

# Carrega pacotes com verificações
required_packages <- c("magrittr", "raster", "sf", "ncdf4", "beepr")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    stop(paste("Pacote necessário não instalado:", pkg))
  }
}

# 2. Definição de Parâmetros ---------------------------------------------------

# Parâmetros do cenário climático
SCENARIO <- "SSP585"
CLIMATE_MODEL <- "NorESM2-MM"
PERIOD <- "2066_2086"
SWL <- "3" # Nível de aquecimento global (3°C)

# Código especial para Fernando de Noronha (a ser pulado)
FERNANDO_NORONHA_CODE <- 2605459

# Diretórios (ajustar conforme necessidade)
BASE_DIR <- getwd()
SHAPEFILE_DIR <- file.path(BASE_DIR, "shapefiles")
INPUT_DIR <- file.path(BASE_DIR, "2-Input")
OUTPUT_DIR <- file.path(BASE_DIR, "4-Output", SCENARIO)

# Verifica e cria diretórios se necessário
dir.create(SHAPEFILE_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(INPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# 3. Funções Auxiliares --------------------------------------------------------

#' Carrega shapefile dos municípios
#' @return Objeto sf com os municípios brasileiros
load_municipalities <- function() {
  shapefile_path <- file.path(SHAPEFILE_DIR, "BR_Municipios_2019.shp")
  if (!file.exists(shapefile_path)) {
    stop(paste("Shapefile não encontrado em:", shapefile_path))
  }
  
  municipalities <- st_read(shapefile_path, quiet = TRUE)
  return(municipalities)
}

#' Carrega dados de decêndios de safra
#' @param file Nome do arquivo CSV
#' @return Dataframe com os decêndios de safra
load_safra_data <- function(file) {
  file_path <- file.path(INPUT_DIR, file)
  if (!file.exists(file_path)) {
    stop(paste("Arquivo de safra não encontrado:", file_path))
  }
  
  safra_data <- read.csv(file_path, header = TRUE, dec = ".", sep = ";")
  return(safra_data)
}

#' Processa dados climáticos para um município
#' @param munic_code Código do município
#' @param munic_uf UF do município
#' @param nc_list Lista de objetos raster com dados climáticos
#' @param p1_safra Dataframe com decêndios da 1ª safra
#' @param p2_safra Dataframe com decêndios da 2ª safra
#' @param data_period Período de análise (para filtro de datas)
#' @return Lista com resultados para todas as métricas
process_municipality <- function(munic_code, munic_uf, nc_list, p1_safra, p2_safra, data_period) {
  # Cria estrutura para armazenar resultados
  results <- list(
    count_5mm_6days = c(P1 = NA, P2 = NA),
    count_15mm_3days = c(P1 = NA, P2 = NA),
    count_20mm = c(P1 = NA, P2 = NA),
    count_50mm = c(P1 = NA, P2 = NA),
    count_cwd_5mm_6days = c(P1 = NA, P2 = NA),
    count_cwd_10mm_3days = c(P1 = NA, P2 = NA)
  )
  
  # Extrai datas e decêndios relevantes
  Dates <- gsub('[.]', '-', gsub('X', '', colnames(nc_list[[1]]@data@values)))
  end_year <- substr(data_period, 1, 4)
  Decendios <- substr(Dates[Dates <= paste0(end_year, '-12-31')], 6, 10)
  
  # Identifica decêndios de interesse para cada safra
  Dec_p1 <- p1_safra[p1_safra[, 3] == munic_uf, ][1, -c(1:3, 40)]
  Dec_p2 <- p2_safra[p2_safra[, 3] == munic_uf, ][1, -c(1:3, 40)]
  
  Dec_p1_sel <- Decendios[which(Dec_p1 == 1)]
  Dec_p2_sel <- Decendios[which(Dec_p2 == 1)]
  
  pos_p1_sel <- which(!is.na(match(substr(Dates, 6, 10), Dec_p1_sel)))
  pos_p2_sel <- which(!is.na(match(substr(Dates, 6, 10), Dec_p2_sel)))
  
  # Processa cada métrica climática
  for (i in seq_along(nc_list)) {
    nc_crop <- nc_list[[i]]
    
    # Converte valores para binários (0 ou 1) quando aplicável
    if (i %in% c(3, 4, 5, 6)) {
      nc_crop@data@values[which(nc_crop@data@values > 1 & 
                                !is.na(nc_crop@data@values) & 
                                !is.nan(nc_crop@data@values))] <- 1
    }
    
    # Calcula médias para cada safra
    if (NROW(nc_crop@data@values) > 1) {
      c1 <- rowMeans(nc_crop@data@values[, pos_p1_sel], na.rm = TRUE)
      c2 <- rowMeans(nc_crop@data@values[, pos_p2_sel], na.rm = TRUE)
    } else {
      c1 <- mean(nc_crop@data@values[, pos_p1_sel], na.rm = TRUE)
      c2 <- mean(nc_crop@data@values[, pos_p2_sel], na.rm = TRUE)
    }
    
    # Armazena a mediana (considerando apenas valores positivos)
    results[[i]]["P1"] <- ifelse(length(which(c1 > 0 & !is.nan(c1))) != 0, 
                               median(c1[which(c1 > 0 & !is.nan(c1))], 0)
    results[[i]]["P2"] <- ifelse(length(which(c2 > 0 & !is.nan(c2))) != 0, 
                               median(c2[which(c2 > 0 & !is.nan(c2))], 0)
  }
  
  return(results)
}

# 4. Execução Principal --------------------------------------------------------

tryCatch({
  # Carrega dados de entrada
  message("Carregando shapefile de municípios...")
  map0 <- load_municipalities()
  map0_df <- map0 %>% st_drop_geometry()
  
  message("Carregando dados de decêndios de safra...")
  p1_safra <- load_safra_data("Decendios_primeira_safra_milho.csv")
  p2_safra <- load_safra_data("Decendios_segunda_safra_milho.csv")
  
  # Cria estrutura de dados para resultados
  results_template <- data.frame(
    CD_MUN = map0_df[, 1],
    NM_MUN = map0_df[, 2],
    UF = map0_df[, 3],
    P1 = NA,
    P2 = NA,
    stringsAsFactors = FALSE
  )
  
  # Inicializa dataframes para cada métrica
  tmp_list <- list(
    count_5mm_6days = results_template,
    count_15mm_3days = results_template,
    count_20mm = results_template,
    count_50mm = results_template,
    count_cwd_5mm_6days = results_template,
    count_cwd_10mm_3days = results_template
  )
  
  # Carrega dados climáticos do cenário SSP585
  message("Carregando dados climáticos do modelo ", CLIMATE_MODEL, " para cenário ", SCENARIO, "...")
  nc_files <- list(
    count_5mm_6days = paste0('count_5/', CLIMATE_MODEL, '-pr-', tolower(SCENARIO), '_', SWL, '_Count_5mm_6days_', PERIOD, '.nc'),
    count_15mm_3days = paste0('count_15/', CLIMATE_MODEL, '-pr-', tolower(SCENARIO), '_', SWL, '_Count_15mm_3days_', PERIOD, '.nc'),
    count_20mm = paste0('count_20/', CLIMATE_MODEL, '-pr-', tolower(SCENARIO), '_', SWL, '_Count_20mm_', PERIOD, '.nc'),
    count_50mm = paste0('count_50/', CLIMATE_MODEL, '-pr-', tolower(SCENARIO), '_', SWL, '_Count_50mm_', PERIOD, '.nc'),
    count_cwd_5mm_6days = paste0('count_cwd_5/', CLIMATE_MODEL, '-pr-', tolower(SCENARIO), '_', SWL, '_Count_cwd_5mm_6days_', PERIOD, '.nc'),
    count_cwd_10mm_3days = paste0('count_cwd_10/', CLIMATE_MODEL, '-pr-', tolower(SCENARIO), '_', SWL, '_Count_cwd_10mm_3days_', PERIOD, '.nc')
  )
  
  nc_list <- lapply(nc_files, function(f) {
    file_path <- file.path(OUTPUT_DIR, f)
    if (!file.exists(file_path)) {
      stop(paste("Arquivo climático não encontrado:", file_path))
    }
    
    # Carrega brick com tratamento especial para variáveis CWD
    if (grepl("cwd", f)) {
      varname <- ifelse(grepl("5mm", f), 
                       "number_of_cwd_periods_with_more_than_6days_per_time_period",
                       "number_of_cwd_periods_with_more_than_3days_per_time_period")
      raster::brick(file_path, varname = varname)
    } else {
      raster::brick(file_path)
    }
  })
  
  # Processa cada município
  message("Processando municípios para o cenário ", SCENARIO, " (", PERIOD, ")...")
  total_munic <- nrow(map0_df)
  skipped_munic <- 0
  skipped_codes <- c(FERNANDO_NORONHA_CODE)
  
  progress_interval <- ifelse(total_munic > 500, 100, 50)
  
  for (i in 1:total_munic) {
    munic_code <- map0_df[i, 1]
    munic_name <- map0_df[i, 2]
    
    # Pula municípios especiais (como Fernando de Noronha)
    if (munic_code %in% skipped_codes) {
      message(sprintf("Pulando %s (código: %d) - sem dados climáticos", 
                     munic_name, munic_code))
      skipped_munic <- skipped_munic + 1
      next
    }
    
    munic_uf <- map0_df[i, 3]
    
    # Recorta dados para o município atual
    map_ref <- subset(map0, CD_MUN %in% munic_code)
    
    # Processa dados climáticos
    nc_crops <- lapply(nc_list, function(nc) {
      crop(nc, extent(map_ref), snap = 'near')
    })
    
    # Calcula probabilidades
    munic_results <- process_municipality(munic_code, munic_uf, nc_crops, p1_safra, p2_safra, PERIOD)
    
    # Armazena resultados
    for (metric in names(tmp_list)) {
      tmp_list[[metric]][i, "P1"] <- munic_results[[metric]]["P1"]
      tmp_list[[metric]][i, "P2"] <- munic_results[[metric]]["P2"]
    }
    
    # Progresso
    if (i %% progress_interval == 0) {
      message(sprintf("Processados %d de %d municípios (%.1f%%)", 
                     i, total_munic, i/total_munic*100))
    }
  }
  
  message(sprintf("\nProcessamento concluído! %d municípios processados, %d pulados.", 
                 total_munic - skipped_munic, skipped_munic))
  if (skipped_munic > 0) {
    message("Municípios pulados:", paste(skipped_codes, collapse = ", "))
  }
  
  # 5. Saída de Resultados -----------------------------------------------------
  
  message("Salvando resultados para cenário ", SCENARIO, "...")
  output_files <- list(
    count_5mm_6days = paste0('Prob_', CLIMATE_MODEL, '_Count_5mm_6days_', SWL, '_', PERIOD, '.csv'),
    count_15mm_3days = paste0('Prob_', CLIMATE_MODEL, '_Count_15mm_3days_', SWL, '_', PERIOD, '.csv'),
    count_20mm = paste0('Prob_', CLIMATE_MODEL, '_Count_20mm_', SWL, '_', PERIOD, '.csv'),
    count_50mm = paste0('Prob_', CLIMATE_MODEL, '_Count_50mm_', SWL, '_', PERIOD, '.csv'),
    count_cwd_5mm_6days = paste0('Prob_', CLIMATE_MODEL, '_Count_cwd_5mm_6days_', SWL, '_', PERIOD, '.csv'),
    count_cwd_10mm_3days = paste0('Prob_', CLIMATE_MODEL, '_Count_cwd_10mm_3days_', SWL, '_', PERIOD, '.csv')
  )
  
  for (i in seq_along(tmp_list)) {
    file_path <- file.path(OUTPUT_DIR, output_files[[i]])
    write.table(tmp_list[[i]], file = file_path, 
                dec = '.', sep = ';', quote = FALSE, 
                row.names = FALSE, col.names = TRUE)
    message(paste("Arquivo salvo:", file_path))
  }
  
  # Notificação sonora de conclusão
  if (interactive()) {
    beep(3)
    message("Processamento concluído! Notificação sonora ativada.")
  }
  
}, error = function(e) {
  message("\nOcorreu um erro durante a execução:")
  message(conditionMessage(e))
  traceback()
  
  # Notificação sonora de erro
  if (interactive()) {
    beep(8)
  }
  
  quit(status = 1)
})
