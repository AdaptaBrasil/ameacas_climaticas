#' Categoriza probabilidades climáticas comparando presente e futuro
#'
#' @param X1 Dataframe contendo as probabilidades de referência (período presente)
#' @param Y1 Lista de dataframes contendo as probabilidades futuras para cada modelo
#' @param M Lista contendo as chances de ocorrência (razão futuro/presente) para cada modelo
#' 
#' @return Lista contendo:
#'   - X_cat: Categorias para o período presente
#'   - Y_cat: Categorias para os períodos futuros
#'   - combined: Combinação ponderada das categorias
#'   - breaks_presente: Limites das categorias para o presente
#'   - breaks_futuro: Limites das categorias para o futuro
#'
#' @details
#' Esta função evita a compressão de categorias no futuro usando a razão entre
#' probabilidades futuras e presentes (chance) em vez de normalização tradicional.


categorize_climate_probabilities <- function(X1, Y1, M) {
  # Inicializa estruturas de retorno
  results <- list(
    X_cat = X1,
    X_clas = X1,  # Adicionado para armazenar X_cat/10
    Y_cat = Y1,
    combined = Y1,
    breaks_presente = list(),
    breaks_futuro = list()
  )
  
  # Labels para as categorias
  present_labels <- 1:5
  future_labels <- 0:5
  
  # Processa cada modelo e cada coluna de probabilidade
  for(j in seq_along(Y1)) {
    results$breaks_presente[[j]] <- list()
    results$breaks_futuro[[j]] <- list()
    
    for(i in 4:ncol(X1)) {
      # 1. Categorização do presente
      present_probs <- X1[, i]
      present_breaks <- quantile(present_probs, probs = seq(0, 1, 0.2), na.rm = TRUE)
      
      if(any(duplicated(present_breaks))) {
        present_breaks <- quantile(unique(present_probs), probs = seq(0, 1, 0.2), na.rm = TRUE)
      }
      
      # Convertemos para numérico diretamente para evitar problemas com fatores
      results$X_cat[, i] <- as.numeric(as.character(cut(
        present_probs,
        breaks = present_breaks,
        include.lowest = TRUE,
        right = FALSE,
        labels = present_labels
      )))
      
      # Adiciona a divisão por 10 para X_clas
      results$X_clas[, i] <- results$X_cat[, i] / 10
      
      # 2. Categorização do futuro
      chance_values <- M[[i-3]]
      significant_chances <- chance_values[chance_values > 1.5]
      
      if(length(significant_chances) > 0) {
        future_breaks <- c(0, quantile(significant_chances, probs = seq(0, 1, 0.2), na.rm = TRUE))
      } else {
        future_breaks <- c(0, rep(Inf, 5))
      }
      
      results$Y_cat[[j]][, i] <- as.numeric(as.character(cut(
        Y1[[j]][, i],
        breaks = future_breaks,
        include.lowest = TRUE,
        right = FALSE,
        labels = future_labels
      )))
      
      # 3. Combinação das categorias
      results$combined[[j]][, i] <- (results$X_cat[, i] + results$Y_cat[[j]][, i])/10
      
      # Armazena os breaks
      results$breaks_presente[[j]][[i-3]] <- present_breaks
      results$breaks_futuro[[j]][[i-3]] <- future_breaks
    }
    
    names(results$breaks_presente[[j]]) <- colnames(X1)[4:ncol(X1)]
    names(results$breaks_futuro[[j]]) <- colnames(X1)[4:ncol(X1)]
  }
  
  return(results)
}
