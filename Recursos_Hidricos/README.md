# Análise da Ocorrência de Seca em Cenários de Mudança Climática no Brasil

Este repositório apresenta um conjunto de scripts e informações utilizados para analisar a ocorrência de secas no Brasil a partir de projeções climáticas do CMIP6 e modelagem hidrológica aplicada pela Agência Nacional de Águas (ANA). O foco está em avaliar a variação da vazão em ottobacias de nível 7, com recorte municipal, e identificar áreas com potencial aumento na frequência ou severidade de eventos de seca ao longo do século XXI.

## 📌 Objetivo

Apresentar uma metodologia para:

- Relacionar dados observados com projeções futuras de vazão;
- Classificar alterações de vazão com base em percentuais de redução;
- Associar ottobacias a municípios para análise integrada;
- Produzir subsídios para avaliação de risco climático à seca em recortes subnacionais.

## 🌍 Fonte de Dados

- **Modelagem Hidrológica**: Realizada pela Agência Nacional de Águas (ANA) com base na equação de Budyko.
- **Nível de análise**: Ottobacias de nível 7 para todo o território nacional.
- **Dados Climáticos**: Modelos do projeto CMIP6.
- **Referência Observacional**: Conjunto BR-DWDG.
- **Cenários Avaliados**:
  - SSP2-4.5
  - SSP3-7.0
  - SSP5-8.5
- **Períodos de análise**:
  - 2015–2040 (curto prazo)
  - 2041–2070 (médio prazo)
  - 2071–2100 (longo prazo)
- **Modelos climáticos utilizados (versão AdaptaBrasil)**:
  - GFDL-ESM4
  - INM-CM5-0
  - MPI-ESM1-2
  - MRI-ESM2-0
  - NorESM2-MM

## 🧮 Metodologia

1. **Extração e organização dos dados de vazão** por ottobacia e período.
2. **Conversão de ottobacias para municípios**, permitindo análises integradas por unidades administrativas.
3. **Classificação da vazão atual**, com base em dados observados, considerando características regionais por bioma.
4. **Classificação das projeções futuras** com base na variação percentual de vazão (delta), conforme fornecido pela ANA:
   - **< 0% a -20%**: presença de redução de vazão (indicador de seca);
   - **≥ 0%**: classificado como "Muito Baixa" probabilidade de seca.
5. **Escala de intensidade** de seca utilizada: **Muito Baixa → Muito Alta** (foco nos negativos/diminuições).

## 🔁 Estrutura dos Processos Computacionais

- Pré-processamento de dados climáticos e hidrológicos.
- Cruzamento de dados espaciais (ottobacia ↔ município).
- Classificação da severidade de seca a partir de deltas de vazão.
- Geração de saídas organizadas por cenário, período e região.

## 📎 Documento de Referência

Estudo original da Agência Nacional de Águas:  
🔗 [Mudanças Climáticas – Avaliação Hidrológica para o Brasil](https://metadados.snirh.gov.br/geonetwork/srv/api/records/31604c98-5bbe-4dc9-845d-998815607b33/attachments/Mudancas_Climaticas_25012024.pdf)

## 🗂 Organização do Repositório

