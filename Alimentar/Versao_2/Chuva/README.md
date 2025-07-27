# Scripts – Etapa 2 – Setor de Segurança Alimentar (Chuvas)

Este diretório reúne os scripts utilizados na produção da **componente climática do setor de Segurança Alimentar – com foco em eventos de precipitação**.

## Organização dos scripts

Para facilitar o processamento simultâneo e evitar confusões entre os cenários avaliados, foi adotada a estratégia de **manter um conjunto de scripts separado para cada cenário climático**:

- **Histórico**
- **SSP2-4.5**
- **SSP5-8.5**

Apesar das análises serem equivalentes entre si, essa separação permite mais controle na etapa de execução. Os scripts estão nomeados com **prefixos numéricos** que indicam sua ordem de uso:

| Prefixo | Função |
|---------|--------|
| `0_`    | Funções gerais utilizadas por outros scripts |
| `1_`    | Cálculo dos indicadores climáticos de precipitação (via CDO em shell script) |
| `2_`    | Cálculo da frequência decendial com base nas datas de colheita do milho – 1ª safra |
| `3_`    | Cálculo da chance de ocorrência e construção da matriz de correspondência |

## Indicadores climáticos calculados (Prefixo 1)

- **P6D-5mm**: Pelo menos 6 dias com precipitação ≥ 5mm no decêndio  
- **P3D-15mm**: Pelo menos 3 dias com precipitação ≥ 15mm no decêndio  
- **P20mm**: Número de dias com precipitação ≥ 20mm no decêndio  
- **P50mm**: Número de dias com precipitação ≥ 50mm no decêndio  
- **PC6D-5mm**: Pelo menos 6 dias consecutivos com precipitação ≥ 5mm no decêndio  
- **PC3D-10mm**: Pelo menos 3 dias consecutivos com precipitação ≥ 10mm no decêndio  

## Considerações sobre a chance de ocorrência (Prefixo 3)

A etapa final substitui a padronização convencional (mínimo-máximo), que se mostrou inadequada devido à baixa frequência histórica de eventos, por uma abordagem baseada na **chance de ocorrência**:

> **Chance de ocorrência = frequência futura / frequência histórica**

Essa razão permitiu classificar os municípios quanto ao tipo de impacto esperado em diferentes cenários, mantendo coerência espacial e evitando superestimações indevidas nos cenários futuros.

---

Para dúvidas ou reprodutibilidade, consulte os scripts diretamente ou abra uma issue neste repositório.
