#!/bin/sh

###############################################################################
# SCRIPT: calculo_indicadores_historico.sh
# DESCRIÇÃO: Calcula indicadores climáticos de precipitação em períodos decendiais
# PERÍODO: 1995-2014
# SAÍDAS: 
#   - Arquivos NetCDF com contagens de eventos extremos por decêndio
#   - Médias climatológicas decendiais
# AUTOR: George Ulguim Pedra
###############################################################################

# ============================== CONFIGURAÇÕES ================================
# Definir ano inicial e final do período de análise
ANO_INICIAL=1995
ANO_FINAL=2014

# Caminhos de entrada/saída
PATH_IN="/home/usuario/Downloads/NC/Xavier/pr_Tmax_Tmin_NetCDF_Files/pr/pr_19610101_20231231_BR-DWGD_UFES_UTEXAS_v_3.2.3.nc"
PATH_OUT="/home/usuario/Documentos/AdaptaBrasil/Seg_Alimentar/IC_chuva/4-Output/Historical/"

# Meses e dias correspondentes (considerando ano não-bissexto)
MESES="01 02 03 04 05 06 07 08 09 10 11 12"
DIAS="31 28 31 30 31 30 31 31 30 31 30 31"

# Flags comuns para comandos CDO
CDO_FLAGS="-s -L -b F32"

# ============================== FUNÇÕES AUXILIARES ===========================

# Função para verificar se ano é bissexto
is_leap_year() {
    local year=$1
    if [ $((year % 4)) -eq 0 ] && ([ $((year % 100)) -ne 0 ] || [ $((year % 400)) -eq 0 ]); then
        return 0 # verdadeiro
    else
        return 1 # falso
    fi
}

# Função para criar diretórios se não existirem
create_dirs() {
    local dirs=(
        "count_5" 
        "count_15" 
        "count_20" 
        "count_50" 
        "count_cwd_5" 
        "count_cwd_10"
    )
    
    for dir in "${dirs[@]}"; do
        mkdir -p "${PATH_OUT}/${dir}"
    done
}

# Função para processar um indicador específico
process_indicador() {
    local tipo=$1          # Tipo de indicador
    local data_ini=$2      # Data inicial (YYYY-MM-DD)
    local data_end=$3      # Data final (YYYY-MM-DD)
    local arq_saida=$4     # Arquivo de saída
    local operadores=$5    # Operadores CDO
    
    echo "Processando ${tipo} para período ${data_ini} a ${data_end}"
    
    cdo $CDO_FLAGS -settaxis,"$data_ini" $operadores -seldate,"$data_ini","$data_end" "$PATH_IN" "$arq_saida"
    
    if [ $? -ne 0 ]; then
        echo "ERRO: Falha ao processar ${tipo} para ${data_ini}-${data_end}" >&2
        return 1
    fi
}

# Função para processar um decêndio completo
process_decendio() {
    local ano=$1 mes=$2 dia_ini=$3 dia_fim=$4 sufixo=$5
    
    local data_ini="$ano-$mes-$dia_ini"
    local data_end="$ano-$mes-$dia_fim"
    local base_name="$ano-$mes-$sufixo"
    
    # Flag para remover 29 de fevereiro se necessário
    local feb29_flag=""
    if [ "$mes" = "02" ] && [ "$dia_fim" = "29" ]; then
        feb29_flag="-del29feb"
    fi
    
    # 1. Eventos ≥5mm por ≥6 dias (não consecutivos)
    process_indicador "5mm_6dias" "$data_ini" "$data_end" \
        "${PATH_OUT}count_5/tmp_${base_name}.nc" \
        "-gec,6 -timsum -gec,5 $feb29_flag"
    
    # 2. Eventos ≥15mm por ≥3 dias (não consecutivos)
    process_indicador "15mm_3dias" "$data_ini" "$data_end" \
        "${PATH_OUT}count_15/tmp_${base_name}.nc" \
        "-gec,3 -timsum -gec,15 $feb29_flag"
    
    # 3. Eventos ≥5mm por ≥6 dias consecutivos (CWD)
    process_indicador "5mm_6dias_consec" "$data_ini" "$data_end" \
        "${PATH_OUT}count_cwd_5/tmp_${base_name}.nc" \
        "-eca_cwd,5,6 $feb29_flag"
    
    # 4. Eventos ≥10mm por ≥3 dias consecutivos (CWD)
    process_indicador "10mm_3dias_consec" "$data_ini" "$data_end" \
        "${PATH_OUT}count_cwd_10/tmp_${base_name}.nc" \
        "-eca_cwd,10,3 $feb29_flag"
    
    # 5. Dias com precipitação ≥20mm (R20mm)
    process_indicador "20mm" "$data_ini" "$data_end" \
        "${PATH_OUT}count_20/tmp_${base_name}.nc" \
        "-eca_r20mm $feb29_flag"
    
    # 6. Dias com precipitação ≥50mm
    process_indicador "50mm" "$data_ini" "$data_end" \
        "${PATH_OUT}count_50/tmp_${base_name}.nc" \
        "-eca_pd,50 $feb29_flag"
}

# Função para consolidar arquivos temporários
consolidar_arquivos() {
    local tipo=$1
    local padrao=$2
    
    echo "Consolidando arquivos para ${tipo}..."
    
    local arq_saida="${PATH_OUT}${tipo}/Count_${padrao}_${ANO_INICIAL}_${ANO_FINAL}.nc"
    local arq_mean="${PATH_OUT}${tipo}/Count_${padrao}_mean_${ANO_INICIAL}_${ANO_FINAL}.nc"
    
    cdo $CDO_FLAGS -mergetime "${PATH_OUT}${tipo}/tmp_"*".nc" "$arq_saida"
    
    if [ $? -eq 0 ]; then
        rm -f "${PATH_OUT}${tipo}/tmp_"*".nc"
        # Calcular média climatológica
        cdo $CDO_FLAGS -ydaymean "$arq_saida" "$arq_mean"
    else
        echo "ERRO: Falha ao consolidar arquivos para ${tipo}" >&2
    fi
}

# ============================= PROCESSAMENTO PRINCIPAL =======================

# Verificar se CDO está instalado
if ! command -v cdo &> /dev/null; then
    echo "ERRO: CDO (Climate Data Operators) não está instalado ou não está no PATH"
    exit 1
fi

# Criar diretórios de saída
create_dirs

echo "Iniciando processamento de indicadores climáticos (${ANO_INICIAL}-${ANO_FINAL})..."

# Converter listas para arrays legíveis pelo sh
set -- $DIAS

# Loop pelos anos
for ano in $(seq $ANO_INICIAL $ANO_FINAL); do
    echo "Processando ano $ano..."
    
    i=0
    for mes in $MESES; do
        # Pega o número de dias do mês correspondente
        dias=$(eval echo \${$((i + 1))})
        
        # Ajuste para anos bissextos (fevereiro com 29 dias)
        if [ "$mes" = "02" ] && is_leap_year $ano; then
            dias=29
        fi
        
        # Processar cada decêndio
        process_decendio "$ano" "$mes" "01" "10" "01"      # 1º decêndio (1-10)
        process_decendio "$ano" "$mes" "11" "20" "11"      # 2º decêndio (11-20)
        process_decendio "$ano" "$mes" "21" "$dias" "21"   # 3º decêndio (21-fim)
        
        i=$((i + 1))
    done
done

# Consolidar arquivos temporários e calcular médias
consolidar_arquivos "count_5" "5mm_6days"
consolidar_arquivos "count_15" "15mm_3days"
consolidar_arquivos "count_20" "20mm"
consolidar_arquivos "count_50" "50mm"
consolidar_arquivos "count_cwd_5" "cwd_5mm_6days"
consolidar_arquivos "count_cwd_10" "cwd_10mm_3days"

echo "Processamento concluído com sucesso!"
exit 0
