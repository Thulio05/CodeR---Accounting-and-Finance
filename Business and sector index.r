library(tidyr)
library(dplyr)
library(tidyverse)
library(GetDFPData2)
library(BatchGetSymbols)

# Informações das Empresas
df_info <- get_info_companies()
names(df_info)
print(df_info)

# Definindo parâmetros
companies_ids <- c(1023, 25500, 20087)  # Lista de IDs das empresas
first_year <- 2021
last_year <- 2023

# Loop para baixar e processar dados para cada empresa
for (id_company in companies_ids) {
  
  # Obtendo o nome da empresa a partir de df_info
  company_name <- df_info %>%
    filter(id_company == CD_CVM) %>%
    select(DENOM_SOCIAL) %>%
    pull()
  
  # Baixando dados DFP
  l_dfp <- get_dfp_data(companies_cvm_codes = id_company,
                        type_docs = '*',  # pegar todos os tipos de documentos
                        type_format = 'con',  # consolidado
                        first_year = first_year,
                        last_year = last_year)
  
  fr_ativ <- l_dfp$'DF Consolidado - Balanço Patrimonial Ativo' %>%
    select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)
  At_total <- sum(fr_ativ$VL_CONTA)
  At_circulante <- unlist(fr_ativ[2, 4])
  
  fr_passiv <- l_dfp$'DF Consolidado - Balanço Patrimonial Passivo' %>%
    select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)
  Ps_total <- sum(fr_passiv$VL_CONTA)
  Ps_circulante <- unlist(fr_passiv[2, 4])
  
  fr_dre <- l_dfp$'DF Consolidado - Demonstração do Resultado do Exercício'
  
  
  balanco_comercial <- At_total - Ps_total
  indice_liquidez <- At_circulante/Ps_circulante
  
  # Exibindo os resultados para cada empresa
  cat("Empresa ID:", id_company, "\n")
  cat("Nome da Empresa:", company_name, "\n")
  cat("-----------------------------------", "\n")
  cat("Ativo Total:", At_total, "\n")
  cat("Ativo Circulante:", At_circulante, "\n")
  cat("Passivo Total:", Ps_total, "\n")
  cat("Passivo Circulante:", Ps_circulante, "\n")
  cat("-----------------------------------", "\n")
  cat("Patrimônio Líquido:", balanco_comercial, "\n")
  cat("Índice de Liquidez:", indice_liquidez, "\n")
  cat("--------------------------------------------\n")
}
