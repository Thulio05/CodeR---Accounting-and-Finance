library(tidyr)
library(dplyr)
library(tidyverse)
library(GetDFPData2)
library(BatchGetSymbols)

# Informações das Empresas
df_info <- get_info_companies()
names(df_info)
print(df_info)

companies_ids <- c(1023, 25500, 20087)  # Lista de IDs das empresas
first_year <- 2021
last_year <- 2022

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
  
  # Salvando dados referentes a Liquidez e semelhantes
  fr_ativ <- l_dfp$'DF Consolidado - Balanço Patrimonial Ativo' %>%
    select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)
  At_total <- unlist(fr_ativ[1, 4])
  At_circulante <- unlist(fr_ativ[2, 4])
  At_caixa <- unlist(fr_ativ[3, 4])
  At_receber <- unlist(fr_ativ[10, 4])
  At_estoque <- unlist(fr_ativ[16, 4])
  At_Ncirculante <- unlist(fr_ativ[30, 4])
  
  fr_passiv <- l_dfp$'DF Consolidado - Balanço Patrimonial Passivo' %>%
    select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)
  Ps_total <- unlist(fr_passiv[1, 4])
  Ps_circulante <- unlist(fr_passiv[2, 4])
  Ps_Ncirculante <- unlist(fr_passiv[59, 4])
  
  balanco_comercial <- At_total - Ps_total
  liquidez_corrente <- At_circulante/Ps_circulante
  liquidez_seca <- (At_circulante - At_estoque)/Ps_circulante
  liquidez_imediata <- At_caixa/Ps_circulante
  liquidez_geral <- (At_Ncirculante + At_circulante)/(Ps_circulante + Ps_Ncirculante)
  ind_caixa <- At_caixa/Ps_circulante
  
  # Salvando dados referentes a gestão de ativos e lucratividade
  fr_dre <- l_dfp$'DF Consolidado - Demonstração do Resultado' %>%
    select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)
  Custo_Venda <- unlist(fr_dre[2, 4])
  Receita_Venda <- unlist(fr_dre[1, 4])
  Receita_financeiro <- unlist(fr_dre[53, 4])
  
  Giro_estoque <- Custo_Venda * -1 / At_estoque
  Giro_conta_receber <- Receita_Venda / At_receber
  Margem_Lucro <- Receita_financeiro / Receita_Venda
  ROA <- Receita_financeiro / At_total
  
  # Salvando dados referentes a mutações do património liquido
  Mut_Patrimoliq <- l_dfp$'DF Consolidado - Demonstração das Mutações do Patrimônio Líquido'%>%
    select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)
  x <- unlist(fr_dre[152, 4])
  
  endiv_total <- (At_total - x) / At_total
  mult_ptl <- x / At_total
  
  # Exibindo os resultados para cada empresa
  cat("Empresa ID:", id_company, "\n")
  cat("Nome da Empresa:", company_name, "\n")
  cat("-----------------------------------", "\n")
  cat("Ativo Total:", At_total, "\n")
  cat("Ativo Circulante:", At_circulante, "\n")
  cat("Passivo Total:", Ps_total, "\n")
  cat("Passivo Circulante:", Ps_circulante, "\n")
  cat("Patrimônio Líquido:", balanco_comercial, "\n")
  cat("-----------------------------------", "\n")
  cat("Liquidez Corrente:", liquidez_corrente, "\n")
  cat("Liquidez Seca:", liquidez_seca, "\n")
  cat("Liquidez Imediata:", liquidez_imediata, "\n")
  cat("Liquidez Geral:", liquidez_geral, "\n")
  cat("Índice de Caixa:", ind_caixa, "\n")
  cat("-----------------------------------", "\n")
  cat("Giro de Estoque:", Giro_estoque, "\n")
  cat("Giro de contas a receber:", Giro_conta_receber, "\n")
  cat("Margem de Lucro:", Margem_Lucro, "\n")
  cat("ROA:", ROA, "\n")
  cat("-----------------------------------", "\n")
  cat("Endividamento Total:", endiv_total, "\n")
  cat("Multiplicador do Patrimônio Líquido:", mult_ptl, "\n")
  cat("--------------------------------------------\n")
}

