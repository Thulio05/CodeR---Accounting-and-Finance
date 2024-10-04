library(tidyr)
library(dplyr)
library(GetDFPData2)

# Informações das Empresas
df_info <- get_info_companies()

first_year <- 2021
last_year <- 2022

indi_calc <- function(id_company) {
  company_name <- df_info %>%
    filter(CD_CVM == id_company) %>%
    select(DENOM_SOCIAL) %>%
    pull()
  
  cat("Analisando a empresa:", company_name, "\n")
  
  l_dfp <- get_dfp_data(companies_cvm_codes = id_company,
                        type_docs = '*',  # pegar todos os tipos de documentos
                        type_format = 'con',  # consolidado
                        first_year = first_year,
                        last_year = last_year)
  
  fr_ativ <- l_dfp$'DF Consolidado - Balanço Patrimonial Ativo' %>%
    select(VL_CONTA)
  At_circulante <- unlist(fr_ativ[2, 1])  # Pegando o Ativo Circulante
  
  fr_passiv <- l_dfp$'DF Consolidado - Balanço Patrimonial Passivo' %>%
    select(VL_CONTA)
  Ps_circulante <- unlist(fr_passiv[2, 1])  # Pegando o Passivo Circulante
  
  if (Ps_circulante > 0) {
    liquidez_corrente <- At_circulante / Ps_circulante
    cat("Índice de Liquidez Corrente para", company_name, ":", liquidez_corrente, "\n")
  } else {
    cat("Dados insuficientes para calcular a liquidez de", company_name, "\n")
  }
}

# Menu de seleção de setor
cat("\nDigite qual setor deve ser analisado\n")
cat("[1] - Informática\n[2] - Bancos\n[0] - Fechar\n\n")

entr <- as.numeric(scan(what = integer(), nmax = 1, quiet = TRUE))

if (entr == 0) {
  cat("Saindo...\n")
} else if (entr == 1) {
  empresas_ativas_informatica <- df_info %>%
    filter(SIT_REG == "ATIVO" & SETOR_ATIV == "Comunicação e Informática") %>%
    select(CD_CVM) %>%
    pull()
  
  for (id_empresa in empresas_ativas_informatica) {
    indi_calc(id_empresa)
    cat("Empresa ID:", id_empresa, "\n")
  }
  
} else if (entr == 2) {
  empresas_ativas_bancos <- df_info %>%
    filter(SIT_REG == "ATIVO" & SETOR_ATIV == "Bancos") %>%
    select(CD_CVM) %>%
    pull()
  
  for (id_empresa in empresas_ativas_bancos) {  # Corrigido o parêntese
    indi_calc(id_empresa)
    cat("Empresa ID:", id_empresa, "\n")
  }
}
