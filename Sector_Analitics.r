library(tidyr)
library(dplyr)
library(tidyverse)
library(GetDFPData2)
library(BatchGetSymbols)

# Informações das Empresas

df_info <- get_info_companies()
names(df_info)
print(df_info)

# --- --- ---

print("Análise de Dados")
# Qtd Empresas Ativas
qtd_empresas_ativas <- df_info %>%
  filter(SIT_REG == "ATIVO") %>%
  nrow()
cat("Quantidade total de empresas ativas:", qtd_empresas_ativas, "\n")

qtd_empresas_suspensa <- df_info %>%
  filter(SIT_REG == "SUSPENSO(A) - DECISÃO ADM") %>%
  nrow()
cat("Quantidade total de empresas suspensas:", qtd_empresas_suspensa, "\n")

qtd_empresas_cancelada <- df_info %>%
  filter(SIT_REG == "CANCELADA") %>%
  nrow()
cat("Quantidade total de empresas canceladas:", qtd_empresas_cancelada, "\n\n")
cat("Quantidade total de empresas :", qtd_empresas_cancelada+qtd_empresas_suspensa+qtd_empresas_ativas, "\n\n")


# Contar empresas ativas por setor e ordenar pelo maior número
setores_qntd_ativas <- df_info %>%
  filter(SIT_REG == "ATIVO") %>%
  count(SETOR_ATIV) %>%
  arrange(desc(n))

# --- --- ---

lista_palavras <- list(
  termo1 = "Comunicação e Informática",
  termo2 = "Energia Elétrica",
  termo3 = "Bancos"
)

# Percorrer a lista com um loop for

cat("Setores selecionados: \nComunicação e Informática\nEnergia Elétrica\nBancos\n\n")

empresas_ativas_informatica <- df_info %>%
  filter(SIT_REG == "ATIVO" & SETOR_ATIV == "Comunicação e Informática") %>%
  select(CNPJ, DENOM_SOCIAL, DENOM_COMERC, DT_REG, DT_CANCEL, DT_CONST, MOTIVO_CANCEL, SIT_REG, DT_INI_SIT, CD_CVM, SETOR_ATIV, TP_MERC, CATEG_REG, DT_INI_CATEG, SIT_EMISSOR, DT_INI_SIT_EMISSOR, CONTROLE_ACIONARIO, UF)
empresas_ativas_energia <- df_info %>%
  filter(SIT_REG == "ATIVO" & SETOR_ATIV == "Energia Elétrica") %>%
  select(CNPJ, DENOM_SOCIAL, DENOM_COMERC, DT_REG, DT_CANCEL, DT_CONST, MOTIVO_CANCEL, SIT_REG, DT_INI_SIT, CD_CVM, SETOR_ATIV, TP_MERC, CATEG_REG, DT_INI_CATEG, SIT_EMISSOR, DT_INI_SIT_EMISSOR, CONTROLE_ACIONARIO, UF)
empresas_ativas_bancos <- df_info %>%
  filter(SIT_REG == "ATIVO" & SETOR_ATIV == "Bancos") %>%
  select(CNPJ, DENOM_SOCIAL, DENOM_COMERC, DT_REG, DT_CANCEL, DT_CONST, MOTIVO_CANCEL, SIT_REG, DT_INI_SIT, CD_CVM, SETOR_ATIV, TP_MERC, CATEG_REG, DT_INI_CATEG, SIT_EMISSOR, DT_INI_SIT_EMISSOR, CONTROLE_ACIONARIO, UF)

for (termo in lista_palavras) {
  empresas_ativas_setor <- df_info %>%
    filter(SIT_REG == "ATIVO" & SETOR_ATIV == termo)
  
  # Contar e Imprimir a Quantidade de Empresas Ativas nesse Setor
  qtd_empresas_ativas_setor <- empresas_ativas_setor %>%
    nrow()
  cat("Quantidade de empresas ativas no setor",termo,":", qtd_empresas_ativas_setor, "\n")
}
cat("\n")
for (termo in lista_palavras) {
  empresas_ativas_setor <- df_info %>%
    filter(SIT_REG == "SUSPENSO(A) - DECISÃO ADM" & SETOR_ATIV == termo)
  
  # Contar e Imprimir a Quantidade de Empresas Ativas nesse Setor
  qtd_empresas_ativas_setor <- empresas_ativas_setor %>%
    nrow()
  cat("Quantidade de empresas suspensas no setor",termo,":", qtd_empresas_ativas_setor, "\n")
}
cat("\n")
for (termo in lista_palavras) {
  empresas_ativas_setor <- df_info %>%
    filter(SIT_REG == "CANCELADA" & SETOR_ATIV == termo)
  
  # Contar e Imprimir a Quantidade de Empresas Ativas nesse Setor
  qtd_empresas_ativas_setor <- empresas_ativas_setor %>%
    nrow()
  cat("Quantidade de empresas canceladas no setor",termo,":", qtd_empresas_ativas_setor, "\n")
}

