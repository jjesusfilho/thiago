busca <- r"("acidente de trânsito" E (invalidez OU incapacidade OU morte OU óbito OU falecimento) NAO DPVAT)"

assuntos <- "10433,10434,10435,10436,10437,14920,14922"

library(tidyverse)

library(tjsp)

autenticar()

tjsp_baixar_cjsg(busca, assunto = assuntos, diretorio = "data-raw/cjsg_dano_moral")

arquivos <- JurisMiner::listar_arquivos("data-raw/cjsg_dano_moral")
cjsg <- tjsp_ler_cjsg(arquivos)
cjsg <- cjsg |> 
     filter(str_detect(classe, "(?i)apelação"))

saveRDS(cjsg, "data/dano_moral/cjsg.rds")
library(tjsp)

autenticar()


tjsp_baixar_cposg(cjsg$processo,"data-raw/cposg_dano_moral")

arquivos <- JurisMiner::listar_arquivos("data-raw/cposg_dano_moral")

dados <- ler_dados_cposg(arquivos)

partes <- ler_partes(arquivos)

dispositivos <- tjsp_ler_dispositivo(arquivos)

dispositivos <- dispositivos |> 
     mutate(decisao = tjsp::tjsp_classificar_recurso(dispositivo))

cjsg <- cjsg |> 
    filter(assunto != "Erro Médico", assunto != "Direito de Imagem")

cjsg <- cjsg |> 
    filter(str_detect(ementa, "COMPET.NCIA", negate = T))


cjsg <- cjsg  %>%
  mutate(inc_ementa = str_detect(ementa,"(?i)incapaci\\w+"),
         inv_ementa = str_detect(ementa,"(?i)invalidez"),
         perm_ementa = str_detect(ementa, "(?i)permanente"),
         temp_ementa = str_detect(ementa,"(?i)tempor[aá]ria"),
         total_ementa = str_detect(ementa,"(?i)total"),
         parcial_ementa = str_detect(ementa,"(?i)parcial"),
         laborativa_ementa = str_detect(ementa,"(?i)laborativa"),
         funcional_ementa = str_detect(ementa,"(?i)funcional"),
         obito_ementa = str_detect(ementa,"(?i)(óbito|fatal|morte|falec|homic.dio)"),
         lesao_ementa = str_detect(ementa,"(?i)lesão")
  )


cjsg <- cjsg |> 
    rowwise() |> 
  mutate(soma = sum(c_across(contains("_ementa"))))

zeros <- cjsg |> 
     filter(soma == 0)

zeros <- zeros |> 
    filter(str_detect(ementa, "(?i)culpa.{5,30}da vítima", negate =T))

zeros <- zeros |> 
       left_join(dispositivos, by = "processo")

zeros |> 
   count(decisao)

zeros <- zeros |> 
    filter(str_detect(decisao, "parcial|provido"))

a <- JurisMiner::listar_arquivos("data-raw/acordaos_dano_moral")

julgados <- tjsp_ler_acordaos_cjsg(a)
