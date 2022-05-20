conn <- DBI::dbConnect(RPostgres::Postgres(),host="direito.consudata.com.br",dbname="projetos",user=Sys.getenv("DBUSER"),password=Sys.getenv("DBPASSWORD"))

DBI::dbExecute(conn,"set search_path = 'thiago'")
df <- rpsql::pg_d(conn)

assuntos <- DBI::dbGetQuery(conn, "select assunto from assuntos where relevancia = 'sim'")

library(tidyverse)
cjsg <- cjsg %>% 
  filter(str_detect(classe,"(?i)apelação")) %>% 
  filter(str_detect(classe, "(?i)criminal", negate = TRUE)) %>% 
  semi_join(assuntos)


cjsg <- cjsg  %>%
  mutate(inc_ementa = str_detect(ementa,"(?i)incapaci\\w+"),
         inv_ementa = str_detect(ementa,"(?i)invalidez"),
         perm_ementa = str_detect(ementa, "(?i)permanente"),
         temp_ementa = str_detect(ementa,"(?i)tempor[aá]ria"),
         total_ementa = str_detect(ementa,"(?i)total"),
         parcial_ementa = str_detect(ementa,"(?i)parcial"),
         laborativa_ementa = str_detect(ementa,"(?i)laborativa"),
         funcional_ementa = str_detect(ementa,"(?i)funcional")
  )   


cjsg <- cjsg %>% 
  rowwise() %>%
  mutate(soma = sum(c_across(inc_ementa:funcional_ementa)))

cjsg <- cjsg %>% 
      filter(soma > 0)


saveRDS(cjsg,"data/cjsg_filtrado.rds")

tjsp::autenticar()

tjsp::tjsp_baixar_cposg(cjsg$processo, "data-raw/cposg")

arquivos <- JurisMiner::listar_arquivos("data-raw/cposg")

dados <- tjsp::ler_dados_cposg(arquivos)

dados <- dados %>% 
   select(processo, assunto,orgao_julgador, origem, relator, valor_da_acao) %>% 
   mutate(valor_da_acao = numero(valor_da_acao))

partes <- tjsp_ler_partes(arquivos)

partes <- partes %>% 
       mutate(tipo_parte = snakecase::to_snake_case(tipo_parte)) %>% 
       filter(str_detect(tipo_parte,"apel")) %>% 
       mutate(tipo_parte = case_when(
         tipo_parte == "apelada" ~ "apelado",
         TRUE ~ tipo_parte
         
       ))


partes <- partes %>%
  mutate(pessoa = case_when(
    str_detect(parte, "(?i)(bradesco|\\bbb\\b|segur[ao]|tokio|banco)") ~ "banco_seguradora",
    str_detect(parte, "(?i)(transp|estrada|via[çc][aã]o|[ôo]nibus)") ~ "transporte",
    str_detect(parte, "(?i)(t[ée]rio p[úu]blico|justi[cç]a\\s+p[uú]blica|\\bmp\\b)") ~ "MP",
    str_detect(parte, "(?i)defensoria") ~ "Defensoria",
    str_detect(parte,"(?i)fazenda") ~ "poder_executivo",
    str_detect(parte,"(?i)partido") ~ "partido",
    str_detect(parte,"(?i)(assoc|direitos|defesa|sociedade|federa[cç][ãa]o|estudant|acad.mico|ocupante|funcion[áa]rio|prote[cç]|sindicato|\\bsind\\.?\\b|sint|morador|funda[çc][ãa]o|instituto|movimento|organiza|consumidor|cons\\.|ong|usu[áa]rio|conselho)") ~ "TS",
    str_detect(parte,"(?i)(munic|prefeit|\\estad\\w+|departamento)") ~ "poder_executivo",
    str_detect(parte,"(?i)\\bcentro\\b") ~ "centro",
    str_detect(parte, "(?i)cooperativa") ~  "cooperativa",
    str_detect(parte,"(?i)(\\bs[./]?a\\.?$|\\bs\\.\\a\\.|\\bs/a.?\\b|ltda\\.?|\\b[aá]gua\\b|usina|empreend|com[ée]rci|representa|\\bME\\.?\\b|\\bMEI\\.?\\b|\\bEPP\\.?\\b|eirel[ei]|\\bs/?c\\b|companhia|\\bcia\\b)") ~ "PJ",
    TRUE ~  "PF"))

partes <- partes %>% 
        select(processo, tipo_parte, parte, pessoa) %>% 
        pivot_wider(id_cols = processo, names_from = "tipo_parte", values_from = c("parte","pessoa"))

saveRDS(partes, "data/partes.rds")

db <- tibble(arquivo = arquivos) %>% 
      mutate(processo = stringr::str_extract(arquivo, "\\d{20}"))

db <- db %>% 
    semi_join(dados, by = "processo") %>% 
    semi_join(partes, by = "processo")

dispositivo <- tjsp::tjsp_ler_dispositivo(db$arquivo)

dispositivo <- dispositivo %>% 
            mutate(decisao = tjsp::tjsp_classificar_recurso(dispositivo) )

saveRDS(dispositivo, "data/dispositivo.rds")


### Organização das partes
library(tidyverse)
p <- partes |> 
    mutate(apelante = map(pessoa_apelante, ~{.x |> sort() |> unique() |> str_c(collapse = ",")}) |> unlist()) |> 
   mutate(apelado = map(pessoa_apelado, ~{.x |> sort() |> unique() |> str_c(collapse = ",")}) |> unlist())

p <- p |> 
    select(processo, apelante, apelado)

p <- p |> 
    filter(apelante != "")


p |> 
   count(apelante, apelado) |> 
    View()
