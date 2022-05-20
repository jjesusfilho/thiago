library(tjsp)
library(tidyverse)
busca <- r"("ACIDENTE DE TRÂNSITO" e "pensão mensal" e vitalícia')"

tjsp::baixar_cjsg(busca, diretorio = "~/Documents/pacotes/projetos/thiago/data-raw")


cjsg <- tjsp_ler_cjsg(diretorio = "data-raw")

assuntos <- cjsg %>% 
         count(assunto, sort = TRUE)

apelacao <- cjsg %>% 
          filter(str_detect(classe,"(?i)apelação")) %>% 
          filter(str_detect(classe, "(?i)criminal", negate = TRUE))

count(apelacao, classe, sort = TRUE) %>% View()
