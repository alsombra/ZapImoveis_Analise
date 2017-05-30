#######################################
library(xlsx)
library(rvest)
library(stringr)
#library(DBI)
#library(RSQLite)
#######################################
zapmoveis_crawler <- function(total_paginas = 0){
  if(total_paginas == 0){
    i <- 1
    i <- as.character(i)
    url <- paste0('https://www.zapimoveis.com.br/venda/apartamentos/agr+rj+rio-de-janeiro+zona-sul/#{"precomaximo":"2147483647","parametrosautosuggest":[{"Bairro":"","Zona":"","Cidade":"RIO DE JANEIRO","Agrupamento":"ZONA SUL","Estado":"RJ"}],"pagina":"',
                  i,'","ordem":"Relevancia","paginaOrigem":"ResultadoBusca","semente":"921582413","formato":"Lista"}')
    html <- read_html(url)
    total_pag_aux <- html %>%
      html_node(".pull-right.num-of") %>%
      html_text(trim=TRUE)
    
    total_pag <- str_sub(string = total_pag_aux,
                         start = 4,
                         end = -1)
    total_pag <- sub(x = total_pag,
                     pattern = "[[:punct:]]",
                     replacement = "")
    total_pag <- as.numeric(total_pag)
    cat("Total de paginas: ", total_pag)
  } else {
    total_pag <- total_paginas
  }
  frame_principal <- data.frame(link = character(),
                                preco = character(),
                                bairro = character(),
                                endereco = character(),
                                quartos = character(),
                                area = character(),
                                suites=character(),
                                vagas=character(),
                                descricao = character())
  for(i in 1:total_pag){
    i <- as.character(i)
    url <- paste0('https://www.zapimoveis.com.br/venda/apartamentos/agr+rj+rio-de-janeiro+zona-sul/#{"precomaximo":"2147483647","parametrosautosuggest":[{"Bairro":"","Zona":"","Cidade":"RIO DE JANEIRO","Agrupamento":"ZONA SUL","Estado":"RJ"}],"pagina":"',
                  i,'","ordem":"Relevancia","paginaOrigem":"ResultadoBusca","semente":"921582413","formato":"Lista"}')
    html <- read_html(url)
    precos <- html %>%
              html_nodes(".preco strong") %>%
              html_text(trim=TRUE)
    
    links <- html%>%
             html_nodes(".caracteristicas a") %>%
             html_attr(name = "href")
    bairro <- html %>%
              html_nodes(".endereco h2 strong") %>%
              html_text(trim=TRUE)
    
    endereco <- html %>%
                html_nodes(".endereco h2") %>%
                html_text(trim=TRUE)
    for(j in 1:length(endereco)){
      endereco[j] <- sub(x = endereco[j],
                         pattern = bairro[j],
                         replacement = "")
      endereco[j] <- sub(x = endereco[j],
                         pattern = "\r\n",
                         replacement = "")
    }
    
    caracteristicas <- html %>%
                       html_nodes(".caracteristicas")
    suites <- character()
    vagas <- character()
    quartos <- character()
    for(j in 1:length(caracteristicas)){
      suite <- caracteristicas[j] %>%
               html_node(".icone-suites") %>% 
               html_text(trim=TRUE)
      
      quarto <- caracteristicas[j] %>%
        html_node(".icone-quartos") %>% 
        html_text(trim=TRUE)
      
      vaga <- caracteristicas[j] %>%
              html_node(".icone-vagas") %>% 
              html_text(trim=TRUE)
      suites[j] <- suite
      vagas[j] <- vaga
      quartos[j] <- quarto
     }
    area <- html %>%
            html_nodes(".caracteristicas .icone-area") %>%
            html_text(trim=TRUE)
    descricao <- html %>%
                 html_nodes(".endereco p") %>%
                 html_text(trim=TRUE)
    novo_df <- data.frame(link = links,
                          preco = precos,
                          bairro = bairro,
                          endereco = endereco,
                          quartos = quartos,
                          area = area,
                          suites=suites,
                          vagas=vagas,
                          descricao = descricao)
    frame_principal <- rbind(frame_principal,novo_df)
    cat(paste0("Pagina ",i," - ",total_pag,"\n"))
  }
  return(frame_principal)
}
#### Como usar a funcao ####
df <- zapmoveis_crawler(total_paginas = 1400)
write.xlsx2(x = df,
            file = paste0("zap_resultado1400_",Sys.Date(),".xlsx"),
            append = FALSE)







