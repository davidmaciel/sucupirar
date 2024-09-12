library(RSelenium)
library(rvest)
library(janitor)
library(dplyr)
library(purrr)
library(stringr)

#funcões de conexao-------
selenium_start <- function(){
  system("powershell docker run -d -p 4445:4444 -p 5901:5900 selenium/standalone-firefox-debug")


}

selenium_driver <- function(){
  remDr <- remoteDriver(
    port = 4445L
  )
  remDr$open(silent = F)
  remDr$setTimeout(type = "implicit", 30000)
  return(remDr)
}

selenium_stop <- function(){
  system("powershell docker stop $(docker ps -q)")
}

#login na sucupira, entra no painel de módulos
sucupira_login <- function(remDr, usuario, senha){
  remDr$navigate("https://sucupira-legado.capes.gov.br/sucupira/public/index.xhtml")
  b1 <-
    remDr$findElement("css", 
                      "#cabecalho > div > div.col-sm-4.col-md-4.col-lg-3 > a > p")
  b1$setTimeout("implicit", 10000)
  b1$clickElement()
  
  
  
  ele <- remDr$findElement(using = "css", 
                           "#buttonAcessoCapes")
  ele$setTimeout("implicit", 10000)
  ele$clickElement()
  
  login <- remDr$findElement(using = "css",
                             "#login")
  password <- remDr$findElement(using = "css",
                             "#senha")
  login$sendKeysToElement(list(usuario))
  password$sendKeysToElement(list(senha))
  btn_entrar <- remDr$findElement(using = "css",
                                  "#textoBotao")
  btn_entrar$setTimeout("implicit", 3e4)
  btn_entrar$clickElement()
  return(remDr)
  }

#funções de navegação-----
#do painel de modulos, entra no portal do coordenador
enter_coord <- function(remDr){
coordenador <- remDr$findElement("css", "#formPortais\\:portalCoordenadorPrograma > img:nth-child(1)")
coordenador$setTimeout("implicit", 1e4)
coordenador$clickElement()
return(remDr)
}

#do portal do cordenador, entra na parte de docentes
enter_docente <- function(remDr){
  remDr$setTimeout("implicit", 1e4)
remDr$navigate("https://sucupira-legado.capes.gov.br/sucupira/coleta_online/manutencaoDocente/listaDocente.jsf")
return(remDr)
}

enter_discente <- function(remDr){
  remDr$setTimeout("implicit", 1e4)
  remDr$navigate("https://sucupira-legado.capes.gov.br/sucupira/coleta_online/discente/listagemManutencaoDiscente.jsf")
  return(remDr)
}

enter_egresso <- function(remDr){
  remDr$navigate("https://sucupira-legado.capes.gov.br/sucupira/portais/coord_programa/index.jsf")
  egresso <- remDr$findElement("css", "#formAbaColeta\\:btnEgressos > img:nth-child(1)")
  egresso$setTimeout("implicit", 1e4)
  egresso$clickElement()
  return(remDr)
}


enter_pi <- function(remDr){
  remDr$setTimeout("implicit", 1e4)
  remDr$navigate("https://sucupira-legado.capes.gov.br/sucupira/coleta_online/producaoIntelectual/listaProducao.jsf")
  return(remDr)
}

enter_conclusao <- function(remDr){
  botao <- remDr$findElement(using = "css", "#formAbaColeta\\:btnTrabalhoConclusao > img:nth-child(1)")
  botao$clickElement()
  return(remDr)
}

enter_projeto <- function(remDr){
  remDr$setTimeout("implicit", 1e4)
  remDr$navigate("https://sucupira-legado.capes.gov.br/sucupira/coleta_online/projetoPesquisa/listaProjetoPesquisa.jsf")
  return(remDr)
}

enter_conc <- function(remDr){
  remDr$setTimeout("implicit", 1e4)
  remDr$navigate("https://sucupira-legado.capes.gov.br/sucupira/coleta_online/manutencaoTrabalhoConclusao/listaTrabalhoConclusao.jsf")
  return(remDr)
}
#funções de busca-----

#buscar produção intelectual não precisa marcar o ano
search_pi <- function(remDr,tipo = c("BIBLIOGRÁFICA", 
                                     "ARTÍSTICA", 
                                     "TÉCNICA"),
                      subtipo = c("ARTIGO EM PERIÓDICO",
                                  "LIVRO",
                                  "TRABALHO EM ANAIS",
                                  "ARTIGO EM JORNAL OU REVISTA",
                                  "TRADUÇÃO",
                                  "OUTRO")){
  remDr$navigate("https://sucupira-legado.capes.gov.br/sucupira/coleta_online/producaoIntelectual/listaProducao.jsf")
  tipo <- match.arg(tipo)
  subtipo <- match.arg(subtipo)
  # campo_ano <- remDr$findElement("css", "#form\\:ano")
  # campo_ano$sendKeysToElement(list(as.character(ano)))
  
  campo_tipo <- remDr$findElement("css", "#form\\:tipo")
  campo_tipo$sendKeysToElement(list(tipo))
  subtipo_campo <- remDr$findElement("css", "#form\\:subtipo")
  subtipo_campo$sendKeysToElement(list(subtipo))
  
  
  
  
  btn_consultar <- remDr$findElement("css", "#form\\:consultar")
  btn_consultar$setTimeout("implicit", 1e4)
  btn_consultar$clickElement()
  return(remDr)
  
}

#como o vínculo da pessoa muda por ano, essa busca tem que levar em conta o ano. 
search_pessoa <- function(remDr, ano){
  year <- remDr$findElement(using = "css", 
                            "#form\\:ano")
  year$clearElement()
  year$sendKeysToElement(list(as.character(ano)))#argumento
  consultar <- remDr$findElement(using = "css", "#form\\:consultar")
  btn_consultar$setTimeout("implicit", 1e4)
  btn_consultar$clickElement()
  return(remDr)
}


#funções de pegar tabelas--------

pega_tabela_artigos <- function(remDr){
  #achar o máximo de páginas
  max_page <- remDr$findElement("css", "#form\\:j_idt214\\:j_idt221")
  pages <- max_page$findChildElements("tag name", "option")
  page_numbers <- 1:length(pages)
  
  pegador_artigos <- function(page_number){
    max_page <- remDr$findElement("css", "#form\\:j_idt214\\:j_idt221")
    pages <- max_page$findChildElements("tag name", "option")
    pages[[page_number]]$setTimeout("implicit", 1e4)
    pages[[page_number]]$clickElement()
    Sys.sleep(sample(c(5:10), 1))
    try(remDr$dismissAlert(), silent = T)
    
    
    tab_elem <- remDr$findElement("css", ".listagem")
    
    tab_elem$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_table() %>% 
      pluck(1) %>% 
      clean_names() %>% 
      select(titulo, "periodico" = titulo_periodico, 
             autor_principal, "ano" = ano_da_publicacao)->
      tabela_artigos
    
    tab_elem$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_element("table") %>% 
      html_elements("a") %>% 
      html_attr("href") ->
      links
    
    links <- links[!str_detect(links, "^#")]
    links <- str_c("https://sucupira-legado.capes.gov.br", links) %>% str_replace( "true", "false")
    tabela_artigos %>% 
      mutate(link = links)
  }
  map_dfr(page_numbers, pegador_artigos)
  
}

pega_tabela_livros <- function(remDr){
  #achar o máximo de páginas
  max_page <- remDr$findElement("css", "#form\\:j_idt214\\:j_idt221")
  pages <- max_page$findChildElements("tag name", "option")
  page_numbers <- 1:length(pages)
  
  pegador_livros <- function(page_number){
    max_page <- remDr$findElement("css", "#form\\:j_idt214\\:j_idt221")
    pages <- max_page$findChildElements("tag name", "option")
    pages[[page_number]]$setTimeout("implicit", 1e4)
    pages[[page_number]]$clickElement()
    Sys.sleep(sample(c(5:10), 1))
    try(remDr$dismissAlert(), silent = T)
    
    
    tab_elem <- remDr$findElement("css", ".listagem")
    
    tab_elem$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_table() %>% 
      pluck(1) %>% 
      clean_names() %>% 
      select(titulo,  
             autor_principal, "ano" = ano_da_publicacao)->
      tabela_livros
    
    tab_elem$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_element("table") %>% 
      html_elements("a") %>% 
      html_attr("href") ->
      links
    
    links <- links[!str_detect(links, "^#")]
    links <- str_c("https://sucupira-legado.capes.gov.br", links) %>% str_replace( "true", "false")
    tabela_livros %>% 
      mutate(link = links)
  }
  map_dfr(page_numbers, pegador_livros)
  
}

pega_tabela_anais <- function(remDr){
  max_page <- remDr$findElement("css", "#form\\:j_idt214\\:j_idt221")
  pages <- max_page$findChildElements("tag name", "option")
  page_numbers <- 1:length(pages)
  
  pegador_anais <- function(page_number){
    max_page <- remDr$findElement("css", "#form\\:j_idt214\\:j_idt221")
    pages <- max_page$findChildElements("tag name", "option")
    pages[[page_number]]$setTimeout("implicit", 1e4)
    pages[[page_number]]$clickElement()
    Sys.sleep(sample(c(5:10), 1))
    try(remDr$dismissAlert(), silent = T)
    
    
    tab_elem <- remDr$findElement("css", ".listagem")
    
    tab_elem$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_table() %>% 
      pluck(1) %>% 
      clean_names() %>% 
      select(titulo,  
             autor_principal, "ano" = ano_da_publicacao)->
      tabela_anais
    
    tab_elem$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_element("table") %>% 
      html_elements("a") %>% 
      html_attr("href") ->
      links
    
    links <- links[!str_detect(links, "^#")]
    links <- str_c("https://sucupira-legado.capes.gov.br", links) %>% str_replace( "true", "false")
    tabela_anais %>% 
      mutate(link = links)
  }
  map_dfr(page_numbers, pegador_anais)
}

pega_tabela_conclusao <- function(remDr){
  
  max_page <- remDr$findElement("css", "#formListagem\\:j_idt189\\:j_idt196")
  pages <- max_page$findChildElements("tag name", "option")
  page_numbers <- 1:length(pages)
  
  pegador_conc <- function(page_number){
    max_page <- remDr$findElement("css", "#formListagem\\:j_idt189\\:j_idt196")
    pages <- max_page$findChildElements("tag name", "option")
    pages[[page_number]]$setTimeout("implicit", 1e4)
    pages[[page_number]]$clickElement()
    Sys.sleep(sample(c(5:10), 1))
    try(remDr$dismissAlert(), silent = T)
    
    
    tab_elem <- remDr$findElement("css", ".listagem")
    
    tab_elem$getPageSource()[[1]] %>%  read_html() %>% 
      html_table() %>%
      pluck(3) %>% 
      clean_names() %>% 
      select(titulo, autor, "tipo" = tipo_de_trabalho_de_conclusao, "data" = data_da_defesa) %>% 
      filter(!is.na(autor)) %>% 
      distinct()->
      tabela_conc
    
    tab_elem$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_elements("table") %>% 
      pluck(3) %>% 
      html_elements("a") %>% 
      html_attr("href") -> links
    links <- links[!str_detect(links, "^#")]
    links <- str_c("https://sucupira-legado.capes.gov.br", links) %>% str_replace( "true", "false")
    tabela_conc %>% 
      mutate(link = links)
  }
  map_dfr(page_numbers, pegador_conc)
  
}

pega_tabela_discente <- function(remDr){
  
  max_page <- remDr$findElement("css", "#form\\:j_idt200\\:cmbPagina")
  pages <- max_page$findChildElements("tag name", "option")
  page_numbers <- 1:length(pages)
  
  pegador_disc <- function(page_number){
    max_page <- remDr$findElement("css", "#form\\:j_idt200\\:cmbPagina")
    pages <- max_page$findChildElements("tag name", "option")
    pages[[page_number]]$setTimeout("implicit", 1e4)
    pages[[page_number]]$clickElement()
    Sys.sleep(sample(c(5:10), 1))
    try(remDr$dismissAlert(), silent = T)
    
    
    tab_elem <- remDr$findElement("css", ".listagem")
    
    tab_elem$getPageSource()[[1]] %>%  read_html() %>% 
      html_table() %>%
      pluck(3) %>% 
      clean_names() %>% 
      select(discente, orientador, nivel, situacao) %>% 
      filter(!is.na(discente)) %>% 
      distinct() ->
      tabela_disc
    
    tab_elem$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_elements("table") %>% 
      pluck(3) %>% 
      html_elements("a") %>% 
      html_attr("href") -> links
    links <- links[!str_detect(links, "^#")]
    links <- str_c("https://sucupira-legado.capes.gov.br", links) %>% str_replace( "true", "false")
    tabela_disc %>% 
      mutate(link = links)
  }
  map_dfr(page_numbers, pegador_disc)
  
}

pega_tabela_egresso <- function(remDr){
  max_page <- remDr$findElement("css", "#form\\:j_idt120\\:cmbPagina")
  pages <- max_page$findChildElements("tag name", "option")
  page_numbers <- 1:length(pages)
  
  pegador_egresso <- function(page_number){
    max_page <- remDr$findElement("css", "#form\\:j_idt120\\:cmbPagina")
    pages <- max_page$findChildElements("tag name", "option")
    pages[[page_number]]$setTimeout("implicit", 1e4)
    pages[[page_number]]$clickElement()
    Sys.sleep(sample(c(5:10), 1))
    try(remDr$dismissAlert(), silent = T)
    
    
    tab_elem <- remDr$findElement("css", ".listagem")
    
    tab_elem$getPageSource()[[1]] %>%  read_html() %>% 
      html_table() %>%
      pluck(2) %>% 
      clean_names() %>% 
      select(egresso, nivel, "ano" = ano_da_titulacao) %>% 
      filter(!is.na(egresso)) %>% 
      distinct() ->
      tabela_egresso
    
    tab_elem$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_elements("table") %>% 
      pluck(2) %>% 
      html_elements("a") %>% 
      html_attr("href") -> links
    ind <- str_detect(links, "view")
    links <- links[ind]
    links <- str_c("https://sucupira-legado.capes.gov.br", links) %>% str_replace( "true", "false")
    tabela_egresso %>% 
      mutate(link = links)
  }
  map_dfr(page_numbers, pegador_egresso)
  
}

pega_tabela_docente <- function(remDr){
  html <- remDr$getPageSource()[[1]]
  html %>% read_html() %>% 
    html_element(css = ".listagem") %>% 
    html_table() %>% 
    clean_names() %>% 
    select(docente, categoria) -> tabela_docente
  
  html %>% read_html() %>% 
    html_element(css = ".listagem") %>% 
    html_elements("a") %>% 
    html_attr("href") -> links
  links <- links[!str_detect(links, "^#")]
  links <- str_c("https://sucupira-legado.capes.gov.br", links) %>% str_replace( "true", "false")
  tabela_docente %>% 
    mutate(link = links)
}
#pega_detalhes------

pega_detalhes_artigo <- function(remDr, link){
  remDr$navigate(link)
  html <- remDr$getPageSource()[[1]] %>% read_html()
  autores <- html %>% 
    html_element("div.form-container:nth-child(11) > div:nth-child(1) > table:nth-child(1)") %>% 
    html_table() %>% 
    clean_names() %>% 
    mutate("link" = link)
  vinc_conc <- html %>% 
    html_element("div.form-container:nth-child(8) > div:nth-child(2)") %>% 
    html_text2()
  conc <- NA
  
  if(vinc_conc == "SIM"){
    html %>% 
      html_element("div.form-container:nth-child(13) > div:nth-child(1) > table:nth-child(1)") %>% 
      html_table() %>% 
      clean_names() %>% 
      mutate(link = link) -> conc
    linha <- html %>% 
      html_element("div.form-container:nth-child(22) > div:nth-child(2)") %>% 
      html_text2()
    projeto <- html %>% 
      html_element("div.form-container:nth-child(23) > div:nth-child(2)") %>% 
      html_text2()
    
    linha_proj <- tibble(
      "linha" = linha,
      "projeto" = projeto,
      "link" = link
    )
  } else {
    linha <- html %>% 
      html_element("div.form-container:nth-child(23) > div:nth-child(2)") %>% 
      html_text2()
    projeto <- html %>% 
      html_element("div.form-container:nth-child(24) > div:nth-child(2)") %>% 
      html_text2()
    
    linha_proj <- tibble(
      "linha" = linha,
      "projeto" = projeto,
      "link" = link
    )
    
  }
  list(
    list(
      "autores" = autores,
      "linha_proj" = linha_proj,
      "trabalho_conclusao" = conc
    )
  ) -> lista
  names(lista) <- link
  lista
  
}
#restos-------
get_docentes <- function(remDr, ano){
year <- remDr$findElement(using = "css", 
                         "#form\\:ano")
year$clearElement()
year$sendKeysToElement(list(as.character(ano)))#argumento
consultar <- remDr$findElement(using = "css", "#form\\:consultar")
consultar$clickElement()

html <- remDr$getPageSource()[[1]]
html %>% read_html() %>% 
  html_element(css = ".listagem") %>% 
  html_table() %>% 
  clean_names() %>% 
  select(docente, categoria) %>% 
  mutate(ano = ano) 
}

#função auxiliar
get_text <- function(html, css){
  html %>% html_element(css = css) %>% 
    html_text2()
}

#pegar detalhes dos docentes
get_docentes_details <- function(remDr, ano){
  remDr$navigate("https://sucupira.capes.gov.br/sucupira/portais/menu_portal.jsf")
  remDr <- enter_coord(remDr)
  remDr <- enter_docente(remDr)
  year <- remDr$findElement(using = "css", 
                            "#form\\:ano")
  year$clearElement()
  year$sendKeysToElement(list(as.character(ano)))#argumento
  consultar <- remDr$findElement(using = "css", "#form\\:consultar")
  consultar$clickElement()
  
  html <- remDr$getPageSource()[[1]]
  #links para os detalhes
html %>% 
  html_element(css = ".listagem") %>% 
  html_elements("a") %>% 
  html_attr("href") -> links
url <- remDr$getCurrentUrl()
links <- links[!str_detect(links, "#")]

  #lista de seletores de detalhes
seletores <- list(
  nome = "div.form-container:nth-child(5) > div:nth-child(2)",
  nivel_tit = "div.form-container:nth-child(18) > div:nth-child(2)",
  area_tit = "div.form-container:nth-child(19) > div:nth-child(2)",
  ano_tit = "div.form-container:nth-child(18) > div:nth-child(4)",
  inst_tit = "div.form-container:nth-child(20) > div:nth-child(2)",
  ies_vinc = "div.form-container:nth-child(24) > div:nth-child(2)",
  ies_vinc_tipo = "div.form-container:nth-child(23) > div:nth-child(2)",
  reg_trab = "div.form-container:nth-child(23) > div:nth-child(4)"
  
)

#função para navegar para os links e pegar as informações
get_information_doc <- function(link){
  remDr$navigate(paste0("https://sucupira.capes.gov.br",link))  
  html <- remDr$getPageSource()[[1]] %>% read_html() 
  map_dfc(seletores, get_text, html = html)
  }


#aplicando em loop
map_dfr(links, get_information_doc) %>% 
  mutate(ano = ano)
}

get_docentes_programas <- function(remDr, ano){
  remDr$navigate("https://sucupira.capes.gov.br/sucupira/portais/menu_portal.jsf")
  remDr <- enter_coord(remDr)
  remDr <- enter_docente(remDr)
  year <- remDr$findElement(using = "css", 
                            "#form\\:ano")
  year$clearElement()
  year$sendKeysToElement(list(as.character(ano)))#argumento
  consultar <- remDr$findElement(using = "css", "#form\\:consultar")
  consultar$clickElement()
  
  html <- remDr$getPageSource()[[1]]
  #links para os detalhes
  html %>% 
    html_element(css = ".listagem") %>% 
    html_elements("a") %>% 
    html_attr("href") -> links
  url <- remDr$getCurrentUrl()
  links <- links[!str_detect(links, "#")]
  #função para pegar os outros programas
  get_programas <- function(remDr){
  html <- remDr$getPageSource()[[1]] %>% read_html()
  html %>% rvest::html_elements("#formConsulta .conteudo-painel") %>% 
    html_text2() -> programas
  tibble(
    nome = html %>% html_element("div.form-container:nth-child(5) > div:nth-child(2)") %>% html_text2(),
    outros_programas = programas
  )
  }
  #função para navegar para os links e pegar as informações
  
  get_information_outros_programas <- function(link){
    remDr$navigate(paste0("https://sucupira.capes.gov.br",link))  
    get_programas(remDr)
  }
  map_dfr(links, get_information_outros_programas)
}
  






