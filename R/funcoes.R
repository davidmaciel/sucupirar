


#funcões de conexao-------

driver_start <- function(port = 4444L){
  
  rs_driver_object <- rsDriver(browser = "chrome",
                               chromever = "129.0.6668.58",
                               verbose = F,
                               port=4444L
  )
  return(rs_driver_object)

}

client_start <- function(rs_driver_object){
  remDr<- rs_driver_object$client
  remDr$open()
  return(remDr)
}


selenium_stop <- function(remDr){
  remDr$closeWindow()
  
  system("powershell taskkill /im java.exe /f")
 
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

sucupira_logout <- function(RemDr){
  btn <- remDr$findElement("css", "#form_base_interna\\:botaoDeslogarSSO")
  btn$clickElement()
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
  docente <- remDr$findElement("css", "#formAbaColeta\\:btnDocentes")
  docente$setTimeout("implicit", 1e4)
  docente$clickElement()
  return(remDr)
}

enter_discente <- function(remDr){
  discente <- remDr$findElement("css", "#formAbaColeta\\:btnDiscente")
  discente$setTimeout("implicit", 1e4)
  discente$clickElement()
  return(remDr)
}

enter_egresso <- function(remDr){
  egresso <- remDr$findElement("css", "#formAbaColeta\\:btnEgressos")
  egresso$setTimeout("implicit", 1e4)
  egresso$clickElement()
  return(remDr)
}


enter_pi <- function(remDr){
  pi <- remDr$findElement("css", "#formAbaColeta\\:btnProducaoIntelectual")
  pi$setTimeout("implicit", 1e4)
  pi$clickElement()
  return(remDr)
}

enter_conclusao <- function(remDr){
  botao <- remDr$findElement(using = "css", "#formAbaColeta\\:btnTrabalhoConclusao > img:nth-child(1)")
  botao$clickElement()
  return(remDr)
}

enter_projeto <- function(remDr){
  projeto <- remDr$findElement("css", "#formAbaColeta\\:btnProjetoPesquisa")
  projeto$setTimeout("implicit", 1e4)
  projeto$clickElement()
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
  consultar$setTimeout("implicit", 1e4)
  consultar$clickElement()
  return(remDr)
}


#funções de pegar tabelas--------

pega_tabela_artigos <- function(remDr){
  #achar o máximo de páginas
  
  max_page <- try(
    remDr$findElement("css", "#form\\:j_idt217\\:j_idt224"), silent = T
  )
  if(inherits(max_page, 'try-error')){
    max_page <- remDr$findElement("css", "#form\\:j_idt214\\:j_idt221")
    
  }
  
  pages <- max_page$findChildElements("tag name", "option")
  page_numbers <- 1:length(pages)
  
  pegador_artigos <- function(page_number){
    
    max_page <- try(
      remDr$findElement("css", "#form\\:j_idt217\\:j_idt224"), silent = T
    )
    if(inherits(max_page, 'try-error')){
      max_page <- remDr$findElement("css", "#form\\:j_idt214\\:j_idt221")
      
    }
    
    
    pages <- max_page$findChildElements("tag name", "option")
    pages[[page_number]]$setTimeout("implicit", 1e4)
    pages[[page_number]]$clickElement()
    Sys.sleep(sample(c(5,10), 1))
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
  
  pegador_artigos <- insistently(pegador_artigos, rate = rate_delay(3, max_times = 10))
  map_dfr(page_numbers, pegador_artigos)
  
}

pega_tabela_livros <- function(remDr){
  #achar o máximo de páginas
  
  max_page <- try(
    remDr$findElement("css", "#form\\:j_idt214\\:j_idt221"), silent = T
  )
  if(inherits(max_page, 'try-error')){
    max_page <- remDr$findElement("css", "#form\\:j_idt217\\:j_idt224")
    
  }
  
  pages <- max_page$findChildElements("tag name", "option")
  page_numbers <- 1:length(pages)
  
  pegador_livros <- function(page_number){
    max_page <- try(
      remDr$findElement("css", "#form\\:j_idt214\\:j_idt221"), silent = T
    )
    if(inherits(max_page, 'try-error')){
      max_page <- remDr$findElement("css", "#form\\:j_idt217\\:j_idt224")
      
    }
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
  
  
  max_page <- try(
   remDr$findElement("css", "#formListagem\\:j_idt189\\:j_idt196"), silent = T
  )
  if(inherits(max_page, 'try-error')){
    max_page <- remDr$findElement("css", "#formListagem\\:j_idt195\\:j_idt202")
    
  }
  pages <- max_page$findChildElements("tag name", "option")
  page_numbers <- 1:length(pages)
  
  pegador_conc <- function(page_number){
    max_page <- try(
      remDr$findElement("css", "#formListagem\\:j_idt189\\:j_idt196"), silent = T
    )
    if(inherits(max_page, 'try-error')){
      max_page <- remDr$findElement("css", "#formListagem\\:j_idt195\\:j_idt202")
      
    }
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
  
  
  max_page <- try(
    remDr$findElement("css", "#form\\:j_idt200\\:cmbPagina"), silent = T
  )
  if(inherits(max_page, 'try-error')){
    max_page <- remDr$findElement("css", "#form\\:j_idt194\\:cmbPagina")
    
  }
  
  
  pages <- max_page$findChildElements("tag name", "option")
  page_numbers <- 1:length(pages)
  
  pegador_disc <- function(page_number){
    max_page <- try(
      remDr$findElement("css", "#form\\:j_idt200\\:cmbPagina"), silent = T
    )
    if(inherits(max_page, 'try-error')){
      max_page <- remDr$findElement("css", "#form\\:j_idt194\\:cmbPagina")
      
    }
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
  max_page <- try(
    remDr$findElement("css", "#form\\:j_idt120\\:cmbPagina"), silent = T)
  if(inherits(max_page, 'try-error')){
    max_page <- remDr$findElement("css", "div.paginacao:nth-child(1) > ul:nth-child(2) > li:nth-child(3)")
    
  }
  
  
 
  pages <- max_page$findChildElements("tag name", "option")
  page_numbers <- 1:length(pages)
  
  pegador_egresso <- function(page_number){
    max_page <- try(
      remDr$findElement("css", "#form\\:j_idt120\\:cmbPagina"), silent = T)
    if(inherits(max_page, 'try-error')){
      max_page <- remDr$findElement("css", "div.paginacao:nth-child(1) > ul:nth-child(2) > li:nth-child(3)")
      
    }
    
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

pega_vinc_conc_aux <- function(remDr, link){
  remDr$navigate(link)
  html <- remDr$getPageSource()[[1]] %>% read_html()
  
  html %>% 
    html_element("div.form-container:nth-child(8) > div:nth-child(2)") %>% 
    html_text2()
}


pega_vinc_conc <- function(art21_22){
  rs_driver_object <- driver_start()
  remDr <- client_start(rs_driver_object)
  
  remDr$setTimeout("implicit", 5e4)
  sucupira_login(remDr,
                 usuario = Sys.getenv("LOGIN_SUCUPIRA"), 
                 senha = Sys.getenv("SENHA_SUCUPIRA"))
  Sys.sleep(10)
  enter_coord(remDr)
  enter_pi(remDr)
  rate <- rate_delay(1)
  
  pega_vinc_conc_aux <- insistently(pega_vinc_conc_aux, rate = rate_delay(3, 10))
  x <- art21_22 |> 
    mutate(vinc_conc =
             map_chr(link, pega_vinc_conc_aux, remDr = remDr))
  selenium_stop(remDr)
  x
}




pega_detalhes_artigo_aux <- function(remDr, link){
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
  Sys.sleep(sample(c(1:8), 1))
  lista
  
}

pega_detalhes_conc_aux <- function(remDr, link){
  remDr$navigate(link)
  html <- remDr$getPageSource()[[1]] %>% read_html()
  resumo <- html %>% 
    html_element("div.form-container:nth-child(8) > div:nth-child(2)") %>% 
    html_text2() 
  linha <- html %>% 
    html_element("div.form-container:nth-child(21) > div:nth-child(2)") %>% 
    html_text2()
  projeto <- html %>% 
    html_element("div.form-container:nth-child(22) >div:nth-child(2)") %>% 
    html_text2()
  orientador <- html %>% 
    html_element("#nomeOrientador") %>% 
    html_text2()
  Sys.sleep(sample(c(1:8), 1))
  tibble("resumo" = resumo,
         "orientador" = orientador,
         "linha" = linha,
         "projeto" = projeto,
         "link" = link)
}

pega_detalhes_discente_aux <- function(remDr, link){
  remDr$navigate(link)
  html <- remDr$getPageSource()[[1]] %>% read_html()
  projeto <- try(html %>% 
    html_element("table.listagem:nth-child(34)") %>% 
    html_table() |> 
    clean_names() |> 
    mutate(link = link), silent = T)
  if(inherits(projeto, "try-error")){
    projeto2 <- html %>% 
                     html_element("div.form-container:nth-child(34) > div:nth-child(1)") %>% 
                     html_text2() 
    projeto <- tibble("projeto_de_pesquisa" = projeto2,
                      "link" = link)
  }
  
  Sys.sleep(sample(c(seq(1,2,by = 0.1)), 1))
  projeto
}

pega_detalhes_discente <- function(links){
  rate <- rate_delay(pause = 1, max_times = 10)
  rs_driver_object <- driver_start()
  remDr <- client_start(rs_driver_object)

  remDr$setTimeout("implicit", 5e4)
  sucupira_login(remDr,
                 usuario = Sys.getenv("LOGIN_SUCUPIRA"), 
                 senha = Sys.getenv("SENHA_SUCUPIRA"))
  Sys.sleep(10)
  enter_coord(remDr)
  enter_discente(remDr)
  rate <- rate_delay(1)
  
  pega_detalhes_discente_aux <- insistently(pega_detalhes_discente_aux, rate)
  x <- map_dfr(links, pega_detalhes_discente_aux, remDr = remDr)
  selenium_stop(remDr)
  x
}


pega_detalhes_artigo <- function(links){
  
  rs_driver_object <- driver_start()
  remDr <- client_start(rs_driver_object)
  
  remDr$setTimeout("implicit", 5e4)
  sucupira_login(remDr,
                 usuario = Sys.getenv("LOGIN_SUCUPIRA"), 
                 senha = Sys.getenv("SENHA_SUCUPIRA"))
  Sys.sleep(10)
  enter_coord(remDr)
  enter_pi(remDr)
  rate <- rate_delay(1)
  
  pega_detalhes_artigo_aux <- insistently(pega_detalhes_artigo_aux, rate)
  x <- map_dfr(links, pega_detalhes_artigo_aux, remDr = remDr)
  selenium_stop(remDr)
  x
}

pega_detalhes_discente_ins <- insistently(pega_detalhes_discente, 
                                          rate = rate_delay(1))


#funnções para pegar para o targets------
pega_conc <- function(){
  
  rs_driver_object <- driver_start()
  remDr <- client_start(rs_driver_object)
  remDr$setTimeout("implicit", 5e4)
  sucupira_login(remDr,
                 usuario = Sys.getenv("LOGIN_SUCUPIRA"), 
                 senha = Sys.getenv("SENHA_SUCUPIRA"))
  Sys.sleep(10)
  enter_coord(remDr)
  enter_conclusao(remDr)
  conc<-pega_tabela_conclusao(remDr)
  selenium_stop(remDr)

  conc
}
pega_conc_ins <- insistently(pega_conc)

pega_discente <- function(ano){
  rs_driver_object <- driver_start()
  remDr <- client_start(rs_driver_object)
  remDr$setTimeout("implicit", 5e4)
  sucupira_login(remDr,
                 usuario = Sys.getenv("LOGIN_SUCUPIRA"), 
                 senha = Sys.getenv("SENHA_SUCUPIRA"))
  Sys.sleep(10)
  enter_coord(remDr)
  enter_discente(remDr)
  search_pessoa(remDr, ano = ano )
  disc<-pega_tabela_discente(remDr)
  selenium_stop(remDr)
  print(ano)
  disc %>% 
    mutate(ano = ano)
}
pega_discente_ins <- insistently(pega_discente)

pega_docente <- function(ano){
  rs_driver_object <- driver_start()
  remDr <- client_start(rs_driver_object)
  remDr$setTimeout("implicit", 5e4)
  sucupira_login(remDr,
                 usuario = Sys.getenv("LOGIN_SUCUPIRA"), 
                 senha = Sys.getenv("SENHA_SUCUPIRA"))
  Sys.sleep(10)
  enter_coord(remDr)
  
  enter_docente(remDr)
  search_pessoa(remDr, ano = ano )
  doc <- pega_tabela_docente(remDr)
  sucupira_logout(remDr)
  selenium_stop(remDr)
  doc %>% 
    mutate(ano = ano)
}
pega_docente_ins <- insistently(pega_docente)

pega_egresso <- function(){
  rs_driver_object <- driver_start()
  remDr <- client_start(rs_driver_object)
  remDr$setTimeout("implicit", 5e4)
  sucupira_login(remDr,
                 usuario = Sys.getenv("LOGIN_SUCUPIRA"), 
                 senha = Sys.getenv("SENHA_SUCUPIRA"))
  Sys.sleep(10)
  enter_coord(remDr)
  enter_egresso(remDr)
  egre <- pega_tabela_egresso(remDr)
  selenium_stop(remDr)
  egre
}
pega_egresso_ins <- insistently(pega_egresso)

pega_artigo <- function(){
  rs_driver_object <- driver_start()
  remDr <- client_start(rs_driver_object)
  remDr$setTimeout("implicit", 5e4)
  sucupira_login(remDr,
                 usuario = Sys.getenv("LOGIN_SUCUPIRA"), 
                 senha = Sys.getenv("SENHA_SUCUPIRA"))
  Sys.sleep(5)
  enter_coord(remDr)
  enter_pi(remDr)
  search_pi(remDr,tipo = "BIBLIOGRÁFICA", subtipo = "ARTIGO EM PERIÓDICO")
  art <- pega_tabela_artigos(remDr)
  selenium_stop(remDr)
  art
}
pega_artigo_ins <- insistently(pega_artigo)

pega_anais <- function(){
  rrs_driver_object <- driver_start()
  remDr <- client_start(rs_driver_object)
  remDr$setTimeout("implicit", 5e4)
  sucupira_login(remDr,
                 usuario = Sys.getenv("LOGIN_SUCUPIRA"), 
                 senha = Sys.getenv("SENHA_SUCUPIRA"))
  Sys.sleep(10)
  enter_coord(remDr)
  enter_pi(remDr)
  search_pi(remDr,tipo = "BIBLIOGRÁFICA", subtipo = "TRABALHO EM ANAIS")
  anais <- pega_tabela_anais(remDr)
  selenium_stop(remDr)
  anais
}
pega_anais_ins <- insistently(pega_anais)

pega_livros <- function(){
  rs_driver_object <- driver_start()
  remDr <- client_start(rs_driver_object)
  remDr$setTimeout("implicit", 5e4)
  sucupira_login(remDr,
                 usuario = Sys.getenv("LOGIN_SUCUPIRA"), 
                 senha = Sys.getenv("SENHA_SUCUPIRA"))
  Sys.sleep(10)
  enter_coord(remDr)
  enter_pi(remDr)
  search_pi(remDr,tipo = "BIBLIOGRÁFICA", subtipo = "LIVRO")
  livros <- pega_tabela_livros(remDr)
  selenium_stop(remDr)
  livros
}
pega_livros_ins <- insistently(pega_livros)

#limpezas----

clean_discente <- function(discente_ano){

discente_ano |> 
     
    filter(nivel %in% c("Mestrado", "Doutorado") & 
             between(ano, 2021, 2022)) |> 
    distinct() |> 
    select(discente, nivel, link, ano) |> 
    mutate(id_discente =  str_remove(link, "&ano=\\d{4}$") |> 
             str_extract("idDiscente=(\\d{1,}$)", group = 1))
    
}

clean_dis_detalhes <- function(dis_detalhes){
  dis_detalhes |> 
    mutate(
      id_discente = str_remove(link, "&ano=\\d{4}$") |> 
        str_extract("idDiscente=(\\d{1,}$)", group = 1)) |> 
    select(
      id_discente,
      "projeto" = projeto_de_pesquisa, 
           "linha" = linha_de_pesquisa,
           "natureza" = natureza_do_projeto)
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
  






