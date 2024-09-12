selenium_start()
remDr <- selenium_driver()

sucupira_login(remDr,
               usuario = Sys.getenv("LOGIN_SUCUPIRA"), 
               senha = Sys.getenv("SENHA_SUCUPIRA"))

enter_coord(remDr)


enter_pi(remDr)
search_pi(remDr)

pi <- pega_tabela_artigos(remDr)

pi2124 <- pi %>% filter(between(ano, 2021, 2024))
remDr$navigate(pi2124$link[1])

