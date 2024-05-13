library(tidyverse)
library(rvest)
library(xml2)

#url base da sucupira
url <- "https://sucupira.capes.gov.br/sucupira/public/index.xhtml"

#iniciando sessão
s <- session(url)

#link para o login
s2 <- s |> session_follow_link(css = "#cabecalho > div > div.col-sm-4.col-md-4.col-lg-3 > a")
s2 |> html_nodes("#modalLoginCapes")
s2 |> read_html() |> write_xml(file = "temp.html")

formulario <- s2 |> html_form()
f <- html_form_set(formulario[[1]], 
                   tipoLogin = "capes",
              login = Sys.getenv("LOGIN_SUCUPIRA"),
              senha = Sys.getenv("SENHA_SUCUPIRA"))
# f$target <- "_self"
f$action <- "https://sso.capes.gov.br/sso/oauth?response_type=code&client_id=sucupira_oauth&redirect_uri=https%3A%2F%2Fsucupira.capes.gov.br%2Fsucupira%2Foauth%2Fcode&state=https://sucupira.capes.gov.br/sucupira/portais/menu_portal.jsf"

s3 <- session_submit(s2, f)
s3 |> html_nodes("script") -> scrip
scrip
s3 |> read_html() |> write_html("temp.html")

s3 <- session_jump_to(s3, "https://sucupira.capes.gov.br/sucupira/portais/menu_portal.jsf")

n11 <- scrip[[11]]
