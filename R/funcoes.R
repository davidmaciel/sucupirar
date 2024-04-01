library(tidyverse)
library(rvest)


url <- "https://sucupira.capes.gov.br/sucupira/"

s <- session(url)

s <- s |> session_follow_link(css = "#cabecalho > div > div.col-sm-4.col-md-4.col-lg-3 > a")
formulario <- s |> html_form()
html_form_set(formulario[[1]], 
              login = Sys.getenv())

rvest::html_form(s)
