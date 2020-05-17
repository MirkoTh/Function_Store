library(stringr)

create_folder_structure <- function(){
  dir.create("R")
  dir.create("Rmd")
  dir.create("sql")
  dir.create("presentations")
  dir.create("figures")
}

get_href <- function(nm){
  first <- str_c("    ", "- text: ", '"', nm, '.Rmd"\n',
                 "      href: ", nm, ".html")
}

get_yml <- function(name_website, title_website, name_rmd){
  name <- str_c("name: ", '"', name_website, '"')
  navbar <- "navbar:"
  title <- str_c("  ", "title: ", '"', title_website, '"')
  left <- str_c("  ", "left:\n")
  content <- list()
  for (i in seq_along(name_rmd)){
    content[[i]] <- get_href(str_remove(name_rmd[i], ".Rmd"))
  }
  return(c(name, navbar, title, left, content))
}

create_index <- function(name_project){
  txt <- str_c("---\ntitle: ", '"', name_project, '"\n', "---\n\n", 
               "Date Project: ", lubridate::now())
  return(txt)
}

render_project <- function(name_project, name_website, title_website){
  name_rmd <- dir("Rmd")[endsWith(dir("Rmd"), ".Rmd")]
  yml_lines <- get_yml(name_website, title_website, name_rmd)
  index_lines <- create_index(name_project)
  writeLines(unlist(yml_lines), con = "_site.yml")
  writeLines(index_lines, con = "index.Rmd")
  file.copy(str_c("Rmd/", name_rmd), name_rmd)
  rmarkdown::render_site()
  file.remove(name_rmd)
}


name_project <- "Mirkos Project"
name_website <- "Mirkos Website"
title_website <- "Still Mirkos Website"

create_folder_structure()

render_project(name_project, name_website, title_website)

