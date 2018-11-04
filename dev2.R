rm(list = ls())
library(purrr)
library(data.table)
library(ggplot2)
source('R/utils.R')
url = 'http://www.gutenberg.org/cache/epub/2000/pg2000.txt'
path <- 'books/eduardo_mendoza_sin_noticias_de_gurb_esp.txt'
type = 'txt'
first_sentence = "día 9"
last_sentence = "02.00 Sin noticias de Gurb."
chapter_word <- NULL
width <- 100
encoding = 'ES_ES.UTF-8'
regex_bold_set <- c('[0-9][0-9].[0-9][0-9]','DÍA [0-9][0-9]','DÍA [0-9]')
book_text_pre <- function(path,type,first_sentence,last_sentence,chapter_word,width,encoding){
  # set LC_ALL
  Sys.setlocale("LC_ALL", encoding)
  # read
  if (type == 'txt'){
    txt <- readLines(con = path, encoding = 'UTF-8',warn = F,skipNul = T)
  }
  # remove trims
  txt %>% map(trimws) -> txt
  # get min and max sentence and limit the file
  txt %>% map_lgl(~grepl(tolower(first_sentence),tolower(.))) %>% which() -> min_limit
  txt %>% map_lgl(~grepl(tolower(last_sentence),tolower(.))) %>% which() -> max_limit
  txt <- txt[min_limit:max_limit]
  # remove chapter titles
  if(is.null(chapter_word)==F){
    txt %>% map_lgl(~grepl(tolower(last_sentence),tolower(.))) %>% which() -> chapters
    txt <- txt[-chapters]
  }
  # remove white sentences
  txt %>% map_lgl(~.!="") %>% txt[.] -> txt
  # word by word
  txt <- map(txt,~unlist(strsplit(.,' ')))
  # remove empty words
  txt <- map(txt,~.[. != ''])
  # crete lines
  txt <- map(txt,~paste0(.,collapse = ' '))
  # full text
  txt <- paste0(txt,collapse = ' ')
  txt <- gsub(' . ','. ',txt)
  txt <- gsub(' , ',', ',txt)
  strwrap(x = txt,width = width,indent = 0,exdent = 0) -> wrap_


  wrap_ %>% head(-1L) %>% map(~add_spaces(.,width = width)) -> wrap_add_
  tail(wrap_,1) -> last_
  add_ <- width - nchar(last_)
  rep(' ',add_) %>% paste0(collapse = '') %>% paste0(last_,.)-> last_
  wrap_add_[length(wrap_)] <- last_
  wrap_add_ %>% paste0(collapse = '\n')-> ret
  return(ret)
}
gurb <- function(path=env_var('path'),
                 type = env_var('type'),
                 first_sentence = env_var('first_sentence'),
                 last_sentence=env_var('last_sentence'),
                 chapter_word=env_var('chapter_word'),
                 width=env_var('width'),
                 encoding=env_var('encoding')){
  # set LC_ALL
  Sys.setlocale("LC_ALL", encoding)
  # read
  if (type == 'txt'){
    txt <- readLines(con = path, encoding = 'UTF-8',warn = F,skipNul = T)
  }
  # remove trims
  txt %>% map(trimws) -> txt
  # get min and max sentence and limit the file
  txt %>% map_lgl(~grepl(tolower(first_sentence),tolower(.))) %>% which() -> min_limit
  txt %>% map_lgl(~grepl(tolower(last_sentence),tolower(.))) %>% which() -> max_limit
  txt <- txt[min_limit:max_limit]
  # remove chapter titles
  if(is.null(chapter_word)==F){
    txt %>% map_lgl(~grepl(tolower(last_sentence),tolower(.))) %>% which() -> chapters
    txt <- txt[-chapters]
  }
  # remove white sentences
  txt %>% map_lgl(~.!="") %>% txt[.] -> txt
  # word by word
  txt <- map(txt,~unlist(strsplit(.,' ')))
  # remove empty words
  txt <- map(txt,~.[. != ''])
  # crete lines
  txt <- map(txt,~paste0(.,collapse = ' '))
  # full text
  txt <- paste0(txt,collapse = ' ')
  #txt <- gsub(' . ','. ',txt)
  #txt <- gsub(' , ',', ',txt)
  # get dias

  txt %>% strsplit(' ') %>% unlist() -> words_
  words_ %>% map_lgl(~grepl('DÍA',.)) %>% which()-> days
  days_<-c(tail(days,-1L),length(words_))
  map2(.x=days,.y=days_,.f=function(x,y){words_[x:(y-1)]})->txt
  txt %>% map(~paste0(.,collapse = ' '))-> txt
  txt %>% map(~strwrap(.,width = width))-> wrap_
  wrap_ %>% map(~map_int(.,nchar)) %>% unlist() %>% print
  wrap_ %>% map(~add_spaces_list(l = ., width = width)) -> wrap_
  wrap_ %>% map(~map_int(.,nchar)) %>% unlist() %>% print
  return(wrap_)
}

x <- gurb()
f_ <- x[[1]] %>% paste0(collapse = '\n')

x[[1]] %>% nchar()

dt <- data.table(label = f_)
ggplot(dt,aes('',''))+
  geom_text(aes(label=label),size=2,parse = F,family = 'mono')+
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank())


