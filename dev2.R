rm(list = ls())
library(purrr)
library(data.table)
source('R/utils.R')
url = 'http://www.gutenberg.org/cache/epub/2000/pg2000.txt'
path <- 'books/eduardo_mendoza_sin_noticias_de_gurb_esp.txt'
type = 'txt'
first_sentence = "día 9"
last_sentence = "02.00 Sin noticias de Gurb."
chapter_word <- NULL
width <- 500
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
  strwrap(x = txt,width = width) -> wrap_

  tokens <- strsplit(wrap_, '\\s+')
  tokens_fill_ <- map(head(tokens,-1L),~add_spaces(.,width = width,fill = 'center'))
  tokens_fill <- c(tokens_fill_,paste(tail(tokens, 1L)[[1]], collapse = ' '))

  tokens_fill %>% paste0(collapse = '\n')-> ret
  return(ret)
}

book_text_pre(path = path,
              type = type,
              first_sentence = first_sentence,
              last_sentence = last_sentence,
              chapter_word = chapter_word,
              width = width,
              encoding = encoding)-> book

library(showtext)
font_add_google("Gochi Hand", "gochi")

ggdf <- data.table(text_ = book)

ggplot(ggdf,aes('',''))+
  geom_text(aes(label=text_),size=0.25)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        text = element_text()) -> gg


