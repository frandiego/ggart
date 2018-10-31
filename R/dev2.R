rm(list = ls())
library(purrr)
library(data.table)
source('R/utils.R')
url = 'http://www.gutenberg.org/cache/epub/2000/pg2000.txt'
first_sentence = "En un lugar de la Mancha, de cuyo nombre no quiero acordarme, no ha mucho"
last_sentence = "Quijote, van ya tropezando, y han de caer del todo, sin duda alguna. Vale."
chapter_word <- 'Capítulo'
nlines <- 1000

add_random_spaces <- function(str,add){
  idx <- seq_along(str)
  idx_add <- sample(idx,add,replace = F)
  str[idx_add] <- str[idx_add] %>% paste0(.,' ')
  return(str)
}
# set LC_ALL
Sys.setlocale("LC_ALL", "ES_ES.UTF-8")
# read
txt <- readLines(con = url, encoding = 'UTF-8',warn = F,skipNul = T)
txt %>% map_lgl(~grepl(first_sentence,.)) %>% which() -> min_limit
txt %>% map_lgl(~grepl(last_sentence,.)) %>% which() -> max_limit
txt <- txt[min_limit:max_limit]
# remove chapter titles
txt %>% map_lgl(~grepl(chapter_word,.)) %>% which() -> chapters
txt <- txt[-chapters]

txt %>% map_lgl(~.!="") %>% txt[.] -> txt
txt <- map(txt,~unlist(strsplit(.,' ')))
txt <- map(txt,~.[. != ''])
txt <- map(txt,~paste0(.,collapse = ' '))

txt <- txt %>% unlist() %>% paste(.,collapse = ' ')
nchar_ <- ceiling(nchar(txt)/nlines)



l <- list()
txt %>%  strsplit(' ') %>% unlist() -> words
words %>% map_int(nchar) -> word_len
data.table(word = words, nchar = word_len) -> dtext
dtext[,nchar := nchar+1]
dtext[,idx := .I]
for (i in 1:nlines){
  dtext[,cs := cumsum(nchar)]
  idx_ <- dtext[,pos := abs(cs-nchar_)] %>% .[which.min(pos),as.integer(idx)]
  l[[i]]<-idx_
  dtext <- dtext[idx>idx_]
}


l_ <- unlist(l)
lp_ <- c(1,head(l_,length(l_)-1))
lines <- map2(.x = lp_,.y = l_,.f = function(x,y){words[x:y]})

lines_full <- map(lines,~trimws(paste0(unlist(.),collapse = ' ')))
lines_full <- map(lines_full,~gsub('  ',' ',.))
map_int(lines_full,nchar) %>% max -> max_nchar
map_int(lines_full,~max_nchar - nchar(.))-> to_add
lines_ <- head(lines,length(lines)-1)
to_add_ <-head(to_add,length(to_add)-1)
lines_check <- map2(.x = lines_, .y = to_add_ , .f = function(x,y) add_random_spaces(str = x,add = y))
lines_check_full <- map(lines_check,~paste(.,collapse = ' '))
lines_last <- tail(lines,1)
to_add_last <- tail(to_add,1)
lines_last_full <- paste0(lines_last[[1]],collapse =' ')
last_white <- rep(' ',to_add_last) %>% paste0(.,collapse = '')
lines_last_full <- paste0(lines_last_full,last_white)
lines_check_full[[length(lines_check_full)+1]] <- lines_last_full
full_text <- paste0(lines_check_full,collapse = '\n')


# change font
ggdata <- data.table(text= full_text)

ggplot(ggdata,aes('',''))+
  geom_text(aes(label=text),size=0.25)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        text = element_text()) -> gg

