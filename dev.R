library(purrr)
library(data.table)
source('R/utils.R')
url = 'http://www.gutenberg.org/cache/epub/2000/pg2000.txt'
first_sentence = "En un lugar de la Mancha, de cuyo nombre no quiero acordarme, no ha mucho"
last_sentence = "Quijote, van ya tropezando, y han de caer del todo, sin duda alguna. Vale."
chapter_word <- 'Capítulo'
nlines <- 1000
add_random_spaces <- function(x,y){
  smp <- sample(seq_along(x),y)
  x[smp] <- x[smp] %>% map(~paste0(.,' '))
  return(paste0(x,collapse = ''))
}

# set LC_ALL
Sys.setlocale("LC_ALL", "ES_ES.UTF-8")
# read
txt <- readLines(con = url, encoding = 'UTF-8',warn = F,skipNul = T)
txt %>% map_lgl(~grepl(first_sentence,.)) %>% which() -> min_limit
txt %>% map_lgl(~grepl(last_sentence,.)) %>% which() -> max_limit
txt <- txt[min_limit:max_limit]
txt %>% map_lgl(~grepl(chapter_word,.)) %>% which() -> chapters
txt <- txt[-chapters]
txt %>% map_lgl(~.!="") %>% txt[.] -> txt
txt_ <- paste0(txt,collapse = ' ')
nchar_ <- ceiling(nchar(txt_)/nlines)




l <- list()
txt_ %>%  strsplit(' ') %>% unlist() -> txtw_
txtw_ %>% map_int(nchar) -> txtwl_
dtext <- data.table(word = txtw_, nchar = txtwl_)
dtext[,nchar := nchar+1]
dtext[,idx := .I]

for (i in 1:nlines){
  dtext[,cs := cumsum(nchar)]
  idx_ <- dtext[,pos := abs(cs-nchar_)] %>% .[which.min(pos),as.integer(idx)]
  l[[i]]<-idx_
  dtext <- dtext[idx>idx_]
}

l_ <- unlist(l)
txtwp_ <- txtw_ %>% map(~paste0(.,' '))
textl_ <- map2(.x = c(1,head(l_,length(l_)-1)),.y = l_,.f = function(x,y){txtwp_[x:y]})
texll_ <- map_int(textl_,~map_int(.,~nchar(.)) %>% length)
maxchar_ <- max(texll_)
texll_ %>% map_int(~maxchar_ - .)-> to_add
map2(.x = textl_,.y = to_add,.f = function(x,y) add_random_spaces(x,y)) -> textr_
textr_ %>% map(~paste0(.,'\n')) -> textr_
textf <- paste0(textr_)

ggdata <- data.table(text = textf)
ggplot(ggdata,aes(y=1,x=1, label = text))+
  geom_text()










