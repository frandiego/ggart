Sys.setlocale("LC_ALL", "ES_ES.UTF-8")
library(purrr)
library(data.table)
library(ggplot2)
url <- "http://www.gutenberg.org/cache/epub/2000/pg2000.txt"
first_line <- 'en un lugar de la mancha'
last_line <- 'sin duda alguna. vale'
n <- 2000

text <- readLines(url,encoding = 'UTF-8')

first_ <- map_lgl(text,~grepl(first_line,tolower(.)))
last_ <- map_lgl(text,~grepl(last_line,tolower(.)))
text <- text[which(first_):which(last_)]
text <- text[map_lgl(text,~.!='')]
text_full <- paste0(text,collapse = ' ')
nchar_ <- nchar(text_full)
nlines <- ceiling(nchar_/n)
text_full_sep <- unlist(strsplit(text_full,''))
text_full_list <- split(text_full_sep, ceiling(seq_along(text_full_sep)/n))

text_full_list <- map(text_full_list,~paste0(.,collapse = ''))


text_full_list <- map(text_full_list,~paste(.,'\n'))
text_full_paste <- reduce(text_full_list,paste)
text_df <- data.table(text = text_full_paste)
text_full_list %>% length()


install.packages('gdtools',dependencies = T)



ggplot(text_df,aes('',''))+
  geom_text(aes(label=text),size=0.25)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank()) -> gg

install.packages('Cairo')
ggsave(plot=gg,
       filename = 'test.svg',
       units = 'cm',
       height = 100,
       width = 60,
       dpi = 1000)
