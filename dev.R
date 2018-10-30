library(purrr)
library(data.table)
source('R/utils.R')
url = 'http://lepetitprinceexupery.free.fr/telecharger/le-petit-prince--antoine-de-saint-exupery.txt'
first_sentence = 'Chapitre 1'
last_sentence = "Et aucune grande personne ne comprendra jamais que ça a tellement d'importance !"
# read
txt <- readLines(con = url, encoding = 'UTF-8',warn = F,skipNul = T)
enc <- guess_encoding(txt,1)

# clean
txt %>% map_lgl(~.!='') %>% txt[.] -> txt
txt %>% map_lgl(~. == first_sentence) %>% which()

guess_encoding <- function(text_list,n=1){
  stringi::stri_enc_detect(text_list) %>%
    map(as.data.table) %>%
    rbindlist() %>%
    .[,.(Confidence = mean(Confidence)),by=.(Encoding,Language)] %>%
    .[order(-Confidence)] %>%
    head(n)
}
guess_encoding(txt)
