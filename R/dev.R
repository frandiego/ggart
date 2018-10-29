library(purrr)
url = 'http://lepetitprinceexupery.free.fr/telecharger/le-petit-prince--antoine-de-saint-exupery.txt'
first_sentence = 'Chapitre 1'
last_sentence = "Et aucune grande personne ne comprendra jamais que ça a tellement d'importance !"
# read
txt <- readLines(con = url, encoding = 'UTF-8',warn = F,skipNul = T)
# clean
txt %>% map_lgl(~.!='') %>% txt[.] -> txt
txt %>% map_lgl(~. == first_sentence) %>% which()
