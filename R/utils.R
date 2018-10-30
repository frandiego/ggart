
guess_encoding <- function(text_list,n=1){
  stringi::stri_enc_detect(text_list) %>%
    map(as.data.table) %>%
    rbindlist() %>%
    .[,.(Confidence = mean(Confidence)),
      by=.(Encoding,Language)] %>%
    .[order(-Confidence)] %>%
    head(n)
}

