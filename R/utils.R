
guess_encoding <- function(text_list,n=1){
  stringi::stri_enc_detect(text_list) %>%
    map(as.data.table) %>%
    rbindlist() %>%
    .[,.(Confidence = mean(Confidence)),
      by=.(Encoding,Language)] %>%
    .[order(-Confidence)] %>%
    head(n)
}


x = 'convencional'
add_spaces <- function(x,width,fill) {
  nspace <- length(x)-1L
  extra <- width - sum(nchar(x)) - nspace
  reps <- extra %/% nspace
  extra <- extra %% nspace
  times <- rep.int(if (reps>0) reps+1L else 1L, nspace)
  if(is.nan(extra)){
    extra = 0
  }
  if (extra > 0) {
    if (fill=='right') times[1:extra] <- times[1:extra]+1L
    else if (fill=='left')
      times[(nspace-extra+1L):nspace] <- times[(nspace-extra+1L):nspace]+1L
    else times[inds] <- times[(inds <- sample(nspace, extra))]+1L
  }
  spaces <- c('', unlist(lapply(times, formatC, x=' ', digits=NULL)))
  return(paste(c(rbind(spaces, x)), collapse=''))
}

add_random_spacewidths <- function(str,add){
  idx <- seq_along(str)
  idx_add <- sample(idx,add,replace = F)
  str[idx_add] <- str[idx_add] %>% paste0(.,' ')
  return(str)
}

bold_regex <- function(str,regex){
  stringi::stri_match_all(str,regex = regex) %>% unlist() -> match_
  s_ <- str
  for(i in match_){
    r_ <- gsub(' ','_',paste0('bold(',i,')'))
    s_ <- gsub(pattern = i,replacement = r_,x = s_)
  }
  return(s_)
}
