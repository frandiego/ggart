
guess_encoding <- function(text_list,n=1){
  stringi::stri_enc_detect(text_list) %>%
    map(as.data.table) %>%
    rbindlist() %>%
    .[,.(Confidence = mean(Confidence)),
      by=.(Encoding,Language)] %>%
    .[order(-Confidence)] %>%
    head(n)
}





add_spaces <- function(x,width){
  #goal_ <<- goal_ +1
  #print(goal_)
  nchar_ <- nchar(x)
  add_   <- width - nchar_
  words_ <- strsplit(x,' ') %>% unlist()
  idx_   <- seq_along(words_)
  nwords_ <- length(words_)
  if(add_<nwords_){
    idx_add<- sample(x = idx_,size = add_,replace = F)
    rep_ <- paste0(words_[idx_add],' ')
    words_[idx_add] <- rep_
    x_ <- paste0(words_,collapse = ' ')
    return(x_)
  }else{
    return(add_spaces(x = gsub(' ','  ',x),width = width))
  }
}

add_spaces_list <- function(l,width){
  l_ <- head(l,-1L)
  u_ <- tail(l,1)
  l_c <- map(l_,~add_spaces(x=.,width = width))
  add_ <- width - nchar(u_)
  rep(' ',add_) %>% paste0(collapse = '') %>% paste0(u_,.)-> u_c
  l_c[length(l_c)+1] <- u_c
  return(l_c)
}


env_var <- function(var,envir = .GlobalEnv){
  ls_ <- ls(envir = envir)
  eval(parse(text=ls_[ls_==var]))
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
