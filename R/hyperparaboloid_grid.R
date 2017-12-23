# hyperparaboloid_grid.R
hyperparaboloid_grid <- function(n=80){
  m <- matrix(NA,n,n)
  ax <- seq(-1,1,2/(n-1))
  for (i in seq(n)){
    for (j in seq(n)){
      m[i,j] <- 2-ax[i]*ax[j]
    }
  }
  rownames(m) <- paste0('r',seq(nrow(m)))
  colnames(m)<- paste0('c',seq(ncol(m)))
  mm<-  melt(m)
  mm$color <- 0
  red_j <- n-round(n/phi)
  red_i <- round(n/phi)
  colnames(mm) <- c('r','c','value','color')
  mm[mm$r == paste0('r',red_i) & mm$c == paste0('c',red_j), 'color' ] <- 1
  ggplot(mm,aes(r,c)) +
    geom_point(aes(size=jitter(value**3,amount =1),color=factor(color)))+
    scale_color_manual(values=c('black','red'))+
    theme_void()+
    theme(legend.position="none")+
    coord_equal()
}

