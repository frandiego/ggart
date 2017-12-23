# polar_clock.R
polar_clock <- function(n=4){
  phi <- (1+sqrt(5))/2
  v <- c(0,map_dbl(1:n,~phi*.))
  data <- data.frame(
    x = seq(n+1),
    y = v[-(n+2)],
    fill=  c(rep(0,2),rep(1,n-1))
  )

  ggplot(data,aes(x,y,fill=factor(fill))) +
    geom_bar(stat='identity')+
    theme_void() +
    ylim(c(0,max(v[-(n+2)])/2*3)) +
    theme(legend.position="none")+
    coord_polar(theta='y')+
    scale_fill_manual(values=c('red','black'))
}


