# polar_clock.R
polar_clock <- function(n=4){
  phi <- (1+sqrt(5))/2
  data.frame(
    x = seq(n+1),
    y = c(0,map_dbl(1:n,~phi*.)),
    fill=  c(c(0,1),rep(2,n-1))
  ) -> dt
  ggplot(dt,aes(x,y,fill=factor(fill))) +
    geom_bar(stat='identity')+
    theme_void() +
    ylim(c(0,max(dt$y[-(n+2)])/2*3)) +
    theme(legend.position="none")+
    coord_polar(theta='y')+
    scale_fill_manual(values=c('white','red','black'))
}


