library(purrr)
library(ggplot2)

n_bar = 4
bar_color = 'white'
highlighted_bar_color = '#F93822'
background_color = "#1E1A34"
filename = 'polar_clock.png'
path = '~/Desktop'
height = 20
units = 'cm'
aspect = 4/4


phi <- (1+sqrt(5))/2
data.frame(x = seq(n_bar+1),
           y = c(0,map_dbl(1:n_bar,~phi*.)),
           fill=  c(c(0,1),rep(2,n_bar-1))) -> dt

ggplot(dt,aes(x,y,fill=factor(fill))) +
  geom_bar(stat='identity')+
  theme_void() +
  ylim(c(0,max(dt$y[-(n+2)])/2*3)) +
  coord_polar(theta='y')+
  scale_fill_manual(values=c(bar_color,highlighted_bar_color,bar_color))+
  theme(legend.position="none",
        plot.background = element_rect(fill = background_color)) -> gg


ggsave(file.path(path, filename),
       plot = gg,
       device = 'png',
       width = aspect * height ,
       height = height,
       units = units)
