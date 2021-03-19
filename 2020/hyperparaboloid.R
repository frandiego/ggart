# hyperparaboloid_grid.R


n_lines = 35
lines_color = 'white'
highlighted_line_color = '#F93822'
background_color = "#1E1A34"
filename = 'ascension_of_gauss.png'
path = '~/Desktop'
height = 20
units = 'cm'
aspect = 4/5

phi <- (1+sqrt(5))/2
m <- matrix(NA,n_lines,n_lines)
ax <- seq(-1,1,2/(n_lines-1))
for (i in seq(n_lines)){
  for (j in seq(n_lines)){
    m[i,j] <- 2-ax[i]*ax[j]
  }
}
rownames(m) <- paste0('r',seq(nrow(m)))
colnames(m)<- paste0('c',seq(ncol(m)))
mm<-  melt(m)
mm$color <- 0
red_j <- n_lines-round(n_lines/phi)
red_i <- round(n_lines/phi)
colnames(mm) <- c('r','c','value','color')
mm[mm$r == paste0('r',red_i) & mm$c == paste0('c',red_j), 'color' ] <- 1
ggplot(mm) +
  geom_point(aes(r,c,size=jitter(value**3,amount =1),color=factor(color)),inherit.aes = F)+
  scale_color_manual(values=c(lines_color,highlighted_line_color))+
  theme_void()+
  theme(legend.position="none",
        plot.background = element_rect(fill = background_color),)+

  coord_equal()


