library(ggplot2)
library(data.table)
library(purrr)


n_lines = 15
lines_color = 'white'
highlighted_line_color = '#F93822'
background_color = "#1E1A34"
filename = 'ascension_of_gauss.png'
path = '~/Desktop'
height = 20
units = 'cm'
aspect = 4/5

nhalf <- ceiling(n_lines/2)
phi <- (1+sqrt(5))/2
x=seq(-100,100,length=10000)
dt <- as.data.table(map(seq(3,nhalf*phi,1/phi),~dnorm(x,mean=0,sd=.)))
dt <- dt[,c(names(dt)[1:n_lines]),with=F]
colnames(dt) <- as.character(1:ncol(dt))
dt[['x']] <- x
dt <- melt(dt,id.vars = c('x'))
dt[['c']] <- 0
dt[variable == dt[,round(uniqueN(variable)*(1-1/phi))],c:=1]
ggplot(data=dt, aes(x=x,y=value,group=variable,color =factor(c),size = c)) +
  geom_line(aes(size=c))+
  ylim(c(0,phi/10)) +
  scale_x_continuous(limits = c(-40,40)) +
  scale_color_manual(values = c(lines_color,highlighted_line_color))+
  scale_size(range = c(0.3,1))+
  theme_void()+
  theme(legend.position="none",
        plot.background = element_rect(fill = background_color),
        panel.grid = element_blank()) -> gg



ggsave(file.path(path, filename),
       plot = gg,
       device = 'png',
       width = aspect * height ,
       height = height,
       units = units)
