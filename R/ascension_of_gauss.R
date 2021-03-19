ascension_of_gauss <- function(n=14){
  nhalf <- ceiling(n/2)
  phi <- (1+sqrt(5))/2
  x=seq(-100,100,length=10000)
  dt <- as.data.table(map(seq(3,nhalf*phi,1/phi),~dnorm(x,mean=0,sd=.)))
  dt <- dt[,c(names(dt)[1:n]),with=F]
  colnames(dt) <- as.character(1:ncol(dt))
  dt[['x']] <- x
  dt <- melt(dt,id.vars = c('x'))
  dt[['c']] <- 0
  dt[variable == dt[,round(uniqueN(variable)*(1-1/phi))],c:=1]
  ggplot(data=dt, aes(x=x,y=value,group=variable,color =factor(c),size = c)) +
    geom_line()+
    ylim(c(0,phi/10)) +
    scale_x_continuous(limits = c(-40,40)) +
    scale_color_manual(values = c('black','red'))+
    scale_size(range = c(1,2))+
    theme_void()+
    theme(legend.position="none")
}



