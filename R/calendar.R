# calendar.R
calendar <- function(year,n_columns = 4){
  data.table(date=seq.Date(as.Date(paste0(year,'-01-01')),
                           as.Date(paste0(year,'-12-31')),1)) %>%
    .[,c('month','day','week','wday') :=
        invoke_map(list(month,day,isoweek,
                        function(x) wday(x=x,week_start = 1)), x = date)] %>%
    .[,color := ifelse(wday<6,1,0)] %>%
    .[month==12 & week == 1, week := 53] -> dt

  dt[month==12 & week == 1, week := 53]
  names(month.name) <- 1:12

  ggplot(dt,aes(x=factor(wday),y=factor(-week),color=factor(color))) +
    geom_text(aes(label = day)) +
    facet_wrap(~month,scales = 'free',
               labeller = labeller(month = month.name),ncol =  n_columns) +
    scale_color_manual(values = c('red','black')) +
    ggtitle(year)+
    theme(axis.title = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), panel.background = element_blank(),
          legend.position = 'none',
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 12),
          strip.background  = element_blank(),
          strip.text = element_text(hjust = 0.5,face = 'bold'))
}

