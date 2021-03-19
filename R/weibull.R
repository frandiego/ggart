library(data.table)
library(magrittr)
library(ggplot2)
library(purrr)
n_points = 100000
shape = 3
n_dist = 5
scale_distance = 5
seed = 0

path = '~/Desktop'
filename = 'weibull.png'
height = 20
units = 'cm'
aspect = 4/5

set.seed(seed)

background_color = "#1E1A34"
highlighted_line_color = '#F93822'
phi = (1 + sqrt(5)) /2

color = 1:n_dist %>% quantile(1/phi) %>% round() %>% as.integer()

map(1:n_dist, ~rweibull(n_points, shape = shape, scale = .)) %>%
  as.data.table() %>%
  melt() %>%
  .[, variable := as.numeric(gsub("[^\\d]+", "", variable, perl=TRUE))] %>%
  .[, color := as.integer(variable == color)] %>%
  ggplot(aes(x=value)) +
  geom_density(aes(group=factor(variable),
                   alpha=factor(variable),
                   color=factor(color)),
               size=1,
               fill='white') +
  theme_void() +
  theme(plot.background = element_rect(fill = background_color),
        legend.position = 'none')+
  scale_alpha_discrete(range = c((1-1/phi)/10, 1-1/phi))+
  scale_color_manual(values=c('white',highlighted_line_color)) -> gg

ggsave(file.path(path, filename),
       plot = gg,
       device = 'png',
       width = aspect * height ,
       height = height,
       units = units)

