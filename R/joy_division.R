

joy_division <- function(n=50,seed = 1234){

  phi <- (1+sqrt(5))/2

  set.seed(seed)

  j1 <- data.frame(Group = 1:n,
                   n1 = sample(c(500, 1000, 2500, 5000), n, TRUE, c(0.1, 0.2, 0.4, 0.3)),
                   n2 = sample(c(200, 400, 500, 1000), n, TRUE, prob = c(0.3, 0.5, 0.15, 0.05)),
                   m1 = runif(n, -1, 1),
                   m2 = rnorm(n, 5, 0.5),
                   sd1 = sample(c(0.7, 1.5, 2.5), n, TRUE, prob = c(0.15, 0.5, 0.35)),
                   sd2 = sample(c(0.7, 1, 3.5), n, TRUE, prob = c(0.05, 0.6, 0.35)))
  j2 <- j1 %>%
    group_by(Group) %>%
    do(x = c(rnorm(.$n1, .$m1, .$sd1), rnorm(.$n2, .$m2, .$sd2))) %>%
    tidy(x)
  j3 <- j2 %>%
    mutate(GroupNum = rev(as.numeric(Group))) %>%
    group_by(Group, GroupNum) %>%
    do(tidy(density(.$x, n = 10000))) %>%
    group_by() %>%
    mutate(ymin = GroupNum * (max(y) / 10), #This constant controls how much overlap between groups there is
           ymax = y + ymin)

  j4 <- j3 %>%
    group_by(Group, GroupNum) %>%
    do(data.frame(approx(.$x, .$ymax, xout = seq(min(j3$x), max(j3$x), length.out = 200)))) %>%
    mutate(y = ifelse(is.na(y), j3$ymin[j3$Group == Group][1], y),
           ymin = j3$ymin[j3$Group == Group][1],
           ymaxN = y + rnorm(n(), 0.001, 0.001)) %>%
    arrange(x) %>%
    mutate(ymaxN = ifelse(row_number() %in% c(1, n()), ymin + min(ymaxN - ymin), ymaxN))

  j4$ymaxS <- smooth(j4$ymaxN, kind = "S", endrule = "copy", do.ends = FALSE)

  p <- ggplot()
  iterable <- rev(unique(j4$GroupNum))
  red_i <- 22
  for (j in seq_along(iterable)) {
    i = iterable[j]
    fill = ifelse(j == red_i,'red','black')
    col = ifelse(j == red_i,'white','white')
    p <- p + geom_ribbon(data = j4[j4$GroupNum == i,],
                         aes(x = x, ymin = ymin + min(j4$ymaxN - j4$ymin),
                             ymax = ymaxS, group = GroupNum),
                         colour = col,
                         fill = fill) +
      geom_hline(yintercept = j4$ymin[j4$GroupNum == i][1] + min(j4$ymaxN - j4$ymin), colour = "#000000")
  }
  p +
    coord_fixed(13) +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())

}


