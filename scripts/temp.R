rm(list=ls())

library(ggplot2)
library(tidyverse)

generation <- seq(1,100)
sim <- rep(c('x1', 'x2', 'x3', 'x4', 'x5'), 20)
freq <- runif(100, 0.35, 0.65)

df <- data.frame(generation, sim, freq)
df2 <- df %>%
  mutate(sim = as.character(sim))

df3 <- rownames(df2$generation)
  

ggplot(df2, aes(generation, freq)) + 
  geom_line(aes(color = sim)) + 
  scale_y_continuous(limits = c(0,1))
