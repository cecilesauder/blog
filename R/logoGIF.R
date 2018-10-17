library(tidyverse)

#setRepositories(ind=1:2)
## install.packages("devtools")
#devtools::install_github("GuangchuangYu/ggimage")
library(ggimage)

library(magick)
#devtools::install_github('thomasp85/gganimate')
library(gganimate)

##modelisation des sauts d'R
x0 <- -8:8
y <- rep((-x0^2)/100 +1 ,3 )
y <- y + seq(0,3, length.out = length(y))
y <- c(y, rev(y))

#just to have a look
plot(y)

ly <- length(y)

#first tibble pour la trajectoire de R
d <- tibble(
x = 1:ly,
x1 = 1:ly,
y = y,
image = rep("https://www.r-project.org/logo/Rlogo.png",ly)
)

#local copy of Rlogo
"https://www.r-project.org/logo/Rlogo.png" %>%
  image_read() %>%
  image_scale("200") %>%
  image_write("Rlogo.png")

d <- d %>%
  mutate(image2 = rep("Rlogo.png",ly))


x3 = seq(0,ly, by = 17)
y3 = c(0.36,d$y[x3])
y3 = y3 - 0.4

d2 <- tibble(
x3 = x3+2,
y3 = y3,

hex = c(
"https://github.com/rstudio/hex-stickers/raw/master/PNG/blogdown.png",
"https://github.com/rstudio/hex-stickers/raw/master/PNG/devtools.png",
"https://github.com/rstudio/hex-stickers/raw/master/PNG/tibble.png",
"https://github.com/rstudio/hex-stickers/raw/master/PNG/pipe.png",
"https://github.com/thomasp85/gganimate/raw/master/man/figures/logo.png",
"https://github.com/rstudio/hex-stickers/raw/master/PNG/tidyverse.png",
"https://github.com/rstudio/hex-stickers/raw/master/PNG/ggplot2.png"
)
)


d2$hex %>%
  image_read() %>%
  image_scale("200") -> hex2

im_write <- function(x, y){
  image_write(y , path = x)
}

hex3 <- map2_chr(as.list(hex2), basename(d2$hex), image_write)


d2 <- d2 %>%
  mutate(hex3 = hex3)

options(gganimate.dev_args = list(width = 800, height = 600))

ggplot(d, aes(x, y)) + 
  #coord_fixed(ratio = 10) +
  geom_image(data = d2, aes(x=x3, y=y3, image = hex3), size = .1, by = 'height')+
  geom_image(aes(image=image2), size=.1 , by = 'width')+
  xlim(c(-2,108)) + ylim(c(-0.5,4))+
  theme_nothing() +
  transition_time(x1) +
  ease_aes('bounce-out')

anim_save("test2.gif")


