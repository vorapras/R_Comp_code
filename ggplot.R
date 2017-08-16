install.packages("ggplot2")
library(ggplot2)
?ggplot2

?summary

install.packages("mosaic")
library(mosaic)
data("diamonds")

?ggplot2
ggplot(data=diamonds)+
  geom_point(aes(x=x, y=y, color=color))+
  ggtitle("Zer Winner")

xyplot(x=x, y=y, data=diamonds)
hist(diamond$)

?arima
?arch

Depth <- equal.count(quakes$depth, number=8, overlap=.1)
xyplot(lat ~ long | Depth, data = quakes)
update(trellis.last.object(),
       strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
       par.strip.text = list(cex = 0.75),
       aspect = "iso")


