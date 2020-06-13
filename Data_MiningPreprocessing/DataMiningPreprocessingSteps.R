
#Linear Regression 

d <- mtcars[,c("mpg", "disp", "wt")]
fit1 <- lm(mpg ~ ., data=d)
summary(fit1)

#standardization z score

d.z <- data.frame(scale(d))
fit3 <- lm(mpg ~ ., data=d.z)
summary(fit3)

#mean centering 
d.mc <- scale(d, scale=FALSE)
fit3 <- lm(mpg ~ ., data=as.data.frame(d.mc))
summary(fit3)

#person mean entering
d.pmc <- data.frame(scale(d[, c("wt", "disp")], scale=FALSE), mpg=d$mpg)
fit4 <- lm(mpg ~ ., data=d.pmc)
summary(fit4)



##Transformation 

#Softmax


softmax <- function(x) {
  res <- exp(x) / sum(exp(x))
  return(res)
}
z <- c(1.0, 2.0, 3.0, 4.0, 1.0, 2.0, 3.0)
# x <- exp(1.0)/sum(exp(1.0),exp(2.0),exp(3.0),exp(4.0),exp(1.0),exp(2.0),exp(3.0) )

round(softmax(z), 5)
## [1] 0.02364 0.06426 0.17468 0.47483 0.02364 0.06426 0.17468



##Filtering

number_of_cycles = 2
max_y = 40
x = 1:500
a = number_of_cycles * 2*pi/length(x)
y = max_y * sin(x*a)
noise1 = max_y * 1/10 * sin(x*a*10)
y <- y + noise1
plot(x, y, type="l", lwd = 2, col = "darkblue")
## Low pass Filter

library(signal)
bf <- butter(2, 1/50, type="low")
b1 <- filtfilt(bf, y)
lines(x, b1, col="red", pch=20, lwd=1)


##Smooth

plot(dist ~ speed, cars)
## see also function loess()!
loess()
cars.loe <- loess(dist ~ speed, cars)
cars.lo <- lowess( cars$speed, cars$dist)
lines(cars.lo, col="dodgerblue4")
lines(cars.loe, col="dodgerblue4")






######Visualization

#boxplot
require(ggplot2) ## load 'mpg' data set
boxplot(hwy ~ class, data=mpg)

#boxplot with lattice
require(lattice)
p <- bwplot(hwy ~ class, data=mpg)
p


#boxplot with ggplot2
require(ggplot2)
p <- ggplot(mpg, aes(class, hwy))
p + geom_boxplot()

#Facetting with lattice
install.packages("latticeExtra")
require(latticeExtra)
p <- xyplot(cty ~ displ | drv, data=mpg,
            panel=function(x,y, ...) {
              panel.grid(h=-1, v=-1)
              panel.xyplot(x,y,...)
              panel.lmline(x,y,col="red", ...)
              panel.smoother(x,y, col="darkblue", ...)
            })
p

#Facetting with ggplot2
require(ggplot2)
p <- ggplot(mpg, aes(displ, cty)) + geom_point()
p + facet_grid(drv ~ .) + geom_smooth()
