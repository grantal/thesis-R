library(tidyverse)
library(rgl)

firvecs <- read_csv("~/thesis/firingvectors.csv")

firvecs <- firvecs %>%
  mutate(x2 = x^2, y2 = y^2) 

# make linear regressions
lrs <- firvecs %>%
  group_by(diameter) %>%
  do(lr = lm(z ~ x + x2 + y + y2, data = .)) %>%
  mutate(intercept = coef(lr)["(Intercept)"],
         x  = coef(lr)["x"] , y  = coef(lr)["y"],
         x2 = coef(lr)["x2"], y2 = coef(lr)["y2"]) %>%
  modify_at("lr", ~NULL) %>%
  mutate(d2 = diameter^2)

# models of all of the coefficients as a function of the diameter
# and the diameter squared
fi = lm(intercept ~ diameter + d2, data=lrs)
fx = lm(x ~ diameter + d2, data=lrs)
fy = lm(y ~ diameter + d2, data=lrs)
fx2 = lm(x2 ~ diameter + d2, data=lrs)
fy2 = lm(y2 ~ diameter + d2, data=lrs)

# function that given a diameter and a model that takes diameter and diameter squared,
# computes the function at that diameter
complm <- function(d, m){
  m$coef[1] + (m$coef[2]*d) + (m$coef[3]*d^2)
}
# function that generates the function for predicting the firing
# vector at a given diameter
firvecFuncMaker <- function(d) {
  # calculate the coefficients for this diameter
  ci = complm(d, fi)
  cx = complm(d, fx)
  cy = complm(d, fy)
  cx2 = complm(d, fx2)
  cy2 = complm(d, fy2)
  function (x,y){
    ci + (cx * x) + (cy * y) + (cx2 * x^2) + (cy2 * y^2)
  } 
}


# make a graph comparing the predicted and the real firing vector at diameter 30
mydiam <- 31
firvecSlice <- firvecs %>%
  filter(diameter == mydiam)

plot3d(x = firvecSlice$x, y = firvecSlice$y, z = firvecSlice$z, col = "steelblue", 
         xlab = "x", ylab = "y", zlab = "z")

persp3d(firvecFuncMaker(mydiam),
        xlim = c(-(mydiam/2), mydiam/2), ylim = c(-(mydiam/2), mydiam/2),
        alpha = 0.4, col = "lightgray", add=TRUE)


firvecs <- firvecs %>%
  mutate(r = sqrt(x^2 + y^2), r2 = r^2)

# model based on radius
lrs_r <- firvecs %>%
  group_by(diameter) %>%
  do(lr = lm(z ~ r + r2, data = .)) %>%
  mutate(intercept = coef(lr)["(Intercept)"],
         r  = coef(lr)["r"] , r2  = coef(lr)["r2"]) %>%
  modify_at("lr", ~NULL) %>%
  mutate(d2 = diameter^2)

intercept = lm(intercept ~ diameter + d2, data=lrs_r)
r = lm(r ~ diameter + d2, data=lrs_r)
r2 = lm(r2 ~ diameter + d2, data=lrs_r)

# firvecs <- firvecs %>%
#   filter(diameter == 20)
# 
# m1 = lm(z ~ x + x2 + y + y2, data=firvecs)
