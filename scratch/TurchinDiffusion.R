## Trying to understand Turchin content ##
rm(list=ls())
x = seq(0,1.5,by=0.001)
b = 1
d1 = exp(-b*x)
d2 = exp(-x^2)

plot(x,d2,type="l")
total = auc(x=x,y=d2,from = 0,to = 1,type = "spline",absolutearea = T)
close = auc(x=x,y=d2,from = 0,to = 0.1,type = "spline",absolutearea = T)
far = auc(x=x,y=d2,from = 0.9,to = 1,type = "spline",absolutearea = T)

segment/total


plot(x,d1,col="red",type="l")
lines(x,d2,col="blue",type="l")


diff_fun = 
d = diff_fun(x)
integrate(f = diff_fun,lower = 50,upper = 60)


#### metaRange ####

gaus_dispersal = function(x){exp(-b*x)}
  
  
library(metaRange)
x = calculate_dispersal_kernel(max_dispersal_dist = 1,kfun = gaus_dispersal,normalize = T)

plot(x)

