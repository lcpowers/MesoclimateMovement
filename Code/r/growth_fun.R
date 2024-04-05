#' Growth function
#' Creates vectors to but used to build 3x3 matrices for sub populations
#' 
#' @param sizes A vector of names elements indicating the average sizes of y, j, and a size classes
#' @param growth.rates Average survival rates for y, j, and a size classes 
#' @param G_c Not sure about description yet
#' @param G_d not sure about description yet
#' @param clim average macroclimate?
#' @param asp.effect Parameter that shifts macroclimate value to be cooler or warmer
#' @param asp.mag Magnitude of the aspect.effect
#' 

growth_fun = function(sizes,growth.rates,G_c,G_d,clim=0,G.asp.effect=0,G.asp.mag=0){
  
  grow_lm = glm(growth.rates~sizes[c("sdlg","juv")])
  G_a = grow_lm$coefficients[1]
  G_b = grow_lm$coefficients[2]
  g.rates = G_a + G_b*sizes[c("sdlg","juv")] + G_c*(clim + G.asp.mag*G.asp.effect) + G_d*(clim + G.asp.mag*G.asp.effect)^2
  
  return(g.rates)
  
}

