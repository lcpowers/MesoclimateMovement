#' Survival function
#' Creates vectors to but used to build 3x3 matrices for sub populations
#' 
#' @param sizes A vector of names elements indicating the average sizes of y, j, and a size classes
#' @param survival.rates Average survival rates for y, j, and a size classes 
#' @param S_c Not sure about description yet
#' @param S_d not sure about description yet
#' @param clim average macroclimate?. Default = 1
#' @param asp.effect Parameter that shifts macroclimate value to be cooler or warmer
#' @param asp.mag Magnitude of the aspect.effect
#' 

survival_fun = function(sizes,survival.rates,S_c,S_d,clim=0,S.asp.effect=0,S.asp.mag=0){
  
  # survival.rates = surv_params$survival.rates
  # sizes = surv_params$sizes
  # S_c = surv_params$S_c
  # S_d = surv_params$S_d
  # clim = surv_params$clim
  # S.asp.mag = surv_params$S.asp.mag
  # S.asp.effect = surv_params$S.asp.eff
  # 
  survs_lm = suppressWarnings(glm(survival.rates~sizes,family = binomial( link = "logit" )))
  S_a = survs_lm$coefficients[1]
  S_b = survs_lm$coefficients[2]
  s.rates = logit2prob(S_a + S_b*sizes + S_c*(clim + S.asp.mag*S.asp.effect) + S_d*(clim + S.asp.mag*S.asp.effect)^2)
  
  return(s.rates)
  
}
