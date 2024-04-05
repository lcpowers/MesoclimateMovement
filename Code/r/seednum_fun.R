
seednum_fun <- function(seednum){

  sg.mx[1,3] <- as.numeric(seednum*surv.rates["flrg"]*P.flr*P.germ)
  lam <- Re(eigen(sg.mx)$values[1])
  x = (1.01-lam)^2
  return(x)

  }

