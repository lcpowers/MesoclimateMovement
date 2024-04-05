#' Wrapper function to create matrix
#' @param surv_fun_params Input values need for surv_fun
#' @param growth_fun_params Input values need for growth_fun
#' 

big_mx_fun = function(surv_params,growth_params,seeds,n.subpops){
  
  source("Code/r/subpop_mx_fun.R")
  surv.rates <- do.call(survival_fun,surv_params)
  growth.rates <- do.call(growth_fun,growth_params)
  
  # large zero matrix
  pop.mx = matrix(data = 0,ncol = 3*n.subpops,nrow=3*n.subpops)
  
  # coords of [1,1] cell for each subpop within G mx
  start.cells = seq(1,n.subpops*3,by=3)
  
  for(i in 1:n.subpops){
    
    start.cell = start.cells[i]
    params = list(surv_params = surv_params,growth_params=growth_params,seeds=seeds)
    mx.i = do.call(subpop_mx_fun,params)
    pop.mx[(0+start.cell):(2+start.cell),(0+start.cell):(2+start.cell)]=mx.i
    # Re(eigen(mx.i)$values[1])
    }

  return(pop.mx)
  
}