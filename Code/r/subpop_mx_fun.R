#' Wrapper function to create matrix
#' @param surv_fun_params Input values need for surv_fun
#' @param growth_fun_params Input values need for growth_fun
#' 

subpop_mx_fun = function(surv_params,growth_params,seeds){
  
  surv.rates <- do.call(survival_fun,surv_params)
  growth.rates <- do.call(growth_fun,growth_params)
  
  # list2env(surv_params,envir = environment());list2env(growth_params,envir = environment())
  
  # Initiate empty matrix
  mx = matrix(data=0,nrow=3,ncol=3,dimnames = list(c("sdlg","juv","flrg"),c("sdlg","juv","flrg")))
  
  ##### sdlgs #####
  # survive but don't grow
  mx["sdlg","sdlg"] = surv.rates["sdlg"]*(1-growth.rates["sdlg"])
  
  ## survive and grow 
  mx["juv","sdlg"] = surv.rates["sdlg"]*growth.rates["sdlg"]
  #####
  
  ##### Juvenile #####
  # survive but don't grow
  mx["juv","juv"] = surv.rates["juv"]*(1-growth.rates["juv"])
  
  # survive and grow
  mx["flrg","juv"] = surv.rates["juv"]*growth.rates["juv"]
  #####
  
  ##### Adults ######
  # survive 
  mx["flrg","flrg"] = surv.rates["flrg"]
  
  ##### Fecundity #####
  mx["sdlg","flrg"] <- seeds*P.germ
  
  # out.list = list(seeds=seeds,surv.rates = surv.rates, growth.rates = growth.rates,subpop_mx=mx)
  #return(out.list)
  return(mx)
  ######
  
}

