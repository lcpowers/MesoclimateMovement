#' Build survival/growth matrix that doens't yet include fecundity
#' 
#' 
#' @param surv.rates description
#' @param growth.rates description
#' 

sg_mx_fun = function(surv.rates,growth.rates){
  
  # Initiate empty matrix
  mx = matrix(data=0,nrow=3,ncol=3,dimnames = list(c("sdlg","juv","flrg"),c("sdlg","juv","flrg")))
  
  ##### Young #####
  # Seedlings survive and don't transition to juveniles
  mx["sdlg","sdlg"] = as.numeric(surv.rates["sdlg"]*(1-growth.rates["sdlg"]))
    
  ## survive and grow 
  mx["juv","sdlg"] = as.numeric(surv.rates["sdlg"]*growth.rates["sdlg"])
  #####
  
  ##### Juvenile #####
  # survive but don't grow
  mx["juv","juv"] = as.numeric(surv.rates["juv"]*(1-growth.rates["juv"]))
  
  # survive and grow
  mx["flrg","juv"] = as.numeric(surv.rates["juv"]*(growth.rates["juv"]))
  #####
  
  ##### Adults ######
  # survive 
  mx["flrg","flrg"] = as.numeric(surv.rates["flrg"])
  return(mx)
  
}