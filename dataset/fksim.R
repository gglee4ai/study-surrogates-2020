## simulator:
##
## Feynman-Kac simulation for ocean oxygen concentration taken,
## with small modification, from https://github.com/herbei/FK_Simulator

simulator <- function(domain, sites, U, V, KX, KY, OXY, NPATHS) 
 {
  M = dim(OXY)[1]
  N = dim(OXY)[2]

  delx = (domain[2]-domain[1])/(N-1)
  dely = (domain[4]-domain[3])/(M-1)

  NOBS = dim(sites)[1]
  HH    = 1e7
  lam   = 1e-11
  
  OUT = matrix(0, nrow = NOBS, ncol = NPATHS)
  
  for(i in 1:NOBS){
    #print(i)
    for(kk in 1:NPATHS){
      
      
      xc = sites[i,1]
      yc = sites[i,2]
      
      tau=0;
      while( ((xc - domain[1])*(xc-domain[2])<0) & ( (yc-domain[3])*(yc-domain[4])<0)    ){
        
        #--------
        # find indices
        jx = ceiling( (xc - domain[1])/delx );
        ix = ceiling( (yc - domain[3])/dely );
        if (jx<=0){
          jx=1
        }
        else{
          if (jx>N){
            jx=N
          }
        }
        
        if (ix<=0){
          ix=1
        }
        else{
          if (ix>M) ix=M
        }
        #------------
        
        uc = U[ix,jx]
        vc = V[ix,jx]
        
        # Euler step
        xn = xc + HH * uc + sqrt(HH)*sqrt(2*KX)*rnorm(1);
        yn = yc + HH * vc + sqrt(HH)*sqrt(2*KY)*rnorm(1);    
        
        xc=xn;
        yc=yn;
        tau = tau + HH;
        
        
      }
      JJ = ceiling( (xc - domain[1])/delx );
      II = ceiling( (yc - domain[3])/dely );
      
      if (JJ<=0){
        JJ=1
      }
      else{
        if (JJ>N) JJ=N
      }
      
      if (II<=0){
        II=1
      }
      else{
        if (II>M) II=M
      }
      
      OUT[i,kk] = OXY[II,JJ]*exp(-lam*tau)
    }
  }
  
  return(OUT)
}


## fksim:
##
## invokes simulator on coded inputs with defaults on KY and
## KY (coded), and automating averaging over NPATHS simulations

fksim = function(x, u=c(2/3,1/9), NPATHS=6)
 {
  ## input checking
  if((length(x) != 2) && (length(x) != 4)) 
    stop("x should be of length 2 or 4, as xu")
  if(length(x) == 4) {
    u <- x[3:4]
    x <- x[1:2]
  }

  ## moved inside to prevent masking of domain, U and V
  load("/root/dataset/fkset.RData") 

  #unnormalize
  long = x[1] * (domain[2]-domain[1]) + domain[1]
  lat = x[2] * (domain[4]-domain[3]) + domain[3]
  KX = u[1] * 900 + 100
  KY = u[2] * 900 + 100

  ## return averages
  mean(simulator(domain, matrix(c(long,lat),nrow=1), U, V, KX, KY, OXY, NPATHS))
 }
