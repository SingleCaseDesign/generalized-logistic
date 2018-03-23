

# Parameters
#
# x0 = center of x values in the crossover area: point at which y = 0.5(At + Ab)
# Ab = asymptotic base of the function
# At = asymptotic top of the function
# B  = scaling factor, controls steepness of curve (growth rate)
# v  = set fixed to 1
#
# input: dataset with two or three columns: (1) measurement numbering, (2) scores and (3) phase indictor called "phase"
#
# author: Peter Verboon (may, 2017)




genlog <- function(dat=dat, x0 = 10, B = 0.5, At = 7, Ab = 1, v = 1, nA = NULL, plot = TRUE) {

require(minpack.lm)     # contains function nlsLM for nonlinear fitting
require(ggplot2)
  
result  <- list() 
  
GLF <- "y ~ Ab + (At - Ab)/ (1 + exp(-B*(x-x0)))**(1/v)"    # definition generalized logistic function

dat <- SexTherapy2[,c(1,2)]
dat$y <- SexTherapy2$selfEsteem
dat$x <- SexTherapy2$measurementNumber
dat[,c(1,2)] <- NULL

out <- nlsLM(GLF, data=dat, start=list(x0 = 11, B = 1, At = 6, Ab = 2, v = 1),
             lower = c((min(dat$x)+2), -2, (max(dat$y)-2), (min(dat$y)-1),1), 
             upper = c((max(dat$x)-2), 2, (max(dat$y)+1), (min(dat$y)+2),1))        
              

x0 <- as.numeric(coef(out)[1])
B <-  as.numeric(coef(out)[2])
At <- as.numeric(coef(out)[3])
Ab <- as.numeric(coef(out)[4])
v <-  as.numeric(coef(out)[5])  

Dev <- deviance(out)
SSQtot <- sum((dat$y - mean(dat$y))**2)
RSQ <-  ((SSQtot - Dev)/SSQtot)
ES1 = (At -Ab)/sd(dat$y)
ES2 = (At -Ab)/(max(dat$y) - min(dat$y))
res <- c(RSQ, ES, B, x0, At, Ab)
names(res) <- c("RSQ","ES", "B", "X0","At","Ab")

if (plot == TRUE) {
  
  yfit = Ab + ((At - Ab)/ (1 + exp(-B*(dat$x-x0)))**(1/v)) 
  
  nA <- ifelse (is.null(nA), (nA <- sum(dat$Phase == 0)), nA )    
  nA <- 6.5

 g
  
  result$plot <- g
  
   }

result$res <- res

return(result)

}  # end function genlog


 genlog(dat,x0=x0,Ab=Ab,At=At,B=B, v=1, plot=TRUE)

