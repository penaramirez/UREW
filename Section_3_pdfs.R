# Created by Renata Rojas Guerra (renata.r.guerra@ufsm.br), august/2020

################### Equations and plots Section 3 ###################
b=1.3
q=q=.7
tau<-.5
x<-.2

###############################
## 3.1. Unit ratio-Gompertz  ##
###############################
f_URG<-function(par,x){
  b= par[1]
  q= par[2]
  b*log(1/(1-tau))*exp(b*x/(1-x))/(
    (1-x)^2*(exp(q*b/(1-q))-1))*
    (1-tau)^((exp(b*x/(1-x))-1)/(exp(q*b/(1-q))-1))
}


F_URG<-function(par,x){
  b= par[1]
  q= par[2]
  1-(1-tau)^((exp(b*x/(1-x))-1)/(exp(q*b/(1-q))-1))
}

Q_URG<-function(par,x){
  b= par[1]
  q= par[2]
  log(log(1-u)/log(1-tau)*(exp(b*q/(1-q))-1)+1)/
    (b+log(log(1-u)/log(1-tau)*(exp(b*q/(1-q))-1)+1))
}

# cheking
u<-F_URG(c(b,q),.29)
Q_URG(c(b,q),u)
f_URG(c(b,q),.29)
integrate(f_URG,0,.29, par=c(b,q))
u

###############################
## 3.2. Unit ratio-Burr XII  ##
###############################
f_URBXII<-function(par,x){
  b = par[1]
  q = par[2]
  b*x^(b-1)/((1-x)^(b+1)*log(1+q^b/(1-q)^b))*log(1/(1-tau))*(1+x^b/(1-x)^b)^(log(1-tau)/log(1+q^b/(1-q)^b)-1)
}

F_URBXII<-function(par,x){
  b = par[1]
  q = par[2]
  1-(1+x^b/(1-x)^b)^(log(1-tau)/log(1+q^b/(1-q)^b))
}

Q_URBXII<-function(par,x){
  b = par[1]
  q = par[2]
  ((1-u)^(log(1+q^b/(1-q)^b)/log(1-tau))-1)^(1/b)/(1+((1-u)^(log(1+q^b/(1-q)^b)/log(1-tau))-1)^(1/b))
}

# cheking
u<-F_URBXII(c(b,q),x)
Q_URBXII(c(b,q),u)
f_URBXII(c(b,q),x)
integrate(f_URBXII,0,x, par=c(b,q))
u

###########################
## 3.3. Unit ratio-Lomax ##
###########################
f_URL<-function(par,x){
  b= par[1]
  q= par[2]
  log((1-tau)^(-1))/((1-x)*log(1+q/(b*(1-q)))*(b*(1-x)+x))*(1+x/(b*(1-x)))^(log(1-tau)/log(1+q/(b*(1-q))))
}

F_URL<-function(par,x){
  b= par[1]
  q= par[2]
  1-(1+x/(b*(1-x)))^(log(1-tau)/log(1+q/(b*(1-q))))
}

Q_URL<-function(par,x){
  b= par[1]
  q= par[2]
  b*((1-u)^(log(1+q/(b*(1-q)))/log(1-tau))-1)/
    (1+b*((1-u)^(log(1+q/(b*(1-q)))/log(1-tau))-1))
}

# cheking
u<-F_URL(c(b,q),x)
Q_URL(c(b,q),u)
f_URL(c(b,q),x)
integrate(f_URL,0,x, par=c(b,q))
u

###############################
## 3.4.a. Unit ratio-Weibull ##
###############################

f_URW<-function(par,x){
  b=par[1]
  q=par[2]
  b*x^(b-1)*(1-q)^b/(q^b*(1-x)^(b+1))*log((1-tau)^(-1))*(1-tau)^(x^b*(1-q)^b/((q^b)*(1-x)^b))
}

F_URW<-function(par,x){
  b=par[1]
  q=par[2]
  1-(1-tau)^(x^b*(1-q)^b/(q^b*(1-x)^b))
}

Q_URW<-function(par,x){
  b=par[1]
  q=par[2]
  (q^b*log(1-u)/((1-q)^b*log(1-tau)))^(1/b)/(1+(q^b*log(1-u)/((1-q)^b*log(1-tau)))^(1/b))
}



# cheking
u<-F_URW(c(b,q),x)
Q_URW(c(b,q),x)
f_URW(c(b,q),x)
integrate(f_URW,0,x, par=c(b,q))
u


################################
## 3.4.b. Unit ratio-Rayleigh ##
################################
f_URR<-function(par,x){
  q=par[1]
  2*x*(1-q)^2/(q^2*(1-x)^3)*log((1-tau)^(-1))*(1-tau)^(x^2*(1-q)^2/(q^2*(1-x)^2))
}

F_URR<-function(par,x){
  q=par[1]
  1-(1-tau)^(x^2*(1-q)^2/(q^2*(1-x)^2))
}

Q_URR<-function(par,x){
  q=par[1]
  sqrt(q^2*log(1-u)/((1-q)^2*log(1-tau)))/(1+sqrt(q^2*log(1-u)/((1-q)^2*log(1-tau))))
}

# cheking
u<-F_URR(c(q),x)
Q_URR(c(q),u)
f_URR(c(q),x)
integrate(f_URR,0,x, par=c(q))
u
