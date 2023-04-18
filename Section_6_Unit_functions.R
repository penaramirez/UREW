# Created by Renata Rojas Guerra (renata.r.guerra@ufsm.br), august/2020

############
##  Beta  ##
############
# pdf
f_beta<-function(par,x){
  phi= par[1]
  mu= par[2]
  dbeta(x,mu*phi,(1-mu)*phi)
}
# cdf
F_beta<-function(par,x){
  phi= par[1]
  mu= par[2]
  pbeta(x,mu*phi,(1-mu)*phi)
}
###################
##  Kumaraswamy  ##
###################
# pdf
f_kw<-function(par,x){
  phi= par[1]
  mu= par[2]
  l<-phi
  b<-log(.5)/log(1-mu^phi)
  b*l*x^(l-1)*(1-x^l)^(b-1)
}
# pdf
F_kw<-function(par,x){
  phi= par[1]
  mu= par[2]
  l<-phi
  b<-log(.5)/log(1-mu^phi)
  1-(1-x^l)^b
}
###############
##  Unit BS  ##
###############
# pdf
f_ubs<-function(par,x){
  b = par[1]
  l = par[2]
  1/(2*x*b*l*sqrt(2*pi))*((-l/log(x))^(.5)+(-l/log(x))^(1.5))*exp(1/(2*b^2)*(log(x)/l+l/log(x)+2))
}
# cdf
F_ubs<-function(par,x){
  b = par[1]
  l = par[2]
  1 - pnorm(1/b*((-log(x)/l)^(.5)-(-l/log(x))^(.5)))
}
##################
##  Unit gamma  ##
##################
# pdf
f_ugamma<-function(par,x){
  phi= par[1]
  mu= par[2]
  (mu^(1/phi)/(1-mu^(1/phi)))^phi/gamma(phi)*x^((mu^(1/phi)/(1-mu^(1/phi)))-1)*(-log(x))^(phi-1)
}
# cdf
F_ugamma<-function(par,x){
  phi= par[1]
  mu= par[2]
  1-pgamma(-log(x),shape=phi,rate=(mu^(1/phi)/(1-mu^(1/phi))))
}
####################
##  Unit Weibull  ##
####################
# pdf
f_wei2<-function(par,x){
  b= par[1]
  mu= par[2]
  l<-mu
  (b/x)*(log(0.5)/log(l))*((log(x)/log(l))^(b-1))*((0.5)^((log(x)/log(l))^b))
}
# cdf
F_wei2<-function(par,x){
  b= par[1]
  mu= par[2]
  l<-mu
  (.5)^((log(x)/log(l))^b)
}
##################################
##  Complementary Unit Weibull  ##
##################################
# pdf
f_cwei<-function(par,x){
  g = par[1]
  mu= par[2]
  g*log(2)/(1-x)*(-log(1-mu))^(-1)*(log(1-x)/log(1-mu))^(g-1)*
    2^(-(log(1-x)/log(1-mu))^g)
}
# cdf
F_cwei<-function(par,x){
  g = par[1]
  mu = par[2]
  1-2^(-(log(1-x)/log(1-mu))^g)
}