# Created by Renata Rojas Guerra (renata.r.guerra@ufsm.br), august/2020

library(AdequacyModel)
library(readr)

#####################################
### source the unit distributions ###
#####################################
source("Section_6_Unit_functions.R")
source("Section_3_pdfs.R")

#####################################
### Maximum Likelihood Estimation ###
#####################################
## choosing the data set
# 1- civil engineering; 2- Economics; 3- Computer Sciences; 4- Control engineering
i<- 4 
data<-read_csv(paste0("data",i,".csv"))
attach(data)
x<- DROPOUT_RATE

## fitting the models
tau<-.5
# Beta
mod1 = goodness.fit(pdf=f_beta, cdf= F_beta, 
                    starts = c(1,mean(x)), data = x,
                    method="B", domain=c(0,1), mle=NULL)
# Unit Birnbaum-Saunders
mod2 = goodness.fit(pdf=f_ubs, cdf=F_ubs, 
                    starts = c(1,1), data = x,
                    method="B", domain=c(0,.9),mle=NULL)
# Kumaraswamy
mod3 =  goodness.fit(pdf= f_kw, cdf=F_kw, 
                     starts = c(1,median(x)), data = x,
                     method="B", domain=c(0,1),mle=NULL)
# Unit Unit Gamma
mod4 = goodness.fit(pdf=f_ugamma, cdf=F_ugamma, 
                    starts = c(1,mean(x)), data = x,
                    method="B", domain=c(0,1),mle=NULL)
# Unit Weibull
mod5 =  goodness.fit(pdf=f_wei2, cdf=F_wei2, 
                     starts = c(1,median(x)), data = x,
                     method="B", domain=c(0,1),mle=NULL)
# Unit complementary Weibull
mod6 = goodness.fit(pdf=f_cwei, cdf=F_cwei, 
                    starts = c(1,median(x)), data = x,
                    method="B", domain=c(0,1),mle=NULL)
# Unit ratio-Burr XII
mod7 = goodness.fit(pdf=f_URBXII, cdf=F_URBXII, 
                    starts = c(1,median(x)), data = x,
                    method="B", domain=c(0,.9),mle=NULL)
# Unit ratio-Gompertz
mod8 = goodness.fit(pdf=f_URG, cdf=F_URG,
                        starts = c(1,median(x)), data = x,
                        method="B", domain=c(0,1),mle=NULL)
# Unit ratio-Lomax 
mod9 =  goodness.fit(pdf=f_URL, cdf=F_URL, 
                     starts = c(1,median(x)), data = x,
                     method="B", domain=c(0,1),mle=NULL)
# Unit ratio-Rayleigh 
mod10 = goodness.fit(pdf=f_URR, cdf=F_URR, 
                          starts = c(median(x)), data = x,
                          method="B", domain=c(0,1),mle=NULL)
# Unit ratio-Weibull
mod11 =  goodness.fit(pdf=f_URW, cdf=F_URW,
                      starts = c(1,median(x)), data = x,
                      method="B", domain=c(0,1),mle=NULL)

## table with results
results<-matrix(0,ncol=3,nrow=22)
colnames(results)<-c("parameter 1","parameter 2","W*")
rownames(results)<-c("Beta","", "UBS","", "UKw","", "UG","","UW","", "CUW","","URBXII","","URG","",
                     "URL","","URR","","URW","")
results[1,] <- round(c(mod1$mle,mod1$W),4)
results[2,] <- c(round(mod1$Erro,4),"") 
results[3,] <- round(c(mod3$mle,mod3$W),4) 
results[4,] <- c(round(mod3$Erro,4),"")   
results[5,] <- round(c(mod4$mle,mod4$W),4) 
results[6,] <- c(round(mod4$Erro,4),"") 
results[7,] <- round(c(mod2$mle,mod2$W),4) 
results[8,] <- c(round(mod2$Erro,4),"") 
results[9,] <- round(c(mod5$mle,mod5$W),4) 
results[10,] <- c(round(mod5$Erro,4),"") 
results[11,] <- round(c(mod6$mle,mod6$W),4)
results[12,] <- c(round(mod6$Erro,4),"") 
results[13,] <- c(round(mod7$mle,4),round(mod7$W,4))
results[14,] <- c(round(mod7$Erro,4),"") 
results[15,] <- round(c(mod8$mle,mod8$W),4)
results[16,] <- c(round(mod8$Erro,4),"")
results[17,] <- round(c(mod9$mle,mod9$W),4)
results[18,] <- c(round(mod9$Erro,4),"") 
results[19,] <- c(round(mod10$mle,4),"",round(mod10$W,4))
results[20,] <- c(round(mod10$Erro,4),"","") 
results[21,] <- round(c(mod11$mle,mod11$W),4)
results[22,] <- c(round(mod11$Erro,4),"") 

Ranking<-rank(as.numeric(results[,3]))
Ranking[Ranking>11]<-""
results<-cbind(results,Ranking)

#########################################
### Histogram and estimated densities ###
#########################################
fromx=0
tox= 1
yliminf=0
y_lim = c(0,3.7)
x_lim = c(0,.8)
breaks<-10
if(i==2){y_lim = c(0,5.5)}
if(i==4){breaks<-5}

nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
par(mar=c(3.1, 3.1, 1.1, 2.1))
y <- seq(from = 0,to = 1,length.out = 10000)
hist(x, xlab="", ylab="", main="", freq = F, breaks=breaks, col="gray75", border="gray75",
     ylim=y_lim,xlim=x_lim, cex.axis = 1.7
     )
if(i==1){
  lines(y,f_URL(c(mod9$mle),y),lty=3, col=4,
        lwd=10,type="l")
  lines(y,f_URW(c(mod11$mle),y),lty=2, col=2,
        lwd=5,type="l")
  lines(y,f_URG(c(mod8$mle),y),lty=1, col= 1,
        lwd=3,type="l")
  leg = c("URL","URW","URG")
}
if(i==2){
  lines(y,f_beta(c(mod1$mle),y),lty=3, col=4,
        lwd=10,type="l")
  lines(y,f_kw(c(mod3$mle),y),lty=2, col=2,
        lwd=5,type="l")
  lines(y,f_URL(c(mod9$mle),y),lty=1, col=1,
        lwd=3,type="l")
  leg = c("Beta", "Kw","URL")
}
if(i==3){
  lines(y,f_URG(c(mod8$mle),y),lty=3, col=4,
        lwd=10,type="l")
  lines(y,f_URL(c(mod9$mle),y),lty=2, col=2,
        lwd=5,type="l")
  lines(y,f_URW(c(mod11$mle),y),lty=1, col=1,
        lwd=3,type="l")
  leg = c("URG", "URL","URW")
}
if(i==4){
  lines(y,f_ubs(c(mod2$mle),y),lty=3, col=4,
        lwd=10,type="l")
  lines(y,f_URL(c(mod9$mle),y),lty=2, col=2,
        lwd=5,type="l")
  lines(y,f_URW(c(mod11$mle),y),lty=1, col=1,
        lwd=3,type="l")
  leg = c("UBS", "URL","URW")
}
legend("topright",leg, cex= 2,
       lty=c(3,2,1), col=c(4,2,1), lwd=c(10,5,3),#pch=tipo_linha,
       pt.bg="white", bty="n")
boxplot(x, horizontal=TRUE, outline=TRUE,ylim=x_lim, frame=F, col = "grey75",
        cex.axis=1.8)

xtable(results)
