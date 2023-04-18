# Created by Renata Rojas Guerra (renata.r.guerra@ufsm.br), august/2020

# source("Section_3_pdfs.R")

###############
## pdf plots ##
###############
w1<-9 # largura da figura
h11<-9 # altura da figura
fromx=0
tox= 1
yliminf=0

##### Figure 1a #####
setEPS()
postscript(file = "dens_UGo.eps",width = w1, height = h11,family = "Times")
ylimsup= 4

q=.15
b =1.5
curve(f_URG(c(b,q),x),from=fromx, to=tox, add = FALSE, lty=1, type = "l", cex.lab=1.7, cex.axis = 1.7,
      ylab = "f(y)",xlab = "y",ylim =c(yliminf,ylimsup), col =1, lwd = 3.0)

q=.3
b =2
curve(f_URG(c(b,q),x),from=fromx, to=tox, add = TRUE, lty=2, type = "l",
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col = 2, lwd = 3.0)

q= .5
b = 2
curve(f_URG(c(b,q),x),from=fromx, to=tox, add = T, lty=3, type = "l",
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col = 4, lwd = 5.0)

q= .6
b = .5
curve(f_URG(c(b,q),x),from=fromx, to=tox, add = TRUE, lty=5, type = "l",
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col = 3, lwd = 3.0)

q= .7
b = .1
curve(f_URG(c(b,q),x),from=fromx, to=tox, add = T, lty=1, type = "l", cex.lab=1.3,
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col =6, lwd = 3.0)

legend("topright", c(expression(paste(plain(q(0.5)), " = 0.15, ", plain(beta), " = 1.5 ")),
                     c(expression(paste(plain(q(0.5)), " = 0.30, ", plain(beta), " = 2.0 ")),
                       c(expression(paste(plain(q(0.5)), " = 0.50, ", plain(beta), " = 2.0 ")),
                         c(expression(paste(plain(q(0.5)), " = 0.60, ", plain(beta), " = 0.5 ")),
                           c(expression(paste(plain(q(0.5)), " = 0.70, ", plain(beta), " = 0.1 "))))))),
       col = c(1,2,4,3,6),
       lty= c(1,2,3,5,1),
       lwd = c(3.5,3.5,3.5,3.5,3), bty="n", cex = 1.6)
dev.off()


##### Figure 1b #####
setEPS()
postscript(file = "dens_UBXII.eps",width = w1, height = h11,family = "Times")
yliminf=0
ylimsup= 3.1

b=.2
q=.2
curve(f_URBXII(c(b,q),x),from=fromx, to=tox, add = FALSE, lty=1, type = "l", cex.lab=1.7, cex.axis = 1.7,
      ylab = "f(y)",xlab = "y",ylim =c(yliminf,ylimsup), col =1, lwd = 3.0)

b=1.8
q=.4
curve(f_URBXII(c(b,q),x),from=fromx, to=tox, add = TRUE, lty=2, type = "l",
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col = 2, lwd = 3.0)

b=2
q= .5
curve(f_URBXII(c(b,q),x),from=fromx, to=tox, add = T, lty=3, type = "l",
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col = 4, lwd = 5.0)

b=3.2
q= .6
curve(f_URBXII(c(b,q),x),from=fromx, to=tox, add = TRUE, lty=5, type = "l",
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col = 3, lwd = 3.0)

b=5
q= .7
curve(f_URBXII(c(b,q),x),from=fromx, to=tox, add = T, lty=1, type = "l", cex.lab=1.3,
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col =6, lwd = 3.0)

legend("top", c(expression(paste(plain(q(0.5)), " = 0.2 ", plain(beta), " = 0.2 ")),
                c(expression(paste(plain(q(0.5)), " = 0.4 ", plain(beta), " = 1.8 ")),
                  c(expression(paste(plain(q(0.5)), " = 0.5 ", plain(beta), " = 2.0 ")),
                    c(expression(paste(plain(q(0.5)), " = 0.6 ", plain(beta), " = 3.2 ")),
                      c(expression(paste(plain(q(0.5)), " = 0.7 ", plain(beta), " = 5.0 "))))))),
       col = c(1,2,4,3,6),
       lty= c(1,2,3,5,1),
       lwd = c(3.5,3.5,3.5,3.5,3), bty="n", cex = 1.6)
dev.off()


##### Figure 1c #####
setEPS()
postscript(file = "dens_UL.eps",width = w1, height = h11,family = "Times")
ylimsup= 2

q=.2
b = .1
curve(f_URL(c(b,q),x),from=fromx, to=tox, add = FALSE, lty=1, type = "l", cex.lab=1.7, cex.axis = 1.7,
      ylab = "f(y)",xlab = "y",ylim =c(yliminf,ylimsup), col =1, lwd = 3.0)

q=.4
b = 9
curve(f_URL(c(b,q),x),from=fromx, to=tox, add = TRUE, lty=2, type = "l",
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col = 2, lwd = 3.0)

q= .6
b = 8
curve(f_URL(c(b,q),x),from=fromx, to=tox, add = T, lty=3, type = "l",
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col = 4, lwd = 5.0)

q= .9
b = .1
curve(f_URL(c(b,q),x),from=fromx, to=tox, add = TRUE, lty=5, type = "l",
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col = 3, lwd = 3.0)

q= .7
b = 5
curve(f_URL(c(b,q),x),from=fromx, to=tox, add = T, lty=1, type = "l", cex.lab=1.3,
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col =6, lwd = 3.0)

legend("top", c(expression(paste(plain(q(0.5)), " = 0.2, ", plain(beta), " = 0.1 ")),
                c(expression(paste(plain(q(0.5)), " = 0.4, ", plain(beta), " = 9.0 ")),
                  c(expression(paste(plain(q(0.5)), " = 0.6, ", plain(beta), " = 8.0 ")),
                    c(expression(paste(plain(q(0.5)), " = 0.9, ", plain(beta), " = 0.1 ")),
                      c(expression(paste(plain(q(0.5)), " = 0.7, ", plain(beta), " = 5.0 "))))))),
       col = c(1,2,4,3,6),
       lty= c(1,2,3,5,1),
       lwd = c(3.5,3.5,3.5,3.5,3), bty="n", cex = 1.6)
dev.off()

##### Figure 1d #####
setEPS()
postscript(file = "dens_UR.eps",width = w1, height = h11,family = "Times")
yliminf=0
ylimsup= 4.3

q=.2
curve(f_URR(q,x),from=fromx, to=tox, add = FALSE, lty=1, type = "l", cex.lab=1.7, cex.axis = 1.7,
      ylab = "f(y)",xlab = "y",ylim =c(yliminf,ylimsup), col =1, lwd = 3.0)

q=.4
curve(f_URR(q,x),from=fromx, to=tox, add = TRUE, lty=2, type = "l",
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col = 2, lwd = 3.0)

q= .5
curve(f_URR(q,x),from=fromx, to=tox, add = T, lty=3, type = "l",
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col = 4, lwd = 5.0)

q= .6
curve(f_URR(q,x),from=fromx, to=tox, add = TRUE, lty=5, type = "l",
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col = 3, lwd = 3.0)

q= .7
curve(f_URR(q,x),from=fromx, to=tox, add = T, lty=1, type = "l", cex.lab=1.3,
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col =6, lwd = 3.0)

legend("top", c(expression(paste(plain(q(0.5)), " = 0.2 ")),
                c(expression(paste(plain(q(0.5)), " = 0.4 ")),
                  c(expression(paste(plain(q(0.5)), " = 0.5 ")),
                    c(expression(paste(plain(q(0.5)), " = 0.6 ")),
                      c(expression(paste(plain(q(0.5)), " = 0.7 "))))))),
       col = c(1,2,4,3,6),
       lty= c(1,2,3,5,1),
       lwd = c(3.5,3.5,3.5,3.5,3), bty="n", cex = 1.6)
dev.off()