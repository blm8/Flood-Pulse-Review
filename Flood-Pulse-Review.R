# Description:  Figures for flood pulse review manuscript, submitted to Canadian Journal of Fisheries and Aquatic Sciences

# Author(s):  B.L. Miller

# Read Me file? (y/n):  y

install.packages("knitr")
library(knitr)
install.packages("hysteresis")
library(hysteresis)


###############################################################
##  Figure 2a. Discharge & floodplain water depth over time  ##
###############################################################

rm(list=ls())

par(mfrow=c(3, 1))

par(mar=c(2, 3, 2, 3)) # Bottom, left, top, right
par(oma=c(4, 3, 3, 3))
x <- seq(-4, 4, length=100)
hx <- dnorm(x)
min(hx)
max(hx)
plot(x, hx, type="l", lty=1, lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-4, 4), ylim=c(0, 0.43), col="darkblue")
abline(h=0.25, col="dimgray", lwd=2, lty=2)
points(-0.96, 0.25, pch=19, col="dimgray", cex=2)
points(-0.96, 0.25, pch=1, col="dimgray", cex=4)
points(-0.96, 0.25, pch=1, col="dimgray", cex=6)
points(0.96, 0.25, pch=19, col="darkblue", cex=2)
points(0.96, 0.25, pch=1, col="darkblue", cex=4)
par(new=TRUE)
plot(x, hx, type="l", lty=1, lwd=2, xlab="", bty="n", ylab="", xaxt="n", yaxt="n", xlim=c(-4.5, 3.5), ylim=c(0, 0.43), col="forestgreen")
mtext(expression("Discharge"), side=2, cex=1.6, font=1, line=1)
mtext(expression("Floodplain Water Depth"), side=4, cex=1.6, font=1, line=2.25, col="forestgreen")
mtext(expression("Time"), side=1, cex=1.6, font=1, line=2)
legend("topleft", inset=-0.025, expression((a)), bty="n", cex=2.25)


#######################################################################
##  Figure 2b. Holling type alternative neutrally stable equilibria  ##
#######################################################################

# Tile 1 # Figure

par(mar=c(2, 3, 2, 3)) # Bottom, left, top, right
hysteresis <- mloop(retention=0.5, n.points=100, period=99, extended.classical=T, phase.angle=1, b.x=-0.6, b.y=0.5)
plot(hysteresis$x, hysteresis$y, type="l", lty=1, lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-1.6, 1.6), ylim=c(-1.6, 1.6), col="black")
arrows(-0.529, 0.2, x1=-0.535, y1=0.22, length=0.1, angle=25, lwd=1.5, col="black")
arrows(0.529, -0.2, x1=0.535, y1=-0.22, length=0.1, angle=25, lwd=1.5, col="black")
arrows(-0.22, 0.65, x1=-0.20, y1=0.64, length=0.1, angle=25, lwd=1.5, col="black")
arrows(0.22, -0.65, x1=0.20, y1=-0.64, length=0.1, angle=25, lwd=1.5, col="black")
par(new=T)
hysteresis <- mloop(retention=0.5, n.points=100, period=99, extended.classical=T, phase.angle=1, b.x=-0.6, b.y=0.5)
plot(hysteresis$x, hysteresis$y, type="l", lty=1, lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8), col="dimgray")
abline(v=0, col="dimgray", lwd=2, lty=2)
arrows(-0.529, 0.2, x1=-0.535, y1=0.22, length=0.1, angle=25, lwd=1.5, col="dimgray")
arrows(0.529, -0.2, x1=0.535, y1=-0.22, length=0.1, angle=25, lwd=1.5, col="dimgray")
arrows(-0.22, 0.65, x1=-0.20, y1=0.64, length=0.1, angle=25, lwd=1.5, col="dimgray")
arrows(0.22, -0.65, x1=0.20, y1=-0.64, length=0.1, angle=25, lwd=1.5, col="dimgray")
points(0, -0.5, pch=19, col="dimgray", cex=2)
points(0, -0.5, pch=1, col="dimgray", cex=4)
points(0, 0.5, pch=19, col="dimgray", cex=2)
points(0, 0.5, pch=1, col="dimgray", cex=4)
points(0, 0.5, pch=1, col="dimgray", cex=6)
mtext("Ecosystem Function", side=2, cex=1.6, font=1, line=1)
mtext(expression("Discharge"), side=1, cex=1.6, font=1, line=2)
legend("topleft", inset=-0.025, expression((b)), bty="n", cex=2.25)

# Tile 2 # Legend

par(mar=c(4, 9, 4, 9)) # Bottom, left, top, right
plot(0, 0, type="l", lty=1, bty="n", lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8))
text(0, 0.5, labels="Year 1", cex=2, font=1, col="black")
text(0, 0.25, labels="Year 2", cex=2, font=1, col="dimgray")
points(-0.7, 0, pch=19, col="dimgray", cex=2)
points(-0.7, 0, pch=1, col="dimgray", cex=4)
points(-0.7, 0, pch=1, col="dimgray", cex=6)
text(0, 0, labels="Rising-water", cex=2, font=1, col="dimgray")
points(-0.7, -0.25, pch=19, col="dimgray", cex=2)
points(-0.7, -0.25, pch=1, col="dimgray", cex=4)
text(0, -0.25, labels="Falling-water", cex=2, font=1, col="dimgray")


#########################################
##  Figure //.  Percent O2 saturation  ##
#########################################

# Outside bubble # More comprehensive

rm(list=ls())

par(mar=c(1, 6, 6, 1)) #bottom, left, top, right
setwd("~/Desktop")
FP <- read.csv("FP_O2.csv", header=TRUE)
colnames(FP)
# View(FP)
min(FP$O2_PERCENT) #4
max(FP$O2_PERCENT) #90
min(FP$DEPTH) #0
max(FP$DEPTH) #9
FP$TRANSECT
Open <- subset(FP, TRANSECT=="Pelagic")
plot(Open$O2_PERCENT, Open$DEPTH, pch=16, axes=FALSE, xlim=c(0, 100), ylim=rev(c(0, 10)), xlab="", ylab="", type="b", font=2, las=1, cex=2.5, col="blue3")
mtext("Depth" ~ (m), side=2, line=3, cex=2.75, font=2)
mtext(expression("Percent" ~ O[2] ~ "Saturation"), side=3, line=3, cex=2.5, font=2)
axis(2, ylim=rev(c(0, 10)), col="black", las=1, cex=1.5, cex.axis=2, cex.lab=2.5, font=2)  #las=1 makes horizontal labels
axis(3, xlim=rev(c(0, 100)), col="black", las=1, cex=1.5, cex.axis=2, cex.lab=2.5, font=2)  #las=1 makes horizontal labels
par(new=TRUE)
Edge <- subset(FP, TRANSECT=="Edge")
plot(Edge$O2_PERCENT, Edge$DEPTH, pch=15,  xlab="", ylab="", xlim=c(0,100), ylim=rev(c(0, 10)), axes=FALSE, type="b", cex=3, font=2, col="cyan3")
par(new=TRUE)
Forest <- subset(FP, TRANSECT=="Floodplain")
plot(Forest$O2_PERCENT, Forest$DEPTH, pch=2,  xlab="", ylab="", xlim=c(0,100), ylim=rev(c(0, 10)), axes=FALSE, type="b", cex=2.5, col="darkgreen")
legend(x=50, y=6, legend=c("Main Channel", "Edge", "Floodplain"), text.col=c("blue3", "cyan3", "darkgreen"), text.font=2, bty="n", lty=c(1, 1, 1), 
       cex=c(2.5, 2.5, 2.5), col=c("blue3", "cyan3", "darkgreen"))

# Inside bubble

rm(list=ls())

par(mar=c(0, 7, 7, 0)) #bottom, left, top, right
setwd("~/Desktop")
FP <- read.csv("FP_O2.csv", header=TRUE)
colnames(FP)
# View(FP)
FP$TRANSECT
Open <- subset(FP, TRANSECT=="Pelagic")
min(Open$O2_PERCENT) #43
max(Open$O2_PERCENT) #90
min(Open$DEPTH) #0.5
max(Open$DEPTH) #7.5
plot(Open$O2_PERCENT, Open$DEPTH, pch=16, axes=FALSE, xlim=c(40, 90), ylim=rev(c(0, 8)), xlab="", ylab="", type="b", font=2, las=1, cex=5, col="darkgreen")
mtext("Depth" ~ (m), side=2, line=4.5, cex=6, font=2)
mtext(expression(O[2] ~ "%"), side=3, line=5, cex=6, font=2)
axis(2, ylim=rev(c(0, 10)), col="black", las=1, cex=5, cex.axis=5, cex.lab=5, font=2)  #las=1 makes horizontal labels
axis(3, xlim=rev(c(0, 100)), col="black", las=1, cex=5, cex.axis=5, cex.lab=5, font=2)  #las=1 makes horizontal labels


#######################################
##  Figure //.  Low-water discharge  ##
#######################################

rm(list=ls())

par(mar=c(2, 2, 0, 0)) # Bottom, left, top, right
par(oma=c(5, 4, 4, 4))
x <- seq(-4, 4, length=100)
hx <- dnorm(x)
min(hx)
max(hx)
plot(x, hx, type="l", lty=1, lwd=4, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-4, 4), ylim=c(0, 0.43), col="darkblue")
points(-3.5, 0, pch=19, col="darkblue", cex=4)
mtext(expression("Discharge" ~ (m^{3} ~ s^{-1})), side=2, cex=4, font=2, line=1)
mtext(expression("Time" ~ (Days)), side=1, cex=4, font=2, line=4)
legend(x=-6, y=0.45, expression((a)), bty="n", cex=5)


##########################################
##  Figure //.  Rising-water discharge  ##
##########################################

rm(list=ls())

par(mar=c(2, 2, 0, 0)) # Bottom, left, top, right
par(oma=c(5, 4, 4, 4))
x <- seq(-4, 4, length=100)
hx <- dnorm(x)
min(hx)
max(hx)
plot(x, hx, type="l", lty=1, lwd=4, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-4, 4), ylim=c(0, 0.43), col="darkblue")
points(-0.96, 0.25, pch=19, col="darkblue", cex=4)
points(-0.96, 0.25, pch=1, col="darkblue", cex=6)
points(-0.96, 0.25, pch=1, col="darkblue", cex=8)
mtext(expression("Discharge" ~ (m^{3} ~ s^{-1})), side=2, cex=4, font=2, line=1)
mtext(expression("Time" ~ (Days)), side=1, cex=4, font=2, line=4)
legend(x=-6, y=0.45, expression((a)), bty="n", cex=5)


########################################
##  Figure //.  High-water discharge  ##
########################################

rm(list=ls())

par(mar=c(2, 2, 0, 0)) # Bottom, left, top, right
par(oma=c(5, 4, 4, 4))
x <- seq(-4, 4, length=100)
hx <- dnorm(x)
min(hx)
max(hx)
plot(x, hx, type="l", lty=1, lwd=4, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-4, 4), ylim=c(0, 0.43), col="darkblue")
points(0, 0.398, pch=19, col="darkblue", cex=4)
points(0, 0.398, pch=1, col="darkblue", cex=6)
points(0, 0.398, pch=1, col="darkblue", cex=8)
points(0, 0.398, pch=1, col="darkblue", cex=10)
mtext(expression("Discharge" ~ (m^{3} ~ s^{-1})), side=2, cex=4, font=2, line=1)
mtext(expression("Time" ~ (Days)), side=1, cex=4, font=2, line=4)
legend(x=-6, y=0.45, expression((a)), bty="n", cex=5)


###########################################
##  Figure //.  Falling-water Discharge  ##
###########################################

rm(list=ls())

par(mar=c(2, 2, 0, 0)) # Bottom, left, top, right
par(oma=c(5, 4, 4, 4))
x <- seq(-4, 4, length=100)
hx <- dnorm(x)
min(hx)
max(hx)
plot(x, hx, type="l", lty=1, lwd=4, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-4, 4), ylim=c(0, 0.43), col="darkblue")
abline(h=0.25, col="dimgray", lwd=4, lty=2)
points(-0.96, 0.25, pch=19, col="dimgray", cex=4)
points(-0.96, 0.25, pch=1, col="dimgray", cex=6)
points(-0.96, 0.25, pch=1, col="dimgray", cex=8)
points(0.96, 0.25, pch=19, col="darkblue", cex=4)
points(0.96, 0.25, pch=1, col="darkblue", cex=6)
mtext(expression("Discharge" ~ (m^{3} ~ s^{-1})), side=2, cex=4, font=2, line=1)
mtext(expression("Time" ~ (Days)), side=1, cex=4, font=2, line=4)
legend(x=-6, y=0.45, expression((a)), bty="n", cex=5)


###########################################
##  Figure //.  Discharge Relationships  ##
###########################################

# Tile 1

rm(list=ls())

par(mfrow=c(3,1))

par(mar=c(3, 3, 1, 3)) # Bottom, left, top, right
par(oma=c(5, 4, 4, 4))
x <- seq(-4, 4, length=100)
hx <- dnorm(x)
min(hx)
max(hx)
plot(x, hx, type="p", lty=1, lwd=2, xlab="", ylab="", pch="", xaxt="n", yaxt="n", xlim=c(-4.5, 4), ylim=c(0, 0.43), col="darkblue")
par(new=TRUE)
plot(x, hx, type="l", lty=1, lwd=2, xlab="", bty="n", ylab="", xaxt="n", yaxt="n", xlim=c(-4, 4), ylim=c(0, 0.43), col="darkblue")
par(new=TRUE)
plot(x, hx, type="l", lty=1, lwd=2, xlab="", bty="n", ylab="", xaxt="n", yaxt="n", xlim=c(-4.5, 3.5), ylim=c(0, 0.43), col="dimgray")
mtext(expression("Discharge" ~ (m^{3} ~ s^{-1})), side=2, cex=1.6, font=1, line=1)
mtext(expression("Inundated" ~ "Floodplain" ~ (m^{2})), side=4, cex=1.6, font=1, line=2.25, col="dimgray")
mtext(expression("Time" ~ (Days)), side=1, cex=1.6, font=1, line=2)
legend("topleft", expression((a)), bty="n", cex=2.25)

# Tile 2

par(mar=c(3, 3, 1, 3)) # Bottom, left, top, right
x <- seq(-4, 4, length=100)
y <- seq(-4, 4, length=100)
plot(x, y, type="l", lty=1, lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-5, 5), ylim=c(5, -5), col="black")
mtext(expression("Inundated" ~ "Floodplain" ~ (m^{2})), side=1, cex=1.6, font=1, line=2.25, col="dimgray")
mtext(expression("Mean" ~ "Water" ~ "Depth" ~ (m)), side=2, cex=1.6, font=1, line=2)
legend("topleft", expression((b)), bty="n", cex=2.25)

3130+256+40+1000+2858+65+256 

#######################################
##  Figure //.  Reduction of Carbon  ##
#######################################

rm(list=ls())

# install.packages("knitr")
# install.packages("hysteresis")

library(knitr)
library(hysteresis)

par(mfrow=c(3,2))

?mloop

# Tile 1

par(mar=c(1, 2, 1, 2)) # Bottom, left, top, right
par(oma=c(5, 4, 4, 4))
hysteresis <- mloop(retention=0.5, n.points=100, period=99, extended.classical=F, phase.angle=20, n=2, m=1, b.x=-0.6, b.y=-0.6)
plot(hysteresis$x, hysteresis$y, type="l", lty=1, lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8))
abline(v=0, col="darkgreen", lwd=2, lty=2)
arrows(-0.59, -0.5, x1=-0.588, y1=-0.48, length=0.1, angle=25, lwd=1.5)
arrows(0.588, -0.48, x1=0.59, y1=-0.5, length=0.1, angle=25, lwd=1.5)
arrows(-0.35, 0.2, x1=-0.34, y1=0.22, length=0.1, angle=25, lwd=1.5)
arrows(0.34, 0.22, x1=0.35, y1=0.2, length=0.1, angle=25, lwd=1.5)
arrows(-0.24, -0.555, x1=-0.25, y1=-0.559, length=0.1, angle=25, lwd=1.5)
arrows(0.25, -0.559, x1=0.24, y1=-0.555, length=0.1, angle=25, lwd=1.5)
points(-0.56, -0.7, pch=19, col="chartreuse3", cex=2)
points(0.56, -0.7, pch=19, col="chartreuse3", cex=2)
points(0.56, -0.7, pch=1, col="chartreuse3", cex=4)
points(0.56, -0.7, pch=1, col="chartreuse3", cex=6)
points(0.56, -0.7, pch=1, col="chartreuse3", cex=8)
points(0, -0.5, pch=19, col="chartreuse3", cex=2)
points(0, -0.5, pch=1, col="chartreuse3", cex=4)
points(0, 0.5, pch=19, col="chartreuse3", cex=2)
points(0, 0.5, pch=1, col="chartreuse3", cex=4)
points(0, 0.5, pch=1, col="chartreuse3", cex=6)
mtext("Phytoplankton", side=2, cex=1.6, font=1, line=1)
legend("topleft", expression((a)), bty="n", cex=2.25)

# Tile 2 # Legend

par(mar=c(1, 2, 1, 2)) # Bottom, left, top, right
plot(0, 0, type="l", lty=1, bty="n", lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8))
points(0, 0.5, pch=19, col="chartreuse3", cex=2)
text(0.35, 0.5, labels="Low-water", cex=2, font=1)
points(0, 0.25, pch=19, col="chartreuse3", cex=2)
points(0, 0.25, pch=1, col="chartreuse3", cex=4)
points(0, 0.25, pch=1, col="chartreuse3", cex=6)
text(0.35, 0.25, labels="Rising-water", cex=2, font=1)
points(0, 0, pch=19, col="chartreuse3", cex=2)
points(0, 0, pch=1, col="chartreuse3", cex=4)
points(0, 0, pch=1, col="chartreuse3", cex=6)
points(0, 0, pch=1, col="chartreuse3", cex=8)
text(0.35, 0, labels="High-water", cex=2, font=1)
points(0, -0.25, pch=19, col="chartreuse3", cex=2)
points(0, -0.25, pch=1, col="chartreuse3", cex=4)
text(0.35, -0.25, labels="Falling-water", cex=2, font=1)

# Tile 3

par(mar=c(1, 2, 1, 2)) # Bottom, left, top, right
hysteresis <- mloop(retention=0.5, n.points=100, period=99, extended.classical=T, phase.angle=1, b.x=-0.6, b.y=0.5)
plot(hysteresis$x, hysteresis$y, type="l", lty=1, lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8))
abline(v=0, col="darkgreen", lwd=2, lty=2)
arrows(-0.529, 0.2, x1=-0.535, y1=0.22, length=0.1, angle=25, lwd=1.5)
arrows(0.529, -0.2, x1=0.535, y1=-0.22, length=0.1, angle=25, lwd=1.5)
arrows(-0.22, 0.65, x1=-0.20, y1=0.64, length=0.1, angle=25, lwd=1.5)
arrows(0.22, -0.65, x1=0.20, y1=-0.64, length=0.1, angle=25, lwd=1.5)
points(-0.56, 0.65, pch=19, col="chartreuse3", cex=2)
points(0.56, -0.65, pch=19, col="chartreuse3", cex=2)
points(0.56, -0.65, pch=1, col="chartreuse3", cex=4)
points(0.56, -0.65, pch=1, col="chartreuse3", cex=6)
points(0.56, -0.65, pch=1, col="chartreuse3", cex=8)
points(0, -0.5, pch=19, col="chartreuse3", cex=2)
points(0, -0.5, pch=1, col="chartreuse3", cex=4)
points(0, 0.5, pch=19, col="chartreuse3", cex=2)
points(0, 0.5, pch=1, col="chartreuse3", cex=4)
points(0, 0.5, pch=1, col="chartreuse3", cex=6)
mtext("Terrestrial Plants", side=2, cex=1.6, font=1, line=1)
legend("topleft", expression((b)), bty="n", cex=2.25)

# Tile 4

par(mar=c(1, 2, 1, 2)) # Bottom, left, top, right
hysteresis <- mloop(retention=0.5, n.points=100, period=99, extended.classical=T, phase.angle=1, b.x=0.6, b.y=0.5)
plot(hysteresis$x, hysteresis$y, type="l", lty=1, lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8))
abline(v=0, col="darkgreen", lwd=2, lty=2)
arrows(-0.529, -0.2, x1=-0.535, y1=-0.22, length=0.1, angle=25, lwd=1.5)
arrows(0.529, 0.2, x1=0.535, y1=0.22, length=0.1, angle=25, lwd=1.5)
arrows(-0.22, -0.65, x1=-0.20, y1=-0.64, length=0.1, angle=25, lwd=1.5)
arrows(0.22, 0.65, x1=0.20, y1=0.64, length=0.1, angle=25, lwd=1.5)
points(-0.56, -0.65, pch=19, col="chartreuse3", cex=2)
points(0.56, 0.65, pch=19, col="chartreuse3", cex=2)
points(0.56, 0.65, pch=1, col="chartreuse3", cex=4)
points(0.56, 0.65, pch=1, col="chartreuse3", cex=6)
points(0.56, 0.65, pch=1, col="chartreuse3", cex=8)
points(0, -0.5, pch=19, col="chartreuse3", cex=2)
points(0, -0.5, pch=1, col="chartreuse3", cex=4)
points(0, -0.5, pch=1, col="chartreuse3", cex=6)
points(0, 0.5, pch=19, col="chartreuse3", cex=2)
points(0, 0.5, pch=1, col="chartreuse3", cex=4)
mtext("Floating Macrophytes", side=2, cex=1.6, font=1, line=1)
legend("topleft", expression((c)), bty="n", cex=2.25)

# Tile 5

par(mar=c(1, 2, 1, 2)) # Bottom, left, top, right
hysteresis4 <- mloop(retention=0.5, n.points=100, period=99, extended.classical=T, phase.angle=1, b.x=0.6, b.y=0.5)
plot(hysteresis4$x, hysteresis4$y, type="l", lty=1, lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8))
abline(v=0, col="darkgreen", lwd=2, lty=2)
arrows(-0.529, -0.2, x1=-0.535, y1=-0.22, length=0.1, angle=25, lwd=1.5)
arrows(0.529, 0.2, x1=0.535, y1=0.22, length=0.1, angle=25, lwd=1.5)
arrows(-0.22, -0.65, x1=-0.20, y1=-0.64, length=0.1, angle=25, lwd=1.5)
arrows(0.22, 0.65, x1=0.20, y1=0.64, length=0.1, angle=25, lwd=1.5)
points(-0.56, -0.65, pch=19, col="chartreuse3", cex=2)
points(0.56, 0.65, pch=19, col="chartreuse3", cex=2)
points(0.56, 0.65, pch=1, col="chartreuse3", cex=4)
points(0.56, 0.65, pch=1, col="chartreuse3", cex=6)
points(0.56, 0.65, pch=1, col="chartreuse3", cex=8)
points(0, -0.5, pch=19, col="chartreuse3", cex=2)
points(0, -0.5, pch=1, col="chartreuse3", cex=4)
points(0, -0.5, pch=1, col="chartreuse3", cex=6)
points(0, 0.5, pch=19, col="chartreuse3", cex=2)
points(0, 0.5, pch=1, col="chartreuse3", cex=4)
mtext("Periphyton", side=2, cex=1.6, font=1, line=1)
mtext(expression("Discharge"), side=1, cex=1.6, font=1, line=2)
legend("topleft", expression((d)), bty="n", cex=2.25)

# Tile 6

par(mar=c(1, 2, 1, 2)) # Bottom, left, top, right
hysteresis <- mloop(retention=0.5, n.points=100, period=99, extended.classical=F, phase.angle=20, n=2, m=0.9, b.x=-0.6, b.y=-0.6)
plot(hysteresis$x, hysteresis$y, type="l", lty=1, lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8))
segments(-0.6, -0.59, 0.6, -0.59, lty=1, lwd=2)
abline(v=0, col="darkgreen", lwd=2, lty=2)
arrows(-0.591, -0.48, x1=-0.592, y1=-0.495, length=0.1, angle=25, lwd=1.5)
arrows(0.592, -0.495, x1=0.591, y1=-0.48, length=0.1, angle=25, lwd=1.5)
arrows(-0.344, 0.22, x1=-0.355, y1=0.2, length=0.1, angle=25, lwd=1.5)
arrows(0.355, 0.2, x1=0.344, y1=0.22, length=0.1, angle=25, lwd=1.5)
arrows(-0.25, -0.59, x1=-0.24, y1=-0.59, length=0.1, angle=25, lwd=1.5)
arrows(0.24, -0.59, x1=0.25, y1=-0.59, length=0.1, angle=25, lwd=1.5)
points(-0.6, -0.59, pch=19, col="chartreuse3", cex=2)
points(0.6, -0.59, pch=19, col="chartreuse3", cex=2)
points(0.6, -0.59, pch=1, col="chartreuse3", cex=4)
points(0.6, -0.59, pch=1, col="chartreuse3", cex=6)
points(0.6, -0.59, pch=1, col="chartreuse3", cex=8)
points(0, -0.59, pch=19, col="chartreuse3", cex=2)
points(0, -0.59, pch=1, col="chartreuse3", cex=4)
points(0, -0.59, pch=1, col="chartreuse3", cex=6)
points(0, 0.5, pch=19, col="chartreuse3", cex=2)
points(0, 0.5, pch=1, col="chartreuse3", cex=4)
mtext("Blue-green Algae", side=2, cex=1.6, font=1, line=1)
mtext(expression("Discharge"), side=1, cex=1.6, font=1, line=2)
legend("topleft", expression((e)), bty="n", cex=2.25)


#######################################
##  Figure //.  Oxidation of Carbon  ##
#######################################

rm(list=ls())

# install.packages("knitr")
# install.packages("hysteresis")

library(knitr)
library(hysteresis)

par(mfrow=c(3,2))

?mloop

# Tile 1
par(mar=c(1, 2, 1, 2)) # Bottom, left, top, right
par(oma=c(5, 4, 4, 4))
hysteresis <- mloop(retention=0.5, n.points=100, period=99, extended.classical=F, phase.angle=20, n=2, m=1, b.x=-0.6, b.y=-0.6)
plot(hysteresis$x, hysteresis$y, type="l", lty=1, lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8))
abline(v=0, col="chocolate", lwd=2, lty=2)
arrows(-0.59, -0.5, x1=-0.588, y1=-0.48, length=0.1, angle=25, lwd=1.5)
arrows(0.588, -0.48, x1=0.59, y1=-0.5, length=0.1, angle=25, lwd=1.5)
arrows(-0.35, 0.2, x1=-0.34, y1=0.22, length=0.1, angle=25, lwd=1.5)
arrows(0.34, 0.22, x1=0.35, y1=0.2, length=0.1, angle=25, lwd=1.5)
arrows(-0.24, -0.555, x1=-0.25, y1=-0.559, length=0.1, angle=25, lwd=1.5)
arrows(0.25, -0.559, x1=0.24, y1=-0.555, length=0.1, angle=25, lwd=1.5)
points(-0.56, -0.7, pch=19, col="chocolate4", cex=2)
points(0.56, -0.7, pch=19, col="chocolate4", cex=2)
points(0.56, -0.7, pch=1, col="chocolate4", cex=4)
points(0.56, -0.7, pch=1, col="chocolate4", cex=6)
points(0.56, -0.7, pch=1, col="chocolate4", cex=8)
points(0, -0.5, pch=19, col="chocolate4", cex=2)
points(0, -0.5, pch=1, col="chocolate4", cex=4)
points(0, 0.5, pch=19, col="chocolate4", cex=2)
points(0, 0.5, pch=1, col="chocolate4", cex=4)
points(0, 0.5, pch=1, col="chocolate4", cex=6)
mtext(expression("NEP"), side=2, cex=1.6, font=1, line=1)
legend("topleft", expression((a)), bty="n", cex=2.25)
  
# Tile 2 # Legend

par(mar=c(1, 2, 1, 2)) # Bottom, left, top, right
plot(0, 0, type="l", lty=1, bty="n", lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8))
points(0, 0.5, pch=19, col="chocolate4", cex=2)
text(0.35, 0.5, labels="Low-water", cex=2, font=1)
points(0, 0.25, pch=19, col="chocolate4", cex=2)
points(0, 0.25, pch=1, col="chocolate4", cex=4)
points(0, 0.25, pch=1, col="chocolate4", cex=6)
text(0.35, 0.25, labels="Rising-water", cex=2, font=1)
points(0, 0, pch=19, col="chocolate4", cex=2)
points(0, 0, pch=1, col="chocolate4", cex=4)
points(0, 0, pch=1, col="chocolate4", cex=6)
points(0, 0, pch=1, col="chocolate4", cex=8)
text(0.35, 0, labels="High-water", cex=2, font=1)
points(0, -0.25, pch=19, col="chocolate4", cex=2)
points(0, -0.25, pch=1, col="chocolate4", cex=4)
text(0.35, -0.25, labels="Falling-water", cex=2, font=1)

# Tile 3

par(mar=c(1, 2, 1, 2)) # Bottom, left, top, right
hysteresis <- mloop(retention=0.5, n.points=100, period=99, extended.classical=T, phase.angle=1, b.x=0.6, b.y=0.5)
plot(hysteresis$x, hysteresis$y, type="l", lty=1, lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8))
abline(v=0, col="chocolate", lwd=2, lty=2)
arrows(-0.535, -0.22, x1=-0.529, y1=-0.2, length=0.1, angle=25, lwd=1.5)
arrows(0.535, 0.22, x1=0.529, y1=0.2, length=0.1, angle=25, lwd=1.5)
arrows(-0.20, -0.64, x1=-0.22, y1=-0.65, length=0.1, angle=25, lwd=1.5)
arrows(0.20, 0.64, x1=0.22, y1=0.65, length=0.1, angle=25, lwd=1.5)
points(-0.56, -0.65, pch=19, col="chocolate4", cex=2)
points(0.56, 0.65, pch=19, col="chocolate4", cex=2)
points(0.56, 0.65, pch=1, col="chocolate4", cex=4)
points(0.56, 0.65, pch=1, col="chocolate4", cex=6)
points(0.56, 0.65, pch=1, col="chocolate4", cex=8)
points(0, -0.5, pch=19, col="chocolate4", cex=2)
points(0, -0.5, pch=1, col="chocolate4", cex=4)
points(0, 0.5, pch=19, col="chocolate4", cex=2)
points(0, 0.5, pch=1, col="chocolate4", cex=4)
points(0, 0.5, pch=1, col="chocolate4", cex=6)
mtext(expression("Riverine" ~ "DOC"), side=2, cex=1.6, font=1, line=1)
legend("topleft", expression((b)), bty="n", cex=2.25)

# Tile 4

par(mar=c(1, 2, 1, 2)) # Bottom, left, top, right
hysteresis <- mloop(retention=0.5, n.points=100, period=99, extended.classical=T, phase.angle=1, b.x=0.6, b.y=0.5)
plot(hysteresis$x, hysteresis$y, type="l", lty=1, lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8))
abline(v=0, col="chocolate", lwd=2, lty=2)
arrows(-0.529, -0.2, x1=-0.535, y1=-0.22, length=0.1, angle=25, lwd=1.5)
arrows(0.529, 0.2, x1=0.535, y1=0.22, length=0.1, angle=25, lwd=1.5)
arrows(-0.22, -0.65, x1=-0.20, y1=-0.64, length=0.1, angle=25, lwd=1.5)
arrows(0.22, 0.65, x1=0.20, y1=0.64, length=0.1, angle=25, lwd=1.5)
points(-0.56, -0.65, pch=19, col="chocolate4", cex=2)
points(0.58, 0.35, pch=19, col="chocolate4", cex=2)
points(0.58, 0.35, pch=1, col="chocolate4", cex=4)
points(0.58, 0.35, pch=1, col="chocolate4", cex=6)
points(0.58, 0.35, pch=1, col="chocolate4", cex=8)
points(0, -0.5, pch=19, col="chocolate4", cex=2)
points(0, -0.5, pch=1, col="chocolate4", cex=4)
points(0, -0.5, pch=1, col="chocolate4", cex=6)
points(0, 0.5, pch=19, col="chocolate4", cex=2)
points(0, 0.5, pch=1, col="chocolate4", cex=4)
mtext(expression("Floodplain" ~ "DOC"), side=2, cex=1.6, font=1, line=1)
legend("topleft", expression((c)), bty="n", cex=2.25)

# Tile 5

par(mar=c(1, 2, 1, 2)) # Bottom, left, top, right
hysteresis <- mloop(retention=0.5, n.points=100, period=99, extended.classical=T, phase.angle=1, b.x=0.6, b.y=0.5)
plot(hysteresis$x, hysteresis$y, type="l", lty=1, lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8))
abline(v=0, col="chocolate", lwd=2, lty=2)
arrows(-0.529, -0.2, x1=-0.535, y1=-0.22, length=0.1, angle=25, lwd=1.5)
arrows(0.529, 0.2, x1=0.535, y1=0.22, length=0.1, angle=25, lwd=1.5)
arrows(-0.22, -0.65, x1=-0.20, y1=-0.64, length=0.1, angle=25, lwd=1.5)
arrows(0.22, 0.65, x1=0.20, y1=0.64, length=0.1, angle=25, lwd=1.5)
points(-0.56, -0.65, pch=19, col="chocolate4", cex=2)
points(0.58, 0.35, pch=19, col="chocolate4", cex=2)
points(0.58, 0.35, pch=1, col="chocolate4", cex=4)
points(0.58, 0.35, pch=1, col="chocolate4", cex=6)
points(0.58, 0.35, pch=1, col="chocolate4", cex=8)
points(0, -0.5, pch=19, col="chocolate4", cex=2)
points(0, -0.5, pch=1, col="chocolate4", cex=4)
points(0, -0.5, pch=1, col="chocolate4", cex=6)
points(0, 0.5, pch=19, col="chocolate4", cex=2)
points(0, 0.5, pch=1, col="chocolate4", cex=4)
mtext(expression(italic(p)*CO[2]), side=2, cex=1.6, font=1, line=1)
mtext(expression("Discharge"), side=1, cex=1.6, font=1, line=2)
legend("topleft", expression((d)), bty="n", cex=2.25)

# Tile 6

par(mar=c(1, 2, 1, 2)) # Bottom, left, top, right
hysteresis <- mloop(retention=0.5, n.points=100, period=99, extended.classical=T, phase.angle=1, b.x=-0.6, b.y=0.5)
plot(hysteresis$x, hysteresis$y, type="l", bty="n", lty=1, lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8), col="dimgray")
arrows(-0.529, 0.2, x1=-0.535, y1=0.22, length=0.1, angle=25, lwd=1.5, col="dimgray")
arrows(0.529, -0.2, x1=0.535, y1=-0.22, length=0.1, angle=25, lwd=1.5, col="dimgray")
arrows(-0.22, 0.65, x1=-0.20, y1=0.64, length=0.1, angle=25, lwd=1.5, col="dimgray")
arrows(0.22, -0.65, x1=0.20, y1=-0.64, length=0.1, angle=25, lwd=1.5, col="dimgray")
points(-0.56, 0.65, pch=19, col="dimgray", cex=2)
points(0.56, -0.65, pch=19, col="dimgray", cex=2)
points(0.56, -0.65, pch=1, col="dimgray", cex=4)
points(0.56, -0.65, pch=1, col="dimgray", cex=6)
points(0.56, -0.65, pch=1, col="dimgray", cex=8)
points(0, -0.5, pch=19, col="dimgray", cex=2)
points(0, -0.5, pch=1, col="dimgray", cex=4)
points(0, 0.5, pch=19, col="dimgray", cex=2)
points(0, 0.5, pch=1, col="dimgray", cex=4)
points(0, 0.5, pch=1, col="dimgray", cex=6)
mtext(expression("Dissolved" ~ O[2]), side=4, cex=1.6, col="dimgray", font=1, line=2)
par(new=T)
hysteresis <- mloop(retention=0.5, n.points=100, period=99, extended.classical=T, phase.angle=1, b.x=0.6, b.y=0.5)
plot(hysteresis$x, hysteresis$y, type="l", lty=1, lwd=2, xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8))
abline(v=0, col="chocolate", lwd=2, lty=2)
arrows(-0.535, -0.22, x1=-0.529, y1=-0.2, length=0.1, angle=25, lwd=1.5)
arrows(0.535, 0.22, x1=0.529, y1=0.2, length=0.1, angle=25, lwd=1.5)
arrows(-0.20, -0.64, x1=-0.22, y1=-0.65, length=0.1, angle=25, lwd=1.5)
arrows(0.20, 0.64, x1=0.22, y1=0.65, length=0.1, angle=25, lwd=1.5)
points(-0.56, -0.65, pch=19, col="chocolate4", cex=2)
points(0.56, 0.65, pch=19, col="chocolate4", cex=2)
points(0.56, 0.65, pch=1, col="chocolate4", cex=4)
points(0.56, 0.65, pch=1, col="chocolate4", cex=6)
points(0.56, 0.65, pch=1, col="chocolate4", cex=8)
points(0, -0.5, pch=19, col="chocolate4", cex=2)
points(0, -0.5, pch=1, col="chocolate4", cex=4)
points(0, 0.5, pch=19, col="chocolate4", cex=2)
points(0, 0.5, pch=1, col="chocolate4", cex=4)
points(0, 0.5, pch=1, col="chocolate4", cex=6)
mtext(expression(italic(p)*CH[4]), side=2, cex=1.6, font=1, line=1)
mtext(expression("Discharge"), side=1, cex=1.6, font=1, line=2)
legend("topleft", expression((e)), bty="n", cex=2.25)

