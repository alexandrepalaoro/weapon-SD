#====================================================================
#SCRIPT THAT DRAWS FIGURES 3 AND 5
#
#====================================================================

library(extrafont)
library(scales)

#--------------------------------------------------------------------
#function that draws the median and 95% CI plots
pw95 = function(x, pch=19, col="black", bg="black", cex=1,
                                  scol="black", slwd=1, slty=1, checklines=TRUE, 
                                  labels = colnames(x), xaxt="s", includezero = TRUE, ...)
{
  
  qq = apply(x, 2, quantile, probs = c(0.5, 0.025, 0.975))
  
  ylim = range(qq)
  if(includezero & (ylim[1]>0))
    ylim[1] = 0
  
  plot(x = 1:ncol(x), y=qq[1,], ylim=ylim, xlim=c(0.5, ncol(x)+0.5), 
       xaxt="n", ...)
  
  if(length(labels)<=1)
    labels = 1:ncol(x)
  
  if(xaxt!="n")
    axis(side = 1, at=1:ncol(x), labels = labels, cex=par()$cex*par()$cex.lab)
  
  segments(x0 = 1:ncol(x), x1=1:ncol(x), y0=qq[2,], y1=qq[3,],
           col=scol, lwd=slwd, lty=slty)
  
  if(checklines)
  {
    for(i in 1:ncol(x))
    {
      abline(h=qq[2,i], lty=2, col="darkgrey")
      abline(h=qq[3,i], lty=2, col="darkgrey")
    }
  }
  points(x = 1:ncol(x), y=qq[1,], pch=pch, col=col, bg=bg, cex=cex)
  invisible(t(qq))
}
#--------------------------------------------------------------------

############################################################
#SUPER FIGURE WITH ALL ESTIMATES AND CREDIBLE INTERVALS!
###########################################################

#list with all mcmc results
biglist = list(apodeme = mcmc.ap, 
               ma = mcmc.ma, 
               csize = mcmc.sc, 
               shape = mcmc.sh)

ylabels = list(apodeme = expression(sqrt("Apodeme area")~" (cm)"),
               ma = "Mechanical advantage",
               tamanho = "Standardized centroid size",
               shape = "Procrustes distance")

species.labels = c("Dual-function", "Pure weapon")

cex.x = 1.2

(bxats = rep(1:2, each=2) + rep(c(-0.15, 0.15), 2))

png(filename = "FIGURE.3.png", width=27, height=30, units="cm", res=450)

par(mfrow=c(4,3), las=1, bty="l", family="Arial", mar=c(6,6.5,2,2),
    cex.lab=1.5, cex.axis=1.2, mgp = c(4,1,0), oma=c(2,2,2,2))

#############################
#centroid (claw) size plot
#############################

mcmci = biglist$csize

#dimorphism plots
pw95(mcmci$MCMC[,c("SDA.1", "SDA.2")], pch=19, cex=2,
     labels=species.labels,
     ylab="Difference in intercept", xlab="", checklines = TRUE)

abline(h=0, lty = 3, col="red") #red line on zero
mtext(side=3, adj=1, text="A", cex=1.5) #legend on top

pw95(mcmci$MCMC[,c("SDB.1", "SDB.2")], pch=19, cex=2,
     labels=species.labels,
     ylab="Difference in slope", xlab="", checklines = TRUE)

abline(h=0, lty = 3, col="red")
mtext(side=3, adj=1, text="B", cex=1.5)

#raw data boxplots
boxplot(dat$csize~dat$sex+dat$label, range=0,
        whisklty=1, staplewex=0, col=c("white", "grey"),
        xaxt="n", ylab=ylabels$tamanho, at=bxats, boxwex=0.25)

axis(side=1, at=c(1, 2) , labels=species.labels,
     cex.axis=cex.x)

mtext(side=3, adj=1, text="C", cex=1.5)

legend("topleft", pch=22, pt.bg = c("white", "grey"),
       legend=c("female", "male"), cex=1.5, bty="n")
#color legend

##################################
#APODEME (STRENGTH)
##################################

mcmci = biglist$apodeme

#plot de dimorfismo
pw95(mcmci$MCMC[,c("SDA.1", "SDA.2")], pch=19, cex=2,
     labels=species.labels,
     ylab="Difference in intercept", xlab="", checklines = TRUE)

abline(h=0, lty = 3, col="red") #linha vermelha no zero
mtext(side=3, adj=1, text="D", cex=1.5) #legenda superior

#plot de dimorfismo
pw95(mcmci$MCMC[,c("SDB.1", "SDB.2")], pch=19, cex=2,
     labels=species.labels,
     ylab="Difference in slope", xlab="", checklines = TRUE)

abline(h=0, lty = 3, col="red") #linha vermelha no zero
mtext(side=3, adj=1, text="E", cex=1.5) #legenda superior

boxplot(dat$ap~dat$sex+dat$label, range=0,
        whisklty=1, staplewex=0, col=c("white", "grey"),
        xaxt="n", ylab=ylabels$apodeme, at=bxats, boxwex=0.25)
axis(side=1, at=c(1, 2) , labels=species.labels,
     cex.axis=cex.x)

mtext(side=3, adj=1, text="F", cex=1.5) #legenda superior

#######################
#MECHANICAL ADVANTAGE
#######################

mcmci = biglist$ma

#plot de dimorfismo
pw95(mcmci$MCMC[,c("SDA.1", "SDA.2")], pch=19, cex=2,
     labels=species.labels,
     ylab="Difference in intercept", xlab="", checklines = TRUE)

abline(h=0, lty = 3, col="red") #linha vermelha no zero
mtext(side=3, adj=1, text="G", cex=1.5) #legenda superior

#plot de dimorfismo
pw95(mcmci$MCMC[,c("SDB.1", "SDB.2")], pch=19, cex=2,
     labels=species.labels,
     ylab="Difference in slope", xlab="", checklines = TRUE)

abline(h=0, lty = 3, col="red") #linha vermelha no zero
mtext(side=3, adj=1, text="H", cex=1.5) #legenda superior

boxplot(dat$ma1~dat$sex+dat$label, range=0,
        whisklty=1, staplewex=0, col=c("white", "grey"),
        xaxt="n", ylab=ylabels$ma, at=bxats, boxwex=0.25)
axis(side=1, at=c(1, 2) , labels=species.labels,
     cex.axis=cex.x)

mtext(side=3, adj=1, text="I", cex=1.5) #legenda superior

#############################
#SHAPE (PROCRUSTES DISTANCE)
#############################

mcmci = biglist$shape

pw95(mcmci$MCMC[,c("A.1", "A.2")], pch=19, cex=2,
     labels=species.labels,
     ylab="Intercept", xlab="", checklines = TRUE)

abline(h=0, lty = 3, col="red")
mtext(side=3, adj=1, text="J", cex=1.5) #legenda superior

pw95(mcmci$MCMC[,c("B.1", "B.2")], pch=19, cex=2,
     labels=species.labels,
     ylab="Slope", xlab="", checklines = TRUE)

abline(h=0, lty = 3, col="red") #linha vermelha no zero
mtext(side=3, adj=1, text="K", cex=1.5) #legenda superior

boxplot(shape~label, range=0, data=machos,
        whisklty=1, staplewex=0,
        xaxt="n", ylab=ylabels[[4]], boxwex=0.75)
axis(side=1, at=c(1, 2), labels=species.labels,
     cex.axis=cex.x)
mtext(side=3, adj=1, text="L", cex=1.5) #legenda superior

mtext(text = "Claw type (species)", side = 1, line=0 ,cex=2, outer=TRUE)
dev.off()

#====================================================================
#############################
#PLOT OF ADJUSTED LINES
#############################


xlabels = c("Standardized centroid size",
            "Standardized centroid size",
            "Standardized cephalothorax length",
            "Standardized centroid size")

png("Figure.5.png", height=21, width=25,res=450, units="cm")

{
  colnames(dat)
  colsY = c("sqapodeme", "ma1", "scsize", "shape")
  colsX = c("scsize", "scsize", "scc", "scsize")
  ltypes = c(1, 3)
  lcolors = alpha(c("black", "grey70"),alpha=1) 
  
  par(las=1, bty="l", mar=c(6,7,3,3), family="Arial", mfrow=c(2,2),
      cex.lab=1.5)
  order = c(3,1,2) #trick to change the order of plotting
  
  for(h in 1:3)
  {
    g = order[h] 
    qqi = biglist[[g]]$summary #takes the adequate model summary
    
    intercepts = matrix(qqi[1:4, 2], nrow=2) #intercepts matrix
    slopes = matrix(qqi[5:8, 2], nrow=2) #slopes matrix
    
    #min and max x values (to "crop" the lines adequately)
    minima = tapply(dat[,colsX[g]], list(dat$sp.number, dat$sex.number),min)
    maxima= tapply(dat[,colsX[g]], list(dat$sp.number, dat$sex.number),max)
    
    #plotting without the points
    plot(x = dat[,colsX[g]], y=dat[,colsY[g]], type="n",
         ylab=ylabels[[g]], xlab=xlabels[g], main="")
    
    if(g == order[1]) #legend in the first plot
    {
      legend("topleft", pch=19, col=lcolors, bty="n",
             legend = c("Dual-function weapon", "Pure weapon"))
      
      legend("bottomright", lwd=3, lty=c(1,3), col="grey30", bty="n",
             legend = c("Female", "Male"))
      
    }
    
    #letter at the top of the plot
    mtext(side=3, text=LETTERS[h], cex=1.5, adj=1)
    
    #plotting the lines
    for(i in 1:2)
      for(j in 1:2)
      {
        a = intercepts[i,j]
        b = slopes[i,j]
        xmin = minima[i,j]
        xmax = maxima[i,j]
        segments(x0 = xmin, x1 = xmax,
                 y0 = a+b*xmin, y1 = a+b*xmax,
                 col=lcolors[i], lty=ltypes[j], lwd=3)
      }
  }
  
  #shape plot (the most different one)
  g=4
  qqi = biglist[[4]]$summary
  
  intercepts = qqi[1:2, 2]
  slopes = qqi[3:4,2]
  
  minima = tapply(dat[,colsX[4]], list(dat$sp.number, dat$sex.number),min)[,2]
  maxima = tapply(dat[,colsX[4]], list(dat$sp.number, dat$sex.number),max)[,2]
  
  plot(x = dat[,colsX[4]], y=dat[,colsY[4]], type="n",
       ylab=ylabels[[4]], xlab=xlabels[4], main="")
  
  mtext(side=3, text=LETTERS[g], cex=1.5, adj=1)
  
  for(i in 1:2)
  {
    a = intercepts[i]
    b = slopes[i]
    xmin = minima[i]
    xmax = maxima[i]
    segments(x0 = xmin, x1 = xmax,
             y0 = a+b*xmin, y1 = a+b*xmax,
             col=lcolors[i], lty=3, lwd=3)
  }
}

dev.off()
#====================================================================
