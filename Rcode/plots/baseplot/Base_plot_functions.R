#'===============================================================================
# Base_plot_function.R 
# Collections of functions used for Shiny SR model apps
#'===============================================================================
#'===============================================================================
#  Graphical ----
#'===============================================================================
# Add legend in separate window 
add_legend <- function(...) {
  par(mar=c(0,0,0,0))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n',xlab='',ylab='')
  legend(...)
}

## Show Color Shades -------------------------------------------------------   
tcol <- function(color, percent = 50, name = NULL) {
  #	  color = color name
  #	percent = % transparency
  #	   name = an optional name for the color
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100-percent)*255/100,
               names = name)
  return(t.col)
}

#------ Show two density plots in one fig --------------------------------------
mult.den.plt <- function(dat.a,dat.m,main.tx,xlab.tx,leg.tx){
  d1 <- density(dat.a)
  d2 <- density(dat.m)
  plot(d1,xlim =c(min(d1$x),max(d1$x)),main=main.tx,xlab=xlab.tx,ylab='',lty=2)
  par(new = TRUE)  
  plot(d2 ,lty=1,xlim =c(min(d1$x),max(d1$x)),axes = FALSE,xlab='',ylab='',main='')
  legend('topright',leg.tx, lty=c(2,1),bty='n')
}

# plot_profile  --- Profile plotting  Function  ---------------------------------   
plot_profile <- function(TN,prof,prof.st,S,mip,tp,u){
  # TN: Profile Target name 
  # prof: user defined Profile 
  # profst: standard Profile
  # S: Spawner 
  # mip: user defined min p 
  # tp: user defined target p 
  # u: user defined multiplier output  
  mult <- mult(u)
  S <- S/u
#'---------------------------------------------------------------------------
  par(xaxs='i',yaxs='i',bty='l',las=1)
  #  Standard profile plots 
  plot(S,prof.st[1,],type='l',col=1, ylim=c(0,1),ylab = 'Probability',
       xlab=paste('Escapement',mult),main=paste(TN,'Profile')) 
  lines(S,prof.st[2,],lty = 2,col=1)
  lines(S,prof.st[3,],lty = 4,col=1)
#  abline(h = stp,lwd=1,col=1)
  #  User defined profile 
  lines(S,prof,lty = 1,lwd=2,col=6)
  # User defined target  
  abline(h = tp,lwd=2,col=2)

}


# Prof_fig  --- Profile summary plot  Function  ---------------------------------   
Prof_fig <- function(prof,crit,u){
  layout(matrix(1:2, ncol=2),widths=c(2,1))
#' -------Extract Profile data  -------------------------------------------------
  EG <- reactive({prof$EG()})
  EG.st <- reactive({prof$EG.st()})
  p.min <- reactive({prof$p.min()})   # Minimum goal 
  p.t <- reactive({prof$p.t()})       # % achievement target
  plt.profile <- reactive({prof$plt.profile()})
  # Import minimum Smsy %
  p.min <- as.numeric(p.min())/100
  # Import minimum % achieving 
  p.t <- as.numeric(p.t())/100  
  replayPlot(plt.profile())
  S <- EG()$S.Range/u
  c.col <- ifelse(crit=='MSY',3,4)
  polygon(c(S,rev(S)),c(c(0,0),c(1,1)),col=tcol(c.col,80),border=NA)
  #  Add legends 
  percent <- c(90,80,70,100*p.min)
  apercent <- 100*p.t
  BEG.st <- EG.st()$S.Range.st
  BEG <- EG()$S.Range
  lg <- c(BEG[1],BEG.st[,1])
  ug <- c(BEG[2],BEG.st[,2])
  txt <- c(paste(percent,'%',crit,apercent,'% target:',lg,' - ',ug))
  add_legend("left", legend= txt, lwd=c(1,1,1,2), lty=c(1,2,4,1),
             col=c(1,1,1,6),text.font = c(1,1,1,2),box.lty=0)
 }


