#'===============================================================================
# Base_plot_function.R 
# Collections of functions used for Shiny SR model apps
#'===============================================================================
source("Rcode/plots/baseplot/pointLabelBase.R")   #  Separate year 
#'===============================================================================
#  Graphical ----
#'===============================================================================
# Add legend in separate window 
add_legend <- function(...) {
  par(mar=c(0,0,0,0))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n',xlab='',ylab='')
  legend(...)
}

# Add title 
add_title <- function(plot, title){
  replayPlot(plot)
  title(main = title)
  recordPlot()
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

plot_hist <- function(x,u,title ){
    s <- x/u
    hist(s,main='',ylab='',xlab=title)
    h.mean <- mean(s,na.rm=TRUE)
    abline(v=h.mean,lwd=2,lty=2,col='blue')
    }

plot_den <- function(x,u,title ){
    s <- x/u
    plot(trimden(s),main=title,ylab='',xlab='')
    h.mean <- mean(s,na.rm=TRUE)
    h.med <- median(s,na.rm=TRUE)
    abline(v=h.mean,lwd=2,lty=1,col='blue')
    abline(v=h.med,lwd=2,lty=2,col='blue')
    }

# plot_density 
# plot density distribution of MCMC results 

#-------------------------------------------------------------------------------
plot_density <- function(sim,D,u,ar1,model='Ricker',target='md'){
  par(mfrow=c(3,3),mar = c(2,1,2,1),xaxs='i',yaxs='i',bty='l',cex=1.2)
  if(target =='me'){
    plot_den(sim$alpha.c,1,'alpha.c')
    plot_den(sim$lnalpha.c,1,'lnalpha.c')
    plot_den(sim$beta,1,paste0('beta',' x 10^(',-D,')'))
    if(isTRUE(ar1)){plot_den(sim$phi,1,'Phi')}
    plot_den(sim$Seq.c,u,paste('Seq.c',mult(u)))
    plot_den(sim$Smsy.c,u,paste('Smsy.c',mult(u)))  
    plot_den(sim$Umsy.c,1,'Umsy.c') 
    plot_den(sim$Sgen.c[!is.na(sim$Sgen.c)],u, paste('Sgen.c',mult(u))) 
  } else {
    plot_den(sim$alpha,1,'alpha')
    plot_den(sim$lnalpha,1,'lnalpha')
    plot_den(sim$beta,1,paste0('beta',' x 10^(',-D,')'))
    if(isTRUE(ar1)){plot_den(sim$phi,1,'Phi')}    
    plot_den(sim$Seq,u,paste('Seq',mult(u)))
    plot_den(sim$Smsy,u,paste('Smsy',mult(u)))  
    plot_den(sim$Umsy,1,'Umsy') 
    plot_den(sim$Sgen[!is.na(sim$Sgen)],u, paste('Sgen',mult(u))) 
  }
  if(model=='Ricker'){plot_den(sim$Smax,u,paste('Smax',mult(u)))}
}



# Plt_runesc --- Plot Run-Escapement Time series (when data is "Run") ----------
plot_runesc <- function(dat,u){
     par(yaxs='i',bty='l')
     plot(R/u~Yr,data=dat,type='l',ylim=c(0,with(dat,max(R,S,na.rm =TRUE)/u)),xlab='',ylab='')
     lines(S/u~Yr,data=dat,lty=2)
 }

#------ Show two density plots in one fig --------------------------------------
mult.den.plt <- function(dat.a,dat.m,xlab.tx,leg.tx,u){
  d1 <- density(dat.a/u)
  d2 <- density(dat.m/u)
  par(xaxs='i',yaxs='i',bty='l',las=1,cex=1.2,cex.lab=1.5)  
  plot(d1,xlim =c(min(d1$x),max(d1$x)),main='',xlab=paste(xlab.tx,mult(u)),ylab='',lty=2)
  par(new = TRUE)  
  plot(d2 ,lty=1,xlim =c(min(d1$x),max(d1$x)),axes = FALSE,xlab='',ylab='',main='')
  legend('topright',leg.tx, lty=c(2,1),bty='n')
}


# plot_profile  --- Profile plotting  Function  ---------------------------------   
plot_profile <- function(TN,df,mip,tp,u){
  # TN: Profile Target name 
  # prof: user defined Profile 
  # profst: standard Profile
  # S: Spawner 
  # mip: user defined min p 
  # tp: user defined target p 
  # u: user defined multiplier output  
  df$S <- df$S/u
#'---------------------------------------------------------------------------
layout(matrix(1:2, ncol=2),widths=c(3,1))
    par(xaxs='i',yaxs='i',bty='l',las=1)
  #  Standard profile plots 
  plot(p90~S,dfprof.st[1,],type='l',col=1, ylim=c(0,1),ylab = 'Probability',
       xlab=paste('Spawner',mult(u)),main=paste(TN,'Profile')) 
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

# plot_range:  -------------------------------------------------------------------
plot_range <- function(baseplot,Srange1,Srange2=c(NA,NA),u,goal=NA)
  {
  Srange1 <- as.numeric(Srange1)
  Srange2 <- as.numeric(Srange2)
    replayPlot(baseplot)
    if(!is.na(sum(Srange1)) & Srange1[1]==Srange1[2]) {
    abline(Srnage1[1],lwd=3,col=tcol(3,80))
    } 
    else{
    rect(Srange1[1]/u,par('usr')[3],Srange1[2]/u,par('usr')[4],col=tcol(3,80),border=NA)
    }
  # Plot escapement goal range 
  if(!is.na(sum(Srange2))){
    rect(Srange2[1]/u,par('usr')[3],Srange2[2]/u,par('usr')[4],col=tcol(4,80),border=NA)
   }
  if(!is.na(goal)) {
  abline(h=goal/u,lwd=2,col=2)
  } 
 }

# Ridge plot -------------------------------------------------------------------
plot_ridge <- function(S,ridge.dat,u,n)
  {
  rep <- dim(ridge.dat)[1]
  inc <- median(apply(ridge.dat,1,max,na.rm=TRUE))
  rn <- sample(1:rep,n)
  ry <-c(0,inc*n)
  S <- S/u
  rx <- range(S)
  cl <- rainbow(n)
# Create overall plot area 
 par(xaxs='i',yaxs='i',bty='l',las=1)
 plot(1, type = "n", xlim = rx, ylim = ry,yaxt='n', ylab='',xlab=paste('Spawner',mult(u)))
  for(i in 1:n){
    ridge <- ridge.dat[rn[i],]
    abline(h = (i-1)*inc, col = "grey",lty=3,lwd=0.5)
    polygon(S,
            ridge + (i-1)*inc,
            col = tcol(cl[i],80),border=NA)
   }
 }

