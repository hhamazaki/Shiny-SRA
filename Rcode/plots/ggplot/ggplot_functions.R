#'==============================================================================
#' ggplot2  function Sets
#'==============================================================================
library(ggplot2)    # Used for ggplot graphics 
library(ggrepel)      # Used for ggplot: avoid year overlap 
library(cowplot) # Used to mainly plot_grid function
library(scales)  # Used to make axis scaled
library(rlang)   # Used to make ggplot functions
library(ggpmisc)
source("Rcode/plots/ggplot/ggplot_theme.R") 
source("Rcode/plots/ggplot/theme_adfg.R") 
theme_set(theme_simple())
#theme_set(theme_adfg_bbc())
okabe <- c("#E69F00","#56B4E9", "#009E73","#F0E442","#0072B2","#D55E00","#CC79A7" )
options(ggplot2.discrete.colour = okabe)
options(ggplot2.discrete.fill = list(okabe))
#'------------------------------------------------------------------------------
# 1.0   Histogram-Density functions----
#'------------------------------------------------------------------------------
## Determine Histogram bin width ----
bw <- function(x){2 * IQR(x) / length(x)^(1/3)}
## Create histogram with mean dash line ----
gg_hist <- function(df,x,u,b){
  ggplot(df,aes(x = {{x}})) +
    scale_x_continuous(labels = label_number(scale = 1 /u))+
    geom_histogram(color="black", fill="white",binwidth=bw(b))+
    geom_vline(aes(xintercept=mean({{x}})),color="blue", linetype="dashed", size=1)
  }

#'------------------------------------------------------------------------------
# 2.0   Second Y axis range determination functions----
#       y1: First axis vector
#       y2: Second axis vector
#       s1: 0:First axis min is 0, 1: First axis min is min(y1) 
#       s2: 0:Second axis min is 0, 1: Second axis min is min(y2)
#'------------------------------------------------------------------------------
# Function to scale secondary axis



scale_function <- function(x,y1,y2,s1,s2){
  scale <- (max(y2)-s2*min(y2))/(max(y1)-s1*min(y1))
  shift <- s1*min(y1)-s2*min(y2)
  return (x*scale - shift)
}

# Function to scale secondary variable values
inv_scale_function <- function(y1,y2,s1,s2){
  scale <- (max(y2)-s2*min(y2))/(max(y1)-s1*min(y1))
  shift <- s1*min(y2)-s2*min(y2)
  return ((y2 + shift)/scale)
}

#'  add_title: Add fig title to the plot ---------------------------------------
add_title <- function(plot,title){
  plot+ ggtitle(title)+theme(legend.title = element_blank())
}

#'------------------------------------------------------------------------------
# 2.0   SR plot functions----
#'------------------------------------------------------------------------------
## Create histogram with mean dash line ----
gg_SR <- function(xp,xp2,x,y,u){
  ggplot() +
# Original data    
  geom_point(data = xp, aes(x = {{x}}, y = {{y}}), color = "gray", size = 3) +
# Trimmed data
  geom_point(data = xp2, aes(x = {{x}}, y = {{y}}), color = "black", size = 3) +
  scale_x_continuous(expand=c(0, 0), limits=c(0, NA), 
                     labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, NA), 
                     labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep)
  }


#'------------------------------------------------------------------------------
# gg_2y:  create linedata with two  y axes 
#'------------------------------------------------------------------------------
## Create histogram with mean dash line ----
gg_2y <- function(df,x,y1,y2,u){
 ggplot(data=df)+
       geom_line(aes(x = {{x}}, y = {{y1}}), color = "gray")+
       geom_line(aes(x={{x}}, y= inv_scale_function({{y1}},{{y2}},0,1)),color='black')+
       scale_x_continuous(expand=c(0, 0.5),n.breaks = 10,oob=oob_keep) +
       scale_y_continuous(expand=expansion(mult = c(0, .25)), limits=c(0, max({{y1}})),
                       sec.axis = sec_axis(~scale_function(.,{{y1}},{{y2}},0,1)),
                       labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep)
  }


#'------------------------------------------------------------------------------
# 2.0   SR plot functions----
#'------------------------------------------------------------------------------
## Create histogram with mean dash line ----
gg_SR <- function(xp,xp2,x,y,u){
  ggplot() +
    # Original data    
    geom_point(data = xp, aes(x = {{x}}, y = {{y}}), color = "gray", size = 3) +
    # Trimmed data
    geom_point(data = xp2, aes(x = {{x}}, y = {{y}}), color = "black", size = 3) +
    scale_x_continuous(expand=c(0, 0), limits=c(0, NA), 
                       labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep) +
    scale_y_continuous(expand=c(0, 0), limits=c(0, NA), 
                       labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep)
}
trim <- function(x){x[x<quantile(x,0.99,na.rm=TRUE)]}
trimq <- function(x){quantile(x,0.99,na.rm=TRUE)}
gg_den <- function(df,x,u){
  ggplot(df,aes(x = {{x}})) + scale_x_continuous(labels = label_number(scale = 1 /u))+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank())+
    geom_density()+
    geom_vline(aes(xintercept=mean({{x}},na.rm=TRUE)),color="blue", linetype=1, linewidth=1)+
    geom_vline(aes(xintercept=median({{x}},na.rm=TRUE)),color="blue", linetype=2, linewidth=1)
}


mult.den.plt <- function(dat.a,dat.m,xlab.tx,leg.tx,u){
  df <- rbind(data.frame(x=as.vector(dat.m),a=leg.tx),data.frame(x=as.vector(dat.a),a='Annual'))
  p1 <- ggplot()+theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.line.y = element_blank(),axis.ticks.y=element_blank())+  
    stat_density(data=df,aes(x=x, y=..scaled..,linetype=a), position="dodge", geom="line")+
    scale_linetype_manual(name ="", values=c(2,1))+  
    scale_x_continuous(expand=c(0, 0),labels = label_number(scale = 1/u),n.breaks = 10,oob=oob_keep) + xlab(paste(xlab.tx,mult(u)))+
    scale_y_continuous(expand=c(0, 0),limits=c(0,NA))
  return(p1)  
}

#===============================================================================
# 3.0  plot density distribution of MCMC results -----
#===============================================================================

plot_density_gg <- function(sim,D,ar1,model='Ricker',target='md',u){
  if(target =='me'){
  p1 <- gg_den(sim[sim$alpha.c<trimq(sim$alpha.c),],alpha,1)+ggtitle("alpha.c")
  p2 <- gg_den(sim[sim$lnalpha.c<trimq(sim$lnalpha.c),],lnalpha,1)+ggtitle("lnalpha.c")
  p4 <- gg_den(sim[sim$Seq.c<trimq(sim$Seq.c),],Seq.c,u)+ggtitle(paste("Seq.c",mult(u)))
  p5 <- gg_den(sim[sim$Smsy.c<trimq(sim$Smsy.c),],Seq.c,u)+ggtitle(paste("Smsy.c",mult(u)))
  p6 <- gg_den(sim,Umsy.c,1)+ggtitle(paste("Umsy.c"))
  p7 <- gg_den(sim[sim$Sgen.c<trimq(sim$Sgen.c),],Sgen.c,u)+ggtitle(paste("Sgen.c",mult(u)))
  } else {
  p1 <- gg_den(sim[sim$alpha<trimq(sim$alpha),],alpha,1)+ggtitle("alpha")
  p2 <- gg_den(sim[sim$lnalpha<trimq(sim$lnalpha),],lnalpha,1)+ggtitle("lnalpha")
  p4 <- gg_den(sim[sim$Seq<trimq(sim$Seq),],Seq,u)+ggtitle(paste("Seq",mult(u)))
  p5 <- gg_den(sim[sim$Smsy<trimq(sim$Smsy),],Seq,u)+ggtitle(paste("Smsy",mult(u)))
  p6 <- gg_den(sim,Umsy,1)+ggtitle(paste("Umsy"))
  p7 <- gg_den(sim[sim$Sgen<trimq(sim$Sgen),],Sgen,u)+ggtitle(paste("Sgen",mult(u)))
  }
  p3 <- gg_den(sim,beta,1)+ggtitle(paste0('beta',' x 10^(',-D,')'))
  if(isTRUE(ar1)){
     p8 <- gg_den(sim,phi,1)+ggtitle("phi")
     pout <- plot_grid(p1,p2,p3,p8,p4,p5,p6,p7)  
    } else {
     pout <- plot_grid(p1,p2,p3,p4,p5,p6,p7)
    }
  if(model=='Ricker'){
  p9 <- gg_den(sim,Smax,u)+ggtitle(paste("Smax",mult(u)))
    if(isTRUE(ar1)){
    pout <- plot_grid(p1,p2,p3,p8,p4,p5,p6,p7,p9)      
    } else {
    pout <- plot_grid(p1,p2,p3,p4,p5,p6,p7,p9)  
    }
  }
  return(pout)
  }

# plot_range:  -------------------------------------------------------------------
plot_range <- function(baseplot,Srange1,Srange2=c(NA,NA),u,goal=NA)
  {
  p1 <- baseplot
  Srange1 <- as.numeric(Srange1)
  Srange2 <- as.numeric(Srange2)
  if(!is.na(sum(Srange1)) & Srange1[1]==Srange1[2]) {
   p1 <- p1 + geom_vline(xintercept=Srange1[1],color =3,linewidth=2)
  } else {
  p1 <- p1+annotate('rect', xmin = Srange1[1], xmax = Srange1[2], ymin = -Inf, ymax = Inf, alpha=0.1, fill=3)
  }
  # Plot escapement goal range 
  if(!is.na(sum(Srange2))) {
  p1 <- p1+ annotate('rect', xmin = Srange2[1], xmax = Srange2[2], ymin = -Inf, ymax = Inf, alpha=0.1, fill=4)
  }
  if(!is.na(goal)) {
  p1 <- p1 + geom_hline(yintercept = goal, color = 2,linewidth =1.2 )
  }  
  return(p1)
  }

prof_sim_gg <- function(prof.sim,u,tpg,target){
  # Create profile plot 
  tex <- ifelse(target =='me','Mean','Median')
  srange <- as.numeric(prof.sim$range$b.p)
  prof.ma <- prof.sim$prof.ma
  p1 <- ggplot(prof.ma)+
    geom_line(aes(x=S,y=prof.m,linetype=tex))+
    geom_line(aes(x=S,y=prof.a,linetype='Annual'))+
    geom_hline(yintercept=tpg, col=2)+
    scale_x_continuous(expand=c(0,0),limits=c(0,NA),labels = label_number(scale = 1/u),n.breaks = 10,oob=oob_keep)+
    scale_y_continuous(expand=c(0,0),limits=c(0,1),n.breaks = 10,oob=oob_keep)+
    labs(x=paste('Spawner',mult(u)),y='Probability')+
    annotate('rect', xmin = srange[1], xmax = srange[2], ymin = 0, ymax =1, alpha=0.1, fill=3,na.rm = TRUE)+
    scale_linetype_manual(name ="", values=c(2,1))
  return(p1)  
}  

#'------------------------------------------------------------------------------
#' Base plot functions that needs to be converted to ggplot 
#'------------------------------------------------------------------------------ 
plot_ridge <- function(S, ridge.dat, u, n) {
  # Extract relevant parameters
  rep <- nrow(ridge.dat)
  inc <- median(apply(ridge.dat, 1, max, na.rm = TRUE))
  rn <- sample(1:rep, n)  # Randomly sample n rows
  # Scale S
  S <- S
  rx <- range(S)  # Range of S for x-axis
  # Prepare the data for plotting
  ridge_data <- data.frame()
  # Prepare for each ridge (i) as a separate row
  for (i in 1:n) {
    ridge <- ridge.dat[rn[i], ]
    ridge_data <- rbind(ridge_data, data.frame(
      x = S,
      y = ridge + (i - 1) * inc,
      group = i  # Grouping for each ridge
    ))
  }
  maxy <- max(ridge_data$y)
# Create the ggplot
  p <- ggplot()+
#    ggplot(ridge_data, aes(x = x, y = y, group = group)) +
    # Create polygons for each ridge
    geom_polygon(data=ridge_data, aes(x = x, y = y, fill = factor(group)), color = NA, alpha = 0.3) +
    # Add horizontal dashed lines
    geom_hline(data = data.frame(y = (0:(n-1)) * inc), aes(yintercept = y), 
               color = "grey", linetype = "solid", linewidth = 0.25) +
    # Set axis labels and title
    labs(x = paste('Spawner', mult(u)), y = NULL) +
    scale_fill_manual(values = rainbow(n)) +
#     scale_fill_viridis_c()+
        theme(
#      axis.title = element_text(size = 14),
#      axis.text.y = element_blank(),
#      axis.text.x = element_text(size=14),
#      axis.line.y = element_blank(),
#      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
#      plot.margin = margin(0, 0, 0, 0)  # No extra space
     ) +
    scale_x_continuous(expand=c(0,0),limits=c(0,NA),labels = label_number(scale = 1/u),n.breaks = 10,oob=oob_keep)+
    scale_y_continuous(expand=c(0,0),limits=c(0,maxy),labels = label_number(scale = 1/maxy),breaks = c(0,0.2,0.4,0.6,0.8,1.0)*maxy,oob=oob_keep)
  # Print the plot
  return(p)
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

