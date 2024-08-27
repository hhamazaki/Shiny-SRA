#'==============================================================================
#' ggplot2  functiohn Sets
#'==============================================================================
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
# 2.0   SR plot functions----
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




