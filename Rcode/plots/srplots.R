# Function to scale secondary axis
scale_function <- function(x,first,second,s1,s2){
  scale <- (max(second)-s2*min(second))/(max(first)-s1*min(first))
  shift <- s1*min(first)-s2*min(second)
  return (x*scale - shift)
}

# Function to scale secondary variable values
inv_scale_function <- function(first,second,s1,s2){
  scale <- (max(second)-s2*min(second))/(max(first)-s1*min(first))
  shift <- s1*min(first)-s2*min(second)
  return ((second + shift)/scale)
}



# Plt_runesc --- Plot Run-Escapement Time series (when data is "Run") ----------
plt_runesc <- reactive({
   if(input$dataType== "Run"){
     x <- data()[,c(1:3)]
     names(x) <-c('Yr','S','R')
     x$ex <- with(x,(R-S)/R)
     x$H <- with(x,(R-S))
     u <- unit()
     p1 <- ggplot(data=x)+
       geom_line(aes(x=Yr,y=R,color='Run'),linetype=1)+
       geom_line(aes(x=Yr,y=S,color ='Escapement'),linetype=2)+
       geom_line(aes(x=Yr,y=H,color= 'Harvest'),linetype=3)+
       geom_line(aes(x=Yr,y= inv_scale_function(R,ex,0,0),color='Harvest Rate'),linetype=4)+
        scale_color_manual(values=c(1,2,3,4),breaks=c('Run','Escapement','Harvest','Harvest Rate'),
        guide=guide_legend(override.aes=list(linetype=c(1,2,3,4))))+
    scale_x_continuous(expand=c(0, 0.5),n.breaks = 10,oob=oob_keep) +
    scale_y_continuous(expand=c(0, 0), limits=c(0, max(x$R)),
                      sec.axis = sec_axis(~scale_function(.,x$R,x$ex,0,0),name='Harvest Rate'),
                       labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep)+
    xlab(paste('Year')) + ylab(paste('Abundance',mult(u)))
      return(p1)                      
 
    }
 })

##### Plt_srt Plot SR time series ---------------------------------------
plt_srt <- reactive({
  if(input$dataType != 'Escapement Only'){
    x <- sr.data.0()
    u <- unit()
     x$ex <- with(x,log(R/S))
     u <- unit()
     p1 <- ggplot(data=x)+
       geom_line(aes(x=Yr,y=R,color='Recruit'),linetype=1)+
       geom_line(aes(x=Yr,y=S,color ='Spawner'),linetype=5)+
       geom_line(aes(x=Yr,y= inv_scale_function(R,ex,0,1),color='ln(R/S)'),linetype=1)+
        scale_color_manual(values=c(1,1,4),breaks=c('Recruit','Spawner','ln(R/S)'),
        guide=guide_legend(override.aes=list(linetype=c(1,5,1))))+
    scale_x_continuous(expand=c(0, 0.5),n.breaks = 10,oob=oob_keep) +
    scale_y_continuous(expand=c(0, 0), limits=c(0, max(x$R)),
                       sec.axis = sec_axis(~scale_function(.,x$R,x$ex,0,1),name='ln(R/S)'),
                       labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep)+
    xlab(paste('Year')) + ylab(paste('Abundance',mult(u)))                   
    } else {
    x <- e.data.0()
    u <- unit()
     p1 <- ggplot(data=x)+
       geom_line(aes(x=Yr,y=S,color='Escapement'),linetype=1)+
    scale_x_continuous(expand=c(0, 0.5),n.breaks = 10,oob=oob_keep) +
    scale_y_continuous(expand=c(0, 0), limits=c(0, max(x$S)),
                       labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep)+
    xlab(paste('Year')) + ylab(paste('Escapement',mult(u)))
    }
    # Add Cutting data 
    if(max(input$sryears)<max(x$Yr)|min(input$sryears)>min(x$Yr)){
      p1 <- p1+ 
      geom_vline(xintercept = input$sryears, color =2) 
    }
  return(p1)      
 })
