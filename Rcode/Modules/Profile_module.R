#'===============================================================================
#  Profile MSY-Rmax Profile Based Goal Analyses Module ----- 
#'===============================================================================
#'===============================================================================
#  ProfileUI ---- 
#  UI section: SmsyprofRUI("ns.name")
#  Server section: Smsyprofserver("ns.name",SR.pred)
#  Output:  EG.Smsy, EG.Smsy.st, Srange.st,  
#'===============================================================================
#'  UI -------------------------------------------------------------------
ProfileUI <- function(id,crit){
  # Display choice of Run vs S-R
  ns <- NS(id)
  tagList( 
    p(strong(paste(crit,"Analyses"))),  
    sliderInput(ns("p.min"), paste("Min % of",crit), value=90,min=0,max=100,step=5),
    sliderInput(ns("p.t"), paste("% Meeting",crit,"Target"), value=90,min=0,max=100,step=5),
    checkboxInput(ns('st.out'),'Standard output',FALSE)
  )  # End taglist
  
} # End SmsypfofUI
#'  Server ----------------------------------------------------------------------
#   EG: Escapement Goal based on User defined Profile Analyses
#   EG.st: Escapement Goal based on Standard Profile Analyses
#   BEG: Escapement Goal table based on Profile Analyses
#   Plt.profile: Profile plot without intersections 
#   BEG: Escapement Goal table based on Profile Analyses
#'-------------------------------------------------------------------------------
ProfileServer <- function(id,SR.pred,crit,unit,plt){
  moduleServer(
    id,
    function(input, output, session){
      ## EG:  User defined profile based goal data -------------------
      EG <- reactive({
        # Import MCMC Expected Mean Yield 
        if(crit == 'MSY'){mc.Y <- SR.pred()$Y}
        else if(crit == 'Rmax') {
          mc.Y <- SR.pred()$R
        }
        # Import S 
        S <- SR.pred()$S
        # Import minimum  %
        p.min <- input$p.min/100
        # Import minimum target %   
        p.t <- input$p.t/100
        # Generate profile using profile function 
        S.prof  <- profile(S, mc.Y, p.min, p.t)
        # Rename Output
        names(S.prof) <- c('S','S.prof','S.Range','Dat.prof')
        return(S.prof)  
      }) # End EG
      
      ## EG.st: Standard profile based goal data ------------------------------
      EG.st <- reactive({
        # Import MCMC Expected Mean Yield
        if(crit == 'MSY'){mc.Y <- SR.pred()$Y}
        else if(crit == 'Rmax'){
          mc.Y <- SR.pred()$R
        }     
        p.t <- input$p.t/100
        # Import S 
        S <- SR.pred()$S
        # Standard min Smsy probability 
        st <- c(0.7,0.8,0.9)
        # Create a dummy matrics 
        ncols <- length(S)
        # Create Standard probability matrix 
        prof.st <- matrix(0,nrow=3,ncol=ncols)
        # Create BEG 
        Srange.st <- matrix(NA,nrow=3,ncol=2)
        for(i in 1:3){
          profile <- profile(S,mc.Y, st[i],p.t)
          prof.st[i,] <- profile$S.prof
          Srange.st[i,] <- profile$S.Range
        }
        # Find Profile Intersections 
        out <- list(S.prof.st = prof.st, S.Range.st = Srange.st)
        return(out)   
      }) # End EG.st
      
      ## BEG: Create EG goal table -------------------------------------------
      BEG <- reactive({
        BEG.st <- EG.st()$S.Range.st
        BEG <- EG()$S.Range
        if(input$st.out){
          percent <- c(input$p.min,70,80,90)
          lg <- c(BEG[1],BEG.st[,1])
          ug <- c(BEG[2],BEG.st[,2])
        } else {
          percent <- c(input$p.min)   
          lg <- c(BEG[1])
          ug <- c(BEG[2])
        }
        apercent <- input$p.t
        #        t.BEG <- c(paste(percent,'%',crit,apercent,'% target'))
        t.BEG <- data.frame(percent,apercent, lg,ug)
        names(t.BEG) <- c(paste(crit,'%'),'Target %','Lower','Upper')
        return(t.BEG)
      }) # SA.BEG
      
      ## plt.profile:  Basic Profile fig without frills  ------------------------
      plt.profile <- reactive({
        u <- unit()
        p.min <- input$p.min/100
        if(input$st.out){
          percent <- c(input$p.min,70,80,90)
        } else {
          percent <- c(input$p.min)   
        }
        txt <- c(paste(percent,'%',crit))
        # Generate profile 
        S <- (SR.pred()$S)
        Y.prof <- EG()$S.prof
        Y.prof.st <- data.frame(t(EG.st()$S.prof.st))
        names(Y.prof.st) <- c('p90','p80','p70')
        df <- data.frame(S=SR.pred()$S,Med=EG()$S.prof,Y.prof.st )
# R Base reshape: replace reshape2 tidyr
        df1 <- reshape(df,direction='long', idvar='S',varying = names(df)[-1],
                       v.names='prob',timevar='prof.type',times=names(df)[-1])
        if(plt=='base'){
          par(xaxs='i',yaxs='i',bty='l',las=1,cex=1.2,cex.lab=1.5)
          df1$S <- df1$S/u
          plot(prob~S,df1[which(df1$prof.type =='p90'),],type='n',col=1, ylim=c(0,1),
               ylab ='Probability',xlab=paste('Spawner',mult(u)),main=paste0(crit,'Profile Analyses'))
          if(input$st.out){   
            lines(prob~S,df1[which(df1$prof.type =='p90'),],lty = 1,col=1)
            lines(prob~S,df1[which(df1$prof.type =='p80'),],lty = 2,col=1)
            lines(prob~S,df1[which(df1$prof.type =='p70'),],lty = 4,col=1)
          }
          lines(prob~S,df1[which(df1$prof.type =='Med'),],lty = 1,lwd=2,col=6)
          p1 <- recordPlot()
        }      
        if(plt=='gg') {
          p1 <- ggplot()+
            geom_line(data = df1[which(df1$prof.type == 'Med'),],aes(x=S,y=prob,linetype = prof.type),color=6)+  
            scale_linetype_discrete(name ="",labels=txt,guide=guide_legend(override.aes=list(color=if(input$st.out){c(6,1,1,1)}else{6})))+
            scale_x_continuous(expand=c(0,0),limits=c(0,NA),labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep)+  
            scale_y_continuous(expand=c(0,0),limits=c(0,1),n.breaks = 10,oob=oob_keep)+
            labs(title = paste0(crit,'Profile Analyses'), x=paste('Spawner',mult(u)),y='Probability')
          if(input$st.out){
            p1 <- p1+
              geom_line(data = df1[which(df1$prof.type != 'Med'),],aes(x=S,y=prob,linetype =prof.type),color=1)
          }
        } 
        return(p1)
      })
      
      ## plt.pro.fig:  Profile fig with intersections  --------------------------
      plt.prof.fig <- reactive({
        if(input$st.out){
          percent <- c(input$p.min,70,80,90)
        } else {
          percent <- c(input$p.min)   
        }
        txt <- c(paste(percent,'%',crit))
        u <- unit()
        # Import minimum target %   
        p.t <- input$p.t/100
        c.col <- ifelse(crit=='MSY',3,4)
        eg <- as.numeric(EG()$S.Range)
        if(plt=='base'){
          layout(matrix(1:2, ncol=2),widths=c(4,1))
          replayPlot(plt.profile())
          abline(h=p.t,lwd=1,col=2)
          rect(eg[1]/u,par('usr')[3],eg[2]/u,par('usr')[4],col=tcol(c.col,80),border=NA)
          if(input$st.out){
            add_legend("top", legend= txt, lwd=c(2,1,1,1), lty=c(1,1,2,4),
                       col=c(6,1,1,1,6),text.font = c(2,1,1,1),box.lty=0)
          } else {
            add_legend("top", legend= txt, lwd=c(2), lty=c(1),
                       col=c(6),text.font = c(2),box.lty=0)
          }
          p1 <- recordPlot()
        }
        if(plt=='gg'){
          p1 <- plt.profile()+
            geom_hline(yintercept = p.t,linewidth=1,col=2)+     
            annotate('rect', xmin = eg[1], xmax = eg[2], ymin = -Inf, ymax = Inf, alpha=0.1, fill=c.col)
        }
        return(p1)
      })
      
      p.min <- reactive({input$p.min})
      p.t <- reactive({input$p.t})      
      
      outdata <- list(EG =EG, EG.st = EG.st,BEG=BEG,p.min=p.min, p.t=p.t,plt.profile=plt.profile,plt.prof.fig=plt.prof.fig )
      return(outdata) 
      
    } # End function
  ) # End moduleServer
} # End ProfServer

