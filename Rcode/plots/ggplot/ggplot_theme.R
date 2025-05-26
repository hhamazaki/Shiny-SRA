#------------------------------------------------------------------------------
#  ggplot2 Figure theme 
#-------------------------------------------------------------------------------
theme_simple <- function(
                     font_size = 14,
                       font_family = "",
                       line_size = 0.5,
                       rel_small = 12 / 14,
                       rel_tiny = 11 / 14,
                       rel_large = 16 / 14){
      half_line <- font_size / 2
      small_size <- rel_small * font_size
   theme_classic(base_size=12, base_family=font_family) %+replace% 
      theme(
         plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5,,margin = margin(t = 0, r = 0, b = 15, l = 0)),
         panel.background = element_rect(colour = NA),
         plot.background = element_rect(colour = NA),
         #        panel.border = element_rect(colour = NA),
         axis.title = element_text(face = "bold",size = rel(1.5)),
         axis.title.y = element_text(angle=90,vjust =2,face="bold",margin = margin(t = 0, r = 15, b = 0, l = 0)),
         axis.title.y.right = element_text(angle=90,vjust =2,face="bold",margin = margin(t = 0, r = 0, b = 0, l = 15)),
         axis.title.x = element_text(vjust = -0.2),
         axis.text = element_text(size = rel(1.25)), 
         axis.line = element_line(colour="black"),
         axis.ticks = element_line(),
         #               panel.grid.major = element_line(colour="#f0f0f0"),
         panel.grid.major = element_blank(),			   
         panel.grid.minor = element_blank(),
         legend.background = element_rect(fill="transparent", color=NA),
         legend.key = element_rect(fill="transparent", color=NA),
         #               legend.key = element_rect(colour = NA),
         legend.position = "right",
         legend.justification = "top",
         #        legend.key.size= unit(0.2, "cm"),
         #        legend.margin = margin(0,0,0,0, "cm"),
         legend.title = element_blank(),
         legend.text = element_text(size=rel(1.1)),         
         plot.margin=unit(c(10,5,5,5),"mm"),
         strip.background=element_blank(),				   
         strip.text = element_text(face="bold")
      )
}

theme_Pub <- function(base_size=14, base_family="serif") {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size, base_family=base_family)
       + theme(plot.title = element_text(face = "bold",
                                         size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.y = element_text(angle=90,vjust =2),
               axis.title.x = element_text(vjust = -0.2),
               axis.text = element_text(), 
               axis.line = element_line(colour="black"),
               axis.ticks = element_line(),
#               panel.grid.major = element_line(colour="#f0f0f0"),
               panel.grid.major = element_blank(),			   
               panel.grid.minor = element_blank(),
               legend.key = element_rect(colour = NA),
               legend.position = "bottom",
               legend.direction = "horizontal",
               legend.key.size = unit(0.2, "cm"),
               legend.margin = margin(0,0,0,0, "cm"),
               legend.title = element_text(face="italic"),
               plot.margin=margin(10,10,10,10),
#               strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
               strip.background=element_blank(),			   
               strip.text = element_text(face="bold")
          ))
      
}

scale_fill_Publication <- function(...){
      library(scales)
      discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}

scale_col_Pub <- function(...){
      library(scales)
      discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}


