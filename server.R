shinyServer(function(input, output){
  #unzip("persons.zip")
  df <- reactive({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    data <- readxl::read_excel(paste(inFile$datapath, ".xlsx", sep=""),col_names = T)
    
    infoflow <- function(data){
      por <- c(9.9,8.85,7.8,6.75,5.7,4.65,3.6,2.55,1.5,.45)
      lin <- inner_join(data_frame(name=data[[1]][14:23],tb=c(9.9,8.85,7.8,6.9,5.87,4.83,3.78,2.73,1.68,.63)),
                        data_frame(name=data[[1]][2:11],net=c(9.9,8.85,7.8,6.9,5.87,4.83,3.78,2.73,1.68,.63)),by="name")
      
      lin$tb2 <- 0
      lin$net2 <- 1
      output <- interp_points(lin)
      
      p1 <- ggplot()+
        geom_bar(aes(reorder(data[[1]][2:11],as.numeric(data[[2]][2:11])),
                     as.numeric(data[[3]][2:11])),stat = "identity",width = 0.6,fill="#bcbec0",color=NA)+
        geom_bar(aes(reorder(data[[1]][2:11],as.numeric(data[[2]][2:11])),
                     as.numeric(data[[2]][2:11])),stat = "identity",width = 0.4,fill="#136278",color=NA)+
        geom_text(aes(reorder(data[[1]][2:11],as.numeric(data[[2]][2:11])),0,
                      label=reorder(data[[1]][2:11],as.numeric(data[[2]][2:11]))),hjust=0,vjust=-2,size=6,family = "PT Sans")+
        ylab("")+
        xlab("")+
        scale_x_discrete(labels=rev(data[[1]][2:11]),position = "top")+
        scale_y_continuous(expand = c(0, 0)) +
        coord_flip() + theme(
          panel.grid = element_blank(), panel.border = element_blank(),
          axis.text.y = element_blank(),
          axis.line.x = element_line(size=2,color="#136278"),
          axis.line.y = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_text(hjust=0,vjust=-2,size=14,family = "PT Sans"),
          axis.ticks.length = unit(0.2, "line"),
          #axis.ticks.length = unit(-1, "line"),
          plot.margin=unit(c(0,0,0,0), "cm")
        )
      
      p2 <- ggplot()+
        geom_bar(aes(reorder(data[[1]][14:23],as.numeric(data[[2]][14:23])),
                     as.numeric(data[[3]][14:23])/1000),stat = "identity",width = 0.6,fill="#bcbec0")+
        geom_bar(aes(reorder(data[[1]][14:23],as.numeric(data[[2]][14:23])),
                     as.numeric(data[[2]][14:23])),stat = "identity",width = 0.4,fill="#c41e58")+
        geom_text(aes(reorder(data[[1]][14:23],as.numeric(data[[2]][14:23])),0,
                      label=reorder(data[[1]][14:23],as.numeric(data[[2]][14:23]))),hjust=1,vjust=-2,size=6,family = "PT Sans")+
        ylab("")+
        xlab("")+
        scale_x_discrete(labels=rev(data[[1]][14:23]))+
        coord_flip() + theme(
          panel.grid = element_blank(), panel.border = element_blank(),
          axis.line.x = element_line(color="#c41e58",size=2),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(hjust=1,vjust=-2,size=14,family = "PT Sans"),
          axis.ticks.length = unit(0.2, "line"),
          plot.margin=unit(c(0,0,0,0), "cm")
        ) + scale_y_reverse(expand = c(0, 0))
      
      p3 <- ggplot(output,aes(x=long,y=lat,group=line_id,color=seg_num)) + 
        geom_path(alpha=1,size=2,lineend = "round") +
        scale_color_gradient(high="#136278",low="#c41e58")+
        ylab("")+
        xlab("")+
        scale_x_continuous(limits = c(0,1),expand = c(0, 0))+
        scale_y_continuous(limits = c(0,10))+
        theme(
          legend.position = "none",
          panel.grid = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks.length = unit(-1.4, "line"),
          plot.margin=unit(c(0,0,0,0,0), "cm")
        ) 
      
      p4 <- ggplot()+
        geom_text(aes(1,por,label=ifelse(!is.na(as.numeric(data[[4]][14:23])),paste0(round(as.numeric(data[[4]][14:23])),"%"),"∞")),family = "PT Sans",
                  hjust=1,color=ifelse(as.numeric(data[[4]][14:23])>0 | is.na(as.numeric(data[[4]][14:23])),"#136278","#c41e58"),size=6) +
        geom_segment(aes(x=1.01,y=ifelse(is.na(as.numeric(data[[4]][14:23])) | as.numeric(data[[4]][14:23])>0,por-0.3,por+0.3),
                         xend=1.01,yend=ifelse(is.na(as.numeric(data[[4]][14:23])) | as.numeric(data[[4]][14:23])>0,por+0.3,por-0.3)),
                     arrow = arrow(length = unit(0.2, "cm")),size=1,color=ifelse(as.numeric(data[[4]][14:23])>0 | is.na(as.numeric(data[[4]][14:23])),"#136278","#c41e58"))+
        ylab("")+
        xlab("")+
        scale_x_continuous(limits = c(0.9,1.04))+
        scale_y_continuous(limits = c(0,10.5),expand = c(0, 0))+
        theme(
          legend.position = "none",
          panel.grid = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks.length = unit(-1.4, "line"),
          plot.margin=unit(c(0,0,0,0,0), "cm")
        ) 
      
      p5 <- grid.arrange(ggplot() +
      {if(file.exists(paste0("persons/",data[[1]][2],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][2],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][3],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][3],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][4],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][4],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][5],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][5],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][6],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][6],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][7],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][7],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][8],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][8],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][9],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][9],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][10],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][10],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][11],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][11],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ncol=1)
      p6 <- grid.arrange(ggplot() +
      {if(file.exists(paste0("persons/",data[[1]][14],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][14],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][15],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][15],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][16],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][16],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][17],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][17],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][18],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][18],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][19],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][19],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][20],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][20],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][21],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][21],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][22],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][22],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",data[[1]][23],".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",data[[1]][23],".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ncol=1)
      p7 <- ggplot()+
        geom_text(aes(1,por,label=ifelse(!is.na(as.numeric(data[[4]][2:11])),paste0(round(as.numeric(data[[4]][2:11])),"%"),"∞")),family = "PT Sans",hjust=0,
                  color=ifelse(as.numeric(data[[4]][2:11])>0,"#136278","#c41e58"),size=6) +
        geom_segment(aes(x=0.99,y=ifelse(as.numeric(data[[4]][2:11])>0,por-0.3,por+0.3),
                         xend=0.99,yend=ifelse(as.numeric(data[[4]][2:11])>0,por+0.3,por-0.3)),
                     arrow = arrow(length = unit(0.2, "cm")),size=1,color=ifelse(as.numeric(data[[4]][2:11])>0,"#136278","#c41e58"))+
        ylab("")+
        xlab("")+
        scale_x_continuous(limits = c(0.97,1.1))+
        scale_y_continuous(limits = c(0,10.5),expand = c(0, 0))+
        theme(
          legend.position = "none",
          panel.grid = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks.length = unit(-1.4, "line"),
          plot.margin=unit(c(0,0,0,0,0), "cm")
        ) 
      
      p8 <- ggplot() + 
        annotation_custom(rasterGrob(readPNG("tv.png"), interpolate=TRUE), xmin=0, xmax=1, ymin=0.3, ymax=1) +
        geom_text(aes(x=0.5,y=0.1,label=toupper("топ-персони доби на тб")),size=6) +
        geom_point() + scale_y_continuous(limits = c(0,1)) + theme_void()
      
      p9 <- ggplot() + 
        annotation_custom(rasterGrob(readPNG("net.png"), interpolate=TRUE), xmin=0, xmax=1, ymin=0.3, ymax=1) +
        geom_text(aes(x=0.5,y=0.1,label=toupper("топ-персони доби в інтернеті")),size=6) +
        geom_point() + scale_y_continuous(limits = c(0,1)) + theme_void()
      
      grid.arrange(p4,ggplot(),p6,ggplot(),p2,p3,p1,p5,ggplot(),p7,ggplot(),p8,p9,
                   layout_matrix=cbind(c(11,11,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2), 
                                       c(11,11,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4),
                                       c(12,12,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5),
                                       c(11,11,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6),
                                       c(13,13,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
                                       c(11,11,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,9),
                                       c(11,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11)),
                   widths=c(0.5,0.5,3,1,3,0.5,0.5))
    }
    infoflow(data)
  })
  
  output$plot <- renderPlot({
    tryCatch(df())
  })
  
  output$down <- downloadHandler(
    filename = function(){
      paste0("infoflow-",Sys.Date(),".pdf")
      },
    content = function(file) {
      cairo_pdf(file, width=15.98, height=11.93,bg = "white")
      grid.draw(df())
      dev.off()
    }
  )
  
  output$do <- downloadHandler(
    filename = function(){
      paste0("infoflow-",Sys.Date(),".png")
      },
    content = function(file) {
      png(file, width=2200, height=1700,bg = "white")
      grid.draw(df())
      dev.off()
    }
  )
})