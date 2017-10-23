library(dplyr)
library(tidyr)
library(igraph)
library(shinydashboard)
library(ggplot2)
library(lubridate)

#Loading the data (done ahead of every session)
zombDat<-readRDS(file="./data/zombDat.RDS")
connections<-readRDS(file="./data/patientConnects.RDS")
g<-readRDS(file="./data/networkGraph.RDS")
exposure<-(file="./data/exposureDat.RDS")

description<-"Showing: Patient exposure timing"

#######################
# Shiny server code

shinyServer(function(input, output) {

  output$timeline<-renderPlot({
    #timeline of zombie outbreak
    pBase<-ggplot(data=zombDat, aes(x = dateOfExposureStart,y=ID,group=ID))+
      scale_y_continuous(breaks=0:24)+
      scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 days",date_labels="%b-%d")+
      ylab("Patient ID")+
      theme_bw()+
      theme(legend.position="below")+
      theme(axis.text = element_text(size=12),
            axis.text.x = element_text(angle=90,vjust=0.5),
            axis.title.x = element_blank(),
            panel.grid.major.x = element_line(size=2))
    
    
    #Baseplot is tile data
    if(!is.null(input$colorData)){
      pBase<-pBase +
        geom_tile(aes_string(fill=input$colorData),colour="white",alpha=0.7)
    }else{
      pBase<-pBase +
        geom_tile(fill = "darkgray",colour="white")
    }
    
    ###########################
    # Adding data to graph
    ###########################
    
    #adding exposure tiles
    if(!is.null(input$addData)){
      #add date of symptom onset
      if("Symptom Onset" %in% input$addData){
        if(!is.null(input$colorData)){
          pBase<-pBase+
            geom_segment(aes(x=dateOfExposureEnd,y=ID,xend=dateOfOnset,yend=ID,group=ID),colour="grey")+
            geom_point(aes_string(x="dateOfOnset",y="ID",fill=input$colorData),colour="black",size=3,pch=21)
        }else{
          pBase<-pBase+
            geom_segment(aes(x=dateOfExposureEnd,y=ID,xend=dateOfOnset,yend=ID,group=ID),colour="grey")+
            geom_point(aes(x=dateOfOnset,y=ID),colour="black",fill="red",size=3,pch=21)
        }
      }
      
      #add the network transmission
      if("Patient Relationships" %in% input$addData){
        #if exposure is on
        pBase<-pBase +
          geom_curve(data=connections,aes(x=sourceEnd,y=source,xend=targetStart,yend=ID),colour="black",curvature=-0.15,alpha=0.75)
      }
    }
    
    #plot the plot!
    plot(pBase + theme(legend.position="bottom"))
      
  })
  
  output$textDesc<-renderText({
    textBase<-"The current graph shows the dates a patient was potentially exposed to the zombie (square blocks)."
  
    #color
    if(!is.null(input$colorData)){
      textBase<-paste(textBase,sprintf("The data points are coloured by patient's %s", input$colorData))
    }
    
  })
  
  #changing the content of the filter menu by selection
})
