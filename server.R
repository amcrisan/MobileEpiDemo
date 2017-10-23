library(plyr)
library(dplyr)
library(tidyr)
library(igraph)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(emojifont)
library(plotly)

load.emojifont('OpenSansEmoji.ttf')

#Loading the data (done ahead of every session)
zombDat<-readRDS(file="./data/zombDat.RDS")
connections<-readRDS(file="./data/patientConnects.RDS")
g<-readRDS(file="./data/networkGraph.RDS")
exposure<-readRDS(file="./data/exposureDat.RDS")

zombDat$filterOut<-rep("No",nrow(zombDat))
totalPatients<-zombDat$ID %>% unique() %>% length()

#setting up emjoii font for axis labels
emoji_text=element_text(family="OpenSansEmoji", size=20)

exposure<-exposure %>%
  filter(Exposed =="Y") %>%
  inner_join(zombDat) %>%
  select(ID,eType,status,Age,Occupation,Sex) %>%
  distinct() %>%
  mutate(eTypeEmoji = plyr::mapvalues(eType,
                                      from=sort(unique(exposure$eType)),
                                      to=c(emoji('grimacing'),
                                           emoji('sweat_drops'),
                                           emoji('poop'),
                                           emoji('house'),
                                           emoji('eggplant'),
                                           emoji('hamburger')))) %>%
  mutate(eTypeEmoji = factor(eTypeEmoji))


#######################
# Shiny server code

shinyServer(function(input, output) {

  emphasize<-reactive({
    
    expDat<-exposure
    #filtering by sex
    if(!is.null(input$filterSex)){
      expDat<-expDat %>%
        filter(Sex %in% input$filterSex)
    }
    #filtering by exposure
    if(!is.null(input$filterExposure)){
      expDat<-expDat %>%
        filter(eType %in% input$filterExposure)
    }
    
    #filtering by exposure
    if(!is.null(input$filterStatus)){
      expDat<-expDat %>%
        filter(status %in% input$filterStatus)
    }
    
    #filtering by age of the patient
    if(!is.null(input$filterAge)){
      expDat<-expDat %>%
        filter(Age > input$filterAge[1] & Age < input$filterAge[2])
        
    }
    
    #IDS to emphasize based on combination of factors
    return(expDat$ID %>% unique())
    

  })
  
  ##################################
  # TIMELINE GRAPH
  ##################################
  output$timeline<-renderPlot({
    #timeline of zombie outbreak

    #Data points to emphasize
    if(!is.null(emphasize())){
      zombDat<-zombDat %>% mutate(filterIDs = factor(ifelse(ID %in% emphasize(),"Yes","No"),levels=c("No","Yes")))
    }else{
      zombDat$filterIDs<-factor(rep("Yes",nrow(zombDat)),levels=c("No","Yes"))
    }
    
    pBase<-ggplot(data=zombDat, aes(x = dateOfExposureStart,y=ID,group=ID))+
      scale_y_continuous(breaks=0:24)+
      scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 days",date_labels="%b-%d")+
      ylab("Patient ID")+
      xlab("")+
      theme_bw()+
      theme(legend.position="below")+
      scale_alpha_manual(values=c(0.25,1.0),drop=FALSE)+
      theme(axis.text = element_text(size=14),
            axis.text.x = element_text(angle=90,vjust=0.5),
            axis.title.x = element_blank(),
            panel.grid.major.x = element_line(size=2))
    
    
    #Baseplot is tile data
    if(!is.null(input$colorData)){
      pBase<-pBase +
        geom_tile(aes_string(fill=input$colorData,alpha="filterIDs"),colour="white")
    }else{
      pBase<-pBase +
        geom_tile(fill = "darkgray",colour="white")
    }
    
    
    
    ##################################
    # Adding data to timeline graph
    ##################################
    
    #adding exposure tiles
    if(!is.null(input$addData)){
      #add date of symptom onset
      if("Symptom Onset" %in% input$addData){
        if(!is.null(input$colorData)){
          pBase<-pBase+
            geom_segment(aes(x=dateOfExposureEnd+0.5,y=ID,xend=dateOfOnset,yend=ID,group=ID,alpha=filterIDs),colour="grey")+
            geom_point(aes_string(x="dateOfOnset",y="ID",fill=input$colorData,alpha="filterIDs"),colour="black",size=3,pch=21)
        }else{
          pBase<-pBase+
            geom_segment(aes(x=dateOfExposureEnd,y=ID,xend=dateOfOnset,yend=ID,group=ID),colour="grey")+
            geom_point(aes(x=dateOfOnset,y=ID),colour="black",fill="red",size=3,pch=21)
        }
      }
      
      #add the network transmission
      if("Patient Relationships" %in% input$addData){
        connections<-connections %>%
          mutate(filterIDs = factor(ifelse(ID %in% emphasize(),"Yes","No"),levels=c("No","Yes")))
      
        pBase<-pBase +
          geom_curve(data=connections,aes(x=sourceEnd,y=source,xend=targetStart,yend=ID,alpha=filterIDs),colour="black",curvature=-0.15)
      }
    }
    
    #plot the plot!
    pBase<-pBase + guides(alpha = "none") + theme(legend.position="bottom")
    pBase
      
  })
  
  ##################################
  # SUPPORTING GRAPH
  ##################################
  
  output$exposureDat<-renderPlot({
    ##################################
    # EXPOSURE HISTOGRAM
    tmp<-exposure
    
    if(!is.null(emphasize())){
      tmp<-tmp %>%
        filter(ID %in% emphasize())
    }
     
    tmp<- tmp %>%
      group_by(eTypeEmoji)%>%
      tally() %>%
      complete(eTypeEmoji,fill=list(n=0))
    
    orderOfLevels<-tmp %>% ungroup() %>% group_by(eTypeEmoji) %>% summarise(n = sum(n)) %>% arrange(n)
    tmp$eTypeEmoji<-factor(tmp$eTypeEmoji,levels=orderOfLevels$eTypeEmoji)
    
  
    p<-ggplot(data=tmp,aes(y=n,x=eTypeEmoji))+
      geom_bar(stat="identity")+
      ylab("Total # of individual exposed")+
      xlab("Exposure Type")+
      scale_y_continuous(breaks= 0:totalPatients)+
      coord_flip()+
      theme_bw()+
      theme(axis.text.y=emoji_text,legend.position="none")
    
    p
    
    ##################################
    # NETWORK GRAPH
    
  })

  
  output$textDesc<-renderText({
    textBase<-"The current graph shows the dates a patient was potentially exposed to the zombie (square blocks)."
  
    #color
    if(!is.null(input$colorData)){
      textBase<-paste(textBase,sprintf("The data points are coloured by patient's %s", input$colorData))
    }
    
  })
  
  
  #reveal patient into
  output$click_info<-renderUI({
    #change zombdat so it is plot compatible with the click outputs
    zombDat$dateOfExposureStart<-as.numeric(zombDat$dateOfExposureStart)
    zombDat$dateOfExposureEnd<-as.numeric(zombDat$dateOfExposureEnd)
    
    click <- input$plot_click
    #print(click)
    panelText<-p(HTML("<em>Select</em> a point to reveal the patient's details"))
    if(!is.null(input$colorData)){
      point <- nearPoints(zombDat, click, threshold = 10, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      #left_pct <- (click$domain$left - click$domain$left) / (click$domain$right - click$domain$left)
      #top_pct <- (click$domain$top - 9.93991) / (click$domain$top - click$domain$bottom)
      left_pct <- ((click$x-5) - click$domain$left) / (click$domain$right - click$domain$left)
      top_pct <- (click$domain$top - (click$y -5)) / (click$domain$top - click$domain$bottom)
      
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- click$range$left + left_pct * (click$range$right - click$range$left)
      top_px <- click$range$top + top_pct *(click$range$bottom - click$range$top)
      
      # create style property for tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px +2, "px;")
      
      #print(style)
      
      ptExp<-exposure %>%
        filter(ID == point$ID)
      
      panelText<-p(HTML(paste0("<b> PatientID: </b>",point$ID,
                               "<b>  | Age: </b>",point$Age,
                               "<b>  | Occupation : </b>",point$Occupation,
                               "<b>  | Sex : </b>",point$Sex,"<br>",
                               "<b> Exposure: </b>",paste0(ptExp$eTypeEmoji,collapse = " , "))))
    }
    
    #panel style
    #style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ")
    wellPanel(style = style,
              panelText)
  })
  #hacked tooltip because I kinda need ggplot's flexibility
  #this tool tip cose is borrowed from (https://gitlab.com/snippets/16220)
  #ok - this doesn't adjust easily to changes in the menu
  output$hover_info <- renderUI({
    #change zombdat so it is plot compatible with the hover outputs
    zombDat$dateOfExposureStart<-as.numeric(zombDat$dateOfExposureStart)
    zombDat$dateOfExposureEnd<-as.numeric(zombDat$dateOfExposureEnd)
    
    hover <- input$plot_hover
    print(hover)
    point <- nearPoints(zombDat, hover, threshold = 100, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
   
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct *(hover$range$bottom - hover$range$top)
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px +2, "px;")
    
    # actual tooltip created as wellPanel
    
    #get the patient's exposures
    ptExp<-exposure %>%
      filter(ID == point$ID)
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> PatientID: </b>",point$ID,"<br/>",
                    "<b> Exposure: </b>",paste0(ptExp$eTypeEmoji,collapse = " , "),"<br/>")
      ))
      #p(sprintf("Patient ID:%s \n", point$ID))
      # p(HTML(paste0("<b> Car: </b>", rownames(point), "<br/>",
      #               "<b> mpg: </b>", point$mpg, "<br/>",
      #               "<b> hp: </b>", point$hp, "<br/>",
      #               "<b> Distance from left: </b>", left_px, "<b>, from top: </b>", top_px)))
    )
  })
  
  
  
  #changing the content of the filter menu by selection
})
