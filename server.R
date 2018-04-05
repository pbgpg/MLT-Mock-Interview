# Get the data.
myurl <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSV4O64FOQthFtMQGxa9Z6bAjQ-gGG-a26PmiSVVrAKiL5O0aY-z37rwwMejueycckn8U3x9DQfG-If/pub?gid=915553877&single=true&output=csv"
x <- read.csv(url(myurl), header=TRUE)

# Set up plotting parameters.
thismin <- 1
thismax <- 5
thisstep <- 0.1
dotcex <- 1.5

# Labels.
titles <- c("Research & Preparation", "Technical Literacy",
            "Analytical Skills", "Professional Presence",
            "Interpersonal Skills", "Communication Skills",
            "Leadership Potential", "Cultural Fit")
vars <- names(x)[-which(names(x) %in% c("Timestamp","IntName", "IntComp","industry","name","Invite2nd","comments","password"))]
industries <- sort(unique(x$industry))

# Compute score averages by industry.
indMeans <- aggregate(x[,vars], list(x$industry), mean, na.rm=T)
indMeans[,-1] <- round(indMeans[,-1],digits=1)


# Define server logic required to make a plot
shinyServer(
  function(input, output, session){
    output$myid <- renderText(paste("<b>Showing scores for: </b>", x$name[which(x$password %in% input$pass)][1]))
    output$intNum <- renderText(as.integer(table(x$password)[names(table(x$password))==input$pass]))
    output$interviewNum <- renderText(paste("<b>Number of interviews: </b>", as.integer(table(x$password)[names(table(x$password))==input$pass])))
   
    output$invite1 <- renderText(paste("<b>Would you invite this Fellow to a second round interview/give them a job offer: </b>",x$Invite2nd[which(x$password %in% input$pass)][1]))
    output$industry1 <- renderText(paste("<b>Industry: </b>",x$industry[which(x$password %in% input$pass)][1]))
    output$comments1 <- renderText(paste("<b>Interviewer Comments: </b>",x$comments[which(x$password %in% input$pass)][1]))
    output$invite2 <- renderText(paste("<b>Would you invite this Fellow to a second round interview/give them a job offer: </b>",x$Invite2nd[which(x$password %in% input$pass)][2]))
    output$industry2 <- renderText(paste("<b>Industry: </b>",x$industry[which(x$password %in% input$pass)][2]))
    output$comments2 <- renderText(paste("<b>Interviewer Comments: </b>",x$comments[which(x$password %in% input$pass)][2]))
    output$invite3 <- renderText(paste("<b>Would you invite this Fellow to a second round interview/give them a job offer: </b>",x$Invite2nd[which(x$password %in% input$pass)][3]))
    output$industry3 <- renderText(paste("<b>Industry: </b>",x$industry[which(x$password %in% input$pass)][3]))
    output$comments3 <- renderText(paste("<b>Interviewer Comments: </b>",x$comments[which(x$password %in% input$pass)][3]))
    output$invite4 <- renderText(paste("<b>Would you invite this Fellow to a second round interview/give them a job offer: </b>",x$Invite2nd[which(x$password %in% input$pass)][4]))
    output$industry4 <- renderText(paste("<b>Industry: </b>",x$industry[which(x$password %in% input$pass)][4]))
    output$comments4 <- renderText(paste("<b>Interviewer Comments: </b>",x$comments[which(x$password %in% input$pass)][4]))
    
    
    output$barPlot <-renderPlot({
      
      # Select data based on user input.
      if (input$dataset=="All") {
        dataset <- x
      } else {
        dataset <- x[which(x$industry==input$dataset),]
      }
      
      # Reshape data
      data2<-melt(dataset,id.vars=c("IntName","name","password","IntNum","industry"),measure.vars=vars)
      
      # Compute person-level means now that an industry has been selected.
      means <- melt(apply(x[,vars], 2, mean, na.rm=T))
      means <-cbind(rownames(means),means)
      colnames(means)<-c("variable","value")
      
      #Retrieve individual scores
      data3<-data2[which(data2$password==input$pass),]
      
      #Outliers
      if("Outliers" %in% input$features){
        thisout<-"a"
      }
      else{
        thisout<-""
      }
      
      #ggplot
      p<-ggplot(data2,aes(x=variable,y=value)) + 
      stat_boxplot(geom="errorbar",width=0.5) +
      geom_boxplot(outlier.shape=thisout,outlier.size=2) +
      scale_x_discrete(limits=c('culturalFit','leadershipPotential','communication','interpersonal','professionalPresence',
                                'analyticalSkills','technicalLiteracy','research_preparation'),labels=rev(titles))+
      geom_point(data=data3,aes(color=factor(IntNum)),
                 position=position_dodge(width=-0.5),
                 size=3) +
      theme(axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.y=element_text(size=12,face="bold"),
            legend.position='top')+
      labs(color="Interview Number")+
      coord_flip()
      
      h<-element_blank()
      if ("Averages" %in% input$features) {
        h<-geom_point(data=melt(means),size=4)
      }
      
      p+h
    })
})
      