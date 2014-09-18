# server.R
library(shiny)
library(ggplot2)

# Loading data file 
SubPerfData <- readRDS(file="SubPerfData.Rda")
##Exploring dataset variables
subdata <- with(SubPerfData, data.frame(NumFunc, ReceitaB, SP,SL,SC,ST,SM))
cor.matrix <- round(cor(subdata, use = "pairwise.complete.obs"), digits = 2) 

shinyServer(
  function(input, output) {
    
    
    output$tabela <- renderTable({ 
            cor.matrix
            })
    
    values <- reactiveValues(shouldShow = FALSE)
    
    observe({
      if (input$goButton == 0) return()
      values$shouldShow = TRUE
    })
    
    
    output$text1 <- renderText({ 
      if (values$shouldShow == TRUE) {
            paste("You have selected fit<-lm(", input$y, " ~ ",input$x, ",data=SubPerfData")
      }
     })

    output$resum <- renderText({ 
      if (values$shouldShow == TRUE) {
          fitf<-lm(SubPerfData[,input$y] ~ SubPerfData[,input$x], data=SubPerfData)
          paste("Results: (Intercept)", fitf$coef[1], "(slope): (",  input$x, ")", fitf$coef[2])
      }
    })

    
    
    output$plot <- renderPlot({ 
      fitf<-lm(SubPerfData[,input$y] ~ SubPerfData[,input$x], data=SubPerfData)
      
      if(input$y == "NumFunc") {nylab<-"Employees" }
      if(input$y == "ReceitaB") {nylab<-"Gross Income" }
      if(input$y == "SM") {nylab<-"KM Metrics" }
      if(input$y == "SL") {nylab<-"KM Leadership" }
      if(input$y == "ST") {nylab<-"KM Technology" }
      if(input$y == "SP") {nylab<-"KM Process" }
      if(input$y == "SC") {nylab<-"KM Culture" }
            
      if(input$x == "NumFunc") {
        nxlab<-"Employees"
        if(input$y == "NumFunc" | input$y == "ReceitaB") {xcor=1; ycor=5.5}
        if(input$y == "SP" | input$y == "ST") {xcor=20; ycor=5.5}
        if(input$y == "SL" | input$y == "SC" | input$y == "SM") {xcor=16; ycor=5.5}
      }
      
      if(input$x == "ReceitaB") {
        nxlab<-"Gross Income"
        if(input$y == "NumFunc" | input$y == "ReceitaB") {xcor=1; ycor=5.5}
        if(input$y == "SP" | input$y == "ST") {xcor=20; ycor=5.5}
        if(input$y == "SL" | input$y == "SC" | input$y == "SM") {xcor=16; ycor=5.5}
      }
      
      if(input$x == "SP") {
        nxlab<-"KM Process"
        if(input$y == "NumFunc" | input$y == "ReceitaB") {xcor=1; ycor=20}
        if(input$y == "SP") {xcor=5; ycor=20}
        if(input$y == "SL" ) {xcor=4; ycor=20}
        if(input$y == "SC" ) {xcor=10; ycor=20}
        if(input$y == "ST" ) {xcor=10; ycor=22}
        if(input$y == "SM" ) {xcor=5; ycor=20}
      }
      
      if(input$x == "SL") {
        nxlab<-"KM Leadership"
        if(input$y == "NumFunc" | input$y == "ReceitaB") {xcor=2; ycor=16}
        if(input$y == "SP") {xcor=5; ycor=16}
        if(input$y == "SL" ) {xcor=4; ycor=16}
        if(input$y == "SC" ) {xcor=10; ycor=16}
        if(input$y == "ST" ) {xcor=10; ycor=14}
        if(input$y == "SM" ) {xcor=5; ycor=16}
      }
      
      if(input$x == "SC") {
        nxlab<-"KM Culture"
        if(input$y == "NumFunc" | input$y == "ReceitaB") {xcor=2; ycor=16}
        if(input$y == "SP") {xcor=20; ycor=15}
        if(input$y == "SL" ) {xcor=15; ycor=15}
        if(input$y == "SC" ) {xcor=10; ycor=16}
        if(input$y == "ST" ) {xcor=25; ycor=12}
        if(input$y == "SM" ) {xcor=16; ycor=16}
      }
      
      if(input$x == "ST") {
        nxlab<-"KM Technology" 
        if(input$y == "NumFunc" | input$y == "ReceitaB") {xcor=2; ycor=28}
        if(input$y == "SP") {xcor=20; ycor=20}
        if(input$y == "SL" ) {xcor=15; ycor=15}
        if(input$y == "SC" ) {xcor=10; ycor=26}
        if(input$y == "ST" ) {xcor=10; ycor=25}
        if(input$y == "SM" ) {xcor=16; ycor=20}
      }
      
      if(input$x == "SM") {
        nxlab<-"KM Metrics" 
        if(input$y == "NumFunc" | input$y == "ReceitaB") {xcor=2; ycor=18}
        if(input$y == "SP") {xcor=5; ycor=18}
        if(input$y == "SL" ) {xcor=4; ycor=17}
        if(input$y == "SC" ) {xcor=5; ycor=20}
        if(input$y == "ST" ) {xcor=12; ycor=18}
        if(input$y == "SM" ) {xcor=5; ycor=17}
      }
       
      if (values$shouldShow == TRUE) {
       plot(SubPerfData[,input$y], SubPerfData[,input$x], xlab = nxlab, 
              ylab =nylab, bg= ifelse(SubPerfData$FazGC == 1, "green", "red"), 
              col="black", cex=1.1, pch=21, frame=FALSE)
       legend(xcor,ycor, c("KM", "Not KM"), 
                col = c("green", "red"), lty=c(1,1), lwd=c(2.5,2.5), 
                text.col = "black",bg = "gray90")
       abline(fitf, lwd=2)
      }
      actionButton("goButton", label="Go!", value=0)  
    })
    
    output$corresp <- renderTable({ 
      xcorr<- round(cor(subdata,subdata$SL), digits = 2)
      #xcorr
    })
    
    output$model1 <- renderText({ 
      fit<-lm(SL ~ SM, data=subdata)
      paste("Results: (Intercept):", fit$coef[1], "(slope)", fit$coef[2])
    }) 
    
  }
)