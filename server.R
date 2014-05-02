## Binomail Operating Characterstic Curves app
## Author: Dr. Aous Abdo <aabdo.und@ida.org>
## Company: Institute for Defense Analyses
## Created: April, 15, 2014

library(shiny)
library(ggplot2)
library(plyr)
library(reshape2)
library(grid)

source("./MaxFail.R")
source("./MaxX.R")

shinyServer(function(input, output){
  # Reactive data frame to store data
  data <- reactive({
    proportions <- seq(0.001, 0.2, 0.001)
    Tolerance   <- 1e-3
    Threshold   <- 0.0
    ## Read input from ui.R file
    nfails      <- c(input$fails1)
    nshots      <- c(input$shots1)
    addtest2    <- input$addtest2
    addtest3    <- input$addtest3
    addtest4    <- input$addtest4
    adjustX     <- input$adjustX
    
    if(addtest2){
      nfails[2] <- input$fails2
      nshots[2] <- input$shots2
      if(addtest3){
        nfails[3] <- input$fails3
        nshots[3] <- input$shots3  
        if(addtest4){
          nfails[4] <- input$fails4
          nshots[4] <- input$shots4
        }
      }
    }
    
    if(adjustX){
      PropMin <- input$xmin
      PropMax <- input$xmax
    }
    else{
      PropMin <- 0
      PropMax <- MaxX(nfails[1], nshots[1], max=0.001, tolerance=Tolerance, Threshold=Threshold)
      if(addtest2){
        PropMax  <- max(PropMax, MaxX(nfails[2], nshots[2], max=0.001, tolerance=Tolerance, Threshold=Threshold))
        if(addtest3){
          PropMax  <- max(PropMax, MaxX(nfails[3], nshots[3], max=0.001, tolerance=Tolerance, Threshold=Threshold))
          if(addtest4){
            PropMax  <- max(PropMax, MaxX(nfails[4], nshots[4], max=0.001, tolerance=Tolerance, Threshold=Threshold))
          }
        }
      }
    }
    
    proportions <- seq(PropMin, PropMax, 0.001)
    
    probs <- list()
    for (i in 1:length(nfails)){
      probs[[i]] <- pbinom(nfails[i], nshots[i], proportions)
    }
    
    df <- data.frame(Proportions = proportions, Prob1 = probs[[1]])
    if(addtest2){
      df$Prob2 <- probs[[2]]
      if(addtest3){
        df$Prob3 <- probs[[3]]
        if(addtest4){
          df$Prob4 <- probs[[4]]
        }
      }
    }
    return(df)
  })
  
  # Make plot
  output$OCplot<- renderPlot({makePlot()})
  
  makePlot <- function(){
    df <- data() 
    grid <- TRUE
    if(input$under){
      grid <- input$grid
    }
    if(input$under){
      axessize  <- input$axessize
      linewidth <- input$linewidth
      legtext <- input$legtext
    }
    else{
      axessize  <- 1.5
      linewidth <- 2
      legtext <- 1.2
    }
    
    layout(rbind(1,2), heights=c(7,1))  # put legend on bottom 1/8th of the chart
    
    par(mar=c(5, 6.5, 4, 2) + 0.1) 
    if(input$adjustX){
      xmin <- input$xmin
      xmax <- input$xmax
    }
    else{
      xmin <- min(df$Proportions)
      xmax <- max(df$Proportions)
    }
    ## the xaxs="i", yaxs="i" options enforce the axes limits
    if(grid){
      plot(df$Proportions, df$Prob1, 'l', col="blue", lwd=linewidth,
           xlim=c(xmin, xmax), ylim=c(0,1), 
           xlab=NA, ylab=NA, cex.axis=axessize,
           xaxs="i")
      grid(lwd=2)
      
      par(new=TRUE)
    }
    plot(df$Proportions, df$Prob1, 'l', col="blue", lwd=linewidth,
         xlim=c(xmin, xmax), ylim=c(0,1), 
         xlab=NA, ylab=NA, cex.axis=axessize,
         xaxs="i")
    
    if(input$under){
      Title     <- input$title
      xlab      <- input$xtitle
      ylab      <- input$ytitle
      title(Title, cex.main=input$titlesize,
            xlab=xlab, ylab=ylab, 
            cex.lab=input$axeslabels)
      test1name <- input$test1name
      if(input$addtest2){
        test2name <- input$test2name
        if(input$addtest3){
          test3name <- input$test3name
          if(input$addtest4){
            test4name <- input$test4name
          }
        }
      }
    }
    else{
      title(main="Binomial Operating Characteristic Curves",
            xlab="Proportion Defective", 
            ylab="Probability of Passing", 
            cex.lab=1.6, cex.main=2
      )
      test1name="Test 1"
      if(input$addtest2){
        test2name <- "Test 2"
        if(input$addtest3){
          test3name <- "Test 3"
          if(input$addtest4){
            test4name <- "Test 4"  
          }
        }
      }
    }
    
    if(input$addtest2){
      lines(df$Proportions, df$Prob2, col="red", lwd=linewidth)
      if(input$addtest3){
        lines(df$Proportions, df$Prob3, col="darkgreen", lwd=linewidth)
        if(input$addtest4){
          lines(df$Proportions, df$Prob4, col="black", lwd=linewidth)
        }
      }
    }
    
    leg1 <- sprintf("%s: %.0f failures, %.0f shots", test1name, input$fails1, input$shots1)
    
    legend_entries    <- c(leg1)
    legend_lty        <- c(1)
    legend_colors     <- c("blue")
    
    nCol <- 1
    
    if(input$addtest2){
      nCol <- 2
      leg2 <- sprintf("%s: %.0f failures, %.0f shots", test2name, input$fails2, input$shots2)
      legend_entries[2] <- leg2
      legend_colors[2]  <- "red"
      legend_lty[2]     <- 1
      if(input$addtest3){
        leg3 <- sprintf("%s: %.0f failures, %.0f shots", test3name, input$fails3, input$shots3)
        legend_entries[3] <- leg3
        legend_colors[3]  <- "darkgreen"
        legend_lty[3]     <- 1
        if(input$addtest4){
          leg4 <- sprintf("%s: %.0f failures, %.0f shots", test4name, input$fails4, input$shots4)
          legend_entries[4] <- leg4
          legend_colors[4]  <- "black"
          legend_lty[4]     <- 1
        }
      }
    }
    
    # setup for no margins on the legend
    par(mar=c(0, 0, 0, 0))
    plot.new()
    legend( "center", legend = legend_entries, 
            lty=legend_lty, lwd=rep(linewidth, length(legend_lty)), 
            col=legend_colors, ncol=nCol, bty="y", cex=legtext
    )
  }
  ############## End of plot making funciton #########################
  
  output$savePlot <- downloadHandler(
    filename = function() { paste('Binomial_OC_Curves.pdf') },
    content = function(file){
      pdf(file = file, width=11, height=8.5)
      makePlot()
      dev.off()
    }
  )
  
  output$savePNGPlot <- downloadHandler(
    filename = function() { paste('Binomial_OC_Curves.png') },
    content = function(file){
      png(file = file, width=1200, height=800, units="px")
      makePlot()
      dev.off()
    }
  )
  origdataframe <- reactive({
    df <- data()
    #     # Function to do the dcast
    #     dcastfun <- "MTBF + Test1 ~ Vert"
    #     # Add Test 2 and Test 3 to the function if corresponding tests are selected
    #     if(input$addtest2){
    #       dcastfun <- "MTBF + Test1 + Test2 ~ Vert"
    #       if(input$addtest3){dcastfun <- "MTBF + Test1 + Test2 + Test3 ~ Vert"}
    #     } 
    #     
    #     # Dcast one variable at a time
    #     df <- dcast(df, MTBF + Vert + vert ~ Test , value.var="Prob")
    #     df <- dcast(df, dcastfun, value.var="vert")
    
    #     df$vert1 <- NULL
    
    if(input$under){
      test1name <- input$test1name
      if(input$addtest2){
        test2name <- input$test2name
        if(input$addtest3){
          test3name <- input$test3name
          if(input$addtest4){
            test3name <- input$test4name          
          }
        }
      }
    }
    else{
      test1name <- "Test 1"
      if(input$addtest2){
        test2name <- "Test 2"
        if(input$addtest3){
          test3name <- "Test 3"
          if(input$addtest4){
            test4name <- "Test 4"          
          }
        }
      }
    }
    #Rename column names
    names(df)[1] <- "Proportion Defective"
    names(df)[2] <- sprintf("%s Prob.",test1name)
    
    #Rename the optional columns only if they exist
    if(length(colnames(df)>2)){
      if ("Prob2" %in% colnames(df)){
        colnames(df)[colnames(df)=="Prob2"] <- sprintf("%s Prob.",test2name)
        #         df$vert2 <- NULL
      }
      if ("Prob3" %in% colnames(df)){
        colnames(df)[colnames(df)=="Prob3"] <- sprintf("%s Prob.",test3name)
        #         df$vert3 <- NULL
        if ("Prob4" %in% colnames(df)){
          colnames(df)[colnames(df)=="Prob4"] <- sprintf("%s Prob.",test4name)
          #         df$vert4 <- NULL
        }
      }
    }
    print(df, row.names=FALSE)
  })
  
  output$Text1 <- renderText({paste("For a reliability requriment of ", input$TRV, 
                                    " hours, a probability of acceptance of ", input$PA, ", and 
                                    a confidence level of ", 100*input$CL, "%, we have:" ,sep="")})
  
  output$table <- renderTable({
    df <- data()$df
    TL <- data()$TL
    AF <- data()$AF
    # Function to do the dcast
    dcastfun <- "MTBF + Test1 ~ Vert"
    # Add Test 2 and Test 3 to the function if corresponding tests are selected
    if(input$addtest2){
      dcastfun <- "MTBF + Test1 + Test2 ~ Vert"
      if(input$addtest3){dcastfun <- "MTBF + Test1 + Test2 + Test3 ~ Vert"}
    } 
    
    # Dcast one variable at a time
    df <- dcast(df, MTBF + Vert + vert ~ Test , value.var="Prob")
    df <- dcast(df, dcastfun, value.var="vert")
    
    if(input$under){
      test1name <- input$test1name
      if(input$addtest2){
        test2name <- input$test2name
        if(input$addtest3){
          test3name <- input$test3name
        }
      }
    }
    else{
      test1name <- "Test 1"
      if(input$addtest2){
        test2name <- "Test 2"
        if(input$addtest3){
          test3name <- "Test 3"
        }
      }
    }  
    tests <- c(test1name)
    testl <- c(TL[1])
    testr <- c(AF[1])
    testv <- c(df$vert1[1])
    
    if(length(colnames(df)>2)){
      if ("Test2" %in% colnames(df)){
        tests[2] <- test2name
        testl[2] <- TL[2]
        testr[2] <- AF[2]
        testv[2] <- df$vert2[1]
      }
      if ("Test3" %in% colnames(df)){
        tests[3] <- test3name
        testl[3] <- TL[3]
        testr[3] <- AF[3]
        testv[3] <- df$vert3[1]
      }
    }
    print(testv)
    df <- data.frame("Test"=tests, "Test Length (hours)"=testl, 
                     "Allowed Failures"=testr, "Pre IOT&E Reliability Goal (hours)"
                     =testv, check.names=FALSE)
    return(df)
  })
  
  output$MTBFtable <- renderTable({
    origdataframe()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('BOC_Curves.csv', sep='') },
    content = function(file) {
      write.csv(origdataframe(), file)
    }
  )
})