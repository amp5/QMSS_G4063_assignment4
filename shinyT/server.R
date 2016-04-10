library(shiny)


setwd("/Users/alexandraplassaras/Desktop/Columbia_Courses/Spring_2016/QMSS_G4063/QMSS_G4063_Data_Visualization/assignment4/shinyT")
All_counts <- read.csv("ShinyApp.csv", header = TRUE, sep = ",", quote = "\"")

#View(All_counts)

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Fill in the spot we created for a plot
  output$countPlot <- renderPlot({
    
    # Render a barplot
    barplot(All_counts[,input$name], 
            main=input$name,
            ylab="Topic Count",
            xlab="Topics",
            names.arg=c ("economy", "immigration", "health", "military", "guns",
                         "china", "trade", "race", "climate", "religion"),
            col=topo.colors(10))
  })
})  

