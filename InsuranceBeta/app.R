
library(shiny)
library(rpart)
library(tidyverse)
library(dplyr)
library(scales)
library(ggplot2)

datains <- read.csv("Data/datalog.csv")

fit <- rpart(cost ~ DANO + COMUNIDAD_AUTONOMA,
             method="class", data=datains)

ui <- shinyUI(fluidPage(
  
  br(),
  
  h1("Insurance Claim Prediction", align = "center"),
  br(),
  h5("Please Select the Injury Type, Followed by The Location", align = "center"),
  
  br(),
  br(),
  
  fluidRow(align = "center", 
           column(5, selectInput('Injury', 'Injury Type', choices = as.factor(unique(datains$DANO)), selected = "Digestivo y boca")
           ),
           column(7, selectInput('Location', 'Location', ""))
  ),
  
  hr(),
  
  fluidRow(align = "center", 
           column(5, strong("Predicted Cost:"), textOutput("costs")
           ),
           column(7, strong("Probability:"), textOutput("Prob"))
  ),
  hr(),
  
  fluidRow(align = "center", 
           column(5, strong("AverageCost:"), textOutput("Average")
           ),
           column(7, strong("+/-:"), textOutput("Uncertain"))
  ),
  hr(),
  
  fluidRow(align = "center", 
           column(12, strong("Outlier:"), textOutput("Outlier"))
  ),
  hr(),
  
  h5("Where's the Average Cost?", align = "center"),
  br(),
  p("The dotted line represents the average cost within the known range of costs 
     based on historcal data.The shaded area represents the uncertainty that 
    surrounds the average cost based on injury type and location"),
  
  plotOutput("histPlot"),
  
  
  hr(),
  
  fluidRow(align = "center", 
           column(12, img(src='BDlogo.png', align = "center")))
  )
)

# Define server logic 
server <- shinyServer(function(input, output,session) {
  
  outVar <- reactive({
    newdatains <- datains %>% filter(DANO == input$Injury & IMPORTE != 0)
    unique(newdatains$COMUNIDAD_AUTONOMA)
  })
  observe({
    updateSelectInput(session, "Location", choices = outVar())
  })
  
  modelpred1 <- reactive({
    Injurycosts <- input$Injury
    Locationcosts <- input$Location
    as.character(predict(fit, newdata = data.frame(DANO = Injurycosts, COMUNIDAD_AUTONOMA = Locationcosts), 
                         type = "class"))
  })
  
  modelpred2 <- reactive({
    Injurycosts <- input$Injury
    Locationcosts <- input$Location
    percent(max(predict(fit, newdata = data.frame(DANO = Injurycosts, COMUNIDAD_AUTONOMA = Locationcosts))))
  })
  
  importecos <- reactive({
    sapply(subset(datains, DANO == input$Injury & COMUNIDAD_AUTONOMA == input$Location, 
                  select = c(IMPORTE)), mean)
  })
  
  importeunc <- reactive({
    importelen <- subset(datains, DANO == input$Injury & COMUNIDAD_AUTONOMA == input$Location, 
                         select = c(IMPORTE))
    sd <- sapply(subset(datains, DANO == input$Injury & COMUNIDAD_AUTONOMA == input$Location, 
                        select = c(IMPORTE)), sd)
    elen <- sd/sqrt(nrow(importelen))
    ifelse(is.na(elen) == TRUE, "No Hay Suficientes Puntos de Datos", elen)
  })
  
  importeout <- reactive({
    importelen2 <- subset(datains, DANO == input$Injury & COMUNIDAD_AUTONOMA == input$Location, 
                          select = c(IMPORTE))
    importemean <- sapply(subset(datains, DANO == input$Injury & COMUNIDAD_AUTONOMA == input$Location, 
                                 select = c(IMPORTE)), mean)
    importebox <- boxplot(importelen2, plot=FALSE)$out #outside 1.5 times the interquartile range above the upper quartile and below the lower quartile
    ifelse(importemean %in% importebox, "Yes", "No")
  })
  
  output$costs <- renderText({(modelpred1())
  })
  
  output$Prob <- renderText({(modelpred2())
  })
  
  output$Average <- renderText({importecos() 
  })
  
  output$Uncertain <- renderText({importeunc() 
  })
  
  output$Outlier <- renderText({as.character(importeout()) 
  })
  
  output$histPlot <- renderPlot({
    plotcost <- as.numeric(importecos())
    plotunc <- as.numeric(importeunc())
    
    importeplus <- plotcost + plotunc
    importeminus <- plotcost - plotunc
    
    density_plot <- ggplot(datains, aes(log)) +
      geom_density(binwidth = 0.5) +
      #xlim(0,6) + 
      scale_x_continuous(breaks = 0:6,
                         labels = c("$1", "$10", "$100", "$1,000", "$10,000", "$100,000", "$1,000,000")) +
      xlab("") +
      ylab("") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_blank(), axis.text.x = element_text(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) 
    
    dpb <- ggplot_build(density_plot)
    
    x1 <- min(which(dpb$data[[1]]$x >= log10(importeplus)))
    x2 <- max(which(dpb$data[[1]]$x <= log10(importeminus)))
    
    density_plot + geom_area(data=data.frame(x=dpb$data[[1]]$x[x1:x2],
                                             y=dpb$data[[1]]$y[x1:x2]),
                             aes(x=x, y=y), fill = "light blue") + geom_vline(aes(xintercept=log10(plotcost)),
                                                                              color="black", linetype="dashed", size=0.5)
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

