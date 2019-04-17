#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
  
library(shiny)
library(ggplot2)


future_value <- function(amount, rate, years){
  return(amount*(1+rate)^years)
}
annuity <- function(contrib, rate, years){
  return( contrib*(((1+rate)^years -1)/rate) )
}
growing_annuity <- function(contrib, rate, growth, years){
  return( contrib*(((1+rate)^years-(1+growth)^years)/(rate-growth)))
  
} 

ui <- fluidPage(
   
   # Application title
   titlePanel("Three types of Saving models simulator"),
   
   fluidRow(width = 12,
     column(width = 4,
            sliderInput("init","Initial Amount", min =0 ,max = 100000,value = 1000,step = 500),
            sliderInput("anu_con","Annual Contribution", min =0 ,max = 50000,value = 2000,step = 500)
   ),
     column(width = 4,
            sliderInput("re_rate","Return Rate(in %)", min =0 ,max = 20,value = 5,step = 0.1),
            sliderInput("g_rate","Growth Rate(in %)", min =0 ,max = 20,value = 2,step = 0.1)
   ),
     column(width = 4,
            sliderInput("yr","Years", min =0 ,max = 50,value = 20,step = 1),
            selectInput("facet","Facet?", choices = list("No"= FALSE ,"Yes"= TRUE), selected = FALSE)
  )
   
)  ,
      
      # Show a plot of the generated distribution
      mainPanel(width = 12,
         titlePanel("Timeline"),
         plotOutput("timeline"),
         titlePanel("Balances"),
         verbatimTextOutput("balance")
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
   output$timeline <- renderPlot({
      #generate the table for timeline
 
     
     modalities <- data.frame(year = 0:input$yr,
                              no_contrib = rep(NA, (input$yr+1)),
                              fixed_contrib = rep(NA, (input$yr+1)),
                              growing_contrib = rep(NA, (input$yr+1)))
     for (i in 0:input$yr){
       modalities[i+1,2] <- future_value(amount = input$init, rate = (input$re_rate)*0.01, years = i)
       modalities[i+1,3] <- future_value(amount = input$init, rate = (input$re_rate)*0.01, years = i) + annuity(contrib = input$anu_con, rate = (input$re_rate)*0.01, years =i)
       modalities[i+1,4] <- future_value(amount = input$init, rate = (input$re_rate)*0.01, years = i) + growing_annuity(contrib = input$anu_con, rate = (input$re_rate)*0.01, growth = (input$g_rate)*0.01, years = i)
     }
     
     data <- data.frame(year = rep(0:input$yr,3), 
                        modes = rep(c("No Contribution","Fixed Contribution ($2,000)","Growing Contribution ($2,000 at 4%)"),each=input$yr+1),
                        balance = rep(NA, 3*((input$yr)+1)))
     data[1:(input$yr+1),3] <- modalities[,2]
     
     data[(input$yr+2):(2*input$yr+2),3] <- modalities[,3]
     data[(2*input$yr+3):(3*input$yr+3),3] <- modalities[,4]
     data$type_f <- factor(data$modes,levels = c("No Contribution","Fixed Contribution ($2,000)","Growing Contribution ($2,000 at 4%)"))
     data$colors <- rep(c("#FF0000","#228B22","#000080"),each = input$yr+1)
     if (input$facet){
       ggplot(data = data, aes(x = year,y=balance,group=modes,col=colors)) + geom_line()+facet_grid(~type_f)+ ggtitle("Three modes of investing") + geom_point(size=2)+
         geom_area(fill=data$colors,alpha=0.2)+ 
         scale_color_manual(name = "modes",labels = c("growing_contrib","fixed_contrib","no_contrib"), values = c("#000080","#228B22","#FF0000"))
     }else{
       ggplot(data = data, aes(x = year,y=balance,group=modes,col=colors)) + geom_line()+ ggtitle("Three modes of investing")+ geom_point(size=2)+
         scale_color_manual(name = "modes",labels = c("growing_contrib","fixed_contrib","no_contrib"), values = c("#000080","#228B22","#FF0000")) +theme_bw()
     }
     
     
     
     
     
   })
   output$balance <- renderPrint({
    
   modalities <- data.frame(year = 0:input$yr,
                            no_contrib = rep(NA, (input$yr+1)),
                            fixed_contrib = rep(NA, (input$yr+1)),
                            growing_contrib = rep(NA, (input$yr+1)))
   for (i in 0:input$yr){
     modalities[i+1,2] <- future_value(amount = input$init, rate = (input$re_rate)*0.01, years = i)
     modalities[i+1,3] <- future_value(amount = input$init, rate = (input$re_rate)*0.01, years = i) + annuity(contrib = input$anu_con, rate = (input$re_rate)*0.01, years =i)
     modalities[i+1,4] <- future_value(amount = input$init, rate = (input$re_rate)*0.01, years = i) + growing_annuity(contrib = input$anu_con, rate = (input$re_rate)*0.01, growth = (input$g_rate)*0.01, years = i)
   }
   
   modalities
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

