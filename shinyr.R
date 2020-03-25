library(shiny)
library(ggplot2)
ui <- fluidPage(
  titlePanel("Country"),
  sidebarLayout(
    sidebarPanel(
      textInput("txtInput","Country"),
      selectInput("ctryInput", "Country",choices = gather_table$Country)
    ),
    mainPanel(
      textOutput("txtOutput"),
      textOutput("ctryOutput"),
      plotOutput("p1")
    )
  )
)

server <- shinyServer(function(input, output){
  output$txtOutput <- renderText({
    paste(input$txtInput)
  })
  
  output$ctryOutput <- renderText({
    paste(input$ctryInput)
  })
  
  
  dat <- reactive({dyn_input <- gather_table %>% filter(Country == input$ctryInput) %>%group_by(Date,Status) %>% summarise(Affected = sum(Count))
  })
  
  output$p1  <- renderPlot({
    ggplot(dat(), aes(x=Date, y=Affected)) +
      geom_line(aes(color = Status),na.rm = T) + 
      (scale_x_date(breaks=date_breaks("7 days"),
                    labels=date_format("%b %d")))+
      geom_point(aes(color = Status),na.rm = T) +
      theme_ft_rc()+
      scale_color_manual(values=c("yellow", "red", "green"))+
      xlab(input$ctryInput)
    
  })
  
})

p <- ggplot(dataset(), aes(x=Date, y=Affected)) +
  geom_line(aes(color = Status),na.rm = T) + 
  (scale_x_date(breaks=date_breaks("7 days"),
                labels=date_format("%b %d")))+
  geom_point(aes(color = Status),na.rm = T) +
  theme_ft_rc()+
  scale_color_manual(values=c("yellow", "red", "green"))+
  xlab(input$ctryInput)


shinyApp(ui = ui, server = server)
