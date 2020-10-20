library(shiny)
library(ggplot2)
library(magrittr)

sberry <- read.csv("berries.csv")
sberry1<- read.csv("sberry_cleaned.csv")
sberry1$Value %<>% as.numeric() 


###ui part
ui <- fluidPage(
  titlePanel("Strawberry Shiny App"),
  
  tabsetPanel(
    tabPanel("Data Display",
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("Data",
                             label ="Choose Data:",
                             
                             choices = c("oringinal data",
                                         "cleaned data"))
                 
                 
               ),
               mainPanel(
                 
                 DT::dataTableOutput("Data")
               ) 
               
             )
    ),
    
    tabPanel("EDA",
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("Eda", 
                             label = "Plot display:",
                             choices = c("Chemical & Value",
                                         "State & Value",
                                         "Year & Value"))
                 
                 
               ),
               
               mainPanel(
                 
                 plotOutput("selectEda")
                 
               ) 
               
             )
    )
  )
  
)

###server part
server <- function(input, output){
  
  
  
  datachoose <- reactive({
    switch (input$Data,
            "oringinal data" = sberry,
            "cleaned data" = sberry1)
  })
  
  output$Data <- DT::renderDataTable(DT::datatable({
    datachoose()
  }))
  
  
  plotinput <- reactive({
    switch(input$Eda,
           "Chemical & Value" = ggplot(sberry1) +
             geom_boxplot(mapping = aes(x = Chemical, y = Value)) +
             theme(axis.title = element_text(size = 12, face = "bold")) +
             labs(x = "Chemical Type"),
           "State & Value" = ggplot(sberry1)+ 
             geom_boxplot(aes(x=State,y=Value,fill=State))+ 
             coord_cartesian( ylim=c(0,30))+
             theme(axis.text.x = element_text(angle = 90, hjust = 1))+
             facet_wrap(.~Year,scales = "free"),
           "Year & Value" = ggplot(sberry1)+ 
             geom_boxplot(aes(x=Year,y=Value,group=Year,fill=Year))+ 
             coord_cartesian( ylim=c(0,10))+
             theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ) })
  
  output$selectEda <- renderPlot({
    plotinput()
  })
  
  
}



shinyApp(ui = ui, server = server)

















