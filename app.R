library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  titlePanel("Probability Distribution Calculator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("distribution", "Which distribution do you want to plot?", 
                  c("Binomial", "Hypergeometric", "Poisson"), selected = "Binomial"),
      conditionalPanel(
        condition = "input.distribution == 'Binomial'",
        sliderInput("prob", "Enter Probability p:", .5, min = 0, max = 1),
        numericInput("n", "Enter number of trials n:", 10, min = 1, max = 500),
        checkboxInput("plot_all", "Plot all probabilities", value = F)
      ),
      conditionalPanel(
        condition = "input.distribution == 'Hypergeometric'",
        numericInput("Nhg", "Enter Population size:", 50, min = 1, max = 500),
        numericInput("n_hg", "Enter sample size:", 10, min = 1, max = 500),
        numericInput("mhg", "Enter total occurences with feature in population:", 5, min = 1, max = 500),
        checkboxInput("plot_all_hg", "Plot all probabilities", value = T)
      ),
      conditionalPanel(
        condition = "input.distribution == 'Poisson'",
        numericInput("lambda", "Enter rate lambda:", 1, min = .01, max = 100),
        numericInput("k", "Enter k number of occurences:", 10, min = 1, max = 500),
        checkboxInput("plot_all_p", "Plot all probabilities", value = F)
      )
      
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Plot', plotly::plotlyOutput('binom_plot')),
        tabPanel('Table', tableOutput('binom_table'))
      )
    )
    
  )
)

server <- function(input, output, session ){
  q <- reactive({
    1-input$prob
  })
  
  
  probability_vector <- reactive({
    probs <- c()
    p_comp <- q()
    if(input$distribution == 'Binomial'){
      validate(
        need(input$prob >= 0 & input$n >= 0, "p and/or n must be greater than or equal 0!")
      )
      for(i in seq(0,input$n)){
        probs<- c(probs, choose(input$n,i) * (input$prob)^i * p_comp^((input$n)-i))
      }
      
    }else if(input$distribution == 'Hypergeometric'){
      validate(
        need(input$n_hg <= input$Nhg & input$mhg <= input$Nhg, "Sample size and/or total successes in population must be less than or equal to Population size!")
      )
      for(i in seq(0, input$n_hg)){
        probs <- c(probs, choose(input$mhg, i) * choose(input$Nhg - input$mhg, input$n_hg-i)/choose(input$Nhg,input$n_hg))
      }
      
    }else if(input$distribution == 'Poisson'){
      #need validate clause
      validate(
        need(input$lambda >= 0 & input$k >= 0, "Rate and/or k must be greater or equal than 0!")
      )
      for(i in seq(0,input$k)){
        probs = c(probs, (input$lambda^i * exp(-1*input$lambda))/factorial(i))
      }
    }
    return(probs)
  })
  
  df <- reactive({
    if(input$distribution == 'Binomial'){
      if(input$n<=12){
        prob_df = data.frame(Successes = as.factor(seq(0,input$n)), Probability = probability_vector())
      }else{
        prob_df = data.frame(Successes = seq(0,input$n), Probability = probability_vector())
      }
    }else if(input$distribution == 'Hypergeometric'){
      if(input$n_hg<=12){
        prob_df = data.frame(Successes = as.factor(seq(0,input$n_hg)), Probability = probability_vector())
      }else{ 
        prob_df = data.frame(Successes = seq(0,input$n_hg), Probability = probability_vector())
      }
      
    }else if(input$distribution == 'Poisson'){
      if(input$k<=12){
        prob_df = data.frame(Successes = as.factor(seq(0,input$k)), Probability = probability_vector())
      }else{ 
        prob_df = data.frame(Successes = seq(0,input$k), Probability = probability_vector())
      }
    }
    
    
  })
  
  plot_distribution <- function(plot_all){
    if(input$distribution== 'Binomial'){
      if(input$plot_all==F){
        ggplot(df()%>% filter(Probability>.00001), aes(x = Successes, y = Probability)) + geom_segment(aes(x= Successes, xend = Successes, y = 0, yend = Probability))
      }else{
        ggplot(df(), aes(x = Successes, y = Probability)) + geom_segment(aes(x= Successes, xend = Successes, y = 0, yend = Probability))
      }
    }else if(input$distribution== 'Hypergeometric'){
      if(input$plot_all_hg==F){
        ggplot(df()%>% filter(Probability>.00001), aes(x = Successes, y = Probability)) + geom_segment(aes(x= Successes, xend = Successes, y = 0, yend = Probability))
      }else{
        ggplot(df(), aes(x = Successes, y = Probability)) + geom_segment(aes(x= Successes, xend = Successes, y = 0, yend = Probability))
      }
      
    }else if(input$distribution == 'Poisson'){
      if(input$plot_all_p==F){
        ggplot(df()%>% filter(Probability>.00001), aes(x = Successes, y = Probability)) + geom_segment(aes(x= Successes, xend = Successes, y = 0, yend = Probability))
      }else{
        ggplot(df(), aes(x = Successes, y = Probability)) + geom_segment(aes(x= Successes, xend = Successes, y = 0, yend = Probability))
      }
    }
    
  }
  table_distribution <- function(plot_all){
    if(input$distribution == 'Binomial'){
      if(input$plot_all==F){
        df() %>% filter(Probability>.00001)
      }else{
        df()
      }
    }else if(input$distribution == 'Hypergeometric'){
      if(input$plot_all_hg==F){
        df() %>% filter(Probability>.00001)
      }else{
        df()
      }
    }else if(input$distribution == 'Poisson'){
      if(input$plot_all_p==F){
        df() %>% filter(Probability>.00001)
      }else{
        df()
      }
    }
  }
  
  
  output$binom_plot <- plotly::renderPlotly({
    plot_distribution(input$plot_all)
  })
  
  output$binom_table <- renderTable({
    table_distribution(input$plot_all)
  })
}


shinyApp(ui=ui, server = server)


