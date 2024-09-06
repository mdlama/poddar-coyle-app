library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Gilia Population"),
  sidebarLayout(sidebarPanel(width = 5,
                             
  fluidRow(
    column(5,numericInput(inputId = "S0",
                          label="Germination Rate",
                        value=0, 
                        min=0, max=1)),
    column(2, div()),
    column(5,numericInput(inputId = "n0",
                        label="Number of Seeds",
                        value=0, 
                        min=0, max=1))),
  fluidRow(
    column(5,numericInput(inputId = "S1",
                        label="Seedling Survival Rate",
                        value=0, 
                        min=0, max=1)),
    column(2, div()),
    column(5,numericInput(inputId = "n1",
                        label="Number of Rosettes",
                        value=0, 
                        min=0, max=1))),
  fluidRow(
    column(5,numericInput(inputId = "F",
                                label="Fecundity",
                                value=0, 
                                min=0, max=1)),    
    column(2, div()), 
    column(5,numericInput(inputId = "n2",
                                label="Number of Bolted Plants",
                                value=0, 
                                min=0, max=1))),
  fluidRow(column(3, actionButton("submit", "Submit")))),

  mainPanel(width=6,
  fluidRow(textOutput("R0")),
  fluidRow(column(3, tableOutput("L")),
           column(3, div()),
           column(3,tableOutput("N"))),
  fluidRow(plotOutput("Plot", click="plot_click"),
           shinyjs::hidden(div(id="text_div",
               tableOutput("info")))))))

server <- function(input, output) {
  out<- eventReactive(input$submit,{
    if(input$S0<0 | input$S1<0)
    {out="Error: Survival cannot be negative"}
  else if(input$S0>1 | input$S1>1)
  {out="Error: Survival cannot be >1"}
  else if (input$F <0){"Error: Fecundity cannot be negative"}
  else if(input$n0<0|input$n1<0|input$n2<0)
    {out="Error: Population size cannot be negative"}
  else{
    L <- matrix(c(0,0,input$F, input$S0,0,0, 0,input$S1,0),
                nrow=3, byrow=T)
    colnames(L) <- c("F0", "F1", "F2")
    rownames(L) <- c("F", "S0",  "S1")
    N <- matrix(c(input$n0, input$n1, input$n2))
    rownames(N) <- c("N0", "N1", "N2")
    colnames(N) <- c("N")
    N2 <- L%*%L%*%L%*%N
    R0 <- (sum(N2)/sum(N))
    N_t <- c()
    N_t[1] <- sum(N)
    N_t_matrix <- N
    for(t in 2:10){
      N_t[t] <- sum(L%*%L%*%L%*%N_t_matrix)
      N_t_matrix <- matrix(c(N_t[t], 0,0))
    }
    N_df <- data.frame(Time=1:10, Number_of_Seeds = N_t)
    out <- list(R0,L,N,N_df )
    return(out)
    }})
  
 output$R0 <- renderText({paste("R0 = ",out()[1])})
 output$L <- renderTable({out()[2]}, rownames = T,
                         caption = "Leslie Matrix")
 output$N <- renderTable({out()[3]},rownames=T , colnames=T, 
                         caption="Initial Population Size")
 output$Plot <- renderPlot({plot(as.data.frame(out()[4]),
                          ylab="Number of Seeds",type="b")})
 observeEvent(input$submit,{shinyjs::toggle("text_div")
   # output$info <- renderText({paste("Time=", 
   #                round(input$plot_hover$x, digits=2), 
   #                "\n", "Number of Seeds =",
   #                round(input$plot_hover$y, digits=2))})
   output$info <- renderTable({
     req(input$plot_click)
     nearPoints(as.data.frame(out()[4]), xvar="Time"
                , yvar="Number_of_Seeds",input$plot_click)
   })
   })
}

shinyApp(ui = ui, server=server)

