library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(purrr)
library(spectral)

amplitudMinima <- 0
amplitudMaxima <- 5

periodoMinimo <- 1
periodoMaximo <- 50

fourierMaxY <- 500

tauFraccionalMinimo <- 0.1
tauFraccionalMaximo <- 1

# Bottom - Left - Top - Right
margenesDeGraficas <- c(4, 4, 4, 1)

cantidadDePuntos <- 100
ejeX <- 0:(cantidadDePuntos-1)

# UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Comunicaciones 2018"),
                sidebarLayout(
                  sidebarPanel(
              
                    numericInput("amplitud", "Amplitud:", 5, min = amplitudMinima, max = amplitudMaxima, step = 1),
                    
                    numericInput("periodo", "Periodo:", 20, min = periodoMinimo, max = periodoMaximo),
                    
                    sliderInput("tauFraccional", label = "Tau:", 
                                min = tauFraccionalMinimo, max = tauFraccionalMaximo, value = 0.5, step = 0.1),
                    

                    tags$p("Ancho de banda:"),
                    verbatimTextOutput("anchoDeBanda")
                    
                  ),
                  
                  mainPanel(
                    plotOutput(outputId = "trenDePulso", height = "400px"),
                    plotOutput(outputId = "fourier", height = "400px"),
                    plotOutput(outputId = "lineasEspectrales", height = "400px")
                    
                  )
                )
)

# Server function
server <- function(input, output) {
  
  aplanar <- function(valor, amplitud, periodo, fraccionDeTauSobrePeriodo){
    tau <- fraccionDeTauSobrePeriodo * periodo
    if(valor %% periodo < tau ) amplitud else 0
  }
  
  trenDePulso <- reactive({
    req(input$amplitud)
    req(input$periodo)
    req(input$tauFraccional)
    
    validate(
      need( between(input$amplitud, amplitudMinima, amplitudMaxima), "Error: valor de amplitud incorrecto."),
      need( between(input$periodo, periodoMinimo, periodoMaximo), "Error: valor de periodo incorrecto."),
      need( between(input$tauFraccional, tauFraccionalMinimo, tauFraccionalMaximo), "Error: valor de tau incorrecto.")
    )
    
    map(ejeX, aplanar, input$amplitud, input$periodo, input$tauFraccional)
  })
  
  output$trenDePulso <- renderPlot({
    par(mar = margenesDeGraficas)
    plot(ejeX, trenDePulso(), type = "s", main= "Tren de pulso", 
         xlab="t", ylab = "Amplitud",
         xlim=c(0, cantidadDePuntos-1), ylim=c(0, amplitudMaxima))
    
  })
  
  output$fourier <- renderPlot({
    par(mar = margenesDeGraficas)
    trenDePulsoComoArray <- do.call("rbind", trenDePulso())
    fourier <- fft(trenDePulsoComoArray)
    plot(Mod(fourier),type="l", main = "Fourier",
         xlab="ω", ylab = "Transformada",
         ylim=c(0, fourierMaxY))
  })
  
  output$lineasEspectrales <- renderPlot({
    par(mar = margenesDeGraficas)
    tau <- input$tauFraccional * input$periodo
    y <- map(ejeX, ~ (input$amplitud * tau / input$periodo) * (sin(.x / .x)))
    plot(ejeX, y, type='l')
  })
  
  anchoDeBanda <- function(amplitud, periodo, tauFraccional) {
    tau <- tauFraccional * periodo
    1/tau
  }
  
  output$anchoDeBanda <- renderText({ anchoDeBanda(input$amplitud, input$periodo, input$tauFraccional)})
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)