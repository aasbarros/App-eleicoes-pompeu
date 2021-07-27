#Projeto realizado pelo aluno Andre Alves de Souza Barros na disciplina de VED 
#Bibliotecas
library(shiny)
library(dplyr)
library(leaflet)
library(plotly)

#Leitura dos dados
dados <- read.csv("Resultado.csv", sep=";")
dados$NM_VOTAVEL <- iconv(dados$NM_VOTAVEL, to = "ASCII//TRANSLIT")
dados_esc <- read.csv("locais_de_votacao.txt")
df <- data.frame(dados)
df_esc <- data.frame(dados_esc)
df <- merge(df, df_esc, by = "NR_LOCAL_VOTACAO")
df <- df %>% group_by(DS_CARGO, NM_VOTAVEL, NR_LOCAL_VOTACAO, NOME_LOCAL, LAT, LONG) %>% 
    summarise(QT_VOTOS = sum(QT_VOTOS))
df <- df %>% group_by(NR_LOCAL_VOTACAO, DS_CARGO) %>% mutate(PORC_VOTOS_LOCAL=(QT_VOTOS/sum(QT_VOTOS)))
df <- df %>% group_by(DS_CARGO, NM_VOTAVEL) %>% mutate(QT_VOTOS_TOTAL=(sum(QT_VOTOS)))
df <- df %>% group_by(DS_CARGO) %>% mutate(PORC_VOTOS_TOTAL=paste0(QT_VOTOS_TOTAL/sum(QT_VOTOS)))

library(shiny)

#App
ui <- fluidPage(

    #Título do app
    headerPanel(title = "Análise geográfica das Eleições Municipais de 2020 em Pompéu-MG"),
    #Caixa de seleção
    sidebarLayout(
        sidebarPanel(
            selectInput("cargo", "Selecione o cargo: ", choices = df$DS_CARGO, selected = "PREFEITO "),
            selectInput("candidato","Selecione o candidato/partido: ", choices = NULL, selected = "OZEAS DA SILVA CAMPOS")),
        mainPanel(tabsetPanel(
            tabPanel("Gráfico", plotlyOutput("grafico")),
            tabPanel("Mapa", leafletOutput("mapa"))
            
        ))

    
    

))

server <- function(input, output, session) {
    #Mecânica da segunda parte da caixa de seleção
    observe({
        print(input$cargo)
        x <- df$NM_VOTAVEL[df$DS_CARGO == input$cargo]
        updateSelectInput(session,"candidato","Selecione o candidato/partido: ", choices = unique(x), selected = "OZEAS DA SILVA CAMPOS"
        )})
    
    #Mapa
    observe({
        colorData <- round(100*df$PORC_VOTOS_LOCAL[df$NM_VOTAVEL == input$candidato & df$DS_CARGO == input$cargo], digits = 2)
        pal <- colorBin("Greens", colorData)
        output$mapa <- renderLeaflet(
            df  %>% 
                leaflet() %>% 
                addTiles(attribution = "Dados extraídos do site do TSE") %>% 
                addCircleMarkers(~df$LONG[df$NM_VOTAVEL == input$candidato & df$DS_CARGO == input$cargo], 
                                 ~df$LAT[df$NM_VOTAVEL == input$candidato & df$DS_CARGO == input$cargo], 
                                 color = pal(colorData), 
                                 radius = ~2*log(df$QT_VOTOS[df$NM_VOTAVEL == input$candidato & df$DS_CARGO == input$cargo], 2),
                                 popup = ~ paste0(
                                     sep = " ",
                                     "<b>", NOME_LOCAL[df$NM_VOTAVEL == input$candidato & df$DS_CARGO == input$cargo], "<b><br>",
                                     "<b>Votos no local: </b>", QT_VOTOS[df$NM_VOTAVEL == input$candidato & df$DS_CARGO == input$cargo], "<br>",
                                     "<b>Porcentagem de votos no local: </b>", scales::percent(PORC_VOTOS_LOCAL[df$NM_VOTAVEL == input$candidato & df$DS_CARGO == input$cargo], 0.01, decimal.mark = ",")
                                 ),
                                 label = ~NOME_LOCAL[df$NM_VOTAVEL == input$candidato & df$DS_CARGO == input$cargo]) %>%
                addLegend("bottomright",
                          title = "% de votos no local",
                          pal = pal,
                          values = ~colorData,
                          opacity = 0.8, layerId = "colorLegend"
                )
    )})

    observe({
        output$grafico <- renderPlotly({

        df %>%
            plot_ly() %>% add_bars(x = ~NOME_LOCAL[df$NM_VOTAVEL == input$candidato & df$DS_CARGO == input$cargo], y =~round(100*PORC_VOTOS_LOCAL[df$NM_VOTAVEL == input$candidato & df$DS_CARGO == input$cargo], digits = 2), name = "Quantidade de votos por local") %>%
            layout(
                title = paste0(input$candidato),
                xaxis = list(title = "Local de votação"),
                yaxis = list(title = "Porcentagem"))
        })
    })    
    
}
 

shinyApp(ui = ui, server = server)

