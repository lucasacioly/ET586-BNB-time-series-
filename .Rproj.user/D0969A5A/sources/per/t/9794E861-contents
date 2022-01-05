# ui.R

# título do dashboard
header <- dashboardHeader(title = "BNB")

# barra de menu
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Métricas", tabName = "metricas", icon = icon("chart-bar")),
        menuItem("Comparativos", tabName = "comparativos", icon = icon("chart-bar"))
    )
)

# corpo para cada ítem do menu
body <- dashboardBody(
      tabItems(
        tabItem(tabName = "metricas", 
                
                fluidRow(
                  box(title = 'Seleção de parâmetro', width=12, solidHeader = TRUE, status='warning',
                      selectInput('parameter', 'parâmetro', parameters_list, multiple=FALSE),
                      uiOutput("timedate"),
                      actionButton('go', 'Submeter')
                      )
                  ),
                
                fluidRow(
                  box(title = "Informações do parâmetro", width = 12, solidHeader = TRUE,
                      DTOutput('info')
                  )
                ),
                
                fluidRow(
                  box(title = "Variação do parâmetro em função do tempo", width = 12, solidHeader = TRUE,
                      plotOutput('sh')
                  )
                ),
                
                
                fluidRow(
                  box(title = "Histograma", width = 12, solidHeader = TRUE,
                      plotOutput('hist')
                  )
                ),
                
                fluidRow(
                  box(title = "Boxplot", width = 12, solidHeader = TRUE,
                      plotOutput('box')
                  )
                ),
                
               ),
        
        tabItem(tabName = "comparativos", 
                
                fluidRow(
                  box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='warning',
                      selectInput('parameter_comp', 'parâmetros', parameters_list, multiple=TRUE),
                      uiOutput("timedate_comp"),
                      actionButton('go_comp', 'Submeter')
                  )
                ),
                
                fluidRow(
                  box(title = "Correlação", width = 12, solidHeader = TRUE,
                      plotOutput('correlation_comp')
                  )
                ),
                
                fluidRow(
                  box(title = "Variação do parâmetro em função do tempo - LINHA", width = 12, solidHeader = TRUE,
                      plotOutput('sh_comp')
                  )
                ),
                
                fluidRow(
                  box(title = "Gráfico de Barra das Médias", width = 12, solidHeader = TRUE,
                      plotOutput('bar_comp')
                  )
                ),
                
                fluidRow(
                  box(title = "Variação do parâmetro em função do tempo - SCATTERPLOT", width = 12, solidHeader = TRUE,
                      plotOutput('scp_comp')
                  )
                ),
                
                )
      )
)

ui <- dashboardPage(
      skin = 'yellow',
      header, sidebar, body)
