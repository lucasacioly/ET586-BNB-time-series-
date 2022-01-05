# server.R

server <- function(input, output) {
    ########### INPUT #####################
    select_parameter <- eventReactive(input$go, {
      
      parameter_name <- input$parameter
      twin <- input$true_date
      
      df_parameter <- master_df %>% 
        filter(Index == parameter_name) 
      ## FALTA -> FILTRAR O DF POR DATA!!
      
      df_parameter <- df_parameter %>% filter(Date >= twin[1] & Date <= twin[2])
      
      #df_parameter <- df_parameter[interval[1] <= df_parameter$Date & df_parameter$Date <= interval[2],]
      
      #df_parameter <- df_parameter %>% filter_date(df_parameter, twin)
      
      return(df_parameter)
    })
    
    # time input for isolated parameters
    output$timedate <- renderUI({
      
      parameter_name <- input$parameter
      
      df <- master_df %>% 
        filter(Index == parameter_name)
      
      min_time <- "2017-07-25"
      max_time <- "2021-12-02"
      dateRangeInput("true_date", "Período de análise",
                     end = max_time,
                     start = min_time,
                     min  = min_time,
                     max  = max_time,
                     format = "dd/mm/yy",
                     separator = " to ",
                     language='pt-BR')
    })
    
    # time input for comparatives
    
    output$timedate_comp <- renderUI({
      
      parameter_name <- input$parameter_comp
      
      df <- master_df %>% 
        filter(Index %in% parameter_name)
      
      min_time <- "2017-07-25"
      max_time <- "2021-12-02"
      
      dateRangeInput("true_date_comp", "Período de análise",
                     end = max_time,
                     start = min_time,
                     min    = min_time,
                     max    = max_time,
                     format = "dd/mm/yy",
                     separator = " - ",
                     language='pt-BR')
    })
    
    ################ OUTPUT #####################
    
    ############### PARAMETROS #################
    Info_DataTable <- eventReactive(input$go,{
      df <- select_parameter()
      
      mean <- df %>% select(Value) %>% colMeans()
      Media <- mean[[1]]
      
      Mediana <- df$Value %>% median()
      
      Moda <- (-table(df$Value) %>% sort() %>% names())[1]
      
      Max <- df$Value %>% max()
      
      Min <- df$Value %>% min()
      
      Desvio_Padrao <- df$Value %>% sd()
      
      Parameter <- input$parameter
      
      df_tb <-  data.frame(Parameter, Media, Mediana, Moda, Max, Min, Desvio_Padrao)
      
      df_tb <- as.data.frame(t(df_tb))
      
      # tb  <- as_tibble(cbind(nms = names(df_tb), t(df_tb)))
      # tb <- tb %>% 
      #     rename('Informações' = nms,
      #            'Valores' = V2)
      # 
      return(df_tb)
    })
    
    output$info <- renderDT({
      Info_DataTable() %>%
        as.data.frame() %>% 
        DT::datatable(options=list(
          language=list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
          )
        ))
    })
    
    #gráficos
    
    tag <- eventReactive(input$go,{
      
      if (input$parameter == "Volume")
        return("Volume em $")
      
      return("BNB - $")
      
    })
    
    # GRAFICO DE LINHA
    output$sh <- renderPlot({
      # All the inputs
      df <- select_parameter()
      
      aux <- df$Value %>% na.omit() %>% as.numeric()
      aux1 <- min(aux)
      aux2 <- max(aux)
      
      df$Date <- ymd(df$Date)
      a <- df %>% 
        ggplot(aes(Date, Value, group=1)) +
        geom_path() +
        ylab(tag()) +
        coord_cartesian(ylim = c(aux1, aux2)) +
        theme_bw() +
        scale_x_date(date_labels = "%Y-%m-%d")
      
      a
    })
    
    #HISTOGRAMA
    output$hist <- renderPlot({
     
       df <- select_parameter()
    
      aux <- df$Value %>% na.omit() %>% as.numeric()
      aux1 <- min(aux)
      aux2 <- max(aux)
      
      df$Date <- ymd(df$Date)
      
      a <- df %>% 
        ggplot(aes(Value)) +
        geom_histogram(binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
        xlab(tag()) +
        theme_bw() 
      
      a
      
    })
    
    #BOXPLOT
    output$box <- renderPlot({
      
      df <- select_parameter()
      
      aux <- df$Value %>% na.omit() %>% as.numeric()
      aux1 <- min(aux)
      aux2 <- max(aux)
      
      df$month <- format(df$Date, format="%b")
      df$year <- as.POSIXlt(df$Date)$year + 1900
      df$year_month <- paste(df$year, df$month)
      df$sort_order <- df$year *100 + as.POSIXlt(df$Date)$mon
      
      a <- df %>% 
        
        
        ggplot(aes(reorder(year_month, sort_order), Value)) +
        geom_boxplot(fill="#69b3a2", alpha=0.9) +
        ylab(tag()) +
        xlab("Período") +
        theme_bw() 
        
      
      a
      
    })
    
    
    
    
    
    
    
    ########### COMPARAÇÕES ###################
    
    # GRAFICO DE LINHA
    line <- eventReactive(input$go_comp, {
      name1 <- input$parameter_comp[1]
      name2 <- input$parameter_comp[2]
      
      df <- master_df[
          master_df$Index == name1 |
          master_df$Index == name2,
      ] %>%
        filter_date(input$true_date_comp)
      
      df$Date <- ymd(df$Date)
      
      aux <- df$Value %>% na.omit() %>% as.numeric()
      aux1 <- min(aux)
      aux2 <- max(aux)
      
      df %>%
        ggplot(aes(Date, Value, group = Index, colour = Index)) +
        geom_path() +
        ylab("valor em USD") +
        xlab("período") +
        coord_cartesian(ylim = c(aux1, aux2)) +
        theme_bw() +
        theme(legend.position = "bottom") +
        scale_x_date(date_labels = "%Y-%m-%d")
    })
    
    output$sh_comp <- renderPlot(line())
    
    
    #SCATTERPLOT
    scatterplot <- eventReactive(input$go_comp, {
      name1 <- input$parameter_comp[1]
      name2 <- input$parameter_comp[2]
      interval <- input$true_date_comp
      
      df <- master_df[
          master_df$Index == name1 |
          master_df$Index == name2,
      ] %>%
        filter_date(input$true_date_comp)
      
      df$Date <- ymd(df$Date)
      
      df %>%
        ggplot(aes(Date, Value, color = Index)) +
        geom_point() +
        ylab("valor em USD") +
        xlab("período") +
        theme_bw() +
        scale_x_date(date_labels = "%Y-%m-%d")
    })
    
    output$scp_comp <- renderPlot(scatterplot())
    

    
    #GRÁFICO DE BARRAS DAS MÉDIAS
    
    bar_graph <- eventReactive(input$go_comp, {
      name1 <- input$parameter_comp[1]
      name2 <- input$parameter_comp[2]
      
      twin <- input$true_date_comp
      
      df1 <- master_df %>% 
        filter(Index == name1) %>% filter(Date >= twin[1] & Date <= twin[2])
      
      df2 <- master_df %>% 
        filter(Index == name2) %>% filter(Date >= twin[1] & Date <= twin[2])
      
      mean1 <- df1 %>% select(Value) %>% colMeans()
      Media1 <- mean1[[1]]
      
      mean2 <- df2 %>% select(Value) %>% colMeans()
      Media2 <- mean2[[1]]
      
      
      df <- data.frame(
        parametros=c(name1, name2),
        medias=c(Media1, Media2)
      )
      
      df %>%
        ggplot(aes(x = parametros, y = medias, fill = parametros, group = parametros)) +
        geom_bar(stat = "identity") +
        ylab("Médias") +
        xlab("") +
        theme_bw()
    })
    
    output$bar_comp <- renderPlot(bar_graph())
    
    
    #Correlação
    
    correlation <- eventReactive(input$go_comp, {
      name1 <- input$parameter_comp[1]
      name2 <- input$parameter_comp[2]
      
      twin <- input$true_date_comp
      
      df1 <- master_df %>% 
        filter(Index == name1) %>% filter(Date >= twin[1] & Date <= twin[2])
      
      df2 <- master_df %>% 
        filter(Index == name2) %>% filter(Date >= twin[1] & Date <= twin[2])
      
      df <- data.frame(
        parametro_1=df1$Value,
        parametro_2=df2$Value
      )
      
      cor(df)
      
      corrplot(cor(df), method = "circle")
    })
    
    output$correlation_comp <- renderPlot(correlation())
    
}

    
