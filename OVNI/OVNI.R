                      ##### OVNIs #####


## Processo de coleta de dados --------------------------------

# Pacotes para extração de dados
  install.packages('httr') 
  install.packages('XML') 
  library(httr) 
  library(XML)

# Dados: os últimos 50 anos - Web scrapping do NUFORC.org
  df_OVNI  <- data.frame()
  mes_corrente = 8
  ano_corrente = 1968
  ano_mes_corrente = (ano_corrente * 100) + mes_corrente
  while (ano_mes_corrente  <= 201808) {
    site <- paste("http://www.nuforc.org/webreports/ndxe", as.character(ano_mes_corrente), ".html")
    site <- gsub (" ", "", site)
    html2 <- GET(site)
    parsed <- suppressMessages(htmlParse(html2, asText=TRUE))
    tableNodes <- getNodeSet(parsed, "//table")
    tb <- readHTMLTable(tableNodes[[1]])
    df_OVNI <- rbind(df_OVNI,tb)
    if (mes_corrente == 12)
    {
      mes_corrente <- 1
      ano_corrente <- ano_corrente + 1
      ano_mes_corrente <- (ano_corrente * 100) + mes_corrente
    }
    else
    {
      mes_corrente <- mes_corrente + 1
      ano_mes_corrente <- ano_mes_corrente + 1 
    }
    print(ano_mes_corrente)
  }

# Salvar os dados coletados em arquivo CSV
  write.csv(rbind(df_OVNI), file = "OVNIS.csv") 
# Carregar o CSV gerado no dataframe
  df_OVNI <- read.csv("OVNIS.csv",stringsAsFactors = FALSE)


## Preparação dos dados -----------------------------------------

  # Primeira Limpeza

# Instalar pacote SQL para consultas
  install.packages('sqldf')
  require(sqldf)

# Copiando para as regiões
  df_OVNI_EUA  <- df_OVNI
  df_OVNI_MUNDO  <- df_OVNI

#Limpando os dados americanos #

# Remover Cidades em branco ou NA "Not Available"
  any(df_OVNI_EUA$City=="")
  any(is.na(df_OVNI_EUA$City))
  df_OVNI_EUA <- df_OVNI_EUA[!(df_OVNI_EUA$City == "" | is.na(df_OVNI_EUA$City)), ]

# Remover Estados em branco ou NA "Not Available"
  any(df_OVNI_EUA$State=="")
  any(is.na(df_OVNI_EUA$State))
  df_OVNI_EUA <- df_OVNI_EUA[!(df_OVNI_EUA$State == "" | is.na(df_OVNI_EUA$State)), ] 

# Remover Shapes em branco ou NA "Not Available"
  any(df_OVNI_EUA$Shape=="")
  any(is.na(df_OVNI_EUA$Shape))
  df_OVNI_EUA <- df_OVNI_EUA[!(df_OVNI_EUA$Shape == "" | is.na(df_OVNI_EUA$Shape)), ]

# Validando os estados (Valor deve ser igual a 51)
  unique(df_OVNI_EUA$State) #Número de estados = 68
  #Carregando e utilizando a tabela de Estados válidos
    df_Estados_EUA_Validos <- read.csv("states.csv",stringsAsFactors = FALSE)
    df_OVNI_EUA <- df_OVNI_EUA[(df_OVNI_EUA$State %in% df_Estados_EUA_Validos$Abbreviation),] 
    unique(df_OVNI_EUA$State) #Número de estados = 51

# Remover variáveis que não são relevantes para a análise
  df_OVNI_EUA$Posted <- NULL
  df_OVNI_EUA$Duration <- NULL
  df_OVNI_EUA$Summary <- NULL
  df_OVNI_EUA$X <- NULL

#Passando tudo para maiúsculo
  df_OVNI_EUA$Shape = toupper(df_OVNI_EUA$Shape)
  df_OVNI_EUA$City = toupper(df_OVNI_EUA$City)
  df_OVNI_EUA$State = toupper(df_OVNI_EUA$State)

# Verificando e corrigindo os tipos de Shapes
  unique(df_OVNI_EUA$Shape) #29 tipos de formas
  OVNI_EUA_por_Tipo = sqldf("select Shape, count(*) Views from df_OVNI_EUA 
                          group by Shape order by 1 desc") 
  #Corrigindo Shapes iguais escritos de maneiras diferentes
    df_OVNI_EUA$Shape[df_OVNI_EUA$Shape=="CHANGED"] <- "CHANGING"
    df_OVNI_EUA$Shape[df_OVNI_EUA$Shape=="DELTA"] <- "TRIANGLE"
    df_OVNI_EUA$Shape[df_OVNI_EUA$Shape=="EGG"] <- "OVAL"
    df_OVNI_EUA$Shape[df_OVNI_EUA$Shape=="FLARE"] <- "LIGHT"
    df_OVNI_EUA$Shape[df_OVNI_EUA$Shape=="FLASH"] <- "LIGHT"
    df_OVNI_EUA$Shape[df_OVNI_EUA$Shape=="ROUND"] <- "CIRCLE" 
    unique(df_OVNI_EUA$Shape) #Agora temos 26 tipos de formas

# Separando a data e a hora
  d <- strsplit(df_OVNI_EUA$Date...Time, ' ') 
  e <- do.call(rbind.data.frame, d)
  colnames(e) <- c("Sight_Date", "Sight_Time")
  e <- data.frame(lapply(e, as.character), stringsAsFactors=FALSE)
  df_OVNI_EUA <- cbind(df_OVNI_EUA, e)
  df_OVNI_EUA$Date...Time <- NULL

# Salvar os dados americanos em um arquivo CSV
  write.csv(rbind(df_OVNI_EUA), file = "dados_tratados/primeira_limpeza/OVNIS_EUA.csv") 


# Limpando os dados do Resto do mundo #

#Os registros americanos possuem estado, vamos excluir esses registros
  any(df_OVNI_MUNDO$State=="")
  df_OVNI_MUNDO <- df_OVNI_MUNDO[(df_OVNI_MUNDO$State == ""),]
  df_OVNI_MUNDO$State <- NULL

# Remover Cidades em branco ou NA "Not Available"
  any(df_OVNI_MUNDO$City=="")
  any(is.na(df_OVNI_MUNDO$City))
  df_OVNI_MUNDO <- df_OVNI_MUNDO[!(df_OVNI_MUNDO$City == "" | is.na(df_OVNI_MUNDO$City)), ]

# Remover Shapes em branco ou NA "Not Available"
  any(df_OVNI_MUNDO$Shape=="")
  any(is.na(df_OVNI_MUNDO$Shape))
  df_OVNI_MUNDO <- df_OVNI_MUNDO[!(df_OVNI_MUNDO$Shape == "" | is.na(df_OVNI_MUNDO$Shape)), ]

# Remover variáveis que não são relevantes para a análise
  df_OVNI_MUNDO$Posted <- NULL
  df_OVNI_MUNDO$Duration <- NULL
  df_OVNI_MUNDO$Summary <- NULL
  df_OVNI_MUNDO$X <- NULL
  
#Passando tudo para maiúsculo
  df_OVNI_MUNDO$Shape = toupper(df_OVNI_MUNDO$Shape)
  df_OVNI_MUNDO$City = toupper(df_OVNI_MUNDO$City)

# Verificando e corrigindo os tipos de Shapes
  unique(df_OVNI_MUNDO$Shape) #21 tipos de formas
  OVNI_MUNDO_por_Tipo = sqldf("select Shape, count(*) Views from df_OVNI_MUNDO 
                          group by Shape order by 1 desc") 
  #Corrigindo Shapes iguais escritos de maneiras diferentes
    df_OVNI_MUNDO$Shape[df_OVNI_MUNDO$Shape=="EGG"] <- "OVAL"
    df_OVNI_MUNDO$Shape[df_OVNI_MUNDO$Shape=="FLASH"] <- "LIGHT"
    unique(df_OVNI_MUNDO$Shape) #Agora temos 19 tipos de formas

# Separando a data e a hora
  d <- strsplit(df_OVNI_MUNDO$Date...Time, ' ') 
  e <- do.call(rbind.data.frame, d)
  colnames(e) <- c("Sight_Date", "Sight_Time")
  e <- data.frame(lapply(e, as.character), stringsAsFactors=FALSE)
  df_OVNI_MUNDO <- cbind(df_OVNI_MUNDO, e)
  df_OVNI_MUNDO$Date...Time <- NULL

# Salvar os dados mundiais em um arquivo CSV
  write.csv(rbind(df_OVNI_MUNDO), file = "dados_tratados/primeira_limpeza/OVNIS_MUNDO.csv") 
  
  # Segunda Limpeza
  
# A segunda limpeza não fiz com a linguagem R. 
# Como exigia um tratamento mais detalhado,
# peguei os dois arquivos csv gerados e os editei no Google Planilhas.
# Inclusive, gerei um arquivo só com dados brasileiros.
# A seguir, vamos importar esses csv já tratados.
  df_OVNI_MUNDO <- read.csv("dados_tratados/dados_finais/OVNIS_T_MUNDO.csv",stringsAsFactors = FALSE)
  df_OVNI_EUA <- read.csv("dados_tratados/dados_finais/OVNIS_T_EUA.csv",stringsAsFactors = FALSE)
  df_OVNI_BRA <- read.csv("dados_tratados/dados_finais/OVNIS_T_BRA.csv",stringsAsFactors = FALSE)
  
  # Terceira limpeza

# Aqui na verdade é mais um tratamento do que uma limpeza. 
 
  # Separando dias da semana
  df_OVNI_EUA$Sight_Weekday <- weekdays(as.Date(df_OVNI_EUA$Sight_Date, '%m/%d/%y'))
  df_OVNI_BRA$Sight_Weekday <- weekdays(as.Date(df_OVNI_BRA$Sight_Date, '%m/%d/%y'))
  df_OVNI_MUNDO$Sight_Weekday <- weekdays(as.Date(df_OVNI_MUNDO$Sight_Date, '%m/%d/%y'))
  
  # Separando dia, mês e ano
  d <- strsplit(df_OVNI_EUA$Sight_Date, '/') 
  e <- do.call(rbind.data.frame, d)
  colnames(e) <- c("Sight_Month", "Sight_Day", "Sight_Year")
  e <- data.frame(lapply(e, as.character), stringsAsFactors=FALSE)
  df_OVNI_EUA <- cbind(df_OVNI_EUA, e)
  
  d <- strsplit(df_OVNI_BRA$Sight_Date, '/') 
  e <- do.call(rbind.data.frame, d)
  colnames(e) <- c("Sight_Month", "Sight_Day", "Sight_Year")
  e <- data.frame(lapply(e, as.character), stringsAsFactors=FALSE)
  df_OVNI_BRA <- cbind(df_OVNI_BRA, e)

  d <- strsplit(df_OVNI_MUNDO$Sight_Date, '/') 
  e <- do.call(rbind.data.frame, d)
  colnames(e) <- c("Sight_Month", "Sight_Day", "Sight_Year")
  e <- data.frame(lapply(e, as.character), stringsAsFactors=FALSE)
  df_OVNI_MUNDO <- cbind(df_OVNI_MUNDO, e)
  
  df_OVNI_EUA$Sight_Month[df_OVNI_EUA$Sight_Month=="1"] <- "JANUARY"
  df_OVNI_EUA$Sight_Month[df_OVNI_EUA$Sight_Month=="2"] <- "FEBRUARY"
  df_OVNI_EUA$Sight_Month[df_OVNI_EUA$Sight_Month=="3"] <- "MARCH"
  df_OVNI_EUA$Sight_Month[df_OVNI_EUA$Sight_Month=="4"] <- "APRIL"
  df_OVNI_EUA$Sight_Month[df_OVNI_EUA$Sight_Month=="5"] <- "MAY"
  df_OVNI_EUA$Sight_Month[df_OVNI_EUA$Sight_Month=="6"] <- "JUNE"
  df_OVNI_EUA$Sight_Month[df_OVNI_EUA$Sight_Month=="7"] <- "JULY"
  df_OVNI_EUA$Sight_Month[df_OVNI_EUA$Sight_Month=="8"] <- "AUGUST"
  df_OVNI_EUA$Sight_Month[df_OVNI_EUA$Sight_Month=="9"] <- "SEPTEMBER"
  df_OVNI_EUA$Sight_Month[df_OVNI_EUA$Sight_Month=="10"] <- "OCTOBER"
  df_OVNI_EUA$Sight_Month[df_OVNI_EUA$Sight_Month=="11"] <- "NOVEMBER"
  df_OVNI_EUA$Sight_Month[df_OVNI_EUA$Sight_Month=="12"] <- "DECEMBER"
  
  df_OVNI_BRA$Sight_Month[df_OVNI_BRA$Sight_Month=="1"] <- "JANUARY"
  df_OVNI_BRA$Sight_Month[df_OVNI_BRA$Sight_Month=="2"] <- "FEBRUARY"
  df_OVNI_BRA$Sight_Month[df_OVNI_BRA$Sight_Month=="3"] <- "MARCH"
  df_OVNI_BRA$Sight_Month[df_OVNI_BRA$Sight_Month=="4"] <- "APRIL"
  df_OVNI_BRA$Sight_Month[df_OVNI_BRA$Sight_Month=="5"] <- "MAY"
  df_OVNI_BRA$Sight_Month[df_OVNI_BRA$Sight_Month=="6"] <- "JUNE"
  df_OVNI_BRA$Sight_Month[df_OVNI_BRA$Sight_Month=="7"] <- "JULY"
  df_OVNI_BRA$Sight_Month[df_OVNI_BRA$Sight_Month=="8"] <- "AUGUST"
  df_OVNI_BRA$Sight_Month[df_OVNI_BRA$Sight_Month=="9"] <- "SEPTEMBER"
  df_OVNI_BRA$Sight_Month[df_OVNI_BRA$Sight_Month=="10"] <- "OCTOBER"
  df_OVNI_BRA$Sight_Month[df_OVNI_BRA$Sight_Month=="11"] <- "NOVEMBER"
  df_OVNI_BRA$Sight_Month[df_OVNI_BRA$Sight_Month=="12"] <- "DECEMBER"
  
  df_OVNI_MUNDO$Sight_Month[df_OVNI_MUNDO$Sight_Month=="1"] <- "JANUARY"
  df_OVNI_MUNDO$Sight_Month[df_OVNI_MUNDO$Sight_Month=="2"] <- "FEBRUARY"
  df_OVNI_MUNDO$Sight_Month[df_OVNI_MUNDO$Sight_Month=="3"] <- "MARCH"
  df_OVNI_MUNDO$Sight_Month[df_OVNI_MUNDO$Sight_Month=="4"] <- "APRIL"
  df_OVNI_MUNDO$Sight_Month[df_OVNI_MUNDO$Sight_Month=="5"] <- "MAY"
  df_OVNI_MUNDO$Sight_Month[df_OVNI_MUNDO$Sight_Month=="6"] <- "JUNE"
  df_OVNI_MUNDO$Sight_Month[df_OVNI_MUNDO$Sight_Month=="7"] <- "JULY"
  df_OVNI_MUNDO$Sight_Month[df_OVNI_MUNDO$Sight_Month=="8"] <- "AUGUST"
  df_OVNI_MUNDO$Sight_Month[df_OVNI_MUNDO$Sight_Month=="9"] <- "SEPTEMBER"
  df_OVNI_MUNDO$Sight_Month[df_OVNI_MUNDO$Sight_Month=="10"] <- "OCTOBER"
  df_OVNI_MUNDO$Sight_Month[df_OVNI_MUNDO$Sight_Month=="11"] <- "NOVEMBER"
  df_OVNI_MUNDO$Sight_Month[df_OVNI_MUNDO$Sight_Month=="12"] <- "DECEMBER"
  
  
# Salvando o tratamento e carregando novamente os arquivos
  write.csv(rbind(df_OVNI_EUA), file = "dados_tratados/dados_finais/OVNIS_T_EUA.csv") 
  write.csv(rbind(df_OVNI_BRA), file = "dados_tratados/dados_finais/OVNIS_T_BRA.csv") 
  write.csv(rbind(df_OVNI_MUNDO), file = "dados_tratados/dados_finais/OVNIS_T_MUNDO.csv") 
  
  df_OVNI_EUA <- read.csv("dados_tratados/dados_finais/OVNIS_T_EUA.csv",stringsAsFactors = FALSE)
  df_OVNI_EUA$X <- NULL
  df_OVNI_MUNDO <- read.csv("dados_tratados/dados_finais/OVNIS_T_MUNDO.csv",stringsAsFactors = FALSE) 
  df_OVNI_MUNDO$X <- NULL
  df_OVNI_BRA <- read.csv("dados_tratados/dados_finais/OVNIS_T_BRA.csv",stringsAsFactors = FALSE)
  df_OVNI_BRA$X <- NULL
  
## Processo de análise e visualização dos dados -------------------------- 
  
  # Gráficos
  install.packages('ggplot2')
  library(ggplot2)
  install.packages("ggmap")
  library(ggmap)
  library(dplyr)
  
  
  ## Questões - EUA:
  
  install.packages('zipcode') 
  library(zipcode)
  data(zipcode)
  us <- map_data("state")
  zipcode$city <- toupper(zipcode$city)
  
  ## Visualizações de OVNIs por cidade - EUA
  VIEWS_OVNI_CIDADE_EUA = 
    sqldf("select city as city, state as state, count(*) views from df_OVNI_EUA 
          group by City order by 3 desc")
  # gráfico pontos de avistamentos
  d <- merge(VIEWS_OVNI_CIDADE_EUA, zipcode, by=c("city","state"))
  ggplot(d,aes(longitude,latitude)) +
    geom_polygon(data=us,aes(x=long,y=lat,group=group),color='gray32',fill='black')+
    geom_point(aes(color = Views),colour = "springgreen3",size=.15,alpha=.25) +
    xlim(-125,-65)+ylim(20,50)+ 
    theme(panel.background = element_blank())+
    theme(axis.ticks = element_blank())+
    theme(axis.text = element_blank())+
    labs(x=NULL, y=NULL) + coord_map()+ 
    ggtitle("Pontos de avistamentos de OVNIs - EUA") 
  
  
  ## Visualizações de OVNIs por estado - EUA
  VIEWS_OVNI_ESTADO_EUA = 
    sqldf("select State as state, count(*) views from df_OVNI_EUA 
          group by State order by 2 desc")
        # gráfico
        US_COORD <- read.csv("US_COORD.csv",stringsAsFactors = FALSE)
        f <- merge(VIEWS_OVNI_ESTADO_EUA, US_COORD, by=c("state"))
        f$region = tolower(f$region)
        ggplot()+ geom_map(data=us, map=us, aes(x=long, y=lat, map_id=region),fill="black", color="gray32", size=0.15)+ 
          geom_map(data=f, map=us,aes(fill=views, map_id=region), color="#ffffff", size=0.15) +
          scale_fill_continuous(low='black', high='springgreen3', guide='colorbar')+ 
          theme(panel.background = element_blank())+
          theme(axis.ticks = element_blank())+
          theme(axis.text = element_blank())+
          labs(x=NULL, y=NULL) + coord_map()+ 
          ggtitle("Visualizações de OVNIs por estado - EUA")
  
  ## Visualizações de OVNIs por estado - EUA-TOP 5
  VIEWS_OVNI_ESTADO_EUA_TOP5 = 
    sqldf("select State as state, count(*) views from df_OVNI_EUA 
          group by State order by 2 desc
          limit 5")
  VIEWS_OVNI_ESTADO_EUA_TOP5 = merge(VIEWS_OVNI_ESTADO_EUA_TOP5, US_COORD, by=c("state"))
  VIEWS_OVNI_ESTADO_EUA_TOP5$latitude = NULL
  VIEWS_OVNI_ESTADO_EUA_TOP5$longitude = NULL
        # gráfico
        ggplot(VIEWS_OVNI_ESTADO_EUA_TOP5, aes(x = reorder(region, -views), y = views, label = views)) +
          geom_col(aes(fill = views))+
          scale_fill_continuous(low='grey1', high='springgreen3', guide='colorbar')+
          theme(panel.background = element_blank())+
          geom_text(size = 4, color = "white", position = position_stack(vjust = 0.5))+
          labs(x=NULL, y=NULL)+ 
          ggtitle("Visualizações de OVNIs TOP 5 Estados - EUA")
  
  ## Visualizações de OVNIs por cidade - EUA-TOP 5
  VIEWS_OVNI_CIDADE_EUA_TOP5 = 
    sqldf("select City as city, State as state, count(*) views from df_OVNI_EUA 
          group by City order by 3 desc
          limit 5")
      # gráfico
      ggplot(VIEWS_OVNI_CIDADE_EUA_TOP5, aes(x = reorder(city, -views), y = views, label = views)) +
        geom_col(aes(fill = views))+
        scale_fill_continuous(low='grey1', high='springgreen3', guide='colorbar')+
        theme(panel.background = element_blank())+
        geom_text(size = 4, color = "white", position = position_stack(vjust = 0.5))+
        labs(x=NULL, y=NULL)+ 
        ggtitle("Visualizações de OVNIs TOP 5 Cidades - EUA")
      
  ## Tipo de formato de OVNI mais visto - EUA
  FORMATO_OVNI_EUA = 
    sqldf("select Shape as shape, count(*) views from df_OVNI_EUA 
          group by Shape order by 2 desc")
      # gráfico
      ggplot(FORMATO_OVNI_EUA, aes(x = reorder(shape, views), y = views, label = views)) +
        geom_col(aes(fill = views))+
        scale_fill_continuous(low='grey1', high='springgreen3', guide='colorbar')+
        theme(panel.background = element_blank())+
        labs(x=NULL, y=NULL)+
        coord_flip()+ 
        ggtitle("Formatos de OVNIs - EUA")
  
  ## Tipo de formato de OVNI mais visto - EUA-TOP 5
  FORMATO_OVNI_EUA_TOP5 = 
    sqldf("select Shape as shape, count(*) views from df_OVNI_EUA 
          group by Shape order by 2 desc
          limit 5")
        # gráfico
        ggplot(FORMATO_OVNI_EUA_TOP5, aes(x = reorder(shape, views), y = views, label = views)) +
          geom_col(aes(fill = views))+
          scale_fill_continuous(low='grey1', high='springgreen3', guide='colorbar')+
          theme(panel.background = element_blank())+
          geom_text(size = 4, color = "white", position = position_stack(vjust = 0.5))+
          labs(x=NULL, y=NULL)+
          coord_flip()+ 
          ggtitle("Formatos de OVNIs Top 5 - EUA")
  
  ## Tipo de formato de OVNI mais visto - EUA-TOP 5 - tratado
  FORMATO_OVNI_EUA_TOP5_TR = 
    sqldf("select shape as shape, count(*) views from df_OVNI_EUA 
          Where Shape NOT IN  ('LIGHT','UNKNOWN', 'OTHER')
          group by Shape order by 2 desc
          limit 5")
          # gráfico
          ggplot(FORMATO_OVNI_EUA_TOP5_TR, aes(x = reorder(shape, views), y = views, label = views)) +
            geom_col(aes(fill = views))+
            scale_fill_continuous(low='grey1', high='springgreen3', guide='colorbar')+
            theme(panel.background = element_blank())+
            geom_text(size = 4, color = "white", position = position_stack(vjust = 0.5))+
            labs(x=NULL, y=NULL)+
            coord_flip()+ 
            ggtitle("Formatos de OVNIs Top 5 Tratado - EUA")
  
  
  ## Horário como maior incidência de avistamentos de OVNIs - EUA
  HORARIO_OVNI_EUA= 
    sqldf("select Sight_Time, count(*) Views from df_OVNI_EUA
          group by Sight_Time order by 2 desc")
          # gráfico
          ggplot(HORARIO_OVNI_EUA, aes(x=Sight_Time, y=Views, label = Views)) + 
            geom_point(size=3, color="springgreen3") + 
            geom_segment(aes(x=Sight_Time, 
                             xend=Sight_Time, 
                             y=0, 
                             yend=Views),color="springgreen3", size=1) +
            geom_text(size = 4, color = "black", position = position_stack(vjust = 1.05))+
            theme(panel.background = element_blank())+ 
            ggtitle("Horário de avistamento - EUA")
          
  
  ## Horário como maior incidência de avistamentos de OVNIs - EUA-TOP 5
  HORARIO_OVNI_EUA_TOP5= 
    sqldf("select Sight_Time, count(*) Views from df_OVNI_EUA
          group by Sight_Time order by 2 desc
          limit 5")
          # gráfico
          ggplot(HORARIO_OVNI_EUA_TOP5, aes(x=Sight_Time, y=Views, label = Views)) + 
            geom_point(size=3, color="springgreen3") + 
            geom_segment(aes(x=Sight_Time, 
                             xend=Sight_Time, 
                             y=0, 
                             yend=Views),color="springgreen3", size=1) +
            geom_text(size = 4, color = "black", position = position_stack(vjust = 1.05))+
            theme(panel.background = element_blank())+ 
            ggtitle("Horário de avistamento Top 5- EUA")
          
  
  ## Dia da Semana como maior incidência de avistamentos de OVNIs - EUA
  DIA_DA_SEMANA_OVNI_EUA= 
    sqldf("select Sight_Weekday, count(*) Views from df_OVNI_EUA
          group by Sight_Weekday order by 2 desc")
          # gráfico
          ggplot(DIA_DA_SEMANA_OVNI_EUA, aes(x = reorder(Sight_Weekday, Views), y = Views, label = Views)) +
            geom_col(aes(fill = Views))+
            scale_fill_continuous(low='grey1', high='springgreen3', guide='colorbar')+
            theme(panel.background = element_blank())+
            geom_text(size = 4, color = "white", position = position_stack(vjust = 0.5))+
            labs(x=NULL, y=NULL)
  
  
  ## Dia da Mês como maior incidência de avistamentos de OVNIs - EUA
  DIA_DO_MES_OVNI_EUA= 
    sqldf("select Sight_Day, count(*) Views from df_OVNI_EUA
          group by Sight_Day order by 2 desc")
          # gráfico        
          ggplot(DIA_DO_MES_OVNI_EUA, aes(x = reorder(Sight_Day, Sight_Day), y = Views, label = Views)) +
            geom_col(aes(fill = Views))+
            scale_fill_continuous(low='grey1', high='springgreen3', guide='colorbar')+
            theme(panel.background = element_blank())+
            geom_text(size = 4, color = "white", position = position_stack(vjust = 0.5))+
            labs(x=NULL, y=NULL)+
            ggtitle("Dia do Mês como maior incidência - EUA")
  
  ## Dia da Mês como maior incidência de avistamentos de OVNIs - EUA-TOP 5
  DIA_DO_MES_OVNI_EUA_TOP5= 
    sqldf("select Sight_Day, count(*) Views from df_OVNI_EUA
          group by Sight_Day order by 2 desc
          limit 5")
          # gráfico
          ggplot(DIA_DO_MES_OVNI_EUA_TOP5, aes(x = reorder(Sight_Day, Views), y = Views, label = Views)) +
            geom_col(aes(fill = Views))+
            scale_fill_continuous(low='grey1', high='springgreen3', guide='colorbar')+
            theme(panel.background = element_blank())+
            geom_text(size = 4, color = "white", position = position_stack(vjust = 0.5))+
            labs(x=NULL, y=NULL)+
            coord_flip()
  
  ## Mês como maior incidência de avistamentos de OVNIs - EUA
  MES_OVNI_EUA= 
    sqldf("select Sight_Month, count(*) Views from df_OVNI_EUA
          group by Sight_Month order by 2 desc")
  
          # gráfico
          ggplot(MES_OVNI_EUA, aes(x = reorder(Sight_Month, Views), y = Views, label = Views)) +
            geom_col(aes(fill = Views))+
            scale_fill_continuous(low='grey1', high='springgreen3', guide='colorbar')+
            theme(panel.background = element_blank())+
            geom_text(size = 4, color = "white", position = position_stack(vjust = 0.5))+
            labs(x=NULL, y=NULL)+
            ggtitle("Mês como maior incidência - EUA")
          
  ## Mês como maior incidência de avistamentos de OVNIs - EUA-TOP 5
  MES_OVNI_EUA_TOP5= 
    sqldf("select Sight_Month, count(*) Views from df_OVNI_EUA
          group by Sight_Month order by 2 desc
          limit 5")
  
  ## ANO como maior incidência de avistamentos de OVNIs - EUA
  ANO_OVNI_EUA= 
    sqldf("select Sight_Year, count(*) Views from df_OVNI_EUA
          group by Sight_Year order by 2 desc")
          # Gráfico
          ANO_OVNI_EUA$Year[ANO_OVNI_EUA$Sight_Year < 100] <- ANO_OVNI_EUA$Sight_Year + 1900
          ANO_OVNI_EUA$Year[ANO_OVNI_EUA$Sight_Year < 18] <- ANO_OVNI_EUA$Year + 100
          ANO_OVNI_EUA$Year[ANO_OVNI_EUA$Sight_Year ==0] <- 2000
          ANO_OVNI_EUA$Year[ANO_OVNI_EUA$Sight_Year ==18] <- 2018
          
          ggplot(ANO_OVNI_EUA, aes(x = reorder(Year, Year), y = Views, label = Views)) +
            geom_col(aes(fill = Views))+
            scale_fill_continuous(low='grey1', high='springgreen3', guide='colorbar')+
            theme(panel.background = element_blank())+
            labs(x=NULL, y=NULL)+
            ggtitle("Ano como maior incidência - EUA")
  

  ## ANO como maior incidência de avistamentos de OVNIs - EUA-TOP 5
  ANO_OVNI_EUA_TOP5= 
    sqldf("select Year, Views from ANO_OVNI_EUA
          group by Year order by 2 desc
          limit 5")
          # gráfico
          ggplot(ANO_OVNI_EUA_TOP5, aes(x = reorder(Year, Views), y = Views, label = Views)) +
            geom_col(aes(fill = Views))+
            scale_fill_continuous(low='grey1', high='springgreen3', guide='colorbar')+
            theme(panel.background = element_blank())+
            geom_text(size = 4, color = "white", position = position_stack(vjust = 0.5))+
            labs(x=NULL, y=NULL)+
            coord_flip()+
            ggtitle("Ano como maior incidência Top 5 - EUA")
           
          
  
  
  # Médias Dos últimos 50 anos EUA
  # Quantidade de registros:97922 
  MEDIA_POR_ANO_EUA = (sum(ANO_OVNI_EUA$Views)/50)
  MEDIA_POR_ANO_EUA
  MEDIA_POR_MES_EUA = MEDIA_POR_ANO_EUA/12
  MEDIA_POR_MES_EUA
  MEDIA_POR_DIA_EUA = MEDIA_POR_MES_EUA/31
  MEDIA_POR_DIA_EUA
      #----------------------------------------
  
  ## Questões - BRA:
  
  ## Visualizações de OVNIs por estado - BRA
  VIEWS_OVNI_ESTADO_BRA = 
    sqldf("select State as state, count(*) Views from df_OVNI_BRA 
          group by state order by 2 desc")
  
  ## Visualizações de OVNIs por estado - BRA-TOP 5
  VIEWS_OVNI_ESTADO_BRA_TOP5 = 
    sqldf("select State, count(*) Views from df_OVNI_BRA 
          group by State order by 2 desc
          limit 5")
  
  ## Visualizações de OVNIs por cidade - BRA
  VIEWS_OVNI_CIDADE_BRA = 
    sqldf("select City, State, count(*) Views from df_OVNI_BRA 
          group by City order by 3 desc")
  
  ## Visualizações de OVNIs por cidade - BRA-TOP 5
  VIEWS_OVNI_CIDADE_BRA_TOP5 = 
    sqldf("select City, State, count(*) Views from df_OVNI_BRA 
          group by City order by 3 desc
          limit 5")
  
  ## Tipo de formato de OVNI mais visto - BRA
  FORMATO_OVNI_BRA = 
    sqldf("select Shape, count(*) Views from df_OVNI_BRA 
          group by Shape order by 2 desc")
  
  ## Tipo de formato de OVNI mais visto - BRA-TOP 5
  FORMATO_OVNI_BRA_TOP5 = 
    sqldf("select Shape, count(*) Views from df_OVNI_BRA 
          group by Shape order by 2 desc
          limit 5")
  
  ## Tipo de formato de OVNI mais visto - BRA-TOP 5 - tratado
  FORMATO_OVNI_BRA_TOP5_TR = 
    sqldf("select Shape, count(*) Views from df_OVNI_BRA 
          Where Shape NOT IN  ('LIGHT','UNKNOWN', 'OTHER')
          group by Shape order by 2 desc
          limit 5")
  
  ## Horário como maior incidência de avistamentos de OVNIs - BRA
  HORARIO_OVNI_BRA= 
    sqldf("select Sight_Time, count(*) Views from df_OVNI_BRA
          group by Sight_Time order by 2 desc")
  
  ## Horário como maior incidência de avistamentos de OVNIs - BRA-TOP 5
  HORARIO_OVNI_BRA_TOP5= 
    sqldf("select Sight_Time, count(*) Views from df_OVNI_BRA
          group by Sight_Time order by 2 desc
          limit 5")
  
  ## Dia da Semana como maior incidência de avistamentos de OVNIs - BRA
  DIA_DA_SEMANA_OVNI_BRA= 
    sqldf("select Sight_Weekday, count(*) Views from df_OVNI_BRA
          group by Sight_Weekday order by 2 desc")
  
  ## Dia da Mês como maior incidência de avistamentos de OVNIs - BRA
  DIA_DO_MES_OVNI_BRA= 
    sqldf("select Sight_Day, count(*) Views from df_OVNI_BRA
          group by Sight_Day order by 2 desc")
  
  ## Dia da Mês como maior incidência de avistamentos de OVNIs - BRA-TOP 5
  DIA_DO_MES_OVNI_BRA_TOP5= 
    sqldf("select Sight_Day, count(*) Views from df_OVNI_BRA
          group by Sight_Day order by 2 desc
          limit 5")
  
  ## Mês como maior incidência de avistamentos de OVNIs - BRA
  MES_OVNI_BRA= 
    sqldf("select Sight_Month, count(*) Views from df_OVNI_BRA
          group by Sight_Month order by 2 desc")
  
  ## Mês como maior incidência de avistamentos de OVNIs - BRA-TOP 5
  MES_OVNI_BRA_TOP5= 
    sqldf("select Sight_Month, count(*) Views from df_OVNI_BRA
          group by Sight_Month order by 2 desc
          limit 5")
  
  ## ANO como maior incidência de avistamentos de OVNIs - BRA
  ANO_OVNI_BRA= 
    sqldf("select Sight_Year, count(*) Views from df_OVNI_BRA
          group by Sight_Year order by 2 desc")
  
  ## ANO como maior incidência de avistamentos de OVNIs - BRA-TOP 5
  ANO_OVNI_BRA_TOP5= 
    sqldf("select Sight_Year, count(*) Views from df_OVNI_BRA
          group by Sight_Year order by 2 desc
          limit 5")
  
  # Médias Dos últimos 50 anos BRA
  # Quantidade de registros:102 
  MEDIA_POR_ANO_BRA = (sum(ANO_OVNI_BRA$Views)/50)
  MEDIA_POR_MES_BRA = MEDIA_POR_ANO_BRA/12
  MEDIA_POR_DIA_BRA = MEDIA_POR_MES_BRA/31
  

  #----------------------------------------
  
  ## Questões - MUNDO:

  ## Visualizações de OVNIs por país - MUNDO
  VIEWS_OVNI_PAIS_MUNDO = 
    sqldf("select Country, count(*) Views from df_OVNI_MUNDO 
          group by Country order by 2 desc")
  
  ## Visualizações de OVNIs por país - MUNDO-TOP 5
  VIEWS_OVNI_PAIS_MUNDO_TOP5 = 
    sqldf("select Country,  count(*) Views from df_OVNI_MUNDO 
          group by Country order by 2 desc
          limit 5")
  
  ## Tipo de formato de OVNI mais visto - MUNDO
  FORMATO_OVNI_MUNDO = 
    sqldf("select Shape, count(*) Views from df_OVNI_MUNDO 
          group by Shape order by 2 desc")
  
  ## Tipo de formato de OVNI mais visto - MUNDO-TOP 5
  FORMATO_OVNI_MUNDO_TOP5 = 
    sqldf("select Shape, count(*) Views from df_OVNI_MUNDO 
          group by Shape order by 2 desc
          limit 5")
  
  ## Tipo de formato de OVNI mais visto - MUNDO-TOP 5 - tratado
  FORMATO_OVNI_MUNDO_TOP5_TR = 
    sqldf("select Shape, count(*) Views from df_OVNI_MUNDO 
          Where Shape NOT IN  ('LIGHT','UNKNOWN', 'OTHER')
          group by Shape order by 2 desc
          limit 5")
  
  ## Tipo de formato de OVNI mais visto - MUNDO - sem EUA
  FORMATO_OVNI_MUNDO_S_EUA = 
    sqldf("select Shape, count(*) Views from df_OVNI_MUNDO 
          Where Country NOT IN  ('UNITED STATES')
          group by Shape order by 2 desc")
  
  ## Tipo de formato de OVNI mais visto - MUNDO-TOP 5 - sem EUA
  FORMATO_OVNI_MUNDO_TOP5_S_EUA = 
    sqldf("select Shape, count(*) Views from df_OVNI_MUNDO 
          Where Country NOT IN  ('UNITED STATES')
          group by Shape order by 2 desc
          limit 5")
  
  ## Tipo de formato de OVNI mais visto - MUNDO-TOP 5 - tratado - sem EUA
  FORMATO_OVNI_MUNDO_TOP5_TR_S_EUA = 
    sqldf("select Shape, count(*) Views from df_OVNI_MUNDO 
          Where Shape NOT IN ('LIGHT','UNKNOWN', 'OTHER') AND Country != 'UNITED STATES'
          group by Shape order by 2 desc
          limit 5")
  
  ## Horário como maior incidência de avistamentos de OVNIs - MUNDO
  HORARIO_OVNI_MUNDO= 
    sqldf("select Sight_Time, count(*) Views from df_OVNI_MUNDO
          group by Sight_Time order by 2 desc")
  
  ## Horário como maior incidência de avistamentos de OVNIs - MUNDO-TOP 5
  HORARIO_OVNI_MUNDO_TOP5= 
    sqldf("select Sight_Time, count(*) Views from df_OVNI_MUNDO
          group by Sight_Time order by 2 desc
          limit 5")
  
  ## Dia da Semana como maior incidência de avistamentos de OVNIs - MUNDO
  DIA_DA_SEMANA_OVNI_MUNDO= 
    sqldf("select Sight_Weekday, count(*) Views from df_OVNI_MUNDO
          group by Sight_Weekday order by 2 desc")
  
  ## Dia da Mês como maior incidência de avistamentos de OVNIs - MUNDO
  DIA_DO_MES_OVNI_MUNDO= 
    sqldf("select Sight_Day, count(*) Views from df_OVNI_MUNDO
          group by Sight_Day order by 2 desc")
  
  ## Dia da Mês como maior incidência de avistamentos de OVNIs - MUNDO-TOP 5
  DIA_DO_MES_OVNI_MUNDO_TOP5= 
    sqldf("select Sight_Day, count(*) Views from df_OVNI_MUNDO
          group by Sight_Day order by 2 desc
          limit 5")
  
  ## Mês como maior incidência de avistamentos de OVNIs - MUNDO
  MES_OVNI_MUNDO= 
    sqldf("select Sight_Month, count(*) Views from df_OVNI_MUNDO
          group by Sight_Month order by 2 desc")
  
  ## Mês como maior incidência de avistamentos de OVNIs - MUNDO-TOP 5
  MES_OVNI_MUNDO_TOP5= 
    sqldf("select Sight_Month, count(*) Views from df_OVNI_MUNDO
          group by Sight_Month order by 2 desc
          limit 5")
  
  ## ANO como maior incidência de avistamentos de OVNIs - MUNDO
  ANO_OVNI_MUNDO= 
    sqldf("select Sight_Year, count(*) Views from df_OVNI_MUNDO
          group by Sight_Year order by 2 desc")
  
  ## ANO como maior incidência de avistamentos de OVNIs - MUNDO-TOP 5
  ANO_OVNI_MUNDO_TOP5= 
    sqldf("select Sight_Year, count(*) Views from df_OVNI_MUNDO
          group by Sight_Year order by 2 desc
          limit 5")
  
  # Médias Dos últimos 50 anos MUNDO
  # Quantidade de registros:105724 
  MEDIA_POR_ANO_MUNDO = (sum(ANO_OVNI_MUNDO$Views)/50)
  MEDIA_POR_MES_MUNDO = MEDIA_POR_ANO_MUNDO/12
  MEDIA_POR_DIA_MUNDO = MEDIA_POR_MES_MUNDO/31
  

  
  