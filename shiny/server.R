datos <-read.csv("www/joinedCovidData.csv",dec = ",")

server <- function(input, output) {
  
  fectch_data <- reactive({
    countries <- input$countries_select
    subset <- data_score %>% 
      dplyr::filter(location %in% countries,
                    date <= input$date_select[2],
                    date >= input$date_select[1])
    return(subset)
  })
  
  output$score_table <- renderPlotly({
    data <- fectch_data()
    g <- ggplot(data, aes(x = date, y = overallScore,
                           color = location)) +
      geom_line(lwd = 1)+
      theme_bw() +
      ylab("Scores") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_breaks = "1 month",labels = date_format("%Y-%b")) +
      scale_color_viridis_d() +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(color = "Country/Region") +
      xlab("")
    
    g <-plotly::ggplotly(g,tooltip = c("date","overallScore",'location'))
    g
  })
  
  
  output$RawData <- DT::renderDataTable(
    DT::datatable({
      datos
    },
    options = list(lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE
    ))
  
  output$clustered_world_map<- renderPlotly({
    ggplot(clustered_world_map,aes(x = long, y = lat,group = group, text = region,fill = hdi_cluster == strsplit(input$cluster_select,split=" ")[[1]][2]))+
      geom_polygon(colour = "gray50")+
      scale_fill_manual(values = c('grey','red'))+
      guides(fill=guide_legend(title=NULL)) + theme(legend.position = "none")+
      xlab("Longitude")+
      ylab("Latitude")-> p
    
    ggplotly(p,tooltip = c("region"))
  })
  
  
  observeEvent(input$cluster_select,{
      updateSelectInput(inputId = "countries_select",
                        choices = unique(cluster_data[cluster_data$hdi_cluster ==strsplit(input$cluster_select,split=" ")[[1]][2],]$location),
                        selected = unique(cluster_data[cluster_data$hdi_cluster ==strsplit(input$cluster_select,split=" ")[[1]][2],]$location[1]) )
    
  },ignoreInit = TRUE)
  
  output$cluster_result <- renderFormattable({
    df <- as.data.frame(aggregate(data_score$overallScore, list(data_score$hdiCluster), FUN=mean,na.rm =TRUE))
    df<-df[df$Group.1==strsplit(input$cluster_select,split=" ")[[1]][2],]
    row.names(df) <- NULL
    names(df)[names(df) == "Group.1"] <- "Cluster"
    names(df)[names(df) == "x"] <- "Cluster Average"
    df$`Cluster Average` <- round(df$`Cluster Average`,digits = 2)
    formattable(df, 
                align =c("l","l"), 
                list("Cluster"= formatter(
                  "span", style = ~ style(color = "grey",font.weight = "bold")),
                  "Cluster Average"= color_tile(customGreen0, customGreen)
                ))
    
  })
  
  output$cluster_top2 <- renderFormattable({
    df <- as.data.frame(aggregate(data_score[data_score$hdiCluster == strsplit(input$cluster_select,split=" ")[[1]][2],]$overallScore,list(data_score[data_score$hdiCluster == strsplit(input$cluster_select,split=" ")[[1]][2],]$location), FUN=mean,na.rm =TRUE))
    df <- df %>% top_n(2)
    row.names(df) <- NULL
    names(df)[names(df) == "Group.1"] <- "Cluster top 2"
    names(df)[names(df) == "x"] <- "Average Score"
    df$`Average Score` <- round(df$`Average Score`,digits = 2)
    formattable(df, 
                align =c("l","l"), 
                list("Cluster"= formatter(
                  "span", style = ~ style(color = "grey",font.weight = "bold")),
                  "Average Score"= color_tile(customGreen0, customGreen)
                ))
    
  })
  
  output$cluster_btm2 <- renderFormattable({
    df <- as.data.frame(aggregate(data_score[data_score$hdiCluster == strsplit(input$cluster_select,split=" ")[[1]][2],]$overallScore,list(data_score[data_score$hdiCluster == strsplit(input$cluster_select,split=" ")[[1]][2],]$location), FUN=mean,na.rm =TRUE))
    df <- df %>% top_n(-2)
    row.names(df) <- NULL
    names(df)[names(df) == "Group.1"] <- "Cluster Worst 2"
    names(df)[names(df) == "x"] <- "Average Score"
    df$`Average Score` <- round(df$`Average Score`,digits = 2)
    formattable(df, 
                align =c("l","l"), 
                list("Cluster"= formatter(
                  "span", style = ~ style(color = "grey",font.weight = "bold")),
                  "Average Score"= color_tile(customGreen0, customGreen)
                ))
    
  })
  
  
  output$result <- renderFormattable({
    data <- fectch_data()
    df <- as.data.frame(aggregate(data$overallScore, list(data$location), FUN=mean,na.rm =TRUE))
    names(df)[names(df) == "Group.1"] <- "Country Name"
    names(df)[names(df) == "x"] <- "Average Score"
    df$`Average Score` <- round(df$`Average Score`,digits = 2)
    formattable(df, 
                align =c("l","l"), 
                list("Country Name"= formatter(
                  "span", style = ~ style(color = "grey",font.weight = "bold")),
                  "Average Score"= color_tile(customGreen0, customGreen)
                  
                ))
  })
  
  
  observeEvent(input$cluster_select,{
    if(strsplit(input$cluster_select,split=" ")[[1]][2] == 1){
      output$cluster_desc <-renderText({
        "These countries are primarily African Countries as well, Pacific Island and South East Asia "
      })
    }else if(strsplit(input$cluster_select,split=" ")[[1]][2] == 2){
      output$cluster_desc <-renderText({
        "Western Asia and Americas "
      })
    }else if(strsplit(input$cluster_select,split=" ")[[1]][2] == 3){
      output$cluster_desc <-renderText({
        "All of Western Europe, Oceania, North America "
      })
    }else if(strsplit(input$cluster_select,split=" ")[[1]][2] == 4){
      output$cluster_desc <-renderText({
        "Eastern and South-East Asia, African and South American "
      })
    }else if(strsplit(input$cluster_select,split=" ")[[1]][2] == 5){
      output$cluster_desc <-renderText({
        "Central African Countries "
      })
    }else if(strsplit(input$cluster_select,split=" ")[[1]][2] == 6){
      output$cluster_desc <-renderText({
        "South Asia, African and Central America "
      })
    }
  })
  
  
  
}