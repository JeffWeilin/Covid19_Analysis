library(shiny)
library(shinythemes)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  # App title ----
  titlePanel("Responesive"),
  navbarPage("Let's get started",
             footer = includeHTML("footer.html"),
             tabPanel(icon("home"),
               fluidRow(
                 column(tags$img(src="img1.jpeg",width="200px",height="260px"),width=2),
                 column(
                 
                   br(),
                   p("This application aims to analyse the performance of countries and their responses to COVID-19, and whether Human Development Index (HDI) influences the responses or the performance of a country. Through this, being able to analyse the most ideal responses to this pandemic will assist future recommendations and responses to different pandemic situations, with confidence that these responses analysed can provide ample advice.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                   br(),
                   
                   p("The data used in this project was sourced from ", em("Our World in Data"), "and ", em("The University of Oxford."), " 
                                              The OWID data contains multiple instances of data that analyse cases, deaths and hospital data, as well as demographic data for each location and certain quality of life variables also.
                     The Oxford data looks at a timelined response of Government response to the pandemic over time, and a merge of these two datasets allowed for analysis to be conducted for this product.",style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                   
                   width=8
                 ),
                 column(
                   br(),
                   tags$img(src="oxf.png",width="200px",height="200px"),
                   br(),
                   p("For more information please check the",em("Oxford University’s (UK) dataset"),"page clicking",
                   br(),
                   a(href="https://ourworldindata.org/policy-responses-covid", "Here",target="_blank"),style="text-align:center;color:black"),
                   ,width=2
                 )
              ),
              
              hr(), 
              tags$style(".fa-database {color:#E87722}"),
              h3(p(em("Meta data "),icon("database",lib = "font-awesome"),style="color:black;text-align:center"))
            ),

            tabPanel("Methodology"),
            
            tabPanel("Clusters",
                     # Sidebar layout with input and output definitions ----
                     sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       sidebarPanel(
                         
                         # Input: Select the country ----
                         # selectInput indciates a box with choices to select from
                         # Here choices include all the location in the covid_data; and we will set
                         # Australia as default country
                         selectInput("cluster_select", "Cluster:", 
                                     choices =unique(c('Cluster 1 : (Low HDI countries)',
                                                       'Cluster 2 : (High / Very High HDI countries)',
                                                       'Cluster 3 : (Very High HDI countries)',
                                                       'Cluster 4 : (High HDI countries)',
                                                       'Cluster 5 : (Low HDI countries)',
                                                       'Cluster 6 : (Medium HDI countries)')),
                                     selected = 'Cluster 1 (Low HDI countries)',
                                     multiple = FALSE),
                         selectInput("countries_select", "Countries/Regions:", 
                                     choices = unique(cluster_data[cluster_data$hdi_cluster ==1,]$location),
                                     selected =unique(cluster_data[cluster_data$hdi_cluster ==1,]$location[1]),
                                     multiple = TRUE),
  
                         sliderInput("date_select",
                                     "The last date:",
                                     min = min(data_score$date),
                                     max = max(data_score$date),
                                     value=c(min(data_score$date),max(data_score$date)),
                                     timeFormat="%b %y"),
                         
                         
                       ),
                       
                       
                       # Main panel for displaying outputs ----
                       mainPanel(
                         
                         # Title of the plot ----
                         h4("World map with clusters"),
                         # Output: plot of clustered world map
                         textOutput("cluster_desc"),
                         plotlyOutput(outputId = 'clustered_world_map'),
                         h4("Score Plot"),
                         # Output: plot of new cases ----
                         plotlyOutput(outputId = "score_table"),
                         h4("Cluster Average"),
                         formattableOutput(outputId = "cluster_result"),
                         h4("Best performing countries in Cluster"),
                         formattableOutput(outputId = "cluster_top2"),
                         h4("Worst performing countries in Cluster"),
                         formattableOutput(outputId = "cluster_btm2"),
                         h4("Result Plot"),
                         formattableOutput(outputId = "result"),
                         
                       )
                     )
                     
            ),

            tabPanel("About",
                     includeHTML("about.html"),
                     shinyjs::useShinyjs(),
                     tags$head(
                       tags$link(rel = "stylesheet", 
                                 type = "text/css", 
                                 href = "plugins/carousel.css"),
                       tags$script(src = "plugins/holder.js")
                     ),
                     tags$style(type="text/css",
                                ".shiny-output-error { visibility: hidden; }",
                                ".shiny-output-error:before { visibility: hidden; }"
                     )
            )
  )
  
  
  
  
  
)

