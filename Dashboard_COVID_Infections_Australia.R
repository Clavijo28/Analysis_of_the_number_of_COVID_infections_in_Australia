library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)
require(scales)
require(reshape)
library(nortest)
library(knitr)
library(shiny)
library(stringr)
library(rvest)
library(foreach)
library(TTR)
#install.packages("gridExtra")
#write.xlsx(ttt,"haaa.xlsx")
library(openxlsx)
library(gridExtra)

#readRDS(file = "my_data.rds")
#Options = c(c("ACT Second Doses","Forecast ACT"),c("VIC Second Doses","Forecast VIC"),
#c("NSW Second Doses","Forecast NSW"),c("QLD Second Doses","Forecast QLD"),
#c("WA Second Doses","Forecast WA"),c("SA Second Doses","Forecast SA"),
#c("NT Second Doses","Forecast NT"),c("TAS Second Doses","Forecast TAS"))

#initial <- c(c("NSW Second Doses","Forecast NSW"),c("VIC Second Doses","Forecast VIC"))
initial <- c("SA Second Doses,Forecast SA","WA Second Doses,Forecast WA")
Options <- c("NSW Second Doses,Forecast NSW","VIC Second Doses,Forecast VIC",
             "QLD Second Doses,Forecast QLD","WA Second Doses,Forecast WA",
             "SA Second Doses,Forecast SA","TAS Second Doses,Forecast TAS",
             "NT Second Doses,Forecast NT","ACT Second Doses,Forecast ACT")

initial_VAC <- c("VIC Cases,VIC Second Doses","NSW Cases,NSW Second Doses")
Options_vaccination <- c("VIC Cases,VIC Second Doses","NSW Cases,NSW Second Doses",
                         "QLD Cases,QLD Second Doses","WA Cases,WA Second Doses",
                         "SA Cases,SA Second Doses","NT Cases,NT Second Doses",
                         "ACT Cases,ACT Second Doses","TAS Cases,TAS Second Doses")

ui <- navbarPage(
  
  title="Analisys of COVID cases and Vaccination",
  
  tabPanel("Analysis of Covid Cases AVG Movil",
           h1("Analysis of one-week moving average"),
           numericInput("n", "Periods of Movil Average", 7),
           sliderInput(inputId = "num", label = "Choose last wave to evaluate", value = 20, min = 10, max = 200),
           actionButton("go", "Calculate and Load Data"),
           
           plotOutput("Graph"),
           h2("Calculation of mean Cuadratic ERROR"),
           tableOutput("TableError"),
           #h3("Mean Cuadratic Error"),
           textOutput("Error_mean")
  ),
  
  tabPanel("Evaluation of Vaccination",
           h1("Analysis of percentage of people vaccinated"),
           selectInput(inputId ="Bar", label ='Options',Options,initial, multiple=TRUE, selectize=FALSE),
           dateInput(inputId = "date", label = "Choose date to see data behavior",value = "2021-09-21",
                     min = "2021-01-21",
                     max = "2021-11-25",
                     format = "yyyy-mm-dd"),
           sliderInput(inputId = "Scale", label = "Choose Scale of axis X",value = c(0.4,0.7), min = 0.1, max = 1),
           plotOutput(outputId = "Vaccination_State"),
           h2("Exact date of states reaching 80% 90% and 100% vaccinated"),
            tableOutput("TableForecast")),
           
 tabPanel("Evaluation Relation Vaccination and number of cases",
            h1("Relation between vaccination and new cases per week per State"),
           sliderInput(inputId = "date_range", label = "Choose Date Range",value = c(as.Date("2020-05-05"),Sys.Date()), min = as.Date("2020-01-01"), max = Sys.Date()),
           #selectInput(inputId ="Bar2", label ='Options of State',Options_vaccination,initial_VAC, multiple=TRUE, selectize=FALSE),
           plotOutput(outputId = "Relation")
           )
  )
  

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
server <- function(input, output) {
  
  randomVals <- eventReactive(input$go, {
    option <- input$n
    numb <- input$num
    grab.COVID_data <- function(url){
      
      html <- read_html(url)
      
      html %>% html_elements("td") %>%
        html_text(trim=TRUE) -> raw_data
      
      html %>% html_elements("h2") %>%
        html_text(trim=TRUE) -> title
      
      #State <- sample("NSW", size = 100, replace = TRUE)
      
      Clean_data <- raw_data[-c(1:8)]
      
      matrix_data <- matrix(Clean_data, ncol = 5, byrow = TRUE)
      data <- as.data.frame(matrix_data, stringsAsFactors=FALSE)
      colnames(data) <- c("Date","New","Cases")
      data$State <- title
      
      Clean_data <- data %>% select(Date,New,Cases,State) %>%
        mutate(Date = as.Date(Date, format = "%d %B %y")) %>%
        mutate(New = as.numeric(gsub(",", "", New))) %>%
        mutate(Cases = as.numeric(gsub(",", "", Cases)))
      
      Clean_data <- na.omit(Clean_data)
      #h2 <- h1[order(h1$f),]
      Clean_data_order <- Clean_data[order(Clean_data$Date),]
      
      #Clean_data_order <- Clean_data
      
      Clean_data_order$Forecast_Cases.Daily <- TTR::SMA(Clean_data_order$New, n=option)
      Clean_data_order$Forecast_Cum_Cases.Daily <- TTR::SMA(Clean_data_order$Cases, n=option)
      
      data.frame_Forecast <- Clean_data_order %>% select(Date,New,Cases,Forecast_Cases.Daily,Forecast_Cum_Cases.Daily) %>%
        mutate(State = paste("Movil Average",title))
      
      data_frame_Movil.Avg <- data.frame(Date=c(Clean_data_order[,1],data.frame_Forecast[,1]),
                                         New = c(Clean_data_order[,2],data.frame_Forecast[,2]),
                                         Cases = c(Clean_data_order[,3],data.frame_Forecast[,3]),
                                         Cases_Movil.Average = c(Clean_data_order[,2],data.frame_Forecast[,4]),
                                         factor=c(Clean_data_order[,4],data.frame_Forecast[,6]))
      
      L_limit <- data_frame_Movil.Avg %>% filter(New <numb)
      #L_limit_value <- as.numeric(L_limit[1,, drop = TRUE])
      L_limit_value <- as.numeric(tail(L_limit, n=1))
      
      U_limit <- data_frame_Movil.Avg %>% filter(New >numb)
      #U_limit_value <- as.numeric(U_limit[1,, drop = FALSE])
      U_limit_value <- as.numeric(tail(U_limit, n=1))
      
      #typeof(U_limit_value)
      
      Last_wave <- data_frame_Movil.Avg %>% filter(Cases >= L_limit_value[3] & Cases <= U_limit_value[3])
      
      return(Last_wave)
    }
    
    urls <- c("https://covidlive.com.au/report/daily-cases/nsw",
              "https://covidlive.com.au/report/daily-cases/vic")
    
    Analisys_cases <- foreach(url = urls, .combine = rbind) %do% {
      print(url)
      print("=======")
      grab.COVID_data(url)
      
    }
    
    Analisys_cases
    
    #runif(input$n)
  })
  
  prog_covid <- reactive({
    
    y <- readRDS(file = "Vaccination_per_State.rds")
    
    x <- y %>% filter(Date >= as.Date(input$date))
    Input <- input$Bar
    #g <- c("NT Second Doses, Forecast NT","SA Second Doses, Forecast SA")
    Input_1 <- strsplit(Input, split = ",")
    Input_2 <- unlist(Input_1)

    New.frame <- filter(x, Factor %in% Input_2)
    New.frame
 
  })
  
  Rela <- reactive({
    
    z <- readRDS(file = "Relation_variables.rds")
    
    z <- z %>%
      group_by(Date.i = lubridate::floor_date(Date,"week"),Factor_vac_cases) %>%
      summarize(total_cases.vac_day = sum(Vaccination_and_cases)) %>%
      filter(Date.i >= input$date_range[1] & Date.i <= input$date_range[2])
    
    z
    #filter(Date.i >= input$date_range[1] & Date.i <= input$date_range[2])
  })
  
  output$Relation <- renderPlot({
    
    qplot(Date.i,total_cases.vac_day,data=Rela(),colour=Factor_vac_cases)+
      geom_line() +
      geom_point() +
      labs(x="Days than 100th confirmed case",y="Cumulative cases",title="Cumulative cases count-log scale")+
      theme_minimal()
    
  })
  
  
  output$Vaccination_State <- renderPlot({
    
    qplot(Date,Percentage_vaccinaton ,data=prog_covid(),colour=Factor)+
      geom_line()+
      geom_point(size=1) +
      labs(x="Number of cases per day",y="Percentage of people vaccinated",title="Analysis percentage of people vaccinated in Australia") +
      theme_minimal() +
      scale_y_log10(labels=comma,breaks=c(0.05,0.1,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.5,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,1), limits=input$Scale) +
      geom_hline(aes(yintercept=0.9), colour="#990000", linetype="dashed",size=1) +
      geom_hline(aes(yintercept=1), colour="#990000", linetype="dashed",size=1) +
      geom_hline(aes(yintercept=0.8), colour="#990000", linetype="dashed",size=1)
    #hist(randomVals())
    
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$Graph <- renderPlot({
    qplot(Date,Cases_Movil.Average,data=randomVals(),colour=factor)+
      geom_line() +
      geom_point(size=1) +
      labs(x="Number of cases per day",y="Total cases",title="Analysis of last wave COVID cases NSW and VIC") +
      theme_minimal()
    #hist(randomVals())
    
  })
  #--------------------------------------------------------
  output$Error_mean <- renderText({
    Analysis_MovilAvg <- randomVals() %>% select(New,Cases_Movil.Average,factor) %>% 
      filter(factor=="Movil Average NSW Cases") %>% 
      mutate(Error = round(Cases_Movil.Average - New,2)) %>%
      mutate(Error_2 = round(Error*Error,2))
    Mean_Error_2 <- mean(Analysis_MovilAvg$Error_2)
    paste("The Cuadractic Error is: ",Mean_Error_2, "The less error there is, the better the forecasting model.")
    
  })
  #------------------------------------------
  output$TableError <- renderTable({
    Analysis_MovilAvg <- randomVals() %>% select(New,Cases_Movil.Average,factor) %>% 
      filter(factor=="Movil Average NSW Cases") %>% 
      mutate(Error = round(Cases_Movil.Average - New,2)) %>%
      mutate(Error_2 = round(Error*Error,2))
    head(Analysis_MovilAvg,5)
    
  })
  
  output$TableForecast <- renderTable({
    x <- readRDS(file = "hit.rds")
    x
  })
  #------------------------------------------
  
  
}

shinyApp(ui, server)



#kk <- c("NSW Second Doses,Forecast NSW","VIC Second Doses,Forecast VIC")
#b <-strsplit(kk, split = ",")
#d <- unlist(b)
#d[4]

