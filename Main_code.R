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

# Question 1 -------------------------------------------------------------------

# Extract data from website about covid cases NSW and VIC
grab.COVID_data <- function(url){
  
  html <- read_html(url)
  
  # Extract Raw Data
  html %>% html_elements("td") %>%
    html_text(trim=TRUE) -> raw_data
  
  html %>% html_elements("h2") %>%
    html_text(trim=TRUE) -> title
  
  # Clean data taking off firts data
  Clean_data <- raw_data[-c(1:8)]
  
  # Convert Raw data in a Matrix
  matrix_data <- matrix(Clean_data, ncol = 5, byrow = TRUE)
  # Convert data in dataframe
  data <- as.data.frame(matrix_data, stringsAsFactors=FALSE)
  #Rename Columns
  colnames(data) <- c("Date","New","Cases")
  data$State <- title
  
  # Clean data and select some columns
  Clean_data <- data %>% select(Date,New,Cases,State) %>%
    mutate(Date = as.Date(Date, format = "%d %B %y")) %>%
    mutate(New = as.numeric(gsub(",", "", New))) %>%
    mutate(Cases = as.numeric(gsub(",", "", Cases)))
  
  # Omit Na values
  Clean_data <- na.omit(Clean_data)
  # Order data Ascending order
  Clean_data_order <- Clean_data[order(Clean_data$Date),]
  
  # Forecast of mean movil simple with periods 7
  Clean_data_order$Forecast_Cases.Daily <- TTR::SMA(Clean_data_order$New, n=7)
  Clean_data_order$Forecast_Cum_Cases.Daily <- TTR::SMA(Clean_data_order$Cases, n=7)
  
  # New data frame with forecast
  data.frame_Forecast <- Clean_data_order %>% select(Date,New,Cases,Forecast_Cases.Daily,Forecast_Cum_Cases.Daily) %>%
    mutate(State = paste("Movil Average",title))
  
  # Join data frame to extract informacion
  data_frame_Movil.Avg <- data.frame(Date=c(Clean_data_order[,1],data.frame_Forecast[,1]),
                  New = c(Clean_data_order[,2],data.frame_Forecast[,2]),
                  Cases = c(Clean_data_order[,3],data.frame_Forecast[,3]),
                  Cases_Movil.Average = c(Clean_data_order[,2],data.frame_Forecast[,4]),
                  factor=c(Clean_data_order[,4],data.frame_Forecast[,6]))
  
  # Calculate last wave of virus defining 2 limits
  L_limit <- data_frame_Movil.Avg %>% filter(New <20)
  #L_limit_value <- as.numeric(L_limit[1,, drop = TRUE])
  L_limit_value <- as.numeric(tail(L_limit, n=1))

  U_limit <- data_frame_Movil.Avg %>% filter(New >20)
  #U_limit_value <- as.numeric(U_limit[1,, drop = FALSE])
  U_limit_value <- as.numeric(tail(U_limit, n=1))
  
  #typeof(U_limit_value)
  
  # Data Frame of last wave
  Last_wave <- data_frame_Movil.Avg %>% filter(Cases >= L_limit_value[3] & Cases <= U_limit_value[3])
  
  return(Last_wave)
}

# Websites
urls <- c("https://covidlive.com.au/report/daily-cases/nsw",
          "https://covidlive.com.au/report/daily-cases/vic")

Analisys_cases <- foreach(url = urls, .combine = rbind) %do% {
  print(url)
  print("=======")
  grab.COVID_data(url)
  
}

Analisys_cases

saveRDS(Analisys_cases, file = "my_data.rds")

# Graph of number of cases in NSW and VIC with mean movil simple
Figure_1 <- qplot(Date,Cases_Movil.Average,data=Analisys_cases,colour=factor)+
      geom_line() +
      geom_point(size=1) +
      labs(x="Number of cases per day",y="Total cases",title="Analysis of last wave COVID cases NSW and VIC") +
      theme_minimal()

saveRDS(Figure_1, file = "movil_average.rds")

# Calculate the cuadratic error
Analysis_MovilAvg <- Analisys_cases %>% select(New,Cases_Movil.Average,factor) %>% 
  filter(factor=="Movil Average NSW Cases") %>% 
  mutate(Error = round(Cases_Movil.Average - New,2)) %>%
  mutate(Error_2 = round(Error*Error,2))

# Cuadratic error calculated
saveRDS(Analysis_MovilAvg, file = "Error_table")

Mean_Error_2 <- mean(Analysis_MovilAvg$Error_2)
saveRDS(Mean_Error_2, file = "Sum_Error")

# Question 2 -------------------------------------------------------------------

# Grab data from website about vaccination
grab.Vaccination_data <- function(url){
  
  html <- read_html(url)

  html %>% html_elements("td") %>%
  html_text(trim=TRUE) -> Vaccination
  
  html %>% html_elements("h2") %>%
  html_text(trim=TRUE) -> title_Vacc

  Clean_data <- Vaccination[-c(1:8)]
  
  # Define a vector with population per each state of Australia
  Population_State <- data.frame(State = c("NSW","VIC","QLD","SA","WA","TAS","NT","ACT","Second"),
                               Population = as.numeric(c("8176400","6648600","5206400","1771700","2675800","542000","247000","431800","25704300")))
  # Clean data
  State_evaluate <- unlist(strsplit(title_Vacc, split = " "))[1]
  
  # Filter data per state and select
  Total_population_state <- Population_State %>% filter(State == State_evaluate)
  Total_population_state <- as.numeric(Total_population_state[2])

  matrix_data <- matrix(Clean_data, ncol = 5, byrow = TRUE)
  data <- as.data.frame(matrix_data, stringsAsFactors=FALSE)
  colnames(data) <- c("Date","Second","Percentage")
  data$Factor <- title_Vacc
  
  # Clean data and multiply number of vaccinated per 100 thousand to compare information
  Clean_data <- data %>% select(Date,Second,Factor) %>%
    mutate(Date = as.Date(Date, format = "%d %B %y")) %>%
    mutate(Second = as.numeric(gsub(",", "", Second))) %>%
    mutate(Percentage_vacc = Second/Total_population_state) %>%
    mutate(Vaccination_perHun = Percentage_vacc*100000)
  
  Clean_data_order <- Clean_data[order(Clean_data$Date),]
  Clean_data_order$Days_of_vaccination <- 1:nrow(Clean_data_order)

  data.model <- Clean_data_order %>% select(Second,Days_of_vaccination) %>%
    filter(Days_of_vaccination>=150)
  
  # Regression model to forecast number of vaccination per State
  Model <- lm(Second ~ Days_of_vaccination,data.model)
  summ_model <- summary(Model)
  
  # Extract the coefficients to make the equation to calculate 
  Extrac_coef <- unlist(summ_model[4])
  coeficient_1 <- Extrac_coef[1]
  coeficient_2 <- Extrac_coef[2]

  last_point <- as.numeric(tail(Clean_data_order, n=1))
  last_point[6]
  
  Number_prediction <- 0
  Total <- 0

  x <- 100
  
  # Calculate the forecast
  Value_forcast <- sapply(seq(last_point[6],last_point[6]+x-1),function(i) (coeficient_1 + coeficient_2*i))
  
  last_date <- tail(Clean_data_order, n=1) %>% 
    select(Date,Second,Factor,Percentage_vacc,Percentage_vacc,Vaccination_perHun) %>%
    mutate(Factor = paste("Forecast",State_evaluate))
  
  # Evaluate umber of people vaccinated until 100% and define the exact date
  Forecast <- data.frame(last_date[1],r=seq(1:x)) %>% 
    mutate(Day_forecast = Date+r) %>% 
    select(Day_forecast) %>%
    mutate(value.Forecast = Value_forcast ) %>%
    mutate(Forecast_Factor = paste("Forecast",State_evaluate)) %>%
    mutate(Percentage_va = value.Forecast/Total_population_state) %>%
    mutate(Vacc_PH = Percentage_va*100000) %>%
    filter(Percentage_va <= 1)
  
  new_dataFrame <- data.frame(Date = c(Clean_data_order$Date,Forecast$Day_forecast,last_date$Date),
                  Cases_Forecast = c(Clean_data_order$Second,Forecast$value.Forecast,last_date$Second),
                  Factor = c(Clean_data_order$Factor,Forecast$Forecast_Factor,last_date$Factor),
                  Percentage_vaccinaton = c(Clean_data_order$Percentage_vacc,Forecast$Percentage_va,last_date$Percentage_vacc),
                  Vaccination_PerHundred = c(Clean_data_order$Vaccination_perHun,Forecast$Vacc_PH,last_date$Vaccination_perHun))
  
  return(new_dataFrame)
}
  
urls <- c("https://covidlive.com.au/report/daily-vaccinations-people/vic",
          "https://covidlive.com.au/report/daily-vaccinations-people/qld",
          "https://covidlive.com.au/report/daily-vaccinations-people/nsw",
          "https://covidlive.com.au/report/daily-vaccinations-people/act",
          "https://covidlive.com.au/report/daily-vaccinations-people/wa",
          "https://covidlive.com.au/report/daily-vaccinations-people/sa",
          "https://covidlive.com.au/report/daily-vaccinations-people/nt",
          "https://covidlive.com.au/report/daily-vaccinations-people/tas")
          #"https://covidlive.com.au/report/daily-vaccinations-people/aus")

result <- foreach(url = urls, .combine = rbind) %do% {
  print(url)
  print("=======")
  grab.Vaccination_data(url)
  
}

result

saveRDS(result, file = "Vaccination_per_State.rds")

# Define a date to clarify the informacion and have a better data visualization
x <- result %>% filter(Date >= as.Date("2021-09-21"))

# Visualization of people vaccinate (percentage)
Figure_2 <- qplot(Date,Percentage_vaccinaton ,data=x,colour=Factor)+
  geom_line()+
  geom_point(size=1) +
  labs(x="Number of cases per day",y="Percentage of people vaccinated",title="Analysis percentage of people vaccinated in Australia") +
  theme_minimal() +
  scale_y_log10(labels=comma,breaks=c(0.45,0.5,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,1), limits=c(0.45,1)) +
  geom_hline(aes(yintercept=0.9), colour="#990000", linetype="dashed",size=1) +
  geom_hline(aes(yintercept=1), colour="#990000", linetype="dashed",size=1) +
  geom_hline(aes(yintercept=0.8), colour="#990000", linetype="dashed",size=1)

saveRDS(Figure_2, file = "Figure_2.rds")

# Visualization of people vaccinated (qty of people)
Figure_3 <- qplot(Date,Vaccination_PerHundred ,data=x,colour=Factor) +
  geom_line() +
  scale_y_continuous(labels=comma)+
  geom_point(size=1) +
  labs(x="Number of cases per day",y="Total number of people vaccinated (per hundred thousand)",title="Analysis of total number people vaccinated in Australia") +
  theme_minimal() +
  geom_hline(aes(yintercept=90000), colour="#990000", linetype="dashed",size=1)

saveRDS(Figure_3, file = "Figure_3.rds")

# Define number date which each State reach 80%, 90% and hundred
grab.hit.vaccination <- function(State_vac){

  current_state <- State_vac
  
  Betw_80_100 <- result %>% 
    filter(Percentage_vaccinaton >= 0.8 & Percentage_vaccinaton <= 1) %>%
    filter(Factor == current_state)
  
  Vaccinated_100 <- tail(Betw_80_100, n=1)
  Vaccinated_80 <- Betw_80_100[1,, drop = FALSE]
  
  Vac_90 <- result %>% 
    filter(Percentage_vaccinaton >= 0.9 & Percentage_vaccinaton <= 1) %>%
    filter(Factor == current_state)
  
  Vaccinated_90 <- Vac_90[1,, drop = FALSE]
  
  dataframe_hit <- data.frame(Date = c(Vaccinated_100$Date,Vaccinated_90$Date,Vaccinated_80$Date),
                              Forecast_people_vaccinated = c(Vaccinated_100$Cases_Forecast,Vaccinated_90$Cases_Forecast,Vaccinated_80$Cases_Forecast),
                              Factor=c(Vaccinated_100$Factor,Vaccinated_90$Factor,Vaccinated_80$Factor),
                              Percentage_hit = c(Vaccinated_100$Percentage_vaccinaton,Vaccinated_90$Percentage_vaccinaton,Vaccinated_80$Percentage_vaccinaton))

  return(dataframe_hit)
}

States = c("Forecast VIC","Forecast QLD","Forecast NSW","Forecast ACT","Forecast WA","Forecast SA",
              "Forecast NT","Forecast TAS")

#States = c("Forecast VIC")

hit_v <- foreach(State_vac = States, .combine = rbind) %do% {
  print(State_vac)
  print("=======")
  grab.hit.vaccination(State_vac)
  
}
hit_v

saveRDS(hit_v, file = "hit.rds")

# Question 3 -------------------------------------------------------------------

grab.cases_State <- function(url_cases){

  html <- read_html(url_cases)
  
  html %>% html_elements("td") %>%
    html_text(trim=TRUE) -> raw_data
  
  html %>% html_elements("h2") %>%
    html_text(trim=TRUE) -> title
  
  Clean_data <- raw_data[-c(1:8)]
  
  matrix_data <- matrix(Clean_data, ncol = 5, byrow = TRUE)
  data <- as.data.frame(matrix_data, stringsAsFactors=FALSE)
  colnames(data) <- c("Date","New","Cases")
  data$State <- title
  
  Clean_data <- data %>% select(Date,New,State) %>%
    mutate(Date = as.Date(Date, format = "%d %B %y")) %>%
    mutate(New = as.numeric(gsub(",", "", New)))
  
  Clean_data <- na.omit(Clean_data)
  Clean_data_order <- Clean_data[order(Clean_data$Date),]
  
  return(Clean_data_order)
}

Urls_c = c("https://covidlive.com.au/report/daily-cases/vic",
           "https://covidlive.com.au/report/daily-cases/qld",
           "https://covidlive.com.au/report/daily-cases/nsw",
           "https://covidlive.com.au/report/daily-cases/act",
           "https://covidlive.com.au/report/daily-cases/wa",
           "https://covidlive.com.au/report/daily-cases/sa",
           "https://covidlive.com.au/report/daily-cases/nt",
           "https://covidlive.com.au/report/daily-cases/tas")

Cases_per_states_p <- foreach(url_cases = Urls_c, .combine = rbind) %do% {
  print(url_cases)
  print("=======")
  grab.cases_State(url_cases)
  
}

Cases_per_states_p


grab.Vaccination_State_p <- function(url_Vac.st){

  html <- read_html(url_Vac.st)
  
  html %>% html_elements("td") %>%
    html_text(trim=TRUE) -> Vaccination
  
  html %>% html_elements("h2") %>%
    html_text(trim=TRUE) -> title_Vacc
  
  Clean_data <- Vaccination[-c(1:8)]
  
  matrix_data <- matrix(Clean_data, ncol = 5, byrow = TRUE)
  data <- as.data.frame(matrix_data, stringsAsFactors=FALSE)
  colnames(data) <- c("Date","Second","Percentage")
  data$Factor <- title_Vacc
  
  Clean_data <- data %>% select(Date,Second,Factor) %>%
    mutate(Date = as.Date(Date, format = "%d %B %y")) %>%
    mutate(Second = as.numeric(gsub(",", "", Second))) %>%
    mutate(Second = Second/2000)
  
  return(Clean_data)
}

Qty_vacs <- c("https://covidlive.com.au/report/daily-vaccinations-people/vic",
              "https://covidlive.com.au/report/daily-vaccinations-people/qld",
              "https://covidlive.com.au/report/daily-vaccinations-people/nsw",
              "https://covidlive.com.au/report/daily-vaccinations-people/act",
              "https://covidlive.com.au/report/daily-vaccinations-people/wa",
              "https://covidlive.com.au/report/daily-vaccinations-people/sa",
              "https://covidlive.com.au/report/daily-vaccinations-people/nt",
              "https://covidlive.com.au/report/daily-vaccinations-people/tas")

Vaccinated_per_states_p <- foreach(url_Vac.st = Qty_vacs, .combine = rbind) %do% {
  print(url_Vac.st)
  print("=======")
  grab.Vaccination_State_p(url_Vac.st)
  
}

Cases_per_states_p
Vaccinated_per_states_p

Relation_vac_cases <- data.frame(Date=c(Cases_per_states_p$Date,Vaccinated_per_states_p$Date),
                                 Vaccination_and_cases=c(Cases_per_states_p$New,Vaccinated_per_states_p$Second),
                                 Factor_vac_cases=c(Cases_per_states_p$State,Vaccinated_per_states_p$Factor))

saveRDS(Relation_vac_cases, file = "Relation_variables.rds")

Relation_vac_cases_1 <- Relation_vac_cases %>%
  group_by(Date.i = lubridate::floor_date(Date,"week"),Factor_vac_cases) %>%
  summarize(total_cases.vac_day = sum(Vaccination_and_cases)) %>%
  filter(Date.i >= "2021-07-01" & Date.i < "2021-11-10")

# c("NSW Cases","NSW Second Doses")

target1 <- c("VIC Cases","VIC Second Doses")
target2 <- c("NSW Cases","NSW Second Doses")
target3 <- c("SA Cases","SA Second Doses")
target4 <- c("WA Cases","WA Second Doses")
target5 <- c("QLD Cases","QLD Second Doses")
target6 <- c("NT Cases","NT Second Doses")
target7 <- c("TAS Cases","TAS Second Doses")
target8 <- c("ACT Cases","ACT Second Doses")

Rel_Analysis_VIC <- filter(Relation_vac_cases_1, Factor_vac_cases %in% target1)
Rel_Analysis_NSW <- filter(Relation_vac_cases_1, Factor_vac_cases %in% target2)
Rel_Analysis_SA <- filter(Relation_vac_cases_1, Factor_vac_cases %in% target3)
Rel_Analysis_WA <- filter(Relation_vac_cases_1, Factor_vac_cases %in% target4)
Rel_Analysis_QLD <- filter(Relation_vac_cases_1, Factor_vac_cases %in% target5)
Rel_Analysis_NT <- filter(Relation_vac_cases_1, Factor_vac_cases %in% target6)
Rel_Analysis_TAS <- filter(Relation_vac_cases_1, Factor_vac_cases %in% target7)
Rel_Analysis_ACT <- filter(Relation_vac_cases_1, Factor_vac_cases %in% target8)

Cases_data_disp_VIC <- Rel_Analysis_VIC %>% filter(Factor_vac_cases == target1[1])
Cases_data_disp_NSW <- Rel_Analysis_NSW %>% filter(Factor_vac_cases == target2[1])
Cases_data_disp_SA <- Rel_Analysis_SA %>% filter(Factor_vac_cases == target3[1])
Cases_data_disp_WA <- Rel_Analysis_WA %>% filter(Factor_vac_cases == target4[1])
Cases_data_disp_QLD <- Rel_Analysis_QLD %>% filter(Factor_vac_cases == target5[1])
Cases_data_disp_NT <- Rel_Analysis_NT %>% filter(Factor_vac_cases == target6[1])
Cases_data_disp_TAS <- Rel_Analysis_TAS %>% filter(Factor_vac_cases == target7[1])
Cases_data_disp_ACT <- Rel_Analysis_ACT %>% filter(Factor_vac_cases == target8[1])

vacc_data_disp_VIC <- Rel_Analysis_VIC %>% filter(Factor_vac_cases == target1[2])
vacc_data_disp_NSW <- Rel_Analysis_NSW %>% filter(Factor_vac_cases == target2[2])
vacc_data_disp_SA <- Rel_Analysis_SA %>% filter(Factor_vac_cases == target3[2])
vacc_data_disp_WA <- Rel_Analysis_WA %>% filter(Factor_vac_cases == target4[2])
vacc_data_disp_QLD <- Rel_Analysis_QLD %>% filter(Factor_vac_cases == target5[2])
vacc_data_disp_NT <- Rel_Analysis_NT %>% filter(Factor_vac_cases == target6[2])
vacc_data_disp_TAS <- Rel_Analysis_TAS %>% filter(Factor_vac_cases == target7[2])
vacc_data_disp_ACT <- Rel_Analysis_ACT %>% filter(Factor_vac_cases == target8[2])

new_data_disp_VIC <- merge(x = Cases_data_disp_VIC, y = vacc_data_disp_VIC, by = c("Date.i"))
new_data_disp_NSW <- merge(x = Cases_data_disp_NSW, y = vacc_data_disp_NSW, by = c("Date.i"))
new_data_disp_SA <- merge(x = Cases_data_disp_SA, y = vacc_data_disp_SA, by = c("Date.i"))
new_data_disp_WA <- merge(x = Cases_data_disp_WA, y = vacc_data_disp_WA, by = c("Date.i"))
new_data_disp_QLD <- merge(x = Cases_data_disp_QLD, y = vacc_data_disp_QLD, by = c("Date.i"))
new_data_disp_TAS <- merge(x = Cases_data_disp_TAS, y = vacc_data_disp_TAS, by = c("Date.i"))
new_data_disp_ACT <- merge(x = Cases_data_disp_ACT, y = vacc_data_disp_ACT, by = c("Date.i"))
new_data_disp_NT <- merge(x = Cases_data_disp_NT, y = vacc_data_disp_NT, by = c("Date.i"))

Analysis_VIC <- qplot(Date.i,total_cases.vac_day,data=Rel_Analysis_VIC,colour=Factor_vac_cases)+
  geom_line() +
  geom_point() +
  labs(x="Days than 100th confirmed case",y="Cumulative cases",title="Cumulative cases count-log scale")+
  theme_minimal()

saveRDS(Analysis_VIC, file = "VIC_R")

Analysis_NSW <- qplot(Date.i,total_cases.vac_day,data=Rel_Analysis_NSW,colour=Factor_vac_cases)+
  geom_line() +
  geom_point() +
  labs(x="Days than 100th confirmed case",y="Cumulative cases",title="Cumulative cases count-log scale")+
  theme_minimal()

saveRDS(Analysis_NSW, file = "NSW_R")

Analysis_SA <- qplot(Date.i,total_cases.vac_day,data=Rel_Analysis_SA,colour=Factor_vac_cases)+
  geom_line() +
  geom_point() +
  labs(x="Days than 100th confirmed case",y="Cumulative cases",title="Cumulative cases count-log scale")+
  theme_minimal()

Analysis_WA <- qplot(Date.i,total_cases.vac_day,data=Rel_Analysis_WA,colour=Factor_vac_cases)+
  geom_line() +
  geom_point() +
  labs(x="Days than 100th confirmed case",y="Cumulative cases",title="Cumulative cases count-log scale")+
  theme_minimal()

Analysis_QLD <- qplot(Date.i,total_cases.vac_day,data=Rel_Analysis_QLD,colour=Factor_vac_cases)+
  geom_line() +
  geom_point() +
  labs(x="Days than 100th confirmed case",y="Cumulative cases",title="Cumulative cases count-log scale")+
  theme_minimal()

Analysis_TAS <- qplot(Date.i,total_cases.vac_day,data=Rel_Analysis_TAS,colour=Factor_vac_cases)+
  geom_line() +
  geom_point() +
  labs(x="Days than 100th confirmed case",y="Cumulative cases",title="Cumulative cases count-log scale")+
  theme_minimal()

Analysis_ACT <- qplot(Date.i,total_cases.vac_day,data=Rel_Analysis_ACT,colour=Factor_vac_cases)+
  geom_line() +
  geom_point() +
  labs(x="Days than 100th confirmed case",y="Cumulative cases",title="Cumulative cases count-log scale")+
  theme_minimal()

saveRDS(Analysis_ACT, file = "ACT_R")

Analysis_NT <- qplot(Date.i,total_cases.vac_day,data=Rel_Analysis_NT,colour=Factor_vac_cases)+
  geom_line() +
  geom_point() +
  labs(x="Days than 100th confirmed case",y="Cumulative cases",title="Cumulative cases count-log scale")+
  theme_minimal()

Dis_VIC <- ggplot(new_data_disp_VIC, aes(total_cases.vac_day.x, total_cases.vac_day.y)) +
  geom_point() +
  theme_minimal() + 
  geom_smooth() +
  labs(x="Number of cases",y="people vaccinated",title="Scatter plot, Analysis between Vaccination and number of cases")

saveRDS(Dis_VIC, file = "VIC_D")

Dis_NSW <- ggplot(new_data_disp_NSW, aes(total_cases.vac_day.x, total_cases.vac_day.y)) +
  geom_point() +
  theme_minimal() + 
  geom_smooth() +
  labs(x="Number of cases",y="people vaccinated",title="Scatter plot, Analysis between Vaccination and number of cases")

saveRDS(Dis_NSW, file = "NSW_D")

Dis_SA <- ggplot(new_data_disp_SA, aes(total_cases.vac_day.x, total_cases.vac_day.y)) +
  geom_point() +
  theme_minimal() + 
  geom_smooth() +
  labs(x="Number of cases",y="people vaccinated",title="Scatter plot, Analysis between Vaccination and number of cases")

Dis_WA <- ggplot(new_data_disp_WA, aes(total_cases.vac_day.x, total_cases.vac_day.y)) +
  geom_point() +
  theme_minimal() + 
  geom_smooth() +
  labs(x="Number of cases",y="people vaccinated",title="Scatter plot, Analysis between Vaccination and number of cases")

Dis_QLD <- ggplot(new_data_disp_QLD, aes(total_cases.vac_day.x, total_cases.vac_day.y)) +
  geom_point() +
  theme_minimal() + 
  geom_smooth() +
  labs(x="Number of cases",y="people vaccinated",title="Scatter plot, Analysis between Vaccination and number of cases")

Dis_TAS <- ggplot(new_data_disp_TAS, aes(total_cases.vac_day.x, total_cases.vac_day.y)) +
  geom_point() +
  theme_minimal() + 
  geom_smooth() +
  labs(x="Number of cases",y="people vaccinated",title="Scatter plot, Analysis between Vaccination and number of cases")

Dis_ACT <- ggplot(new_data_disp_ACT, aes(total_cases.vac_day.x, total_cases.vac_day.y)) +
  geom_point() +
  theme_minimal() + 
  geom_smooth() +
  labs(x="Number of cases",y="people vaccinated",title="Scatter plot, Analysis between Vaccination and number of cases")

saveRDS(Dis_ACT, file = "ACT_D")

Dis_NT <- ggplot(new_data_disp_NT, aes(total_cases.vac_day.x, total_cases.vac_day.y)) +
  geom_point() +
  theme_minimal() + 
  geom_smooth() +
  labs(x="Number of cases",y="people vaccinated",title="Scatter plot, Analysis between Vaccination and number of cases")

first_cal <- grid.arrange(arrangeGrob(Analysis_VIC, Dis_VIC, ncol=2), arrangeGrob(Analysis_NSW, Dis_NSW, ncol=2),
             arrangeGrob(Analysis_SA, Dis_SA, ncol=2),arrangeGrob(Analysis_QLD, Dis_QLD, ncol=2), nrow = 4)

saveRDS(first_cal, file = "Pregraph_1.rds")


Second_cal <- grid.arrange(arrangeGrob(Analysis_WA, Dis_WA, ncol=2), arrangeGrob(Analysis_ACT, Dis_ACT, ncol=2),
             arrangeGrob(Analysis_NT, Dis_NT, ncol=2),arrangeGrob(Analysis_TAS, Dis_TAS, ncol=2), nrow = 4)

getwd()
