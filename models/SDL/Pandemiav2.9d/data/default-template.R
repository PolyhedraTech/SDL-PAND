

# installem els paquets si no estan instal·lats
list.of.packages <- c("runner", "dplyr","plotly", "htmlwidgets", "XML")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(runner)
library(dplyr)

report<-"newInfectiveDetected"
values<-as.numeric(as.character(report_values$varvalue[report_values$varname==report]))
time<-as.numeric(as.character(report_values$xTime[report_values$varname==report]))
# Calculo del dt
dt=time[time>=2 & time<3]
values<-sum_run(x=values,k=length(dt))

dataframe<-data.frame(time,values)
write.csv2(x = dataframe, file = paste("newInfectiveDetectedDiary",".csv",sep=""))

#write.csv2(x = dataframe, file = paste(report,".csv",sep=""))

# a<-read.table("..\\new_cases.csv")
casosDiaris<-read.csv("..\\casosDiaris.csv")
grafica1 <- plot_ly(type="bar", 
                    x=c(1:length(casosDiaris$nous_casos_confirmats)+30),
                    y = casosDiaris$nous_casos_confirmats,
                    name = 'observations',
                    text = paste("new cases"))

grafica1 <- add_trace(grafica1, x = ~dataframe$time, y = ~dataframe$values, type = 'scatter', mode = 'lines', name=paste(report))
# grafica1 <- add_trace(grafica1, x = c(1:185),y = as.numeric(as.character(a$V1)), mode = 'lines', type = 'scatter', name = "python", line = list(color = 'rgb(22, 196, 67)',width = 2)) 

grafica1 %>% layout(title = report,
                    xaxis = list(title = "Time"),
                    yaxis = list (title = report))

grafica <- paste(report,"2_plotly.html",sep="")
htmlwidgets::saveWidget(as.widget(grafica1), grafica,selfcontained = FALSE)

############# denominador ##########################

susc<-as.numeric(as.character(report_values$varvalue[report_values$varname=="susceptible"]))[1]
grafica1 <- plot_ly(type="bar", 
                    x=c(1:length(casosDiaris$nous_casos_confirmats)+30),
                    y = casosDiaris$nous_casos_confirmats,
                    name = 'observations',
                    text = paste("new cases"))

grafica1 <- add_trace(grafica1, x = ~dataframe$time, y = ~dataframe$values/susc, type = 'scatter', mode = 'lines', name=paste(report))
# grafica1 <- add_trace(grafica1, x = c(1:185),y = as.numeric(as.character(a$V1)), mode = 'lines', type = 'scatter', name = "python", line = list(color = 'rgb(22, 196, 67)',width = 2)) 

grafica1 %>% layout(title = report,
                    xaxis = list(title = "Time"),
                    yaxis = list (title = report))

grafica <- paste(report,"_perPoblacio_plotly.html",sep="")
htmlwidgets::saveWidget(as.widget(grafica1), grafica,selfcontained = FALSE)



# report<-"newInfective"
# values<-as.numeric(as.character(report_values$varvalue[report_values$varname==report]))
# time<-as.numeric(as.character(report_values$xTime[report_values$varname==report]))
# # Calculo del dt
# dt=time[time>=2 & time<3]
# values<-sum_run(x=values,k=length(dt))
# values<-sum_run(x=values,k=7)
# 
# dataframe<-data.frame(time,values)
# 
# #write.csv2(x = dataframe, file = paste(report,".csv",sep=""))
# 
# a<-read.table("..\\new_cases.csv")
# casosDiaris<-read.csv("..\\casosDiaris.csv")
# grafica1 <- plot_ly(type="bar", 
#                     x=c(1:length(casosDiaris$nous_casos_confirmats)+30),
#                     y = casosDiaris$nous_casos_confirmats,
#                     name = 'observations',
#                     text = paste("new cases"))
# 
# grafica1 <- add_trace(grafica1, x = ~dataframe$time, y = ~dataframe$values, type = 'scatter', mode = 'lines', name=paste(report))
# grafica1 <- add_trace(grafica1, x = c(1:185),y = as.numeric(as.character(a$V1)), mode = 'lines', type = 'scatter', name = "python", line = list(color = 'rgb(22, 196, 67)',width = 2)) 
# 
# grafica1 %>% layout(title = report,
#                     xaxis = list(title = "Time"),
#                     yaxis = list (title = report))
# 
# grafica <- paste(report,"_sum7_plotly.html",sep="")
# htmlwidgets::saveWidget(as.widget(grafica1), grafica,selfcontained = FALSE)

######################## Detectats Vs Reals #######################################

report<-"newInfectiveDetected"
values<-as.numeric(as.character(report_values$varvalue[report_values$varname==report]))
time<-as.numeric(as.character(report_values$xTime[report_values$varname==report]))
# Calculo del dt
dt=time[time>=2 & time<3]
values<-sum_run(x=values,k=length(dt))

dataframe<-data.frame(time,values)

reportInfective<-"newInfective"
valuesInfective<-as.numeric(as.character(report_values$varvalue[report_values$varname==reportInfective]))
timeInfective<-as.numeric(as.character(report_values$xTime[report_values$varname==reportInfective]))
# Calculo del dt
dt=timeInfective[timeInfective>=2 & timeInfective<3]
valuesInfective<-sum_run(x=valuesInfective,k=length(dt))

dataframeInfective<-data.frame(timeInfective,valuesInfective)


#write.csv2(x = dataframe, file = paste(report,".csv",sep=""))

# a<-read.table("..\\new_cases.csv")
casosDiaris<-read.csv("..\\casosDiaris.csv")
grafica1 <- plot_ly(type="bar", 
                    x=c(1:length(casosDiaris$nous_casos_confirmats)+30),
                    y = casosDiaris$nous_casos_confirmats,
                    name = 'observations',
                    text = paste("new cases"))

grafica1 <- add_trace(grafica1, x = ~dataframe$time, y = ~dataframe$values, type = 'scatter', mode = 'lines', name=paste(report))

grafica1 <- add_trace(grafica1, x = ~dataframeInfective$timeInfective, y = ~dataframeInfective$valuesInfective, type = 'scatter', mode = 'lines', name=paste(reportInfective))

grafica1 %>% layout(title = report,
                    xaxis = list(title = "Time"),
                    yaxis = list (title = report))

#grafica <- paste(report,"comparative_plotly.html",sep="")
htmlwidgets::saveWidget(as.widget(grafica1), "comparative_plotly.html",selfcontained = FALSE)