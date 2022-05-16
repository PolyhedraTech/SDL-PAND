

path<-getwd()
# insightmaker1 -> 1 de delay
# insightmaker -> 0.25 de delay
dataPath <- paste(path,"\\..\\insightmaker.csv",sep="")
insightMaker<- read.csv2(dataPath, header = TRUE, sep = ",", dec=".", stringsAsFactors=FALSE)

########################### Para generar los csv ##########################################
write.csv2(x = data.frame(susceptiblexTime,susceptible), file = "susceptibleSDL.csv")
write.csv2(x = data.frame(totalInfectivexTime, totalInfective), file = "totalInfectiveSDL.csv")
write.csv2(x = data.frame(totalDeceasedxTime, totalDeceased), file = "totalDeceasedSDL.csv")
write.csv2(x = data.frame(totalRecoveredxTime, totalRecovered), file = "totalRecoveredSDL.csv")
write.csv2(x = data.frame(totalExposedxTime, totalExposed), file = "totalExposedSDL.csv")
###########################################################################################



library('plotly')
library('htmlwidgets')



#############################################  susceptible   #############################################

path<-getwd()

grafica1 <- plot_ly(data.frame(susceptiblexTime,susceptible), x = ~susceptiblexTime, y = ~susceptible, type = 'scatter', mode = 'lines', name='Susceptible SDL')
grafica1 <- grafica1 %>% add_trace(x = ~insightMaker$Time,y = ~insightMaker$Susceptible, name = 'Susceptible Insight', line = list(color = 'rgb(22, 96, 167)', width = 4)) 

grafica <- paste(currCell,"susceptible.html",sep="")

htmlwidgets::saveWidget(as.widget(grafica1), grafica,selfcontained = FALSE)

#############################################################################################################


#############################################  totalInfective   #############################################

path<-getwd()

grafica1 <- plot_ly(data.frame(totalInfectivexTime,totalInfective), x = ~totalInfectivexTime, y = ~totalInfective, type = 'scatter', mode = 'lines', name='Infective SDL')
grafica1 <- grafica1 %>% add_trace(x = ~insightMaker$Time,y = ~insightMaker$Infective, name = 'Infective InisghtMaker', line = list(color = 'rgb(22, 96, 167)', width = 4)) 

grafica <- paste(currCell,"totalInfective.html",sep="")

htmlwidgets::saveWidget(as.widget(grafica1), grafica,selfcontained = FALSE)


#############################################################################################################

#############################################  totalDeceased   #############################################

path<-getwd()

grafica1 <- plot_ly(data.frame(totalDeceasedxTime,totalDeceased), x = ~totalDeceasedxTime, y = ~totalDeceased, type = 'scatter', mode = 'lines', name='Deceased SDL')
grafica1 <- grafica1 %>% add_trace(x = ~insightMaker$Time,y = ~insightMaker$Death, name = 'Deceased InisghtMaker', line = list(color = 'rgb(22, 96, 167)', width = 4)) 

grafica <- paste(currCell,"totalDeceased.html",sep="")

htmlwidgets::saveWidget(as.widget(grafica1), grafica,selfcontained = FALSE)

#############################################################################################################

#############################################  totalRecovered   #############################################

path<-getwd()

grafica1 <- plot_ly(data.frame(totalRecoveredxTime,totalRecovered), x = ~totalRecoveredxTime, y = ~totalRecovered, type = 'scatter', mode = 'lines', name='Recovered SDL')
grafica1 <- grafica1 %>% add_trace(x = ~insightMaker$Time,y = ~insightMaker$Recovered, name = 'Recovered InisghtMaker', line = list(color = 'rgb(22, 96, 167)', width = 4)) 

grafica <- paste(currCell,"totalRecovered.html",sep="")

htmlwidgets::saveWidget(as.widget(grafica1), grafica,selfcontained = FALSE)

#############################################################################################################


#############################################  totalExposed   #############################################

path<-getwd()

grafica1 <- plot_ly(data.frame(totalExposedxTime,totalExposed), x = ~totalExposedxTime, y = ~totalExposed, type = 'scatter', mode = 'lines', name='Exposed SDL')
grafica1 <- grafica1 %>% add_trace(x = ~insightMaker$Time,y = ~insightMaker$Exposed, name = 'Exposed InisghtMaker', line = list(color = 'rgb(22, 96, 167)', width = 4)) 

grafica <- paste(currCell,"totalExposed.html",sep="")

htmlwidgets::saveWidget(as.widget(grafica1), grafica,selfcontained = FALSE)

#############################################################################################################

totalExposedxTime[totalExposed==max(totalExposed)]

insightMaker$Time[insightMaker$Exposed==max(insightMaker$Exposed)]