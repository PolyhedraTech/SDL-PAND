
xTime <-xpathSApply(data, "//Events/EYE_Report/@xTime")
agent <-xpathSApply(data, "//Events/EYE_Report/@agent")
varname <-xpathSApply(data, "//Events/EYE_Report/@varname")
vartype <-xpathSApply(data, "//Events/EYE_Report/@vartype")
varvalue <-xpathSApply(data, "//Events/EYE_Report/@varvalue")
label <-xpathSApply(data, "//Events/EYE_Report/@label")
cell<-xpathSApply(data, "//Events/EYE_Report/@cell")
report_values<-data.frame(xTime, agent, varname, vartype, varvalue, label, cell)

currCellValues = as.numeric(as.character(report_values$varvalue[report_values$varname=="currCell"]))

totalInfective = as.numeric(as.character(report_values$varvalue[report_values$varname=="totalInfective"]))
totalInfectivexTime = as.numeric(as.character(report_values$xTime[report_values$varname=="totalInfective"]))
totalInfectiveCell = as.numeric(as.character(report_values$cell[report_values$varname=="totalInfective"]))

totalDeceased = as.numeric(as.character(report_values$varvalue[report_values$varname=="totalDeceased"]))
totalDeceasedxTime = as.numeric(as.character(report_values$xTime[report_values$varname=="totalDeceased"]))
totalDeceasedCell = as.numeric(as.character(report_values$cell[report_values$varname=="totalDeceased"]))

susceptible = as.numeric(as.character(report_values$varvalue[report_values$varname=="susceptible"]))
susceptiblexTime = as.numeric(as.character(report_values$xTime[report_values$varname=="susceptible"]))
susceptibleCell = as.numeric(as.character(report_values$cell[report_values$varname=="susceptible"]))

totalRecovered = as.numeric(as.character(report_values$varvalue[report_values$varname=="totalRecovered"]))
totalRecoveredxTime = as.numeric(as.character(report_values$xTime[report_values$varname=="totalRecovered"]))
totalRecoveredCell = as.numeric(as.character(report_values$cell[report_values$varname=="totalRecovered"]))

totalExposed = as.numeric(as.character(report_values$varvalue[report_values$varname=="totalExposed"]))
totalExposedxTime = as.numeric(as.character(report_values$xTime[report_values$varname=="totalExposed"]))
totalExposedCell = as.numeric(as.character(report_values$cell[report_values$varname=="totalExposed"]))


path<-getwd()

# Per afegir la gràfica de InsightMaker
#
# insightmaker1 -> 1 de delay
# insightmaker -> 0.25 de delay
# dataPath <- paste(path,"\\..\\insightmaker.csv",sep="")
# insightMaker<- read.csv2(dataPath, header = TRUE, sep = ",", dec=".", stringsAsFactors=FALSE)


susceptibleFrame<-data.frame(susceptiblexTime,susceptibleCell,susceptible)
totalInfectiveFrame<-data.frame(totalInfectivexTime,totalInfectiveCell,totalInfective)
totalDeceasedFrame<-data.frame(totalDeceasedxTime,totalDeceasedCell,totalDeceased)
totalRecoveredFrame<-data.frame(totalRecoveredxTime,totalRecoveredCell,totalRecovered)
totalExposedFrame<-data.frame(totalExposedxTime,totalExposedCell,totalExposed)

########################### Para generar los csv ##########################################
write.csv2(x = susceptibleFrame[order(susceptibleFrame$susceptibleCell),], file = "susceptibleSDL.csv")
write.csv2(x = totalInfectiveFrame[order(totalInfectiveFrame$totalInfectiveCell),], file = "totalInfectiveSDL.csv")
write.csv2(x = totalDeceasedFrame[order(totalDeceasedFrame$totalDeceasedCell),], file = "totalDeceasedSDL.csv")
write.csv2(x = totalRecoveredFrame[order(totalRecoveredFrame$totalRecoveredCell),], file = "totalRecoveredSDL.csv")
write.csv2(x = totalExposedFrame[order(totalExposedFrame$totalExposedCell),], file = "totalExposedSDL.csv")
###########################################################################################



library('plotly')
library('htmlwidgets')



#############################################  susceptible   #############################################

path<-getwd()

cell0<-susceptibleFrame[susceptibleFrame$susceptibleCell==0,]
cell1<-susceptibleFrame[susceptibleFrame$susceptibleCell==1,]
cell2<-susceptibleFrame[susceptibleFrame$susceptibleCell==2,]
cell3<-susceptibleFrame[susceptibleFrame$susceptibleCell==3,]


grafica1 <- plot_ly(cell0, x = ~cell0$susceptiblexTime, y = ~cell0$susceptible, type = 'scatter', mode = 'lines', name='Susceptible SDL - Cell - 0')
grafica1 <- grafica1 %>% add_trace(x = ~cell1$susceptiblexTime,y = ~cell1$susceptible, name = 'Susceptible SDL - Cell - 1', line = list(color = 'rgb(22, 96, 167)', width = 2)) 
grafica1 <- grafica1 %>% add_trace(x = ~cell2$susceptiblexTime,y = ~cell2$susceptible, name = 'Susceptible SDL - Cell - 2', line = list(color = 'rgb(22, 96, 167)', width = 2)) 
grafica1 <- grafica1 %>% add_trace(x = ~cell3$susceptiblexTime,y = ~cell3$susceptible, name = 'Susceptible SDL - Cell - 3', line = list(color = 'rgb(22, 96, 167)', width = 2)) 
# Per afegir la gràfica de InsightMaker
# grafica1 <- grafica1 %>% add_trace(x = ~insightMaker$Time,y = ~insightMaker$Susceptible, name = 'Susceptible Insight', line = list(color = 'rgb(22, 96, 167)', width = 4)) 

grafica <- "susceptible.html"

htmlwidgets::saveWidget(as.widget(grafica1), grafica,selfcontained = FALSE)

#############################################################################################################


#############################################  totalInfective   #############################################

cell0<-totalInfectiveFrame[totalInfectiveFrame$totalInfectiveCell==0,]
cell1<-totalInfectiveFrame[totalInfectiveFrame$totalInfectiveCell==1,]
cell2<-totalInfectiveFrame[totalInfectiveFrame$totalInfectiveCell==2,]
cell3<-totalInfectiveFrame[totalInfectiveFrame$totalInfectiveCell==3,]


grafica1 <- plot_ly(cell0, x = ~cell0$totalInfectivexTime, y = ~cell0$totalInfective, type = 'scatter', mode = 'lines', name='totalInfective SDL - Cell - 0')
grafica1 <- grafica1 %>% add_trace(x = ~cell1$totalInfectivexTime,y = ~cell1$totalInfective, name = 'totalInfective SDL - Cell - 1', line = list(color = 'rgb(22, 96, 167)', width = 2)) 
grafica1 <- grafica1 %>% add_trace(x = ~cell2$totalInfectivexTime,y = ~cell2$totalInfective, name = 'totalInfective SDL - Cell - 2', line = list(color = 'rgb(22, 96, 167)', width = 2)) 
grafica1 <- grafica1 %>% add_trace(x = ~cell3$totalInfectivexTime,y = ~cell3$totalInfective, name = 'totalInfective SDL - Cell - 3', line = list(color = 'rgb(22, 96, 167)', width = 2)) 
# Per afegir la gràfica de InsightMaker
#grafica1 <- grafica1 %>% add_trace(x = ~insightMaker$Time,y = ~insightMaker$Infective, name = 'totalInfective Insight', line = list(color = 'rgb(22, 96, 167)', width = 4)) 

grafica <- "totalInfective.html"

htmlwidgets::saveWidget(as.widget(grafica1), grafica,selfcontained = FALSE)


#############################################################################################################

#############################################  totalDeceased   #############################################

cell0<-totalDeceasedFrame[totalDeceasedFrame$totalDeceasedCell==0,]
cell1<-totalDeceasedFrame[totalDeceasedFrame$totalDeceasedCell==1,]
cell2<-totalDeceasedFrame[totalDeceasedFrame$totalDeceasedCell==2,]
cell3<-totalDeceasedFrame[totalDeceasedFrame$totalDeceasedCell==3,]


grafica1 <- plot_ly(cell0, x = ~cell0$totalDeceasedxTime, y = ~cell0$totalDeceased, type = 'scatter', mode = 'lines', name='totalDeceased SDL - Cell - 0')
grafica1 <- grafica1 %>% add_trace(x = ~cell1$totalDeceasedxTime,y = ~cell1$totalDeceased, name = 'totalDeceased SDL - Cell - 1', line = list(color = 'rgb(22, 96, 167)', width = 2)) 
grafica1 <- grafica1 %>% add_trace(x = ~cell2$totalDeceasedxTime,y = ~cell2$totalDeceased, name = 'totalDeceased SDL - Cell - 2', line = list(color = 'rgb(22, 96, 167)', width = 2)) 
grafica1 <- grafica1 %>% add_trace(x = ~cell3$totalDeceasedxTime,y = ~cell3$totalDeceased, name = 'totalDeceased SDL - Cell - 3', line = list(color = 'rgb(22, 96, 167)', width = 2)) 
# Per afegir la gràfica de InsightMaker
#grafica1 <- grafica1 %>% add_trace(x = ~insightMaker$Time,y = ~insightMaker$Death, name = 'totalDeceased Insight', line = list(color = 'rgb(22, 96, 167)', width = 4)) 

grafica <- "totalDeceased.html"

htmlwidgets::saveWidget(as.widget(grafica1), grafica,selfcontained = FALSE)

#############################################################################################################

#############################################  totalRecovered   #############################################

cell0<-totalRecoveredFrame[totalRecoveredFrame$totalRecoveredCell==0,]
cell1<-totalRecoveredFrame[totalRecoveredFrame$totalRecoveredCell==1,]
cell2<-totalRecoveredFrame[totalRecoveredFrame$totalRecoveredCell==2,]
cell3<-totalRecoveredFrame[totalRecoveredFrame$totalRecoveredCell==3,]


grafica1 <- plot_ly(cell0, x = ~cell0$totalRecoveredxTime, y = ~cell0$totalRecovered, type = 'scatter', mode = 'lines', name='totalRecovered SDL - Cell - 0')
grafica1 <- grafica1 %>% add_trace(x = ~cell1$totalRecoveredxTime,y = ~cell1$totalRecovered, name = 'totalRecovered SDL - Cell - 1', line = list(color = 'rgb(22, 96, 167)', width = 2)) 
grafica1 <- grafica1 %>% add_trace(x = ~cell2$totalRecoveredxTime,y = ~cell2$totalRecovered, name = 'totalRecovered SDL - Cell - 2', line = list(color = 'rgb(22, 96, 167)', width = 2)) 
grafica1 <- grafica1 %>% add_trace(x = ~cell3$totalRecoveredxTime,y = ~cell3$totalRecovered, name = 'totalRecovered SDL - Cell - 3', line = list(color = 'rgb(22, 96, 167)', width = 2)) 
# Per afegir la gràfica de InsightMaker
#grafica1 <- grafica1 %>% add_trace(x = ~insightMaker$Time,y = ~insightMaker$Recovered, name = 'totalRecovered Insight', line = list(color = 'rgb(22, 96, 167)', width = 4)) 

grafica <- "totalRecovered.html"

htmlwidgets::saveWidget(as.widget(grafica1), grafica,selfcontained = FALSE)


#############################################################################################################


#############################################  totalExposed   #############################################

cell0<-totalExposedFrame[totalExposedFrame$totalExposedCell==0,]
cell1<-totalExposedFrame[totalExposedFrame$totalExposedCell==1,]
cell2<-totalExposedFrame[totalExposedFrame$totalExposedCell==2,]
cell3<-totalExposedFrame[totalExposedFrame$totalExposedCell==3,]


grafica1 <- plot_ly(cell0, x = ~cell0$totalExposedxTime, y = ~cell0$totalExposed, type = 'scatter', mode = 'lines', name='totalExposed SDL - Cell - 0')
grafica1 <- grafica1 %>% add_trace(x = ~cell1$totalExposedxTime,y = ~cell1$totalExposed, name = 'totalExposed SDL - Cell - 1', line = list(color = 'rgb(22, 96, 167)', width = 2)) 
grafica1 <- grafica1 %>% add_trace(x = ~cell2$totalExposedxTime,y = ~cell2$totalExposed, name = 'totalExposed SDL - Cell - 2', line = list(color = 'rgb(22, 96, 167)', width = 2)) 
grafica1 <- grafica1 %>% add_trace(x = ~cell3$totalExposedxTime,y = ~cell3$totalExposed, name = 'totalExposed SDL - Cell - 3', line = list(color = 'rgb(22, 96, 167)', width = 2)) 
# Per afegir la gràfica de InsightMaker
#grafica1 <- grafica1 %>% add_trace(x = ~insightMaker$Time,y = ~insightMaker$Exposed, name = 'totalExposed Insight', line = list(color = 'rgb(22, 96, 167)', width = 4)) 

grafica <- "totalExposed.html"

htmlwidgets::saveWidget(as.widget(grafica1), grafica,selfcontained = FALSE)

#############################################################################################################
