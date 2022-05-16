report<-"newInfectiveDetected"

values <-as.numeric(as.character(report_values$varvalue[report_values$varname == report]))
time <-as.numeric(as.character(report_values$xTime[report_values$varname == report]))
report_stage <-report_values$label[report_values$varname == report]

dataframe <-data.frame(time, report_stage, values)

stage=experiments[1]
report_by_stage<-dataframe[dataframe$report_stage==stage,]
population<-as.numeric(as.character(report_values$varvalue[report_values$varname=="susceptible" & report_values$label==stage]))[1]
grafica1 <- plot_ly(report_by_stage, x = report_by_stage$time, y = report_by_stage$values/population, type = 'scatter', mode = 'lines', name=stage)

n_length<-length(experiments)-1
for (i in (1:n_length)) {
  population<-as.numeric(as.character(report_values$varvalue[report_values$varname=="susceptible" & report_values$label==experiments[i+1]]))[1]
  grafica1 <- add_trace(grafica1, x = dataframe[dataframe$report_stage==experiments[i+1],]$time,y = dataframe[dataframe$report_stage==experiments[i+1],]$values/population, name = experiments[i+1], line = list(color = 'rgb(22, 96, 167)', width = 2)) 
}

grafica1 %>% layout(title = report,
                    xaxis = list(title = "Time"),
                    yaxis = list (title = report))
grafica <- paste(report,"perPopulation_plotly.html",sep="")

htmlwidgets::saveWidget(as_widget(grafica1), grafica,selfcontained = FALSE)