report<-all_reports[9]
values<-as.numeric(as.character(report_values$varvalue[report_values$varname==report]))
time<-as.numeric(as.character(report_values$xTime[report_values$varname==report]))
dataframe<-data.frame(time,values)

#write.csv2(x = dataframe, file = paste(report,".csv",sep=""))

a<-read.table("..\\new_cases.csv")



grafica1 <- plot_ly(dataframe, x = ~dataframe$time, y = ~dataframe$values, type = 'scatter', mode = 'lines', name=paste(report))
grafica1 <- add_trace(grafica1, x = c(1:185),y = as.numeric(as.character(a$V1)), name = "python", line = list(color = 'rgb(222, 96, 67)', width = 2)) 

grafica1 %>% layout(title = report,
                    xaxis = list(title = "Time"),
                    yaxis = list (title = report))

grafica <- paste(report,"_plotly.html",sep="")
htmlwidgets::saveWidget(as.widget(grafica1), grafica,selfcontained = FALSE)