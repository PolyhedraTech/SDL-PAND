## @knitr functions

currentModel="Pandemiav2.9h-CAT-Omni"
currentModel="Pandemiav2.10-Regions"
currentModel="Pandemiav2.10-OLD-Regions"
currentModel="Pandemiav2.11"


edarModel="Pandemiav2.11"

message(currentModel)

#Calculus of a movile average
ma <- function(arr, n=14){
  res = arr
  for(i in n:length(arr)){
    res[i] = mean(arr[(i-n+1):i])
  }
  res
}

#Calculus of the R_eff
Reff<-function(arr,table,n=14){
  mida=nrow(table)
  la_r=arr[mida]/arr[mida-n]
  la_r
}

#Càlcul de l'incidència (RInc)
RInc<-function(arr,table,n=14){
  mida=nrow(table)
  la_r=(arr[mida]*100000*n)/PoblacioTotal
  la_r
}

# https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
readUrl <- function(url,file) {
  out <- tryCatch(
    {
      ICRA_data <- read.csv(url, sep=";", header=TRUE, skip=1)
      write.csv2(ICRA_data, file="ICRA_dataACT.csv")
      return(ICRA_data)
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", url))
      message("Here's the original error message:")
      message(cond)
      
      ICRA_data <- read.csv2(file)
      # Choose a return value in case of error
      return(ICRA_data)
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", url))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      message(paste("Processed URL:", url))
      message("Some other message at the end")
    }
  )    
  return(out)
}
message("OK functions")

## @knitr plotIntro

if (regio=="")
{
  file = paste("../../models/SDL/",currentModel,"/regions-ALL/newInfectiveDetectedDiary.csv",sep="")
} else {
  file = paste(paste("../../models/SDL/",currentModel,"/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectivesDetected2.9 <- read.csv2(file)

index = c(0:((length(newInfectivesDetected2.9$cell_time)-1)/10))
index = index+newInfectivesDetected2.9$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectivesDetected2.9$cell_values[newInfectivesDetected2.9$cell_time %in% index]
times<-newInfectivesDetected2.9$cell_time[newInfectivesDetected2.9$cell_time %in% index]
dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
datetimes<-dt1+times
newInfectivesDetected2.9<-data.frame(datetimes,values)

fig <- plot_ly(type="bar", 
               x = casosDiaris$data,
               y = casosDiaris$nous_casos_confirmats,
               name = 'observations',
               text = paste("new cases"))

annotation1 <- list(yref = 'y', xref = "x", y = 300, x = "2020-03-14", text = "Estat d'alarma")
fig <- fig %>% add_trace(y = newInfectivesDetected2.9$values, x=newInfectivesDetected2.9$datetimes, type='scatter', mode = 'lines', name='Model 2.10 Detected') %>% layout(annotations= list(annotation1))

fig


## @knitr plotAll

if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.6/regions/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.6/regions/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectives2.6 <- read.csv2(file)

# El model sdlps comença el dia 29 de gener, seleccionem només els valors dels dts sense decimals, 1 de cada 10.
index = c(1:(length(newInfectives2.6$cell_time)/10))
dt1 <- rep(as.Date("2020-01-01 CET"),length(index))
datetimes<-dt1+newInfectives2.6$cell_time[index*10]
values<-newInfectives2.6$cell_values[index*10]
newInfectives2.6<-data.frame(datetimes,values)

if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.7/regions/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.7/regions/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectives2.7 <- read.csv2(file)

# El model sdlps comença el dia 29 de gener, seleccionem només els valors dels dts sense decimals, 1 de cada 10.
index = c(1:(length(newInfectives2.7$cell_time)/10))
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+newInfectives2.7$cell_time[index*10]
values<-newInfectives2.7$cell_values[index*10]
newInfectives2.7<-data.frame(datetimes,values)

if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.8-A/regions-ALL/newInfectiveDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-A/regions-ALL/",regio,sep=""),"_newInfectiveDiary.csv",sep="")
}

newInfectives2.8A <- read.csv2(file)

index = c(0:((length(newInfectives2.8A$cell_time)-1)/10))
index = index+newInfectives2.8A$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectives2.8A$cell_values[newInfectives2.8A$cell_time %in% index]
newInfectives2.8A<-data.frame(datetimes,values)

if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.8-A/regions-ALL/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-A/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectivesDetected2.8A <- read.csv2(file)

index = c(0:((length(newInfectivesDetected2.8A$cell_time)-1)/10))
index = index+newInfectivesDetected2.8A$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectivesDetected2.8A$cell_values[newInfectivesDetected2.8A$cell_time %in% index]
times<-newInfectivesDetected2.8A$cell_time[newInfectivesDetected2.8A$cell_time %in% index]
dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
datetimes<-dt1+times
newInfectivesDetected2.8A<-data.frame(datetimes,values)

if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.8-B/regions-ALL/newInfectiveDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-B/regions-ALL/",regio,sep=""),"_newInfectiveDiary.csv",sep="")
}

newInfectives2.8B <- read.csv2(file)

index = c(0:((length(newInfectives2.8B$cell_time)-1)/10))
index = index+newInfectives2.8B$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectives2.8B$cell_values[newInfectives2.8B$cell_time %in% index]
newInfectives2.8B<-data.frame(datetimes,values)

if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.8-B/regions-ALL/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-B/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectivesDetected2.8B <- read.csv2(file)

index = c(0:((length(newInfectivesDetected2.8B$cell_time)-1)/10))
index = index+newInfectivesDetected2.8B$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectivesDetected2.8B$cell_values[newInfectivesDetected2.8B$cell_time %in% index]
times<-newInfectivesDetected2.8B$cell_time[newInfectivesDetected2.8B$cell_time %in% index]
dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
datetimes<-dt1+times
newInfectivesDetected2.8B<-data.frame(datetimes,values)


if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.9/regions/newInfectiveDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.9/regions/",regio,sep=""),"_newInfectiveDiary.csv",sep="")
}

newInfectives2.9 <- read.csv2(file)

index = c(0:((length(newInfectives2.9$cell_time)-1)/10))
index = index+newInfectives2.9$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectives2.9$cell_values[newInfectives2.9$cell_time %in% index]
newInfectives2.9<-data.frame(datetimes,values)

if (regio=="")
{
  file = paste("../../models/SDL/Pandemiav2.9/regions/newInfectiveDetectedDiary.csv",sep="")
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-B/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectivesDetected2.9 <- read.csv2(file)

index = c(0:((length(newInfectivesDetected2.9$cell_time)-1)/10))
index = index+newInfectivesDetected2.9$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectivesDetected2.9$cell_values[newInfectivesDetected2.9$cell_time %in% index]
times<-newInfectivesDetected2.9$cell_time[newInfectivesDetected2.9$cell_time %in% index]
dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
datetimes<-dt1+times
newInfectivesDetected2.9<-data.frame(datetimes,values)

fig <- plot_ly(type="bar", 
               x = casosDiaris$data,
               y = casosDiaris$nous_casos_confirmats,
               name = 'observations',
               text = paste("new cases"))

annotation1 <- list(yref = 'y', xref = "x", y = 300, x = "2020-03-14", text = "Estat d'alarma")
annotation2 <- list(yref = 'y', xref = "x", y = 200, x = "2020-06-24", text = "Sant Joan")
fig <- fig %>% add_trace(y = newInfectives$values, x=newInfectives$datetimes, type='scatter', mode = 'lines', name='Model 1.9') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectives2.6$values, x=newInfectives2.6$datetimes, type='scatter', mode = 'lines', name='Model 2.6') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectives2.7$values, x=newInfectives2.7$datetimes, type='scatter', mode = 'lines', name='Model 2.7') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectives2.8A$values, x=newInfectives2.8A$datetimes, type='scatter', mode = 'lines', name='Model 2.8-A') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectivesDetected2.8A$values, x=newInfectivesDetected2.8A$datetimes, type='scatter', mode = 'lines', name='Model 2.8-A Detected') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectives2.8B$values, x=newInfectives2.8B$datetimes, type='scatter', mode = 'lines', name='Model 2.8-B') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectivesDetected2.8B$values, x=newInfectivesDetected2.8B$datetimes, type='scatter', mode = 'lines', name='Model 2.8-B Detected') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectives2.9$values, x=newInfectives2.9$datetimes, type='scatter', mode = 'lines', name='Model 2.9') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectivesDetected2.9$values, x=newInfectivesDetected2.9$datetimes, type='scatter', mode = 'lines', name='Model 2.9 Detected') %>% layout(annotations= list(annotation1,annotation2))

#https://plotly.com/r/text-and-annotations/

if (!onlyOneModel) {
  fig <- fig %>% add_trace(y = newInfectives2.5$values, x=newInfectives2.5$datetimes, type='scatter', mode = 'lines', name='Model 2.5') %>% layout(annotations= list(annotation1,annotation2))
  fig <- fig %>% add_trace(y = casosDiaris$model1.7, type='scatter', mode = 'lines', name='Model: Contention scenario')
}

fig

## @knitr plotModel

if (regio=="")
{
  file = paste("../../models/SDL/",currentModel,"/regions-ALL/newInfectiveDiary.csv",sep="")
} else {
  file = paste(paste("../../models/SDL/",currentModel,"/regions-ALL/",regio,sep=""),"_newInfectiveDiary.csv",sep="")
}

newInfectives2.9 <- read.csv2(file)

index = c(0:((length(newInfectives2.9$cell_time)-1)/10))
index = index+newInfectives2.9$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectives2.9$cell_values[newInfectives2.9$cell_time %in% index]
newInfectives2.9<-data.frame(datetimes,values)

if (regio=="")
{
  file = paste("../../models/SDL/",currentModel,"/regions-ALL/newInfectiveDetectedDiary.csv",sep="")
} else {
  file = paste(paste("../../models/SDL/",currentModel,"/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectivesDetected2.9 <- read.csv2(file)

index = c(0:((length(newInfectivesDetected2.9$cell_time)-1)/10))
index = index+newInfectivesDetected2.9$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectivesDetected2.9$cell_values[newInfectivesDetected2.9$cell_time %in% index]
times<-newInfectivesDetected2.9$cell_time[newInfectivesDetected2.9$cell_time %in% index]
dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
datetimes<-dt1+times
newInfectivesDetected2.9<-data.frame(datetimes,values)

if (regio=="")
{
  file = paste("../../models/SDL/",currentModel,"/regions-ALL/newExposedRecovered.csv",sep="")
} else {
  file = paste(paste("../../models/SDL/",currentModel,"/regions-ALL/",regio,sep=""),"_newExposedRecovered.csv",sep="")
}

newReinfections2.9 <- read.csv2(file)

index = c(0:((length(newReinfections2.9$cell_time)-1)/10))
index = index+newReinfections2.9$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index


fig <- plot_ly(type="bar", 
               x = casosDiaris$data,
               y = casosDiaris$nous_casos_confirmats,
               name = 'observations',
               text = paste("new cases"))

annotation1 <- list(yref = 'y', xref = "x", y = 300, x = "2020-03-14", text = "Estat d'alarma")
#annotation2 <- list(yref = 'y', xref = "x", y = 200, x = "2020-06-24", text = "Sant Joan")
#fig <- fig %>% add_trace(y = newInfectives2.9f$values, x=newInfectives2.9f$datetimes, type='scatter', mode = 'lines', name='Model 2.9 f') %>% layout(annotations= list(annotation1))
#fig <- fig %>% add_trace(y = newInfectivesDetected2.9f$values, x=newInfectivesDetected2.9f$datetimes, type='scatter', mode = 'lines', name='Model 2.9 f Detected')
fig <- fig %>% add_trace(y = newInfectivesDetected2.9$values, x=newInfectivesDetected2.9$datetimes, type='scatter', mode = 'lines', name='Model 2.10 Detected')
fig <- fig %>% add_trace(y = newInfectives2.9$values, x=newInfectives2.9$datetimes, type='scatter', mode = 'lines', name='Model 2.10 true cases')
fig <- fig %>% add_trace(y = newReinfections2.9$values, x=newReinfections2.9$datetimes, type='scatter', mode = 'lines', name='Model 2.10 reinfections') %>% layout(annotations= list(annotation1))

#https://plotly.com/r/text-and-annotations/

fig


## @knitr plotModelAlternative


# if (regio=="")
# {
#   file = "../../models/SDL/Pandemiav2.9f/regions-ALL/newInfectiveDiary.csv"
# } else {
#   file = paste(paste("../../models/SDL/Pandemiav2.9f/regions-ALL/",regio,sep=""),"_newInfectiveDiary.csv",sep="")
# }
# 
# newInfectives2.9f <- read.csv2(file)
# 
# index = c(0:((length(newInfectives2.9f$cell_time)-1)/10))
# index = index+newInfectives2.9f$cell_time[1]
# dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
# datetimes<-dt1+index
# 
# values<-newInfectives2.9f$cell_values[newInfectives2.9f$cell_time %in% index]
# newInfectives2.9f<-data.frame(datetimes,values)
# 
# if (regio=="")
# {
#   file = "../../models/SDL/Pandemiav2.9f/regions-ALL/newInfectiveDetectedDiary.csv"
# } else {
#   file = paste(paste("../../models/SDL/Pandemiav2.9f/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
# }
# 
# newInfectivesDetected2.9f <- read.csv2(file)
# 
# index = c(0:((length(newInfectivesDetected2.9f$cell_time)-1)/10))
# index = index+newInfectivesDetected2.9f$cell_time[1]
# dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
# datetimes<-dt1+index
# 
# values<-newInfectivesDetected2.9f$cell_values[newInfectivesDetected2.9f$cell_time %in% index]
# times<-newInfectivesDetected2.9f$cell_time[newInfectivesDetected2.9f$cell_time %in% index]
# dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
# datetimes<-dt1+times
# newInfectivesDetected2.9f<-data.frame(datetimes,values)
# 
# fig <- plot_ly(type="bar", 
#                x = casosDiaris$data,
#                y = casosDiaris$nous_casos_confirmats,
#                name = 'observations',
#                text = paste("new cases"))


if (regio=="")
{
  file = paste("../../models/SDL/",currentModel,"/regions-ALL/newInfectiveDiary.csv",sep="")
} else {
  file = paste(paste("../../models/SDL/",currentModel,"/regions-ALL/",regio,sep=""),"_newInfectiveDiary.csv",sep="")
}

newInfectives2.9 <- read.csv2(file)

index = c(0:((length(newInfectives2.9$cell_time)-1)/10))
index = index+newInfectives2.9$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectives2.9$cell_values[newInfectives2.9$cell_time %in% index]
newInfectives2.9<-data.frame(datetimes,values)

if (regio=="")
{
  file = "../../models/SDL/",currentModel,"/regions-ALL/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/",currentModel,"/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectivesDetected2.9 <- read.csv2(file)

index = c(0:((length(newInfectivesDetected2.9$cell_time)-1)/10))
index = index+newInfectivesDetected2.9$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectivesDetected2.9$cell_values[newInfectivesDetected2.9$cell_time %in% index]
times<-newInfectivesDetected2.9$cell_time[newInfectivesDetected2.9$cell_time %in% index]
dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
datetimes<-dt1+times
newInfectivesDetected2.9<-data.frame(datetimes,values)

fig <- plot_ly(type="bar", 
               x = casosDiaris$data,
               y = casosDiaris$nous_casos_confirmats,
               name = 'observations',
               text = paste("new cases"))

annotation1 <- list(yref = 'y', xref = "x", y = 300, x = "2020-03-14", text = "Estat d'alarma")
#annotation2 <- list(yref = 'y', xref = "x", y = 200, x = "2020-06-24", text = "Sant Joan")
#fig <- fig %>% add_trace(y = newInfectives2.9f$values, x=newInfectives2.9f$datetimes, type='scatter', mode = 'lines', name='Model 2.9 f') %>% layout(annotations= list(annotation1))
#fig <- fig %>% add_trace(y = newInfectivesDetected2.9f$values, x=newInfectivesDetected2.9f$datetimes, type='scatter', mode = 'lines', name='Model 2.9 f Detected')
fig <- fig %>% add_trace(y = newInfectives2.9$values, x=newInfectives2.9$datetimes, type='scatter', mode = 'lines', name='Model 2.10 (Omni)') %>% layout(annotations= list(annotation1))
fig <- fig %>% add_trace(y = newInfectivesDetected2.9$values, x=newInfectivesDetected2.9$datetimes, type='scatter', mode = 'lines', name='Model 2.10 Detected (Omni)')

#https://plotly.com/r/text-and-annotations/

fig

## @knitr plothistoria

if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.5/experiment/catalunya/data/traces/newInfectiveDetectedDiary.csv"
} else {
  # file = paste(paste("../../models/SDL/Pandemiav2.5/regions/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
  file = "../../models/SDL/Pandemiav2.5/experiment/catalunya/data/traces/newInfectiveDetectedDiary.csv"
}

newInfectives2.5 <- read.csv2(file)


# El model sdlps comença el dia 29 de gener, seleccionem només els valors dels dts sense decimals, 1 de cada 10.
index = c(1:(length(newInfectives2.5$time)/10))
dt1 <- rep(as.Date("2020-01-29 CET"),length(index))
datetimes<-dt1+index
values<-newInfectives2.5$values[newInfectives2.5$time %in% index]
newInfectives2.5<-data.frame(datetimes,values)


if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.6/regions/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.6/regions/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectives2.6 <- read.csv2(file)

# El model sdlps comença el dia 29 de gener, seleccionem només els valors dels dts sense decimals, 1 de cada 10.
index = c(1:(length(newInfectives2.6$cell_time)/10))
dt1 <- rep(as.Date("2020-01-01 CET"),length(index))
datetimes<-dt1+newInfectives2.6$cell_time[index*10]
values<-newInfectives2.6$cell_values[index*10]
newInfectives2.6<-data.frame(datetimes,values)

if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.7/regions/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.7/regions/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectives2.7 <- read.csv2(file)

# El model sdlps comença el dia 29 de gener, seleccionem només els valors dels dts sense decimals, 1 de cada 10.
index = c(1:(length(newInfectives2.7$cell_time)/10))
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+newInfectives2.7$cell_time[index*10]
values<-newInfectives2.7$cell_values[index*10]
newInfectives2.7<-data.frame(datetimes,values)

if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.8-A/regions-ALL/newInfectiveDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-A/regions-ALL/",regio,sep=""),"_newInfectiveDiary.csv",sep="")
}

newInfectives2.8A <- read.csv2(file)

index = c(0:((length(newInfectives2.8A$cell_time)-1)/10))
index = index+newInfectives2.8A$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectives2.8A$cell_values[newInfectives2.8A$cell_time %in% index]
newInfectives2.8A<-data.frame(datetimes,values)

if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.8-A/regions-ALL/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-A/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectivesDetected2.8A <- read.csv2(file)

index = c(0:((length(newInfectivesDetected2.8A$cell_time)-1)/10))
index = index+newInfectivesDetected2.8A$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectivesDetected2.8A$cell_values[newInfectivesDetected2.8A$cell_time %in% index]
times<-newInfectivesDetected2.8A$cell_time[newInfectivesDetected2.8A$cell_time %in% index]
dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
datetimes<-dt1+times
newInfectivesDetected2.8A<-data.frame(datetimes,values)

if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.8-B/regions-ALL/newInfectiveDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-B/regions-ALL/",regio,sep=""),"_newInfectiveDiary.csv",sep="")
}

newInfectives2.8B <- read.csv2(file)

index = c(0:((length(newInfectives2.8B$cell_time)-1)/10))
index = index+newInfectives2.8B$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectives2.8B$cell_values[newInfectives2.8B$cell_time %in% index]
newInfectives2.8B<-data.frame(datetimes,values)

if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.8-B/regions-ALL/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-B/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectivesDetected2.8B <- read.csv2(file)

index = c(0:((length(newInfectivesDetected2.8B$cell_time)-1)/10))
index = index+newInfectivesDetected2.8B$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectivesDetected2.8B$cell_values[newInfectivesDetected2.8B$cell_time %in% index]
times<-newInfectivesDetected2.8B$cell_time[newInfectivesDetected2.8B$cell_time %in% index]
dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
datetimes<-dt1+times
newInfectivesDetected2.8B<-data.frame(datetimes,values)


if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.9/regions/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-B/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectivesDetected2.9 <- read.csv2(file)

index = c(0:((length(newInfectivesDetected2.9$cell_time)-1)/10))
index = index+newInfectivesDetected2.9$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectivesDetected2.9$cell_values[newInfectivesDetected2.9$cell_time %in% index]
times<-newInfectivesDetected2.9$cell_time[newInfectivesDetected2.9$cell_time %in% index]
dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
datetimes<-dt1+times
newInfectivesDetected2.9<-data.frame(datetimes,values)


if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.9b/regions/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-B/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectivesDetected2.9b <- read.csv2(file)

index = c(0:((length(newInfectivesDetected2.9b$cell_time)-1)/10))
index = index+newInfectivesDetected2.9b$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectivesDetected2.9b$cell_values[newInfectivesDetected2.9b$cell_time %in% index]
times<-newInfectivesDetected2.9b$cell_time[newInfectivesDetected2.9b$cell_time %in% index]
dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
datetimes<-dt1+times
newInfectivesDetected2.9b<-data.frame(datetimes,values)

if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.9c/regions-ALL/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-B/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectivesDetected2.9c <- read.csv2(file)

index = c(0:((length(newInfectivesDetected2.9c$cell_time)-1)/10))
index = index+newInfectivesDetected2.9c$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectivesDetected2.9c$cell_values[newInfectivesDetected2.9c$cell_time %in% index]
times<-newInfectivesDetected2.9c$cell_time[newInfectivesDetected2.9c$cell_time %in% index]
dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
datetimes<-dt1+times
newInfectivesDetected2.9c<-data.frame(datetimes,values)


if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.9d/regions-ALL/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-B/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectivesDetected2.9d <- read.csv2(file)

index = c(0:((length(newInfectivesDetected2.9d$cell_time)-1)/10))
index = index+newInfectivesDetected2.9d$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectivesDetected2.9d$cell_values[newInfectivesDetected2.9d$cell_time %in% index]
times<-newInfectivesDetected2.9d$cell_time[newInfectivesDetected2.9d$cell_time %in% index]
dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
datetimes<-dt1+times
newInfectivesDetected2.9d<-data.frame(datetimes,values)

if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.9e/regions-ALL/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-B/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectivesDetected2.9e <- read.csv2(file)

index = c(0:((length(newInfectivesDetected2.9e$cell_time)-1)/10))
index = index+newInfectivesDetected2.9e$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectivesDetected2.9e$cell_values[newInfectivesDetected2.9e$cell_time %in% index]
times<-newInfectivesDetected2.9e$cell_time[newInfectivesDetected2.9e$cell_time %in% index]
dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
datetimes<-dt1+times
newInfectivesDetected2.9e<-data.frame(datetimes,values)


if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.9f/regions-ALL/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-B/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectivesDetected2.9f <- read.csv2(file)

index = c(0:((length(newInfectivesDetected2.9f$cell_time)-1)/10))
index = index+newInfectivesDetected2.9f$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectivesDetected2.9f$cell_values[newInfectivesDetected2.9f$cell_time %in% index]
times<-newInfectivesDetected2.9f$cell_time[newInfectivesDetected2.9f$cell_time %in% index]
dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
datetimes<-dt1+times
newInfectivesDetected2.9f<-data.frame(datetimes,values)


if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.9e/regions-ALL/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-B/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectivesDetected2.9e <- read.csv2(file)

index = c(0:((length(newInfectivesDetected2.9e$cell_time)-1)/10))
index = index+newInfectivesDetected2.9e$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectivesDetected2.9e$cell_values[newInfectivesDetected2.9e$cell_time %in% index]
times<-newInfectivesDetected2.9e$cell_time[newInfectivesDetected2.9e$cell_time %in% index]
dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
datetimes<-dt1+times
newInfectivesDetected2.9e<-data.frame(datetimes,values)


if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.9g/regions-ALL/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-B/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectivesDetected2.9g <- read.csv2(file)

index = c(0:((length(newInfectivesDetected2.9g$cell_time)-1)/10))
index = index+newInfectivesDetected2.9g$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectivesDetected2.9g$cell_values[newInfectivesDetected2.9g$cell_time %in% index]
times<-newInfectivesDetected2.9g$cell_time[newInfectivesDetected2.9g$cell_time %in% index]
dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
datetimes<-dt1+times
newInfectivesDetected2.9g<-data.frame(datetimes,values)



if (regio=="")
{
  file = "../../models/SDL/Pandemiav2.9h-CAT-Omni/regions-ALL/newInfectiveDetectedDiary.csv"
} else {
  file = paste(paste("../../models/SDL/Pandemiav2.8-B/regions-ALL/",regio,sep=""),"_newInfectiveDetectedDiary.csv",sep="")
}

newInfectivesDetected2.9h <- read.csv2(file)

index = c(0:((length(newInfectivesDetected2.9h$cell_time)-1)/10))
index = index+newInfectivesDetected2.9h$cell_time[1]
dt1 <- rep(as.Date("2019-12-01 CET"),length(index))
datetimes<-dt1+index

values<-newInfectivesDetected2.9h$cell_values[newInfectivesDetected2.9h$cell_time %in% index]
times<-newInfectivesDetected2.9h$cell_time[newInfectivesDetected2.9h$cell_time %in% index]
dt1 <- rep(as.Date("2019-12-01 CET"),length(times))
datetimes<-dt1+times
newInfectivesDetected2.9h<-data.frame(datetimes,values)

fig <- plot_ly(type="bar", 
               x = casosDiaris$data,
               y = casosDiaris$nous_casos_confirmats,
               name = 'observations',
               text = paste("new cases"))

annotation1 <- list(yref = 'y', xref = "x", y = 300, x = "2020-03-14", text = "Estat d'alarma")
annotation2 <- list(yref = 'y', xref = "x", y = 200, x = "2020-06-24", text = "Sant Joan")
if (!onlyOneModel) {
  fig <- fig %>% add_trace(y = casosDiaris$model1.7, type='scatter', mode = 'lines', name='Model: Contention scenario')
  fig <- fig %>% add_trace(y = newInfectives$values, x=newInfectives$datetimes, type='scatter', mode = 'lines', name='Model 1.9') %>% layout(annotations= list(annotation1,annotation2))
  fig <- fig %>% add_trace(y = newInfectives2.5$values, x=newInfectives2.5$datetimes, type='scatter', mode = 'lines', name='Model 2.5') %>% layout(annotations= list(annotation1,annotation2))
}
fig <- fig %>% add_trace(y = newInfectives2.6$values, x=newInfectives2.6$datetimes, type='scatter', mode = 'lines', name='Model 2.6') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectives2.7$values, x=newInfectives2.7$datetimes, type='scatter', mode = 'lines', name='Model 2.7') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectivesDetected2.8A$values, x=newInfectivesDetected2.8A$datetimes, type='scatter', mode = 'lines', name='Model 2.8') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectivesDetected2.9$values, x=newInfectivesDetected2.9$datetimes, type='scatter', mode = 'lines', name='Model 2.9 a') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectivesDetected2.9b$values, x=newInfectivesDetected2.9b$datetimes, type='scatter', mode = 'lines', name='Model 2.9 b') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectivesDetected2.9c$values, x=newInfectivesDetected2.9c$datetimes, type='scatter', mode = 'lines', name='Model 2.9 c') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectivesDetected2.9d$values, x=newInfectivesDetected2.9d$datetimes, type='scatter', mode = 'lines', name='Model 2.9 d') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectivesDetected2.9e$values, x=newInfectivesDetected2.9e$datetimes, type='scatter', mode = 'lines', name='Model 2.9 e') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectivesDetected2.9f$values, x=newInfectivesDetected2.9f$datetimes, type='scatter', mode = 'lines', name='Model 2.9 f') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectivesDetected2.9g$values, x=newInfectivesDetected2.9g$datetimes, type='scatter', mode = 'lines', name='Model 2.9 g') %>% layout(annotations= list(annotation1,annotation2))
fig <- fig %>% add_trace(y = newInfectivesDetected2.9h$values, x=newInfectivesDetected2.9h$datetimes, type='scatter', mode = 'lines', name='Model 2.9 h') %>% layout(annotations= list(annotation1,annotation2))


fig

## @knitr last_updated
darreraData=casosDiaris$data[nrow(casosDiaris)]
valueBox(darreraData, icon = "fa-comments", color = ifelse(darreraData > Sys.Date(), "warning", "primary"))

## @knitr day_cases
dataFinal<-casos$data[1]
dataInicial<-casos$data[nrow(casos)]

R_eff_3=Reff(casosDiaris$m_3,casosDiaris)
R_eff_7=Reff(casosDiaris$m_7,casosDiaris)
R_eff_14=Reff(casosDiaris$m_14,casosDiaris)
 
R_eff_3 <- round(R_eff_3,digits=2)
gauge(R_eff_3, min = 0, max = 5, symbol = ' C3', gaugeSectors(
 success = c(0, 0.9), warning = c(0.9, 1.1), danger = c(1.1, 5)
))
R_eff_7 <- round(R_eff_7,digits=2)
gauge(R_eff_7, min = 0, max = 5, symbol = ' C7', gaugeSectors(
 success = c(0, 0.9), warning = c(0.9, 1.1), danger = c(1.1, 5)
))
R_eff_14 <- round(R_eff_14,digits=2)
gauge(R_eff_14, min = 0, max = 5, symbol = ' C14', gaugeSectors(
 success = c(0, 0.9), warning = c(0.9, 1.1), danger = c(1.1, 5)
))

## @knitr R_calculus
incidence <- casosDiaris %>% rename(I = nous_casos_confirmats)
incidence <- incidence %>% slice(8:n())
incidence$model1.7<-NULL
cat_parametric_si <- estimate_R(incidence,method = "parametric_si", config = make_config(list(mean_si = 5, std_si = 5.25)))
datagraph <- data.frame(casosDiaris%>% slice(8+7:n()),cat_parametric_si$R)
datagraph <- datagraph %>% slice(1:(dim(datagraph)[1]))
fig <- plot_ly(datagraph, x = ~data, y = ~Mean.R., type = 'scatter', mode ='lines',name='Estimated R')
fig

#EpiEstim documentation ----------------------------
#https://timchurches.github.io/blog/posts/2020-02-18-analysing-covid-19-2019-ncov-outbreak-data-with-r-part-1/#estimating-changes-in-the-effective-reproduction-number
#https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/
# https://www.paho.org/es/documentos/ejercicio-modelaje-covid-19-guia-paso-paso-para-calcular-rt-con-epiestim
# http://covid-ete.ouvaton.org/Report1_R0_France.html

# plot_Ri <- function(estimate_R_obj) {
#     p_I <- plot(estimate_R_obj, "incid", add_imported_cases = FALSE)  # plots the incidence
#     p_SI <- plot(estimate_R_obj, "SI")  # plots the serial interval distribution
#     p_Ri <- plot(estimate_R_obj, "R")
#     return(gridExtra::grid.arrange(p_I, p_SI, p_Ri, ncol = 1))
# }
# plot_Ri(cat_parametric_si)
#plot(cat_parametric_si,"R")


## @knitr R_incidencia
R_inc_3=RInc(casosDiaris$m_3,casosDiaris,3)
R_inc_7=RInc(casosDiaris$m_7,casosDiaris,7)
R_inc_14=RInc(casosDiaris$m_14,casosDiaris,14)

R_inc_3 <- round(R_inc_3,digits=2)
gauge(R_inc_3, min = 0, max = 100, symbol = ' I3', gaugeSectors(
  success = c(0, 10), warning = c(11, 30), danger = c(31, 100)
))
R_inc_7 <- round(R_inc_7,digits=2)
gauge(R_inc_7, min = 0, max = 100, symbol = ' I7', gaugeSectors(
  success = c(0, 10), warning = c(11, 30), danger = c(31, 100)
))
R_inc_14 <- round(R_inc_14,digits=2)
gauge(R_inc_14, min = 0, max = 100, symbol = ' I14', gaugeSectors(
  success = c(0, 10), warning = c(11, 30), danger = c(31, 100)
))


## @knitr situation
casosPositiusAcumulats <- sum(casosPositius$numcasos)

testsDiarisAcumulats <- sum(testsDiaris$nous_tests_fets)
values <- list(Positius=casosPositiusAcumulats/PoblacioTotal,
               #Sospitosos=(testsDiarisAcumulats-casosPositiusAcumulats)/PoblacioTotal,
               #Total=(PoblacioTotal-testsDiarisAcumulats)/PoblacioTotal)
                Total=1-casosPositiusAcumulats/PoblacioTotal)
personograph(values, n.icons=100, colors=list(Positius="red", Total="lightgreen"), draw.legend=TRUE, fig.cap = "", icon.style=3)



## @knitr plotCumulativeCurve

#fig <- plot_ly(casos, x = ~data, y = ~total_de_casos_confirmats, text = ~total_de_casos_confirmats, type = 'scatter', mode = 'markers', marker = list(size = ~nous_casos_diaris_confirmats/100, opacity = 0.5, color = 'rgb(255, 65, 54)'))

casosDiaris <- casosDiaris %>% mutate(cumsum = cumsum(casosDiaris$nous_casos_confirmats))
fig <- plot_ly(casosDiaris, x = ~data, y = ~cumsum, text = ~cumsum, type = 'scatter', mode = 'markers', marker = list(size = ~nous_casos_confirmats/(PoblacioTotal/7566000 * 300), opacity = 0.5, color = 'rgb(255, 65, 54)'))

fig <- fig %>% layout(title = 'Total de casos confirmats', xaxis = list(showgrid = FALSE), yaxis = list(showgrid = FALSE))

fig


