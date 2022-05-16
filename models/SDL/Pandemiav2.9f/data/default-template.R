

# installem els paquets si no estan instal·lats
list.of.packages <- c("runner", "dplyr","plotly", "htmlwidgets", "XML")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(runner)
library(dplyr)





