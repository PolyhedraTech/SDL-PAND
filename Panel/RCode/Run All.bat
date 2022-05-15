@echo off

echo *************************************
echo.
echo    Este script no muestra información alguna
echo    Abrir el archivo 'log.log' y refrescarlo para ver los cambios que se van produciendo
echo.
echo *************************************

rem
rem   Estas dos líneas ejecutan el script y redirigen tanto la salida estándard, como el error
rem   al fichero 'log.log'.
rem 
rem   Fuente:
rem      https://stackoverflow.com/a/24482019
rem

>log.log 2>&1 call :start %*
exit /b

:start
echo *************************************
echo  Fecha de inicio de ejecución
date /t
time /t
echo.
echo *************************************

echo 1 de 12
Rscript -e "rmarkdown::render('SDL-PAND_KPIs_v1_9_All.Rmd')"
echo 2 de 12
Rscript -e "rmarkdown::render('SDL-PAND_KPIs_v1_9_7803.Rmd')" 
echo 3 de 12
Rscript -e "rmarkdown::render('SDL-PAND_KPIs_v1_9_7802.Rmd')" 
echo 4 de 12
Rscript -e "rmarkdown::render('SDL-PAND_KPIs_v1_9_7801.Rmd')" 
echo 5 de 12
Rscript -e "rmarkdown::render('SDL-PAND_KPIs_v1_9_7100.Rmd')"
echo 6 de 12
Rscript -e "rmarkdown::render('SDL-PAND_KPIs_v1_9_6700.Rmd')" 
echo 7 de 12
Rscript -e "rmarkdown::render('SDL-PAND_KPIs_v1_9_6400.Rmd')" 
echo 8 de 12
Rscript -e "rmarkdown::render('SDL-PAND_KPIs_v1_9_6300.Rmd')" 
echo 9 de 12
Rscript -e "rmarkdown::render('SDL-PAND_KPIs_v1_9_6200.Rmd')" 
echo 10 de 12
Rscript -e "rmarkdown::render('SDL-PAND_KPIs_v1_9_6100.Rmd')" 
echo 11 de 12
Rscript -e "rmarkdown::render('Pandemic-drivers.Rmd')" 
echo 12 de 12
Rscript -e "rmarkdown::render('AllCatalunyaAlternative.Rmd')" 


echo "R termino"
