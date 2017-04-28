:: Example of using this script
:: 		runNetworkcompare.bat "C:\Temp\Research\SampleCubeNet" "S7_2010_7071.net" Skip NoSkip
:: 		The above command will export only the nodes of the network in the given location. Link exporting is skipped.
::      Use the two "skip" parameters to decide whether one/both node/link need to be exported

SET VOYAGER=C:\Progra~2\Citilabs\CubeVoyager\Voyager.exe

SET netLocation=%1
SET netName=%2
SET skipLink=%3
SET skipNode=%4

echo %netLocation%
set netLocation=%netLocation:"=%
echo %netLocation%
echo %netName%
set netName=%netName:"=%
echo %netName%
set "netNamebegin=%netName:.net="^&REM #%
SET linkShape=%netLocation%\%netNamebegin%_link.shp
SET nodeShape=%netLocation%\%netNamebegin%_node.shp


rem Creating the Cube Script file to export to shapefiles

echo RUN PGM=NETWORK                                                > %netLocation%\NetworkToShape.s  
echo FILEI NETI =%netLocation%\%netName%                            >> %netLocation%\NetworkToShape.s  
if NOT %skipLink%==Skip (
    echo FILEO LINKO=%linkShape% FORMAT=SHP                         >> %netLocation%\NetworkToShape.s
)
if NOT %skipNode%==Skip (
    echo FILEO NODEO=%nodeShape% FORMAT=SHP                         >> %netLocation%\NetworkToShape.s
)
echo ENDRUN                                                         >> %netLocation%\NetworkToShape.s  

%VOYAGER% %netLocation%\NetworkToShape.s /Start
pause