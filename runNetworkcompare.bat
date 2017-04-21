
SET netLocation='C:\Temp\Research\SampleCubeNet\S7_10_REV_10132015 - EW Study.net'
SET skipLink=No
SET skipNode=No


set "netNamebegin=%netLocation:.net="^&REM #%
SET linkShape=%netNamebegin%_link.shp'
SET nodeShape=%netNamebegin%_node.shp'
SET VOYAGER=C:\Progra~1\Citilabs\CubeVoyager\Voyager.exe

rem Creating the Cube Script file to export to shapefiles

echo RUN PGM=NETWORK                                                > NetworkToShape.s  
echo FILEI NETI =%netLocation%                                      >> NetworkToShape.s  
if NOT %skipLink%=="Yes" (
    echo FILEO LINKO=%linkShape% FORMAT=SHP                         >> NetworkToShape.s
)
if NOT %skipNode%=="Yes" (
    echo FILEO NODEO=%nodeShape% FORMAT=SHP                         >> NetworkToShape.s
)
echo ENDRUN                                                         >> NetworkToShape.s  

%VOYAGER% .\NetworkToShape.s /Start

pause

rem following from internet for parsing a string without loop
rem set "str=4567:abcde"
rem echo %str%
rem set "var1=%str::="^&REM #%
rem set "var2=%str:*:=%"
rem echo var1=%var1% var2=%var2%
