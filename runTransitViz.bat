SET netLocation="C:\Temp\Research\SampleCubeNet"
SET netName="S7_2010_7071.net"
SET trnName="S7_2010_7071_Transit.LIN"

SET RLOC=C:\Progra~1\R\R-3.2.3


set netName=%netName:"=%
echo %netName%
set "netNamebegin=%netName:.net="^&REM #%


rem call netExport.bat %netLocation% %netName%  Skip NoSkip

%RLOC%\bin\RScript.exe --vanilla --verbose "BrowseCubeTransit.r" > %netLocation%\BrowseCubeTransit.log  %netLocation% %netNamebegin% %trnName% 2>&1
pause
