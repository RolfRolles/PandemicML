:loop
REM call generate_vmp_example.bat 
call generate_cv_example.bat
"C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\bin\amd64\dumpbin" /section:LOOKHERE /rawdata:1 "c:\paframework\test\vmstuff\blob.cv.exe" > text.blah 
head --lines=-4 text.blah | tail -n+26 | cut -b 13-60 | sed 's/ //g' > rawdata.txt 
REM rm text.blah
main.exe rawdata.txt
REM rm rawdata.txt

goto loop