DECLARE SUB WRITER (atomnum%, geomcount%, filename$, reaccoord$)
DECLARE SUB WRITES (atomnum%, geomcount%, filename$, reaccoord$)
DECLARE SUB PAUSE ()
DIM SHARED coords$(1 TO 20, 1 TO 3)
DIM SHARED atom$(1 TO 20)

' Set folder paths

infolder$ = "B:\"
outfolder$ = "B:\PENDIN~1\"

CLS
PRINT "Batch QCI Job Creater"
PRINT "~~~~~~~~~~~~~~~~~~~~~~~~~~~"

' Clear the pending jobs folder

OPEN outfolder$ + "temp.tmp" FOR OUTPUT AS #1
PRINT #1, "Temporary File"
CLOSE #1
KILL outfolder$ + "*.*"

' Ask for folder containing .csv file and open it

PRINT ""
INPUT "Enter file name (without extension): ", filename$
filename$ = infolder$ + filename$ + ".csv"
PRINT ""
PRINT "Opening File " + UCASE$(filename$) + "..."
OPEN filename$ FOR INPUT AS #1

INPUT "Enter job type (Simple or Readguess):", intext$
IF UCASE$(LEFT$(intext$, 1)) = "S" THEN
     jobtype$ = "SIMPLE"
ELSEIF UCASE$(LEFT$(intext$, 1)) = "R" THEN
     jobtype$ = "READGUESS"
ELSE
     STOP
END IF

PRINT ""
PRINT "Creating " + jobtype$ + " files in " + outfolder$ + "..."
PRINT ""


' Read title line and work out how many atoms

LINE INPUT #1, temp$
temp$ = RIGHT$(temp$, 8)
IF MID$(temp$, 2, 1) = "," THEN
        atomnum% = VAL(LEFT$(temp$, 1))
        ELSE
        atomnum% = VAL(LEFT$(temp$, 2))
        END IF

' Loop over all geometries in IRC

geomcount% = -1

DO

geomcount% = geomcount% + 1

     PRINT RIGHT$("    " + STR$(geomcount%), 4) + " ";

' Read in geometry data

    
     'only include the following line if there is a geometry number
'     INPUT #1, temp$
     '------------------------------------------------------------- 

     INPUT #1, reaccoord$

     FOR x% = 1 TO 3
          INPUT #1, temp$
     NEXT x%

     FOR x% = 1 TO atomnum%
          INPUT #1, atom$(x%)
          FOR y% = 1 TO 3
             INPUT #1, coords$(x%, y%)

' Ensure all coordinates have decimal points

             decimal$ = "NO"
             FOR position% = 1 TO LEN(coords$(x%, y%))
                  IF MID$(coords$(x%, y%), position%, 1) = "." THEN decimal$ = "YES"
             NEXT position%
             IF decimal$ = "NO" THEN coords$(x%, y%) = coords$(x%, y%) + ".0"

          NEXT y%
     NEXT x%


' Write out to batch & gaussian files

filename$ = outfolder$ + "job-" + RIGHT$("000" + LTRIM$(STR$(geomcount%)), 3)
IF jobtype$ = "SIMPLE" THEN CALL WRITES(atomnum%, geomcount%, filename$, reaccoord$)
IF jobtype$ = "READGUESS" THEN CALL WRITER(atomnum%, geomcount%, filename$, reaccoord$)


LOOP UNTIL EOF(1) <> 0

' Close files and end program

CLOSE #1

PRINT ""
PRINT ""
PRINT "Finished"
PAUSE
STOP

SUB PAUSE

WHILE INKEY$ <> CHR$(13)
WEND

END SUB

SUB WRITER (atomnum%, geomcount%, filename$, reaccoord$)

' Open .com file and write route & title lines

OPEN filename$ + ".com" FOR OUTPUT AS #5

PRINT #5, "%mem=120MW"
PRINT #5, "%chk=/data/" + RIGHT$(filename$, 7) + "/" + RIGHT$(filename$, 7) + ".chk"
PRINT #5, "# uqcisd(t maxcyc=500)/cc-pvtz guess=read scf(qc,maxcyc=1000)"
PRINT #5, ""
PRINT #5, "QCI calculation at reaction coordinate " + reaccoord$
PRINT #5, ""
PRINT #5, "0 1"

' Write geometry specification

FOR x% = 1 TO atomnum%
     temp$ = atom$(x%)
     FOR y% = 1 TO 3
          temp$ = temp$ + RIGHT$("               " + coords$(x%, y%), 15)
     NEXT y%
     PRINT #5, temp$
NEXT x%

' Close .com file

PRINT #5, ""
CLOSE #5



END SUB

SUB WRITES (atomnum%, geomcount%, filename$, reaccoord$)

' Open .com file and write route & title lines

OPEN filename$ + ".com" FOR OUTPUT AS #5

PRINT #5, "%mem=120MW"
PRINT #5, "# uqcisd(t maxcyc=500)/cc-pvtz"
PRINT #5, ""
PRINT #5, "QCI calculation at reaction coordinate " + reaccoord$
PRINT #5, ""
PRINT #5, "0 1"

' Write geometry specification

FOR x% = 1 TO atomnum%
     temp$ = atom$(x%)
     FOR y% = 1 TO 3
          temp$ = temp$ + RIGHT$("               " + coords$(x%, y%), 15)
     NEXT y%
     PRINT #5, temp$
NEXT x%

' Close .com file

PRINT #5, ""
CLOSE #5


END SUB

