DECLARE SUB WRITER (atomnum%, geomcount%, filename$, reaccoord$)
DECLARE SUB WRITEI (atomnum%, geomcount%, filename$, reaccoord$)
DECLARE SUB WRITES (atomnum%, geomcount%, filename$, reaccoord$)
DECLARE SUB PAUSE ()
DIM SHARED coords$(1 TO 20, 1 TO 3)
DIM SHARED atom$(1 TO 20)

' Set folder paths

infolder$ = "B:\"
outfolder$ = "B:\PENDIN~1\"

CLS
PRINT "Batch Frequency Job Creater"
PRINT "~~~~~~~~~~~~~~~~~~~~~~~~~~~"

' Clear the pending jobs folder

OPEN outfolder$ + "temp.tmp" FOR OUTPUT AS #1
PRINT #1, "Temporary File"
CLOSE #1
KILL outfolder$ + "*.*"

' Ask for folder containing .csv file and open it

PRINT ""
INPUT "Enter folder name (without slahes): ", foldername$
INPUT "Enter file name (without extension): ", filename$
filename$ = infolder$ + foldername$ + "\" + filename$ + ".csv"
PRINT ""
PRINT "Opening File " + UCASE$(filename$) + "..."
OPEN filename$ FOR INPUT AS #1

' Ask what type of frequency job is required

PRINT ""
INPUT "Enter job type required (Simple, Ionic, ReadGuess): ", jobtype$
IF UCASE$(LEFT$(jobtype$, 1)) = "S" THEN
     jobtype$ = "SIMPLE"
ELSEIF UCASE$(LEFT$(jobtype$, 1)) = "I" THEN
     jobtype$ = "IONIC"
ELSEIF UCASE$(LEFT$(jobtype$, 1)) = "R" THEN
     jobtype$ = "READGUESS"
ELSE
     PRINT "ERROR: Job type not recognised"
     PAUSE
     STOP
END IF
PRINT ""
PRINT "Creating " + jobtype$ + " files in " + outfolder$ + "..."
PRINT ""

' Open batch script file

OPEN outfolder$ + "batch.job" FOR OUTPUT AS #2

' Read title line and work out how many atoms

LINE INPUT #1, temp$
temp$ = RIGHT$(temp$, 8)
IF MID$(temp$, 2, 1) = "," THEN
        atomnum% = VAL(LEFT$(temp$, 1))
        ELSE
        atomnum% = VAL(LEFT$(temp$, 2))
        END IF

' Loop over all geometries in IRC

geomcount% = 0

DO

geomcount% = geomcount% + 1

     PRINT RIGHT$("    " + STR$(geomcount%), 4) + " ";

' Read in geometry data

     INPUT #1, temp$
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

filename$ = outfolder$ + "freq-" + RIGHT$("000" + LTRIM$(STR$(geomcount%)), 3)
IF jobtype$ = "SIMPLE" THEN CALL WRITES(atomnum%, geomcount%, filename$, reaccoord$)
IF jobtype$ = "IONIC" THEN CALL WRITEI(atomnum%, geomcount%, filename$, reaccoord$)
IF jobtype$ = "READGUESS" THEN CALL WRITER(atomnum%, geomcount%, filename$, reaccoord$)

LOOP UNTIL EOF(1) <> 0

' Close files and end program

CLOSE #1
PRINT #2, ""
CLOSE #2

PRINT ""
PRINT ""
PRINT "Finished"
PAUSE
STOP

SUB PAUSE

WHILE INKEY$ <> CHR$(13)
WEND

END SUB

SUB WRITEI (atomnum%, geomcount%, filename$, reaccoord$)

' Open .com file and write route & title lines

OPEN filename$ + ".com" FOR OUTPUT AS #5

PRINT #5, "%chk=/tmp/freq-" + RIGHT$(filename$, 3) + ".chk"
PRINT #5, "# ub3lyp/cc-pvdz"
PRINT #5, "# guess=mix scf(tight,maxcyc=500)"
PRINT #5, ""
PRINT #5, "Ionic integral formation at reaction coordinate " + reaccoord$
PRINT #5, ""
PRINT #5, "-2 1"

' Write geometry specification

FOR x% = 1 TO atomnum%
     temp$ = atom$(x%)
     FOR y% = 1 TO 3
          temp$ = temp$ + RIGHT$("               " + coords$(x%, y%), 15)
     NEXT y%
     PRINT #5, temp$
NEXT x%
PRINT #5, ""
PRINT #5, "--Link1--"
PRINT #5, "%chk=/tmp/freq-" + RIGHT$(filename$, 3) + ".chk"
PRINT #5, "# ub3lyp/cc-pvdz freq=projected"
PRINT #5, "# guess(read,mix) scf(tight,maxcyc=500) geom=checkpoint"
PRINT #5, ""
PRINT #5, "Projected frequency calculation at reaction coordinate " + reaccoord$
PRINT #5, ""
PRINT #5, "0 1"


' Close .com file

PRINT #5, ""
CLOSE #5

' Write to batch script file

temp$ = RIGHT$(filename$, 8)
PRINT #2, "qg98 " + temp$ + ".com -l cput=2:00:00,pvmem=250mb"

END SUB

SUB WRITER (atomnum%, geomcount%, filename$, reaccoord$)

' Open .com file and write route & title lines

OPEN filename$ + ".com" FOR OUTPUT AS #5

PRINT #5, "%chk=/tmp/jc6422/freq-" + RIGHT$(filename$, 3) + ".chk"
PRINT #5, "# ub3lyp/cc-pvdz guess=read"
PRINT #5, "# scf(tight,maxcyc=500) freq=projected"
PRINT #5, ""
PRINT #5, "Projected frequency calculation at reaction coordinate " + reaccoord$
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

' Write .run file which sets up the .chk file on each node

OPEN filename$ + ".run" FOR OUTPUT AS #8
PRINT #8, "#!/bin/bash"
PRINT #8, "qsub -N freq-" + RIGHT$(filename$, 3) + " -j oe -l cput=2:00:00,pvmem=250mb <<EOF"
PRINT #8, "hostname"
PRINT #8, "ulimit -c 0"
PRINT #8, "cd \$PBS_O_WORKDIR"
PRINT #8, "mkdir /tmp/jc6422"
PRINT #8, "cp freq-run.chk /tmp/jc6422/freq-" + RIGHT$(filename$, 3) + ".chk"
PRINT #8, "g98 freq-" + RIGHT$(filename$, 3) + ".com"
PRINT #8, "EOF"
CLOSE #8

' Write to batch script file

temp$ = RIGHT$(filename$, 8)
PRINT #2, "./freq-" + RIGHT$(filename$, 3) + ".run"

END SUB

SUB WRITES (atomnum%, geomcount%, filename$, reaccoord$)

' Open .com file and write route & title lines

OPEN filename$ + ".com" FOR OUTPUT AS #5

PRINT #5, "# ub3lyp/cc-pvdz guess=mix"
PRINT #5, "# scf(tight,maxcyc=500) freq=projected"
PRINT #5, ""
PRINT #5, "Projected frequency calculation at reaction coordinate " + reaccoord$
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

' Write to batch script file

temp$ = RIGHT$(filename$, 8)
PRINT #2, "qg98 " + temp$ + ".com -l cput=2:00:00,pvmem=250mb"

END SUB

