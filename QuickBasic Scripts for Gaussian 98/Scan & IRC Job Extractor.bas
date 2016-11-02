DECLARE SUB TRANSLATEIRC ()
DECLARE SUB TRANSLATESCAN ()
DECLARE FUNCTION REWRITE$ (into$)
DECLARE FUNCTION TRIM$ (intext$)
DECLARE SUB MOVEBUFFER ()
DECLARE SUB PAUSE ()
DIM SHARED buffer$(400)
DIM SHARED coords$(20, 4)


DO

'open input file, a native Gaussian LOG file

        PRINT "==============================================================================="
        INPUT "Enter Sub-Folder Name (without slashes): ", foldername$
        INPUT "Enter LOG File Name (without extension): ", filename$
        IF ((UCASE$(filename$) = "STOP") OR (UCASE$(foldername$) = "STOP")) THEN STOP
        inputfilename$ = "B:\" + foldername$ + "\" + filename$ + ".log"
        OPEN inputfilename$ FOR INPUT AS #1
        PRINT "Found File, opening...   ";

'open output file, based on CSV format (excel compatible)

        OPEN "B:\" + foldername$ + "\" + filename$ + ".csv" FOR OUTPUT AS #2

'decide whether this file is a SCAN or an IRC

        DO
                MOVEBUFFER
                jobtype$ = LEFT$(buffer$(400), 12)
        LOOP UNTIL (jobtype$ = "GradGradGrad" OR jobtype$ = "IRC-IRC-IRC-")


        IF jobtype$ = "GradGradGrad" THEN
                PRINT "This is a SCAN job"
                TRANSLATESCAN
        ELSEIF jobtype$ = "IRC-IRC-IRC-" THEN
                PRINT "This is an IRC job"
                TRANSLATEIRC
        ELSE
                STOP
        END IF

        PRINT
        PRINT "End of log file"
        CLOSE #1
        CLOSE #2
LOOP

SUB MOVEBUFFER

'read in text until a carriage return is found
'used for native unix format which doesn't
'use the line feed characters

'ignore lines containing "Gradient too large..."
'and "LinEq1:  Iter=..."
'and "Density matrix..."

DO
        trip$ = "NO"
        temp$ = ""
        DO
                char$ = INPUT$(1, #1)
                IF ASC(char$) <> 10 THEN temp$ = temp$ + char$
        LOOP UNTIL ASC(char$) = 10
        readline$ = TRIM$(temp$)
        IF EOF(1) <> 0 THEN
                CLOSE #1
                CLOSE #2
                STOP
        END IF
        IF LEFT$(readline$, 37) = "Gradient too large" THEN trip$ = "YES"
        IF LEFT$(readline$, 14) = "LinEq1:  Iter=" THEN trip$ = "YES"
        IF LEFT$(readline$, 14) = "Density matrix" THEN trip$ = "YES"
LOOP UNTIL trip$ = "NO"


'scroll text buffer up

        FOR scroller% = 1 TO 399
                buffer$(scroller%) = buffer$(scroller% + 1)
        NEXT scroller%
        buffer$(400) = readline$

END SUB

SUB PAUSE

WHILE INKEY$ <> CHR$(13)
WEND

END SUB

FUNCTION REWRITE$ (into$)
outto$ = ""
FOR x% = 1 TO LEN(into$)
        IF MID$(into$, x%, 1) <> " " THEN
        IF MID$(into$, x%, 1) <> "," THEN
        IF MID$(into$, x%, 1) <> "(" THEN
        IF MID$(into$, x%, 1) <> ")" THEN outto$ = outto$ + MID$(into$, x%, 1)
        END IF
        END IF
        END IF
NEXT x%
REWRITE$ = outto$
END FUNCTION

SUB TRANSLATEIRC

'now search for optimised parameters

        numpoints% = 0
        DO
                numpoints% = numpoints% + 1

'look for (non-)optimised data box

        DO
                MOVEBUFFER
                IF LEFT$(buffer$(400), 13) = "Job cpu time:" THEN GOTO 1001
        LOOP UNTIL ((buffer$(350) = "! Non-Optimized Parameters !") OR (buffer$(350) = "!   Optimized Parameters   !"))
        IF MID$(buffer$(350), 5, 3) = "Opt" THEN
                PRINT "Optimised Geometry Found: ";
                opttype$ = "Opt"
        ELSEIF MID$(buffer$(350), 3, 7) = "Non-Opt" THEN
                PRINT "Non-Optimised Geometry Found: ";
                opttype$ = "Non"
        ELSE
                GOTO 1000
        END IF

'look for reaction coordinate

        reaction$ = "???"
        FOR xx% = 350 TO 400
                IF LEFT$(TRIM$(buffer$(xx%)), 42) = "NET REACTION COORDINATE UP TO THIS POINT =" THEN reaction$ = TRIM$(RIGHT$(buffer$(xx%), 10))
        NEXT xx%
        PRINT "Reaction Coordinate = " + reaction$


'look for cartesian coordinates

        xx% = 350
        DO
                xx% = xx% - 1
                IF xx% = 0 THEN GOTO 1000
        LOOP UNTIL (buffer$(xx%) = "Input orientation:" OR buffer$(xx%) = "Standard orientation:" OR buffer$(xx%) = "Z-Matrix orientation:")
        PRINT "Found cartesian coordinates";
        x% = 0
        DO
                x% = x% + 1
                IF x% > 20 THEN GOTO 1000
                coords$(x%, 1) = TRIM$(MID$(buffer$(x% + xx% + 4), 11, 3))
                IF coords$(x%, 1) = "1" THEN
                        coords$(x%, 1) = "H"
                ELSEIF coords$(x%, 1) = "5" THEN
                        coords$(x%, 1) = "B"
                ELSEIF coords$(x%, 1) = "6" THEN
                        coords$(x%, 1) = "C"
                ELSEIF coords$(x%, 1) = "7" THEN
                        coords$(x%, 1) = "N"
                ELSEIF coords$(x%, 1) = "8" THEN
                        coords$(x%, 1) = "O"
                ELSEIF coords$(x%, 1) = "9" THEN
                        coords$(x%, 1) = "F"
                ELSEIF coords$(x%, 1) = "13" THEN
                        coords$(x%, 1) = "Al"
                ELSEIF coords$(x%, 1) = "14" THEN
                        coords$(x%, 1) = "Si"
                ELSEIF coords$(x%, 1) = "15" THEN
                        coords$(x%, 1) = "P"
                ELSEIF coords$(x%, 1) = "16" THEN
                        coords$(x%, 1) = "S"
                ELSEIF coords$(x%, 1) = "17" THEN
                        coords$(x%, 1) = "Cl"
                ELSE
                        coords$(x%, 1) = "?"
                END IF
                coords$(x%, 2) = TRIM$(MID$(buffer$(x% + xx% + 4), 33, 10))
                coords$(x%, 3) = TRIM$(MID$(buffer$(x% + xx% + 4), 45, 10))
                coords$(x%, 4) = TRIM$(RIGHT$(buffer$(x% + xx% + 4), 10))
        LOOP UNTIL LEFT$(buffer$(x% + xx% + 5), 10) = "----------"
        numatoms% = x%

'look for rotational constants

        DO
                xx% = xx% + 1
                IF xx% = 400 THEN GOTO 1000
        LOOP UNTIL LEFT$(buffer$(xx%), 27) = "Rotational constants (GHZ):"
        PRINT ", Found rotational constants";
        rot1$ = TRIM$(MID$(buffer$(xx%), 32, 11))
        rot2$ = TRIM$(MID$(buffer$(xx%), 47, 11))
        rot3$ = TRIM$(RIGHT$(buffer$(xx%), 11))

'look for energy

        DO
                xx% = xx% + 1
                IF xx% = 400 THEN GOTO 1000
        LOOP UNTIL LEFT$(buffer$(xx%), 9) = "SCF Done:"
        PRINT ", Found energy";
        Energy$ = TRIM$(MID$(buffer$(xx%), 28, 15))

'look for spin

        IF LEFT$(buffer$(xx% + 4), 24) = "S**2 before annihilation" THEN
                PRINT " and spin"
                PRINT
                Spin$ = TRIM$(MID$(buffer$(xx% + 4), 30, 6))
        ELSE
                PRINT ""
                PRINT ""
        END IF

'loop for next optimised geometry and print data

        IF numpoints% = 1 THEN
                outtext$ = "Geom,Coord,Opt,Energy,S(S+1)"
                FOR x% = 1 TO numatoms%
                        outtext$ = outtext$ + ",Atom" + STR$(x%) + ",X,Y,Z"
                NEXT x%
        PRINT #2, outtext$
        END IF
       
        outtext$ = ""
        FOR x% = 1 TO numatoms%
                FOR xx% = 1 TO 4
                        outtext$ = outtext$ + "," + coords$(x%, xx%)
                NEXT xx%
        NEXT x%
        outtext$ = LTRIM$(STR$(numpoints%)) + "," + reaction$ + "," + opttype$ + "," + Energy$ + "," + Spin$ + outtext$
        PRINT #2, outtext$
       
        LOOP

GOTO 1001

1000 REM End program (caused by unexpected input)

        CLOSE #1
        CLOSE #2
        STOP

1001 REM Finished with file

END SUB

SUB TRANSLATESCAN

'find scanning variable

        DO
                MOVEBUFFER
        LOOP UNTIL buffer$(350) = "!    Initial Parameters    !"
        PRINT "Found initial parameters"
        scanvar$ = "STOP"
        FOR x% = 350 TO 400
                IF MID$(buffer$(x%), 48, 4) = "Scan" THEN scanvar$ = MID$(buffer$(x%), 9, 15)
        NEXT x%
        IF scanvar$ = "STOP" THEN GOTO 2000
        PRINT "Found scanning variable, "; scanvar$
        PRINT


'now search for optimised parameters

        numpoints% = 0
        DO
        numpoints% = numpoints% + 1

'look for (non-)optimised data box

        DO
                MOVEBUFFER
                IF LEFT$(buffer$(400), 13) = "Job cpu time:" THEN GOTO 2001
        LOOP UNTIL ((buffer$(350) = "! Non-Optimized Parameters !") OR (buffer$(350) = "!   Optimized Parameters   !"))
        IF MID$(buffer$(350), 5, 3) = "Opt" THEN
                PRINT "Optimised Geometry Found: ";
                opttype$ = "Opt"
        ELSEIF MID$(buffer$(350), 3, 7) = "Non-Opt" THEN
                PRINT "Non-Optimised Geometry Found: ";
                opttype$ = "Non"
        ELSE
                GOTO 2000
        END IF

'look for scanning variable

        scanval$ = "STOP"
        FOR x% = 350 TO 400
                IF MID$(buffer$(x%), 9, 15) = scanvar$ THEN scanval$ = TRIM$(MID$(buffer$(x%), 29, 12))
        NEXT x%
        IF scanval$ = "STOP" THEN GOTO 2000
        IF RIGHT$(scanval$, 1) = "." THEN scanval$ = scanval$ + "0"
        PRINT REWRITE$(scanvar$) + " = " + scanval$

'look for cartesian coordinates

        xx% = 350
        DO
                xx% = xx% - 1
                IF xx% = 0 THEN GOTO 2000
        LOOP UNTIL (buffer$(xx%) = "Input orientation:" OR buffer$(xx%) = "Standard orientation:")
        PRINT "Found cartesian coordinates";
        x% = 0
        DO
                x% = x% + 1
                IF x% > 20 THEN GOTO 2000
                coords$(x%, 1) = TRIM$(MID$(buffer$(x% + xx% + 4), 11, 3))
                IF coords$(x%, 1) = "1" THEN
                        coords$(x%, 1) = "H"
                ELSEIF coords$(x%, 1) = "5" THEN
                        coords$(x%, 1) = "B"
                ELSEIF coords$(x%, 1) = "6" THEN
                        coords$(x%, 1) = "C"
                ELSEIF coords$(x%, 1) = "7" THEN
                        coords$(x%, 1) = "N"
                ELSEIF coords$(x%, 1) = "8" THEN
                        coords$(x%, 1) = "O"
                ELSEIF coords$(x%, 1) = "9" THEN
                        coords$(x%, 1) = "F"
                ELSEIF coords$(x%, 1) = "13" THEN
                        coords$(x%, 1) = "Al"
                ELSEIF coords$(x%, 1) = "14" THEN
                        coords$(x%, 1) = "Si"
                ELSEIF coords$(x%, 1) = "15" THEN
                        coords$(x%, 1) = "P"
                ELSEIF coords$(x%, 1) = "16" THEN
                        coords$(x%, 1) = "S"
                ELSEIF coords$(x%, 1) = "17" THEN
                        coords$(x%, 1) = "Cl"
                ELSE
                        coords$(x%, 1) = "?"
                END IF
                coords$(x%, 2) = TRIM$(MID$(buffer$(x% + xx% + 4), 33, 10))
                coords$(x%, 3) = TRIM$(MID$(buffer$(x% + xx% + 4), 45, 10))
                coords$(x%, 4) = TRIM$(RIGHT$(buffer$(x% + xx% + 4), 10))
        LOOP UNTIL LEFT$(buffer$(x% + xx% + 5), 10) = "----------"
        numatoms% = x%

'look for rotational constants

        DO
                xx% = xx% + 1
                IF xx% = 400 THEN GOTO 2000
        LOOP UNTIL LEFT$(buffer$(xx%), 27) = "Rotational constants (GHZ):"
        PRINT ", Found rotational constants";
        rot1$ = TRIM$(MID$(buffer$(xx%), 32, 11))
        rot2$ = TRIM$(MID$(buffer$(xx%), 47, 11))
        rot3$ = TRIM$(RIGHT$(buffer$(xx%), 11))

'look for energy

        DO
                xx% = xx% + 1
                IF xx% = 400 THEN GOTO 2000
        LOOP UNTIL LEFT$(buffer$(xx%), 9) = "SCF Done:"
        PRINT ", Found energy";
        Energy$ = TRIM$(MID$(buffer$(xx%), 28, 15))
      
'look for spin

        IF LEFT$(buffer$(xx% + 4), 24) = "S**2 before annihilation" THEN
                PRINT " and spin"
                PRINT
                Spin$ = TRIM$(MID$(buffer$(xx% + 4), 30, 6))
        ELSE
                PRINT ""
                PRINT ""
        END IF

'loop for next optimised geometry and print data

        IF numpoints% = 1 THEN
                outtext$ = "Geom," + REWRITE$(scanvar$) + ",Opt,Energy,S(S+1)"
                FOR x% = 1 TO numatoms%
                        outtext$ = outtext$ + ",Atom" + STR$(x%) + ",X,Y,Z"
                NEXT x%
        PRINT #2, outtext$
        END IF
       
        outtext$ = ""
        FOR x% = 1 TO numatoms%
                FOR xx% = 1 TO 4
                        outtext$ = outtext$ + "," + coords$(x%, xx%)
                NEXT xx%
        NEXT x%
        outtext$ = LTRIM$(STR$(numpoints%)) + "," + scanval$ + "," + opttype$ + "," + Energy$ + "," + Spin$ + outtext$
        PRINT #2, outtext$
      
        LOOP

GOTO 2001

2000 REM End program (caused by unexpected input)

        CLOSE #1
        CLOSE #2
        STOP

2001 REM Finished with file

END SUB

FUNCTION TRIM$ (intext$)
outtext$ = LTRIM$(RTRIM$(intext$))
TRIM$ = outtext$
END FUNCTION

