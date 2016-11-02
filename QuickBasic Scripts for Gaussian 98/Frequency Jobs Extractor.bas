DECLARE FUNCTION TRIM$ (intext$)
DECLARE SUB MOVEBUFFER ()
DIM SHARED buffer$(300)
DIM vib$(55)

PRINT "==============================================================================="
INPUT "Enter Folder Name (without slashes): ", directoryname$
directoryname$ = "B:\" + directoryname$ + "\"
OPEN directoryname$ + "freq-job.csv" FOR OUTPUT AS #2

filecount% = 0
FOR counter% = 0 TO 999
filename$ = directoryname$ + "freq-" + RIGHT$("000" + TRIM$(STR$(counter%)), 3) + ".log"
ON ERROR GOTO 1000
fileexist$ = "YES"
OPEN filename$ FOR INPUT AS #1
ON ERROR GOTO 0
IF fileexist$ = "YES" THEN
        filecount% = filecount% + 1
        PRINT "==============================================================================="
        PRINT UCASE$(directoryname$) + "freq-" + RIGHT$("000" + TRIM$(STR$(counter%)), 3) + ".log file found..."
       
'look for job title

        DO
                MOVEBUFFER
        LOOP UNTIL LEFT$(buffer$(300), 54) = "Projected frequency calculation at reaction coordinate"
        distance$ = MID$(buffer$(300), 56)

'look for rotational constants

        DO
                MOVEBUFFER
                IF LEFT$(buffer$(300), 13) = "Job cpu time:" THEN GOTO 888
        LOOP UNTIL LEFT$(buffer$(300), 27) = "Rotational constants (GHZ):"
        rot1$ = TRIM$(MID$(buffer$(300), 32, 11))
        rot2$ = TRIM$(MID$(buffer$(300), 47, 11))
        rot3$ = TRIM$(RIGHT$(buffer$(300), 11))
        PRINT "Found rotational constants: " + rot1$ + ",  " + rot2$ + ",  " + rot3$

'look for energy

        DO
                MOVEBUFFER
                IF LEFT$(buffer$(300), 13) = "Job cpu time:" THEN GOTO 888
        LOOP UNTIL LEFT$(buffer$(290), 9) = "SCF Done:"
        energy$ = TRIM$(MID$(buffer$(290), 28, 15))
        PRINT "              Found energy: " + energy$
      
'look for spin

        IF LEFT$(buffer$(294), 24) = "S**2 before annihilation" THEN
                spin$ = TRIM$(MID$(buffer$(294), 30, 6))
                PRINT "                Found spin: " + spin$
        END IF

'look for vibrational frequencies

        DO
                MOVEBUFFER
                IF LEFT$(buffer$(300), 13) = "Job cpu time:" THEN GOTO 888
        LOOP UNTIL LEFT$(buffer$(300), 29) = "Total Reaction Path Curvature"

        x% = 300
        DO
                x% = x% - 1
        LOOP UNTIL LEFT$(buffer$(x%), 19) = "Low frequencies ---"
        transvib1$ = TRIM$(MID$(buffer$(x% - 1), 21, 9))
        transvib2$ = TRIM$(MID$(buffer$(x% - 1), 31, 9))
        transvib3$ = TRIM$(MID$(buffer$(x% - 1), 41, 9))
        rotvib1$ = TRIM$(MID$(buffer$(x% - 1), 51, 9))
        rotvib2$ = TRIM$(MID$(buffer$(x% - 1), 61, 9))
        rotvib3$ = TRIM$(RIGHT$(buffer$(x% - 1), 9))
        lowvib1$ = TRIM$(MID$(buffer$(x%), 21, 9))
        lowvib2$ = TRIM$(MID$(buffer$(x%), 31, 9))
        lowvib3$ = TRIM$(RIGHT$(buffer$(x%), 9))
       
        DO
                x% = x% + 1
                IF x% > 300 THEN STOP
        LOOP UNTIL LEFT$(buffer$(x%), 14) = "Frequencies --"
       
        vibset% = -1
        DO
                vibset% = vibset% + 1
                vibnum% = (vibset% * 3) + 1
                vib$(vibnum%) = TRIM$(MID$(buffer$(x%), 16, 10))
                IF LEN(buffer$(x%)) > 30 THEN
                        vibnum% = (vibset% * 3) + 2
                        vib$(vibnum%) = TRIM$(MID$(buffer$(x%), 39, 10))
                        IF LEN(buffer$(x%)) > 55 THEN
                                vibnum% = (vibset% * 3) + 3
                                vib$(vibnum%) = TRIM$(RIGHT$(buffer$(x%), 10))
                        END IF
                END IF
                status$ = "unknown"
                DO
                        x% = x% + 1
                        IF LEFT$(buffer$(x%), 14) = "Frequencies --" THEN status$ = "repeat"
                        IF LEFT$(buffer$(x%), 29) = "Total Reaction Path Curvature" THEN status$ = "finished"
                        IF x% > 300 THEN STOP
                LOOP UNTIL status$ <> "unknown"
        LOOP UNTIL status$ = "finished"
        PRINT "Found" + STR$(vibnum%) + " Vibrational Frequencies."


'Print out the data

        IF filecount% = 1 THEN
                PRINT #2, "Geom,Coord,Energy,S(S+1),Rot A,Rot B,Rot C,";
                FOR x% = 1 TO vibnum%
                        PRINT #2, "Vib" + STR$(x%) + ",";
                NEXT x%
                PRINT #2, "Zero 1,Zero 2,Zero 3,Zero 4,Zero 5,Zero 6,Low 1,Low 2,Low 3"
        END IF
       
        outtext$ = LTRIM$(STR$(filecount%)) + "," + distance$ + "," + energy$ + "," + spin$ + ","
        outtext$ = outtext$ + rot1$ + "," + rot2$ + "," + rot3$ + ","
        FOR x% = 1 TO vibnum%
                outtext$ = outtext$ + vib$(x%) + ","
        NEXT x%
        outtext$ = outtext$ + transvib1$ + "," + transvib2$ + "," + transvib3$ + ","
        outtext$ = outtext$ + rotvib1$ + "," + rotvib2$ + "," + rotvib3$ + ","
        outtext$ = outtext$ + lowvib1$ + "," + lowvib2$ + "," + lowvib3$
        PRINT #2, outtext$

888 REM End of log file

        PRINT "End of log file"
        PRINT
        CLOSE #1
END IF
NEXT counter%

PRINT "End of Program"
CLOSE #2
STOP

1000 REM file doesn't exist

fileexist$ = "NO"
RESUME NEXT

SUB MOVEBUFFER

'read in text until a carriage return is found
'used for native unix format which doesn't
'use the line feed characters

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

'scroll text buffer up

        FOR scroller% = 1 TO 299
                buffer$(scroller%) = buffer$(scroller% + 1)
        NEXT scroller%
        buffer$(300) = readline$

END SUB

SUB PAUSE

WHILE INKEY$ <> CHR$(13)
WEND

END SUB

FUNCTION TRIM$ (intext$)
outtext$ = LTRIM$(RTRIM$(intext$))
TRIM$ = outtext$
END FUNCTION

