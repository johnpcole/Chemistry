DECLARE SUB PAUSE ()
DECLARE FUNCTION TRIM$ (intext$)
DECLARE SUB MOVEBUFFER ()
DIM SHARED buffer$(30)
DIM SHARED atomdata$(20, 5)


'open input file, a native Gaussian LOG file

PRINT "==============================================================================="
INPUT "Enter Sub-Folder Name (without slashes): ", foldername$
INPUT "Enter File Name Prefix: ", fileprefix$
IF ((UCASE$(fileprefix$) = "STOP") OR (UCASE$(foldername$) = "STOP")) THEN STOP
      
'open output file, based on CSV format (excel compatible)
OPEN "B:\" + foldername$ + "\" + fileprefix$ + "Jobs.csv" FOR OUTPUT AS #2
PRINT #2, "Job,Energy,Atom 1,X1,Y1,Z1,Charge 1,Atom 2,X2,Y2,Z2,Charge 2,Atom 3,X3,Y3,Z3,Charge 3,Atom 4,X4,Y4,Z4,Charge 4,Atom 5,X5,Y5,Z5,Charge 5,..."

FOR filenum% = 0 TO 999
filename$ = fileprefix$ + RIGHT$("000" + LTRIM$(STR$(filenum%)), 3)

inputfilename$ = "B:\" + foldername$ + "\" + filename$ + ".log"
ON ERROR GOTO 1000
fileexist$ = "YES"
OPEN inputfilename$ FOR INPUT AS #1
ON ERROR GOTO 0
       
       
        IF fileexist$ = "YES" THEN
       
        PRINT "Found "; inputfilename$; ", opening...   "

' Find atom coordinates

        DO
        MOVEBUFFER
        LOOP UNTIL (buffer$(1) = "Input orientation:" OR buffer$(1) = "Standard orientation:")
        PRINT "Found atom coordinates";
       
        x% = 0
        DO
                x% = x% + 1
                IF x% > 20 THEN STOP
                atomdata$(x%, 1) = TRIM$(MID$(buffer$(x% + 5), 11, 3))
                IF atomdata$(x%, 1) = "1" THEN
                        atomdata$(x%, 1) = "H"
                ELSEIF atomdata$(x%, 1) = "5" THEN
                        atomdata$(x%, 1) = "B"
                ELSEIF atomdata$(x%, 1) = "6" THEN
                        atomdata$(x%, 1) = "C"
                ELSEIF atomdata$(x%, 1) = "7" THEN
                        atomdata$(x%, 1) = "N"
                ELSEIF atomdata$(x%, 1) = "8" THEN
                        atomdata$(x%, 1) = "O"
                ELSEIF atomdata$(x%, 1) = "9" THEN
                        atomdata$(x%, 1) = "F"
                ELSEIF atomdata$(x%, 1) = "13" THEN
                        atomdata$(x%, 1) = "Al"
                ELSEIF atomdata$(x%, 1) = "14" THEN
                        atomdata$(x%, 1) = "Si"
                ELSEIF atomdata$(x%, 1) = "15" THEN
                        atomdata$(x%, 1) = "P"
                ELSEIF atomdata$(x%, 1) = "16" THEN
                        atomdata$(x%, 1) = "S"
                ELSEIF atomdata$(x%, 1) = "17" THEN
                        atomdata$(x%, 1) = "Cl"
                ELSE
                        atomdata$(x%, 1) = "?"
                END IF
                atomdata$(x%, 2) = TRIM$(MID$(buffer$(x% + 5), 33, 10))
                atomdata$(x%, 3) = TRIM$(MID$(buffer$(x% + 5), 45, 10))
                atomdata$(x%, 4) = TRIM$(RIGHT$(buffer$(x% + 5), 10))
        LOOP UNTIL LEFT$(buffer$(x% + 6), 10) = "----------"
        numatoms% = x%

'Find QCISDT Energy

        DO
        MOVEBUFFER
        LOOP UNTIL LEFT$(buffer$(30), 9) = "QCISD(T)="
        PRINT ", QCISDT energy";
        energy$ = TRIM$(MID$(buffer$(30), 10))

        FOR x% = 1 TO LEN(energy$)
        IF MID$(energy$, x%, 1) = "D" THEN MID$(energy$, x%, 1) = "E"
        NEXT x%

'Find Atomic Charges
       
        DO
        MOVEBUFFER
        LOOP UNTIL buffer$(1) = "Total atomic charges:"
        PRINT " & atomic charges."

        FOR x% = 1 TO numatoms%
        atomdata$(x%, 5) = TRIM$(RIGHT$(buffer$(x% + 2), 10))
        NEXT x%

        CLOSE #1
      

'Print data out

        PRINT #2, filename$; ","; energy$; ",";
        FOR x% = 1 TO numatoms%
        FOR y% = 1 TO 5
        PRINT #2, atomdata$(x%, y%); ",";
        NEXT y%
        NEXT x%
        PRINT #2, ""

END IF

NEXT filenum%
       
CLOSE #2

PRINT "--------"
PRINT "All done"
PRINT "--------"

STOP

1000 REM file doesn't exist

fileexist$ = "NO"
RESUME NEXT

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

        FOR scroller% = 1 TO 29
                buffer$(scroller%) = buffer$(scroller% + 1)
        NEXT scroller%
        buffer$(30) = readline$

END SUB

SUB PAUSE

WHILE INKEY$ <> CHR$(13)
WEND

END SUB

FUNCTION TRIM$ (intext$)
outtext$ = LTRIM$(RTRIM$(intext$))
TRIM$ = outtext$
END FUNCTION

