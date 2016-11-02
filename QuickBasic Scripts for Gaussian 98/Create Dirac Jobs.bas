folder$ = "Nitrogen/Path-A"

copychk$ = "YES"

' Clear the pending jobs folder

OPEN "B:\PENDIN~1\temp.tmp" FOR OUTPUT AS #1
PRINT #1, "Temporary File"
CLOSE #1
KILL "B:\PENDIN~1\*.*"



FOR direc% = -1 TO -1

IF direc% = -1 THEN prefix$ = "job-"
IF direc% = 0 THEN prefix$ = "for-"
IF direc% = 1 THEN prefix$ = "rev-"

FOR x% = 0 TO 990 STEP 10

filename$ = prefix$ + RIGHT$("000" + LTRIM$(STR$(x%)), 3)

OPEN "B:\PENDIN~1\" + filename$ + ".job" FOR OUTPUT AS 1

PRINT #1, "#!/bin/tcsh -f"
PRINT #1, "#"
PRINT #1, "#PBS -l walltime=10:00:00"
PRINT #1, "#PBS -j oe"
PRINT #1, "#"
PRINT #1, "#-----------------------------------------------"
PRINT #1, "#"
PRINT #1, "setenv MYDIR ${HOME}/Documents/"; folder$
PRINT #1, "setenv MYINFILE "; filename$; ".com"
PRINT #1, "setenv MYOUTFILE "; filename$; ".log"
PRINT #1, "#"
PRINT #1, "#-----------------------------------------------"
PRINT #1, "#"
PRINT #1, "setenv JOBNO "; CHR$(34); "`echo $PBS_JOBID | sed s/.dirac.beo.org//`"; CHR$(34)
PRINT #1, "setenv SCRATCHDIR "; CHR$(34); "/data/"; filename$; CHR$(34)
PRINT #1, "mkdir ${SCRATCHDIR}"
PRINT #1, "#"
PRINT #1, "#-----------------------------------------------"
PRINT #1, "#"
PRINT #1, "setenv g98root /usr/local"
PRINT #1, "setenv GAUSS_SCRDIR ${SCRATCHDIR}"
PRINT #1, "source ${g98root}/g98/bsd/g98.login"
PRINT #1, "#"
PRINT #1, "#-----------------------------------------------"
PRINT #1, "#"
PRINT #1, "hostname > ${MYDIR}/rundat"
PRINT #1, "echo ${SCRATCHDIR} >> ${MYDIR}/rundat"
PRINT #1, "#"
PRINT #1, "#-----------------------------------------------"
PRINT #1, "#"
PRINT #1, "cd ${MYDIR}"
IF copychk$ = "YES" THEN PRINT #1, "cp ${MYDIR}/guess.chk ${SCRATCHDIR}/"; filename$; ".chk"
PRINT #1, "g98 < ${MYINFILE} > ${MYOUTFILE}"
PRINT #1, "#"
PRINT #1, "#-----------------------------------------------"
PRINT #1, "#"
PRINT #1, "rm -fr ${SCRATCHDIR}"
PRINT #1, ""
PRINT #1, ""
PRINT #1, ""

CLOSE 1

NEXT x%

NEXT direc%

