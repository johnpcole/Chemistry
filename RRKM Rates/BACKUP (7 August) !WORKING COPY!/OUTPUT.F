      SUBROUTINE OUTPUT(THERM,STATES,PATHMODE,OUT_MIN,OUT_MAX,
     &                           OUT_STEP,T_RATE,ARRAY,SYSLIM,
     &                       SYSMAX,GRID,BASE_E,P_FUNC,DOSSOS,
     &                KB,H,A,J_VAL,FILETYPE,FINAL,ECHO,BI_MOL)

      ! THIS SUBROUTINE CONVERTS THE DENSITIES AND SUMS OF STATES INTO
      ! RATE CONSTANTS OR JUST PRINTS THEM OUT AS REQUIRED

      IMPLICIT NONE

      LOGICAL THERM,STATES,PATHMODE,FILETYPE,BI_MOL
      LOGICAL CRIT_FLA,FINAL,ECHO,REACTIVE
      INTEGER ENERGY,E_ZERO,E_MIN,E_MAX,E_STEP,E_GAP
      INTEGER ARRAY,SYSLIM,SYSMAX,J_VAL
      INTEGER INDEX,RATENUM,CUT_OFF
      INTEGER CRIT_LEF,CRIT_RIG,CRIT_GEO
      REAL*8 OUT_MIN,OUT_MAX,OUT_STEP,TEMP
      REAL*8 DOSSOS,EJRATE,T_RATE,P_FUNC
      REAL*8 GRID,FACTOR,KB,A,H
      REAL*8 E_DIFF,BASE_E,CRIT_VAL
      CHARACTER*9 FILENAME

      DIMENSION DOSSOS(0:SYSLIM,ARRAY)
      DIMENSION T_RATE(1000,10)
      DIMENSION P_FUNC(1000)
      DIMENSION EJRATE(10)
      DIMENSION REACTIVE(0:10)
      DIMENSION CUT_OFF(0:10)

C------------------------------------------------------------------------------------------------------
C- CALCULATE THE BIMOLECULAR PARTITION FUNCTION FOR ALL Js SEPARATELY
C------------------------------------------------------------------------------------------------------

      IF (J_VAL.LT.0) THEN

         DO ENERGY=1,ARRAY

            E_DIFF=((DFLOAT(ENERGY)+0.5D0)*GRID)-BASE_E                    ! ENERGY RELATIVE TO REAGENTS

            INDEX=0
            DO TEMP=OUT_MIN,OUT_MAX,OUT_STEP
               INDEX=INDEX+1

               FACTOR=DEXP((-E_DIFF)/(KB*TEMP*A))*GRID                     ! exp(-E/kT)dE
               FACTOR=FACTOR*DOSSOS(0,ENERGY)                              ! rho(E,J)exp(-E/kT)dE

      ! BUILD UP THE PARTITION FUNCTION HERE FOR THE BIMOLECULAR REACTION

               P_FUNC(INDEX)=P_FUNC(INDEX)+FACTOR

               END DO ! LOOP OVER TEMPERATURES

            END DO ! LOOP OVER ENERGIES

         ELSE

C------------------------------------------------------------------------------------------------------
C- CALCULATE THE THERMAL RATE CONSTANTS AND STORE THEM
C------------------------------------------------------------------------------------------------------

      ! CALCULATE THE THERMAL RATE CONSTANTS k(T) FOR CURRENT J VALUE
      ! BY SUMMING UP THE INDIVIDUAL MICROCANONICAL RATE CONSTANTS k(E,J)
      ! AND INCLUDING THE BOLTZMANN DISTRIBUTION PROBABILITY
      !                                    k(T)=k(T)+[k(EJ)*EXP(-E/RT)*dE]
      ! NOTE THAT INTEGRATION REQUIRES MULTIPLICATION WITH GRID SIZE (dE)
      ! ALSO BUILD UP THE REAGENT PARTITION FUNCTION

      IF (THERM) THEN

         DO INDEX=0,10
            REACTIVE(INDEX)=(.FALSE.)
            CUT_OFF(INDEX)=0
            END DO

         DO ENERGY=1,ARRAY

            DO INDEX=1,10
               EJRATE(INDEX)=0.0D0
               END DO

            CALL FORMULA(ENERGY,DOSSOS,ARRAY,SYSLIM,EJRATE,H,A)

            IF ((ECHO).OR.(FINAL)) THEN
               DO RATENUM=1,10
                  IF (.NOT.(REACTIVE(RATENUM))) THEN
                     IF (EJRATE(RATENUM).EQ.(0.0D0)) THEN
                        CUT_OFF(RATENUM)=ENERGY
                        ELSE
                        REACTIVE(RATENUM)=(.TRUE.)
                        END IF
                     END IF
                  END DO
               END IF

            E_DIFF=((DFLOAT(ENERGY)+0.5D0)*GRID)-BASE_E                    ! ENERGY RELATIVE TO REAGENTS

            INDEX=0
            DO TEMP=OUT_MIN,OUT_MAX,OUT_STEP
               INDEX=INDEX+1

               FACTOR=DEXP((-E_DIFF)/(KB*TEMP*A))*GRID                     ! exp(-E/kT)dE
               FACTOR=FACTOR*DOSSOS(0,ENERGY)                              ! rho(E,J)exp(-E/kT)dE

      ! BUILD UP THE THERMAL RATE HERE

               DO RATENUM=1,10
                  T_RATE(INDEX,RATENUM)=T_RATE(INDEX,RATENUM)
     &                                         +(FACTOR*EJRATE(RATENUM))
                  END DO ! LOOP OVER RATE COEFFICIENTS

      ! BUILD UP THE PARTITION FUNCTION HERE, IF UNIMOLECULAR REACTION

               IF (.NOT.(BI_MOL)) P_FUNC(INDEX)=P_FUNC(INDEX)+FACTOR

               END DO ! LOOP OVER TEMPERATURES

            END DO ! LOOP OVER ENERGIES

C---------------------------------
C- TEST TO SEE J IS LARGE ENOUGH
C---------------------------------

         IF ((ECHO).OR.(FINAL)) THEN

            WRITE(6,'(71(''~''))')
            WRITE(6,'(A,I3,A)') '   J=',J_VAL,' CONTRIBUTION CHECK'
            WRITE(6,'(71(''~''))')

            DO ENERGY=1,ARRAY
               IF (.NOT.(REACTIVE(0))) THEN
                  IF (DOSSOS(0,ENERGY).EQ.(0.0D0)) THEN
                     CUT_OFF(0)=ENERGY
                     ELSE
                     REACTIVE(0)=(.TRUE.)
                     END IF
                  END IF
               END DO

            IF (REACTIVE(0)) THEN
               WRITE(6,'(A,F9.3,A)')
     &                             '    Reagent contribution starts at',
     &                               ((1+ARRAY-CUT_OFF(0))*GRID/1000.0),
     &                                             ' kJ/mol below limit'
               ELSE
               WRITE(6,'(A)')
     &                    ' No Reagent contribution for this value of J'
               END IF

            IF (REACTIVE(0)) THEN
               DO RATENUM=1,10
                  IF (REACTIVE(RATENUM)) THEN
                     WRITE(6,'(A,I2,A,F9.3,A)')
     &                    ' Contribution to Rate ',RATENUM,' starts at',
     &                         ((1+ARRAY-CUT_OFF(RATENUM))*GRID/1000.0),
     &                                             ' kJ/mol below limit'
                     ELSE
                     WRITE(6,'(A,I2,A)') ' No contribution to Rate ',
     &                                    RATENUM,' for this value of J'
                     END IF
                  END DO
               END IF

            END IF

C---------------------------------
C- ON FINAL J, FINISH THERMAL
C- RATES AND PRINT OUT RESULTS
C---------------------------------

         IF (FINAL) THEN

            IF (FILETYPE) THEN
c               OPEN(44,FILE=('B:\THERMAL.CSV'))
               ELSE
               OPEN(44,FILE=('THERMAL.CSV'))
               END IF

            WRITE(44,'(A)') 'Temp [K],k1(T),k2(T),k3(T),k4(T),k5(T)'

            INDEX=0
            DO TEMP=OUT_MIN,OUT_MAX,OUT_STEP
               INDEX=INDEX+1

               WRITE(6,'(71(''-''))')
               WRITE(6,'(A,F7.2,A)')
     &                     '               Temperature: ',TEMP,' K'
               IF (ECHO) THEN
                  IF (BI_MOL) THEN
                     WRITE(6,'(A,E11.5,A)')
     &                    ' Total reactant system partition function: ',
     &                                              P_FUNC(INDEX),' /m3'
                     ELSE

                     WRITE(6,'(A,E11.5)')
     &                    ' Total reactant system partition function: ',
     &                                                     P_FUNC(INDEX)
                     END IF
                  END IF

               DO RATENUM=1,10
                  T_RATE(INDEX,RATENUM)=T_RATE(INDEX,RATENUM)
     &                                          /(P_FUNC(INDEX))

                  IF (BI_MOL) THEN
                     WRITE(6,'(A,I2,A,I4,A,E16.10,A)')
     &                   '   Rate constant ',RATENUM,' (',INT(TEMP+0.5),
     &                          'K): ',T_RATE(INDEX,RATENUM),' m3/mol/s'
                     ELSE
                     WRITE(6,'(A,I2,A,I4,A,E16.10,A)')
     &                   '   Rate constant ',RATENUM,' (',INT(TEMP+0.5),
     &                                'K): ',T_RATE(INDEX,RATENUM),' /s'
                     END IF

                  END DO ! RATENUM


               WRITE(44,'(F7.2,10('','',E26.20))')
     &                         TEMP,(T_RATE(INDEX,RATENUM),RATENUM=1,10)

               CALL FLUSH(6)
               CALL FLUSH(44)

               END DO ! TEMPERATURE

               CLOSE(44)

            END IF ! FINAL J_VAL

C------------------------------------------------------------------------------------------------------
C- OPEN MICROCANONICAL OUTPUT FILE IF REQUIRED
C------------------------------------------------------------------------------------------------------

         ELSE ! NOT THERM

         IF (FILETYPE) THEN
c            OPEN(44,FILE=('B:\'//FILENAME(J_VAL)))
            ELSE
            OPEN(44,FILE=(FILENAME(J_VAL)))
            END IF

      E_ZERO=INT(BASE_E/GRID)
      E_MIN=INT((OUT_MIN+BASE_E)/GRID)
      IF (OUT_MIN.LT.-BASE_E) E_MIN=E_MIN-1
      E_MAX=INT((OUT_MAX+BASE_E)/GRID)
      IF (OUT_MAX.LT.-BASE_E) E_MAX=E_MAX-1
      E_STEP=INT(OUT_STEP/GRID)

      IF (STATES) THEN

C------------------------------------------------------------------------------------------------------
C- PRINT OUT THE CRITICAL GEOMETRY AND THE SUMS OF STATES OF EACH GEOMETRY
C------------------------------------------------------------------------------------------------------

      IF (PATHMODE) THEN

         WRITE(44,'(A,100('','',I3))')
     &                 ' Energy [kJ/mol],Critical Geometry',
     &                                  (INDEX,INDEX=1,SYSMAX)

         DO ENERGY=1,ARRAY

            E_GAP=ENERGY-E_MIN

            IF ((ENERGY.GE.E_MIN).AND.(ENERGY.LE.E_MAX)) THEN
            IF (MOD(E_GAP,E_STEP).EQ.(0)) THEN

               CRIT_VAL=DOSSOS(1,ENERGY)                       ! ASSUME THAT FIRST GEOMETRY IS MINIMUM
               CRIT_LEF=1
               DO INDEX=1,SYSMAX
                  IF (DOSSOS(INDEX,ENERGY).LT.CRIT_VAL) THEN   ! WORK WAY OUT
                     CRIT_VAL=DOSSOS(INDEX,ENERGY)
                     CRIT_LEF=INDEX
                     END IF
                  END DO
               IF (CRIT_VAL.LE.(0.0D0)) THEN                   ! IF ZERO SUM OF STATES, USE
                  CRIT_GEO=-CRIT_LEF                           ! CUT-OFF GEOMETRY AND NEGATE
                  ELSE
                  IF (CRIT_LEF.LT.SYSMAX) THEN                 ! IF CRITICAL GEOMETRY IS NOT THE FURTHEST
                     CRIT_RIG=CRIT_LEF                         ! OUT FIND OUT HOW MANY ADJACENT POINTS
                     CRIT_FLA=.TRUE.                           ! HAVE THE SAME SUM OF STATES VALUE
                     DO INDEX=(CRIT_LEF+1),SYSMAX
                        IF (DOSSOS(INDEX,ENERGY).EQ.CRIT_VAL) THEN
                           IF (CRIT_FLA) CRIT_RIG=INDEX
                           ELSE
                           CRIT_FLA=.FALSE.
                           END IF
                        END DO
                     CRIT_GEO=(CRIT_LEF+CRIT_RIG)/2            ! TAKE THE MIDDLE VALUE TO BE THE CRITICAL GEOMETRY
                     ELSE
                     CRIT_GEO=CRIT_LEF
                     END IF
                  END IF

               WRITE(44,'(F10.3,'','',I3,100('','',E26.20))')
     &                     (DFLOAT(ENERGY-E_ZERO)*GRID/1000.0),CRIT_GEO,
     &                             (DOSSOS(INDEX,ENERGY),INDEX=1,SYSMAX)

               END IF
               END IF

            END DO ! ENERGY

C------------------------------------------------------------------------------------------------------
C- PRINT OUT THE DENSITIES OF STATES OF REAGENTS AND INTERMEDIATES, AND THE
C- MINIMISED SUMS OF STATES OF TRANSITION STATES AND MINIMUM ENERGY PATHS
C------------------------------------------------------------------------------------------------------

         ELSE ! NOT PATHMODE

         WRITE(44,'(A,A)')
     &           ' Energy [kJ/mol],Densities [mol/J(/m3)] and Sums of ',
     &          'States of reagents & intermediates & transition states'

         DO ENERGY=1,ARRAY

            E_GAP=ENERGY-E_MIN

            IF ((ENERGY.GE.E_MIN).AND.(ENERGY.LE.E_MAX)) THEN
            IF (MOD(E_GAP,E_STEP).EQ.(0))
     &                             WRITE(44,'(F10.3,100('','',E26.20))')
     &                              (DFLOAT(ENERGY-E_ZERO)*GRID/1000.0),
     &                             (DOSSOS(INDEX,ENERGY),INDEX=0,SYSMAX)

               END IF

            END DO ! ENERGY

         END IF ! PATHOMDE / NOT PATHMODE

C------------------------------------------------------------------------------------------------------
C- CALCULATE THE MICROCANONICAL RATE CONSTANTS, AND PRINT THEM OUT
C------------------------------------------------------------------------------------------------------

         ELSE ! NOT STATES

         WRITE(44,'(A,A)')
     &        ' Energy [kJ/mol],k1(EJ),k2(EJ),k3(EJ),k4(EJ),k5(EJ)',
     &        '... [/s or m3/mol/s]'

         DO ENERGY=1,ARRAY

            E_GAP=ENERGY-E_MIN

            IF ((ENERGY.GE.E_MIN).AND.(ENERGY.LE.E_MAX)) THEN
            IF (MOD(E_GAP,E_STEP).EQ.(0)) THEN

               DO INDEX=1,10
                  EJRATE(INDEX)=0.0D0
                  END DO

               CALL FORMULA(ENERGY,DOSSOS,ARRAY,SYSLIM,EJRATE,H,A)

               WRITE(44,'(F10.3,10('','',E26.20))')
     &                             (DFLOAT(ENERGY-E_ZERO)*GRID/1000.0),
     &                                       (EJRATE(INDEX),INDEX=1,10)

               END IF
               END IF

            END DO ! LOOP OVER ENERGIES

         END IF ! STATES / NOT STATES

C------------------------------------------------------------------------------------------------------
C- CLOSE MICROCANONICAL OUTPUT FILE IF REQUIRED
C------------------------------------------------------------------------------------------------------

         CLOSE(44)

         END IF ! THERM / NOT THERM

         END IF ! NEGATIVE J_VAL

      CALL FLUSH(6)
      RETURN
      END

C------------------------------------------------------------------------------------------------------
C------------------------------------------------------------------------------------------------------
C------------------------------------------------------------------------------------------------------

      FUNCTION FILENAME(J_VAL)

      IMPLICIT NONE

      CHARACTER*9 FILENAME,TEXT
      INTEGER J_VAL

      IF (J_VAL.EQ.0) TEXT='J-000.CSV'
      IF (J_VAL.EQ.1) TEXT='J-001.CSV'
      IF (J_VAL.EQ.2) TEXT='J-002.CSV'
      IF (J_VAL.EQ.3) TEXT='J-003.CSV'
      IF (J_VAL.EQ.4) TEXT='J-004.CSV'
      IF (J_VAL.EQ.5) TEXT='J-005.CSV'
      IF (J_VAL.EQ.6) TEXT='J-006.CSV'
      IF (J_VAL.EQ.7) TEXT='J-007.CSV'
      IF (J_VAL.EQ.8) TEXT='J-008.CSV'
      IF (J_VAL.EQ.9) TEXT='J-009.CSV'
      IF (J_VAL.EQ.10) TEXT='J-010.CSV'
      IF (J_VAL.EQ.11) TEXT='J-011.CSV'
      IF (J_VAL.EQ.12) TEXT='J-012.CSV'
      IF (J_VAL.EQ.13) TEXT='J-013.CSV'
      IF (J_VAL.EQ.14) TEXT='J-014.CSV'
      IF (J_VAL.EQ.15) TEXT='J-015.CSV'
      IF (J_VAL.EQ.16) TEXT='J-016.CSV'
      IF (J_VAL.EQ.17) TEXT='J-017.CSV'
      IF (J_VAL.EQ.18) TEXT='J-018.CSV'
      IF (J_VAL.EQ.19) TEXT='J-019.CSV'
      IF (J_VAL.EQ.20) TEXT='J-020.CSV'
      IF (J_VAL.EQ.21) TEXT='J-021.CSV'
      IF (J_VAL.EQ.22) TEXT='J-022.CSV'
      IF (J_VAL.EQ.23) TEXT='J-023.CSV'
      IF (J_VAL.EQ.24) TEXT='J-024.CSV'
      IF (J_VAL.EQ.25) TEXT='J-025.CSV'
      IF (J_VAL.EQ.26) TEXT='J-026.CSV'
      IF (J_VAL.EQ.27) TEXT='J-027.CSV'
      IF (J_VAL.EQ.28) TEXT='J-028.CSV'
      IF (J_VAL.EQ.29) TEXT='J-029.CSV'
      IF (J_VAL.EQ.30) TEXT='J-030.CSV'
      IF (J_VAL.EQ.31) TEXT='J-031.CSV'
      IF (J_VAL.EQ.32) TEXT='J-032.CSV'
      IF (J_VAL.EQ.33) TEXT='J-033.CSV'
      IF (J_VAL.EQ.34) TEXT='J-034.CSV'
      IF (J_VAL.EQ.35) TEXT='J-035.CSV'
      IF (J_VAL.EQ.36) TEXT='J-036.CSV'
      IF (J_VAL.EQ.37) TEXT='J-037.CSV'
      IF (J_VAL.EQ.38) TEXT='J-038.CSV'
      IF (J_VAL.EQ.39) TEXT='J-039.CSV'
      IF (J_VAL.EQ.40) TEXT='J-040.CSV'
      IF (J_VAL.EQ.41) TEXT='J-041.CSV'
      IF (J_VAL.EQ.42) TEXT='J-042.CSV'
      IF (J_VAL.EQ.43) TEXT='J-043.CSV'
      IF (J_VAL.EQ.44) TEXT='J-044.CSV'
      IF (J_VAL.EQ.45) TEXT='J-045.CSV'
      IF (J_VAL.EQ.46) TEXT='J-046.CSV'
      IF (J_VAL.EQ.47) TEXT='J-047.CSV'
      IF (J_VAL.EQ.48) TEXT='J-048.CSV'
      IF (J_VAL.EQ.49) TEXT='J-049.CSV'
      IF (J_VAL.EQ.50) TEXT='J-050.CSV'
      IF (J_VAL.EQ.51) TEXT='J-051.CSV'
      IF (J_VAL.EQ.52) TEXT='J-052.CSV'
      IF (J_VAL.EQ.53) TEXT='J-053.CSV'
      IF (J_VAL.EQ.54) TEXT='J-054.CSV'
      IF (J_VAL.EQ.55) TEXT='J-055.CSV'
      IF (J_VAL.EQ.56) TEXT='J-056.CSV'
      IF (J_VAL.EQ.57) TEXT='J-057.CSV'
      IF (J_VAL.EQ.58) TEXT='J-058.CSV'
      IF (J_VAL.EQ.59) TEXT='J-059.CSV'
      IF (J_VAL.EQ.60) TEXT='J-060.CSV'
      IF (J_VAL.EQ.61) TEXT='J-061.CSV'
      IF (J_VAL.EQ.62) TEXT='J-062.CSV'
      IF (J_VAL.EQ.63) TEXT='J-063.CSV'
      IF (J_VAL.EQ.64) TEXT='J-064.CSV'
      IF (J_VAL.EQ.65) TEXT='J-065.CSV'
      IF (J_VAL.EQ.66) TEXT='J-066.CSV'
      IF (J_VAL.EQ.67) TEXT='J-067.CSV'
      IF (J_VAL.EQ.68) TEXT='J-068.CSV'
      IF (J_VAL.EQ.69) TEXT='J-069.CSV'
      IF (J_VAL.EQ.70) TEXT='J-070.CSV'
      IF (J_VAL.EQ.71) TEXT='J-071.CSV'
      IF (J_VAL.EQ.72) TEXT='J-072.CSV'
      IF (J_VAL.EQ.73) TEXT='J-073.CSV'
      IF (J_VAL.EQ.74) TEXT='J-074.CSV'
      IF (J_VAL.EQ.75) TEXT='J-075.CSV'
      IF (J_VAL.EQ.76) TEXT='J-076.CSV'
      IF (J_VAL.EQ.77) TEXT='J-077.CSV'
      IF (J_VAL.EQ.78) TEXT='J-078.CSV'
      IF (J_VAL.EQ.79) TEXT='J-079.CSV'
      IF (J_VAL.EQ.80) TEXT='J-080.CSV'
      IF (J_VAL.EQ.81) TEXT='J-081.CSV'
      IF (J_VAL.EQ.82) TEXT='J-082.CSV'
      IF (J_VAL.EQ.83) TEXT='J-083.CSV'
      IF (J_VAL.EQ.84) TEXT='J-084.CSV'
      IF (J_VAL.EQ.85) TEXT='J-085.CSV'
      IF (J_VAL.EQ.86) TEXT='J-086.CSV'
      IF (J_VAL.EQ.87) TEXT='J-087.CSV'
      IF (J_VAL.EQ.88) TEXT='J-088.CSV'
      IF (J_VAL.EQ.89) TEXT='J-089.CSV'
      IF (J_VAL.EQ.90) TEXT='J-090.CSV'
      IF (J_VAL.EQ.91) TEXT='J-091.CSV'
      IF (J_VAL.EQ.92) TEXT='J-092.CSV'
      IF (J_VAL.EQ.93) TEXT='J-093.CSV'
      IF (J_VAL.EQ.94) TEXT='J-094.CSV'
      IF (J_VAL.EQ.95) TEXT='J-095.CSV'
      IF (J_VAL.EQ.96) TEXT='J-096.CSV'
      IF (J_VAL.EQ.97) TEXT='J-097.CSV'
      IF (J_VAL.EQ.98) TEXT='J-098.CSV'
      IF (J_VAL.EQ.99) TEXT='J-099.CSV'
      IF (J_VAL.EQ.100) TEXT='J-100.CSV'
      IF (J_VAL.EQ.101) TEXT='J-101.CSV'
      IF (J_VAL.EQ.102) TEXT='J-102.CSV'
      IF (J_VAL.EQ.103) TEXT='J-103.CSV'
      IF (J_VAL.EQ.104) TEXT='J-104.CSV'
      IF (J_VAL.EQ.105) TEXT='J-105.CSV'
      IF (J_VAL.EQ.106) TEXT='J-106.CSV'
      IF (J_VAL.EQ.107) TEXT='J-107.CSV'
      IF (J_VAL.EQ.108) TEXT='J-108.CSV'
      IF (J_VAL.EQ.109) TEXT='J-109.CSV'
      IF (J_VAL.EQ.110) TEXT='J-110.CSV'
      IF (J_VAL.EQ.111) TEXT='J-111.CSV'
      IF (J_VAL.EQ.112) TEXT='J-112.CSV'
      IF (J_VAL.EQ.113) TEXT='J-113.CSV'
      IF (J_VAL.EQ.114) TEXT='J-114.CSV'
      IF (J_VAL.EQ.115) TEXT='J-115.CSV'
      IF (J_VAL.EQ.116) TEXT='J-116.CSV'
      IF (J_VAL.EQ.117) TEXT='J-117.CSV'
      IF (J_VAL.EQ.118) TEXT='J-118.CSV'
      IF (J_VAL.EQ.119) TEXT='J-119.CSV'
      IF (J_VAL.EQ.120) TEXT='J-120.CSV'
      IF (J_VAL.EQ.121) TEXT='J-121.CSV'
      IF (J_VAL.EQ.122) TEXT='J-122.CSV'
      IF (J_VAL.EQ.123) TEXT='J-123.CSV'
      IF (J_VAL.EQ.124) TEXT='J-124.CSV'
      IF (J_VAL.EQ.125) TEXT='J-125.CSV'
      IF (J_VAL.EQ.126) TEXT='J-126.CSV'
      IF (J_VAL.EQ.127) TEXT='J-127.CSV'
      IF (J_VAL.EQ.128) TEXT='J-128.CSV'
      IF (J_VAL.EQ.129) TEXT='J-129.CSV'
      IF (J_VAL.EQ.130) TEXT='J-130.CSV'
      IF (J_VAL.EQ.131) TEXT='J-131.CSV'
      IF (J_VAL.EQ.132) TEXT='J-132.CSV'
      IF (J_VAL.EQ.133) TEXT='J-133.CSV'
      IF (J_VAL.EQ.134) TEXT='J-134.CSV'
      IF (J_VAL.EQ.135) TEXT='J-135.CSV'
      IF (J_VAL.EQ.136) TEXT='J-136.CSV'
      IF (J_VAL.EQ.137) TEXT='J-137.CSV'
      IF (J_VAL.EQ.138) TEXT='J-138.CSV'
      IF (J_VAL.EQ.139) TEXT='J-139.CSV'
      IF (J_VAL.EQ.140) TEXT='J-140.CSV'
      IF (J_VAL.EQ.141) TEXT='J-141.CSV'
      IF (J_VAL.EQ.142) TEXT='J-142.CSV'
      IF (J_VAL.EQ.143) TEXT='J-143.CSV'
      IF (J_VAL.EQ.144) TEXT='J-144.CSV'
      IF (J_VAL.EQ.145) TEXT='J-145.CSV'
      IF (J_VAL.EQ.146) TEXT='J-146.CSV'
      IF (J_VAL.EQ.147) TEXT='J-147.CSV'
      IF (J_VAL.EQ.148) TEXT='J-148.CSV'
      IF (J_VAL.EQ.149) TEXT='J-149.CSV'
      IF (J_VAL.EQ.150) TEXT='J-150.CSV'
      IF (J_VAL.EQ.151) TEXT='J-151.CSV'
      IF (J_VAL.EQ.152) TEXT='J-152.CSV'
      IF (J_VAL.EQ.153) TEXT='J-153.CSV'
      IF (J_VAL.EQ.154) TEXT='J-154.CSV'
      IF (J_VAL.EQ.155) TEXT='J-155.CSV'
      IF (J_VAL.EQ.156) TEXT='J-156.CSV'
      IF (J_VAL.EQ.157) TEXT='J-157.CSV'
      IF (J_VAL.EQ.158) TEXT='J-158.CSV'
      IF (J_VAL.EQ.159) TEXT='J-159.CSV'
      IF (J_VAL.EQ.160) TEXT='J-160.CSV'
      IF (J_VAL.EQ.161) TEXT='J-161.CSV'
      IF (J_VAL.EQ.162) TEXT='J-162.CSV'
      IF (J_VAL.EQ.163) TEXT='J-163.CSV'
      IF (J_VAL.EQ.164) TEXT='J-164.CSV'
      IF (J_VAL.EQ.165) TEXT='J-165.CSV'
      IF (J_VAL.EQ.166) TEXT='J-166.CSV'
      IF (J_VAL.EQ.167) TEXT='J-167.CSV'
      IF (J_VAL.EQ.168) TEXT='J-168.CSV'
      IF (J_VAL.EQ.169) TEXT='J-169.CSV'
      IF (J_VAL.EQ.170) TEXT='J-170.CSV'
      IF (J_VAL.EQ.171) TEXT='J-171.CSV'
      IF (J_VAL.EQ.172) TEXT='J-172.CSV'
      IF (J_VAL.EQ.173) TEXT='J-173.CSV'
      IF (J_VAL.EQ.174) TEXT='J-174.CSV'
      IF (J_VAL.EQ.175) TEXT='J-175.CSV'
      IF (J_VAL.EQ.176) TEXT='J-176.CSV'
      IF (J_VAL.EQ.177) TEXT='J-177.CSV'
      IF (J_VAL.EQ.178) TEXT='J-178.CSV'
      IF (J_VAL.EQ.179) TEXT='J-179.CSV'
      IF (J_VAL.EQ.180) TEXT='J-180.CSV'
      IF (J_VAL.EQ.181) TEXT='J-181.CSV'
      IF (J_VAL.EQ.182) TEXT='J-182.CSV'
      IF (J_VAL.EQ.183) TEXT='J-183.CSV'
      IF (J_VAL.EQ.184) TEXT='J-184.CSV'
      IF (J_VAL.EQ.185) TEXT='J-185.CSV'
      IF (J_VAL.EQ.186) TEXT='J-186.CSV'
      IF (J_VAL.EQ.187) TEXT='J-187.CSV'
      IF (J_VAL.EQ.188) TEXT='J-188.CSV'
      IF (J_VAL.EQ.189) TEXT='J-189.CSV'
      IF (J_VAL.EQ.190) TEXT='J-190.CSV'
      IF (J_VAL.EQ.191) TEXT='J-191.CSV'
      IF (J_VAL.EQ.192) TEXT='J-192.CSV'
      IF (J_VAL.EQ.193) TEXT='J-193.CSV'
      IF (J_VAL.EQ.194) TEXT='J-194.CSV'
      IF (J_VAL.EQ.195) TEXT='J-195.CSV'
      IF (J_VAL.EQ.196) TEXT='J-196.CSV'
      IF (J_VAL.EQ.197) TEXT='J-197.CSV'
      IF (J_VAL.EQ.198) TEXT='J-198.CSV'
      IF (J_VAL.EQ.199) TEXT='J-199.CSV'
      IF (J_VAL.EQ.200) TEXT='J-200.CSV'
      IF (J_VAL.EQ.201) TEXT='J-201.CSV'
      IF (J_VAL.EQ.202) TEXT='J-202.CSV'
      IF (J_VAL.EQ.203) TEXT='J-203.CSV'
      IF (J_VAL.EQ.204) TEXT='J-204.CSV'
      IF (J_VAL.EQ.205) TEXT='J-205.CSV'
      IF (J_VAL.EQ.206) TEXT='J-206.CSV'
      IF (J_VAL.EQ.207) TEXT='J-207.CSV'
      IF (J_VAL.EQ.208) TEXT='J-208.CSV'
      IF (J_VAL.EQ.209) TEXT='J-209.CSV'
      IF (J_VAL.EQ.210) TEXT='J-210.CSV'
      IF (J_VAL.EQ.211) TEXT='J-211.CSV'
      IF (J_VAL.EQ.212) TEXT='J-212.CSV'
      IF (J_VAL.EQ.213) TEXT='J-213.CSV'
      IF (J_VAL.EQ.214) TEXT='J-214.CSV'
      IF (J_VAL.EQ.215) TEXT='J-215.CSV'
      IF (J_VAL.EQ.216) TEXT='J-216.CSV'
      IF (J_VAL.EQ.217) TEXT='J-217.CSV'
      IF (J_VAL.EQ.218) TEXT='J-218.CSV'
      IF (J_VAL.EQ.219) TEXT='J-219.CSV'
      IF (J_VAL.EQ.220) TEXT='J-220.CSV'
      IF (J_VAL.EQ.221) TEXT='J-221.CSV'
      IF (J_VAL.EQ.222) TEXT='J-222.CSV'
      IF (J_VAL.EQ.223) TEXT='J-223.CSV'
      IF (J_VAL.EQ.224) TEXT='J-224.CSV'
      IF (J_VAL.EQ.225) TEXT='J-225.CSV'
      IF (J_VAL.EQ.226) TEXT='J-226.CSV'
      IF (J_VAL.EQ.227) TEXT='J-227.CSV'
      IF (J_VAL.EQ.228) TEXT='J-228.CSV'
      IF (J_VAL.EQ.229) TEXT='J-229.CSV'
      IF (J_VAL.EQ.230) TEXT='J-230.CSV'
      IF (J_VAL.EQ.231) TEXT='J-231.CSV'
      IF (J_VAL.EQ.232) TEXT='J-232.CSV'
      IF (J_VAL.EQ.233) TEXT='J-233.CSV'
      IF (J_VAL.EQ.234) TEXT='J-234.CSV'
      IF (J_VAL.EQ.235) TEXT='J-235.CSV'
      IF (J_VAL.EQ.236) TEXT='J-236.CSV'
      IF (J_VAL.EQ.237) TEXT='J-237.CSV'
      IF (J_VAL.EQ.238) TEXT='J-238.CSV'
      IF (J_VAL.EQ.239) TEXT='J-239.CSV'
      IF (J_VAL.EQ.240) TEXT='J-240.CSV'
      IF (J_VAL.EQ.241) TEXT='J-241.CSV'
      IF (J_VAL.EQ.242) TEXT='J-242.CSV'
      IF (J_VAL.EQ.243) TEXT='J-243.CSV'
      IF (J_VAL.EQ.244) TEXT='J-244.CSV'
      IF (J_VAL.EQ.245) TEXT='J-245.CSV'
      IF (J_VAL.EQ.246) TEXT='J-246.CSV'
      IF (J_VAL.EQ.247) TEXT='J-247.CSV'
      IF (J_VAL.EQ.248) TEXT='J-248.CSV'
      IF (J_VAL.EQ.249) TEXT='J-249.CSV'
      IF (J_VAL.EQ.250) TEXT='J-250.CSV'
      IF (J_VAL.EQ.251) TEXT='J-251.CSV'
      IF (J_VAL.EQ.252) TEXT='J-252.CSV'
      IF (J_VAL.EQ.253) TEXT='J-253.CSV'
      IF (J_VAL.EQ.254) TEXT='J-254.CSV'
      IF (J_VAL.EQ.255) TEXT='J-255.CSV'
      IF (J_VAL.EQ.256) TEXT='J-256.CSV'
      IF (J_VAL.EQ.257) TEXT='J-257.CSV'
      IF (J_VAL.EQ.258) TEXT='J-258.CSV'
      IF (J_VAL.EQ.259) TEXT='J-259.CSV'
      IF (J_VAL.EQ.260) TEXT='J-260.CSV'
      IF (J_VAL.EQ.261) TEXT='J-261.CSV'
      IF (J_VAL.EQ.262) TEXT='J-262.CSV'
      IF (J_VAL.EQ.263) TEXT='J-263.CSV'
      IF (J_VAL.EQ.264) TEXT='J-264.CSV'
      IF (J_VAL.EQ.265) TEXT='J-265.CSV'
      IF (J_VAL.EQ.266) TEXT='J-266.CSV'
      IF (J_VAL.EQ.267) TEXT='J-267.CSV'
      IF (J_VAL.EQ.268) TEXT='J-268.CSV'
      IF (J_VAL.EQ.269) TEXT='J-269.CSV'
      IF (J_VAL.EQ.270) TEXT='J-270.CSV'
      IF (J_VAL.EQ.271) TEXT='J-271.CSV'
      IF (J_VAL.EQ.272) TEXT='J-272.CSV'
      IF (J_VAL.EQ.273) TEXT='J-273.CSV'
      IF (J_VAL.EQ.274) TEXT='J-274.CSV'
      IF (J_VAL.EQ.275) TEXT='J-275.CSV'
      IF (J_VAL.EQ.276) TEXT='J-276.CSV'
      IF (J_VAL.EQ.277) TEXT='J-277.CSV'
      IF (J_VAL.EQ.278) TEXT='J-278.CSV'
      IF (J_VAL.EQ.279) TEXT='J-279.CSV'
      IF (J_VAL.EQ.280) TEXT='J-280.CSV'
      IF (J_VAL.EQ.281) TEXT='J-281.CSV'
      IF (J_VAL.EQ.282) TEXT='J-282.CSV'
      IF (J_VAL.EQ.283) TEXT='J-283.CSV'
      IF (J_VAL.EQ.284) TEXT='J-284.CSV'
      IF (J_VAL.EQ.285) TEXT='J-285.CSV'
      IF (J_VAL.EQ.286) TEXT='J-286.CSV'
      IF (J_VAL.EQ.287) TEXT='J-287.CSV'
      IF (J_VAL.EQ.288) TEXT='J-288.CSV'
      IF (J_VAL.EQ.289) TEXT='J-289.CSV'
      IF (J_VAL.EQ.290) TEXT='J-290.CSV'
      IF (J_VAL.EQ.291) TEXT='J-291.CSV'
      IF (J_VAL.EQ.292) TEXT='J-292.CSV'
      IF (J_VAL.EQ.293) TEXT='J-293.CSV'
      IF (J_VAL.EQ.294) TEXT='J-294.CSV'
      IF (J_VAL.EQ.295) TEXT='J-295.CSV'
      IF (J_VAL.EQ.296) TEXT='J-296.CSV'
      IF (J_VAL.EQ.297) TEXT='J-297.CSV'
      IF (J_VAL.EQ.298) TEXT='J-298.CSV'
      IF (J_VAL.EQ.299) TEXT='J-299.CSV'
      IF (J_VAL.EQ.300) TEXT='J-300.CSV'
      IF (J_VAL.EQ.301) TEXT='J-301.CSV'
      IF (J_VAL.EQ.302) TEXT='J-302.CSV'
      IF (J_VAL.EQ.303) TEXT='J-303.CSV'
      IF (J_VAL.EQ.304) TEXT='J-304.CSV'
      IF (J_VAL.EQ.305) TEXT='J-305.CSV'
      IF (J_VAL.EQ.306) TEXT='J-306.CSV'
      IF (J_VAL.EQ.307) TEXT='J-307.CSV'
      IF (J_VAL.EQ.308) TEXT='J-308.CSV'
      IF (J_VAL.EQ.309) TEXT='J-309.CSV'
      IF (J_VAL.EQ.310) TEXT='J-310.CSV'
      IF (J_VAL.EQ.311) TEXT='J-311.CSV'
      IF (J_VAL.EQ.312) TEXT='J-312.CSV'
      IF (J_VAL.EQ.313) TEXT='J-313.CSV'
      IF (J_VAL.EQ.314) TEXT='J-314.CSV'
      IF (J_VAL.EQ.315) TEXT='J-315.CSV'
      IF (J_VAL.EQ.316) TEXT='J-316.CSV'
      IF (J_VAL.EQ.317) TEXT='J-317.CSV'
      IF (J_VAL.EQ.318) TEXT='J-318.CSV'
      IF (J_VAL.EQ.319) TEXT='J-319.CSV'
      IF (J_VAL.EQ.320) TEXT='J-320.CSV'
      IF (J_VAL.EQ.321) TEXT='J-321.CSV'
      IF (J_VAL.EQ.322) TEXT='J-322.CSV'
      IF (J_VAL.EQ.323) TEXT='J-323.CSV'
      IF (J_VAL.EQ.324) TEXT='J-324.CSV'
      IF (J_VAL.EQ.325) TEXT='J-325.CSV'
      IF (J_VAL.EQ.326) TEXT='J-326.CSV'
      IF (J_VAL.EQ.327) TEXT='J-327.CSV'
      IF (J_VAL.EQ.328) TEXT='J-328.CSV'
      IF (J_VAL.EQ.329) TEXT='J-329.CSV'
      IF (J_VAL.EQ.330) TEXT='J-330.CSV'
      IF (J_VAL.EQ.331) TEXT='J-331.CSV'
      IF (J_VAL.EQ.332) TEXT='J-332.CSV'
      IF (J_VAL.EQ.333) TEXT='J-333.CSV'
      IF (J_VAL.EQ.334) TEXT='J-334.CSV'
      IF (J_VAL.EQ.335) TEXT='J-335.CSV'
      IF (J_VAL.EQ.336) TEXT='J-336.CSV'
      IF (J_VAL.EQ.337) TEXT='J-337.CSV'
      IF (J_VAL.EQ.338) TEXT='J-338.CSV'
      IF (J_VAL.EQ.339) TEXT='J-339.CSV'
      IF (J_VAL.EQ.340) TEXT='J-340.CSV'
      IF (J_VAL.EQ.341) TEXT='J-341.CSV'
      IF (J_VAL.EQ.342) TEXT='J-342.CSV'
      IF (J_VAL.EQ.343) TEXT='J-343.CSV'
      IF (J_VAL.EQ.344) TEXT='J-344.CSV'
      IF (J_VAL.EQ.345) TEXT='J-345.CSV'
      IF (J_VAL.EQ.346) TEXT='J-346.CSV'
      IF (J_VAL.EQ.347) TEXT='J-347.CSV'
      IF (J_VAL.EQ.348) TEXT='J-348.CSV'
      IF (J_VAL.EQ.349) TEXT='J-349.CSV'
      IF (J_VAL.EQ.350) TEXT='J-350.CSV'
      IF (J_VAL.EQ.351) TEXT='J-351.CSV'
      IF (J_VAL.EQ.352) TEXT='J-352.CSV'
      IF (J_VAL.EQ.353) TEXT='J-353.CSV'
      IF (J_VAL.EQ.354) TEXT='J-354.CSV'
      IF (J_VAL.EQ.355) TEXT='J-355.CSV'
      IF (J_VAL.EQ.356) TEXT='J-356.CSV'
      IF (J_VAL.EQ.357) TEXT='J-357.CSV'
      IF (J_VAL.EQ.358) TEXT='J-358.CSV'
      IF (J_VAL.EQ.359) TEXT='J-359.CSV'
      IF (J_VAL.EQ.360) TEXT='J-360.CSV'
      IF (J_VAL.EQ.361) TEXT='J-361.CSV'
      IF (J_VAL.EQ.362) TEXT='J-362.CSV'
      IF (J_VAL.EQ.363) TEXT='J-363.CSV'
      IF (J_VAL.EQ.364) TEXT='J-364.CSV'
      IF (J_VAL.EQ.365) TEXT='J-365.CSV'
      IF (J_VAL.EQ.366) TEXT='J-366.CSV'
      IF (J_VAL.EQ.367) TEXT='J-367.CSV'
      IF (J_VAL.EQ.368) TEXT='J-368.CSV'
      IF (J_VAL.EQ.369) TEXT='J-369.CSV'
      IF (J_VAL.EQ.370) TEXT='J-370.CSV'
      IF (J_VAL.EQ.371) TEXT='J-371.CSV'
      IF (J_VAL.EQ.372) TEXT='J-372.CSV'
      IF (J_VAL.EQ.373) TEXT='J-373.CSV'
      IF (J_VAL.EQ.374) TEXT='J-374.CSV'
      IF (J_VAL.EQ.375) TEXT='J-375.CSV'
      IF (J_VAL.EQ.376) TEXT='J-376.CSV'
      IF (J_VAL.EQ.377) TEXT='J-377.CSV'
      IF (J_VAL.EQ.378) TEXT='J-378.CSV'
      IF (J_VAL.EQ.379) TEXT='J-379.CSV'
      IF (J_VAL.EQ.380) TEXT='J-380.CSV'
      IF (J_VAL.EQ.381) TEXT='J-381.CSV'
      IF (J_VAL.EQ.382) TEXT='J-382.CSV'
      IF (J_VAL.EQ.383) TEXT='J-383.CSV'
      IF (J_VAL.EQ.384) TEXT='J-384.CSV'
      IF (J_VAL.EQ.385) TEXT='J-385.CSV'
      IF (J_VAL.EQ.386) TEXT='J-386.CSV'
      IF (J_VAL.EQ.387) TEXT='J-387.CSV'
      IF (J_VAL.EQ.388) TEXT='J-388.CSV'
      IF (J_VAL.EQ.389) TEXT='J-389.CSV'
      IF (J_VAL.EQ.390) TEXT='J-390.CSV'
      IF (J_VAL.EQ.391) TEXT='J-391.CSV'
      IF (J_VAL.EQ.392) TEXT='J-392.CSV'
      IF (J_VAL.EQ.393) TEXT='J-393.CSV'
      IF (J_VAL.EQ.394) TEXT='J-394.CSV'
      IF (J_VAL.EQ.395) TEXT='J-395.CSV'
      IF (J_VAL.EQ.396) TEXT='J-396.CSV'
      IF (J_VAL.EQ.397) TEXT='J-397.CSV'
      IF (J_VAL.EQ.398) TEXT='J-398.CSV'
      IF (J_VAL.EQ.399) TEXT='J-399.CSV'
      IF (J_VAL.EQ.400) TEXT='J-400.CSV'

      FILENAME=TEXT
      RETURN
      END

