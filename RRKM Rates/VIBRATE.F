      SUBROUTINE VIBRATE(ARRAY,NOS,VIBSTART,SYSTEM,
     &                        VIBNUM,VIBCON,GEOLIM,
     &                      SYSLIM,GEOM,GRID,Z_P_E)

C- THIS SUBROUTINE CALCULATES THE VIBRATIONAL DENSITY OF STATES EXACTLY
C- FOR EXACT METHOD, IT DOES NOT MATTER HOW SMALL THE VIBRATIONAL CONSTANTS GET
C- THESE ARE FOLDED INTO THE NOS ARRAY

C- DEFINE ALL VARIABLES

      IMPLICIT NONE

      INTEGER ARRAY,GEOLIM,SYSLIM,SYSTEM,GEOM,VIBENE,ROTENE
      INTEGER NUMMODES,V_MODE,VIBSTART,VIBNUM,INDEX
      INTEGER V_1,V_1_MAX,V_2,V_2_MAX,V_3,V_3_MAX,V_4,V_4_MAX
      INTEGER V_5,V_5_MAX,V_6,V_6_MAX,V_7,V_7_MAX,V_8,V_8_MAX
      INTEGER V_9,V_9_MAX,V_10,V_10_MAX,V_11,V_11_MAX,V_12,V_12_MAX
      INTEGER V_13,V_13_MAX,V_14,V_14_MAX,V_15,V_15_MAX
      REAL*8 E_1,E_1_MAX,E_2,E_2_MAX,E_3,E_3_MAX,E_4,E_4_MAX
      REAL*8 E_5,E_5_MAX,E_6,E_6_MAX,E_7,E_7_MAX,E_8,E_8_MAX
      REAL*8 E_9,E_9_MAX,E_10,E_10_MAX,E_11,E_11_MAX,E_12,E_12_MAX
      REAL*8 E_13,E_13_MAX,E_14,E_14_MAX,E_15,E_15_MAX
      REAL*8 VIBCON,HNU,GRID,VIBSTATE,ROTSTATE,Z_P_E,NOS

C- DEFINE ARRAYS

      DIMENSION VIBCON(0:GEOLIM,15)         ! VIBRATIONAL CONSTANTS, UNIQUE FOR EACH GEOMETRY
      DIMENSION VIBNUM(0:SYSLIM)            ! NUMBER OF VIBRATIONAL MODES, COMMON FOR ALL POINTS ON MEP
      DIMENSION VIBSTATE(ARRAY)           ! VIBRATIONAL STATES ARRAY
      DIMENSION ROTSTATE(ARRAY)           ! ROTATIONAL STATES ARRAY
      DIMENSION NOS(ARRAY)                ! NUMBER OF STATES ARRAY
      DIMENSION HNU(15)                   ! REQUIRED VIBRATIONAL CONSTANTS

C------------------------------------------------------------------------------------------------------
C------------------------------------------------------------------------------------------------------
C----- THIS PART CREATES THE VIBRATIONAL STATES -------------------------------------------------------
C------------------------------------------------------------------------------------------------------
C------------------------------------------------------------------------------------------------------

C- WORK OUT HOW MANY MODES ARE USED, AND COPY CONSTANTS TO HNU ARRAY
C- IN REVERSE ORDER SO THAT LARGEST MODES ARE ON OUTSIDE DO LOOPS

      NUMMODES=0
      DO V_MODE=VIBNUM(SYSTEM),VIBSTART,-1
         NUMMODES=NUMMODES+1
         HNU(V_MODE-VIBSTART+1)=VIBCON(GEOM,NUMMODES)
         END DO

C- COPY THE ROTATIONAL STATES ACROSS AND BLANK THE ARRAYS

      DO INDEX=1,ARRAY
         ROTSTATE(INDEX)=NOS(INDEX)
         VIBSTATE(INDEX)=0.0D0
         NOS(INDEX)=0.0D0
         END DO

C- CALCULATE HOW MUCH ENERGY IS AVAILABLE

      E_1_MAX=(GRID*DFLOAT(ARRAY-1))-Z_P_E

C------------------------------------------------------------------------------------------------------
C- EXACT METHOD FOR ONE MODE
C------------------------------------------------------------------------------------------------------

      IF (NUMMODES.EQ.1) THEN

         V_1_MAX=INT(E_1_MAX/HNU(1))
         DO V_1=0,V_1_MAX
      WRITE(*,'(A,I2,A)') ' Calculating Vibrational levels (',
     &                            (100*V_1/(V_1_MAX+1)),'%)...'
            E_1=HNU(1)*DFLOAT(V_1)

            VIBENE=INT(E_1/GRID)+1

            END DO ! V_1

C------------------------------------------------------------------------------------------------------
C- EXACT METHOD FOR TWO MODES
C------------------------------------------------------------------------------------------------------

      ELSEIF (NUMMODES.EQ.2) THEN

         V_1_MAX=INT(E_1_MAX/HNU(1))
         DO V_1=0,V_1_MAX
      WRITE(*,'(A,I2,A)') ' Calculating Vibrational levels (',
     &                            (100*V_1/(V_1_MAX+1)),'%)...'
            E_1=HNU(1)*DFLOAT(V_1)
            E_2_MAX=E_1_MAX-E_1

            V_2_MAX=INT(E_2_MAX/HNU(2))
            DO V_2=0,V_2_MAX
               E_2=HNU(2)*DFLOAT(V_2)

               VIBENE=INT((E_1+E_2)/GRID)+1

               END DO ! V_2
            END DO ! V_1

C------------------------------------------------------------------------------------------------------
C- EXACT METHOD FOR THREE MODES
C------------------------------------------------------------------------------------------------------

      ELSEIF (NUMMODES.EQ.3) THEN

         V_1_MAX=INT(E_1_MAX/HNU(1))
         DO V_1=0,V_1_MAX
      WRITE(*,'(A,I2,A)') ' Calculating Vibrational levels (',
     &                            (100*V_1/(V_1_MAX+1)),'%)...'
            E_1=HNU(1)*DFLOAT(V_1)
            E_2_MAX=E_1_MAX-E_1

            V_2_MAX=INT(E_2_MAX/HNU(2))
            DO V_2=0,V_2_MAX
               E_2=HNU(2)*DFLOAT(V_2)
               E_3_MAX=E_2_MAX-E_2

         V_3_MAX=INT(E_3_MAX/HNU(3))
         DO V_3=0,V_3_MAX
            E_3=HNU(3)*DFLOAT(V_3)

               VIBENE=INT((E_1+E_2+E_3)/GRID)+1

            END DO ! V_3
               END DO ! V_2
            END DO ! V_1

C------------------------------------------------------------------------------------------------------
C- EXACT METHOD FOR FOUR MODES
C------------------------------------------------------------------------------------------------------

      ELSEIF (NUMMODES.EQ.4) THEN

         V_1_MAX=INT(E_1_MAX/HNU(1))
         DO V_1=0,V_1_MAX
      WRITE(*,'(A,I2,A)') ' Calculating Vibrational levels (',
     &                            (100*V_1/(V_1_MAX+1)),'%)...'
            E_1=HNU(1)*DFLOAT(V_1)
            E_2_MAX=E_1_MAX-E_1

            V_2_MAX=INT(E_2_MAX/HNU(2))
            DO V_2=0,V_2_MAX
               E_2=HNU(2)*DFLOAT(V_2)
               E_3_MAX=E_2_MAX-E_2

         V_3_MAX=INT(E_3_MAX/HNU(3))
         DO V_3=0,V_3_MAX
            E_3=HNU(3)*DFLOAT(V_3)
            E_4_MAX=E_3_MAX-E_3

            V_4_MAX=INT(E_4_MAX/HNU(4))
            DO V_4=0,V_4_MAX
               E_4=HNU(4)*DFLOAT(V_4)

               VIBENE=INT((E_1_MAX+E_4-E_4_MAX)/GRID)+1

               END DO ! V_4
            END DO ! V_3
               END DO ! V_2
            END DO ! V_1

C------------------------------------------------------------------------------------------------------
C- EXACT METHOD FOR FIVE MODES
C------------------------------------------------------------------------------------------------------

      ELSEIF (NUMMODES.EQ.5) THEN

         V_1_MAX=INT(E_1_MAX/HNU(1))
         DO V_1=0,V_1_MAX
      WRITE(*,'(A,I2,A)') ' Calculating Vibrational levels (',
     &                            (100*V_1/(V_1_MAX+1)),'%)...'
            E_1=HNU(1)*DFLOAT(V_1)
            E_2_MAX=E_1_MAX-E_1

            V_2_MAX=INT(E_2_MAX/HNU(2))
            DO V_2=0,V_2_MAX
               E_2=HNU(2)*DFLOAT(V_2)
               E_3_MAX=E_2_MAX-E_2

         V_3_MAX=INT(E_3_MAX/HNU(3))
         DO V_3=0,V_3_MAX
            E_3=HNU(3)*DFLOAT(V_3)
            E_4_MAX=E_3_MAX-E_3

            V_4_MAX=INT(E_4_MAX/HNU(4))
            DO V_4=0,V_4_MAX
               E_4=HNU(4)*DFLOAT(V_4)
               E_5_MAX=E_4_MAX-E_4

         V_5_MAX=INT(E_5_MAX/HNU(5))
         DO V_5=0,V_5_MAX
            E_5=HNU(5)*DFLOAT(V_5)

               VIBENE=INT((E_1_MAX+E_5-E_5_MAX)/GRID)+1

            END DO ! V_5
               END DO ! V_4
            END DO ! V_3
               END DO ! V_2
            END DO ! V_1

C------------------------------------------------------------------------------------------------------
C- EXACT METHOD FOR SIX MODES
C------------------------------------------------------------------------------------------------------

      ELSEIF (NUMMODES.EQ.6) THEN

         V_1_MAX=INT(E_1_MAX/HNU(1))
         DO V_1=0,V_1_MAX
      WRITE(*,'(A,I2,A)') ' Calculating Vibrational levels (',
     &                            (100*V_1/(V_1_MAX+1)),'%)...'
            E_1=HNU(1)*DFLOAT(V_1)
            E_2_MAX=E_1_MAX-E_1

            V_2_MAX=INT(E_2_MAX/HNU(2))
            DO V_2=0,V_2_MAX
               E_2=HNU(2)*DFLOAT(V_2)
               E_3_MAX=E_2_MAX-E_2

         V_3_MAX=INT(E_3_MAX/HNU(3))
         DO V_3=0,V_3_MAX
            E_3=HNU(3)*DFLOAT(V_3)
            E_4_MAX=E_3_MAX-E_3

            V_4_MAX=INT(E_4_MAX/HNU(4))
            DO V_4=0,V_4_MAX
               E_4=HNU(4)*DFLOAT(V_4)
               E_5_MAX=E_4_MAX-E_4

         V_5_MAX=INT(E_5_MAX/HNU(5))
         DO V_5=0,V_5_MAX
            E_5=HNU(5)*DFLOAT(V_5)
            E_6_MAX=E_5_MAX-E_5

            V_6_MAX=INT(E_6_MAX/HNU(6))
            DO V_6=0,V_6_MAX
               E_6=HNU(6)*DFLOAT(V_6)

               VIBENE=INT((E_1_MAX+E_6-E_6_MAX)/GRID)+1

               END DO ! V_6
            END DO ! V_5
               END DO ! V_4
            END DO ! V_3
               END DO ! V_2
            END DO ! V_1

C------------------------------------------------------------------------------------------------------
C- EXACT METHOD FOR SEVEN MODES
C------------------------------------------------------------------------------------------------------

      ELSEIF (NUMMODES.EQ.7) THEN

         V_1_MAX=INT(E_1_MAX/HNU(1))
         DO V_1=0,V_1_MAX
      WRITE(*,'(A,I2,A)') ' Calculating Vibrational levels (',
     &                            (100*V_1/(V_1_MAX+1)),'%)...'
            E_1=HNU(1)*DFLOAT(V_1)
            E_2_MAX=E_1_MAX-E_1

            V_2_MAX=INT(E_2_MAX/HNU(2))
            DO V_2=0,V_2_MAX
               E_2=HNU(2)*DFLOAT(V_2)
               E_3_MAX=E_2_MAX-E_2

         V_3_MAX=INT(E_3_MAX/HNU(3))
         DO V_3=0,V_3_MAX
            E_3=HNU(3)*DFLOAT(V_3)
            E_4_MAX=E_3_MAX-E_3

            V_4_MAX=INT(E_4_MAX/HNU(4))
            DO V_4=0,V_4_MAX
               E_4=HNU(4)*DFLOAT(V_4)
               E_5_MAX=E_4_MAX-E_4

         V_5_MAX=INT(E_5_MAX/HNU(5))
         DO V_5=0,V_5_MAX
            E_5=HNU(5)*DFLOAT(V_5)
            E_6_MAX=E_5_MAX-E_5

            V_6_MAX=INT(E_6_MAX/HNU(6))
            DO V_6=0,V_6_MAX
               E_6=HNU(6)*DFLOAT(V_6)
               E_7_MAX=E_6_MAX-E_6

         V_7_MAX=INT(E_7_MAX/HNU(7))
         DO V_7=0,V_7_MAX
            E_7=HNU(7)*DFLOAT(V_7)

               VIBENE=INT((E_1_MAX+E_7-E_7_MAX)/GRID)+1

            END DO ! V_7
               END DO ! V_6
            END DO ! V_5
               END DO ! V_4
            END DO ! V_3
               END DO ! V_2
            END DO ! V_1

C------------------------------------------------------------------------------------------------------
C- EXACT METHOD FOR EIGHT MODES
C------------------------------------------------------------------------------------------------------

      ELSEIF (NUMMODES.EQ.8) THEN

         V_1_MAX=INT(E_1_MAX/HNU(1))
         DO V_1=0,V_1_MAX
      WRITE(*,'(A,I2,A)') ' Calculating Vibrational levels (',
     &                            (100*V_1/(V_1_MAX+1)),'%)...'
            E_1=HNU(1)*DFLOAT(V_1)
            E_2_MAX=E_1_MAX-E_1

            V_2_MAX=INT(E_2_MAX/HNU(2))
            DO V_2=0,V_2_MAX
               E_2=HNU(2)*DFLOAT(V_2)
               E_3_MAX=E_2_MAX-E_2

         V_3_MAX=INT(E_3_MAX/HNU(3))
         DO V_3=0,V_3_MAX
            E_3=HNU(3)*DFLOAT(V_3)
            E_4_MAX=E_3_MAX-E_3

            V_4_MAX=INT(E_4_MAX/HNU(4))
            DO V_4=0,V_4_MAX
               E_4=HNU(4)*DFLOAT(V_4)
               E_5_MAX=E_4_MAX-E_4

         V_5_MAX=INT(E_5_MAX/HNU(5))
         DO V_5=0,V_5_MAX
            E_5=HNU(5)*DFLOAT(V_5)
            E_6_MAX=E_5_MAX-E_5

            V_6_MAX=INT(E_6_MAX/HNU(6))
            DO V_6=0,V_6_MAX
               E_6=HNU(6)*DFLOAT(V_6)
               E_7_MAX=E_6_MAX-E_6

         V_7_MAX=INT(E_7_MAX/HNU(7))
         DO V_7=0,V_7_MAX
            E_7=HNU(7)*DFLOAT(V_7)
            E_8_MAX=E_7_MAX-E_7

            V_8_MAX=INT(E_8_MAX/HNU(8))
            DO V_8=0,V_8_MAX
               E_8=HNU(8)*DFLOAT(V_8)

               VIBENE=INT((E_1_MAX+E_8-E_8_MAX)/GRID)+1

               END DO ! V_8
            END DO ! V_7
               END DO ! V_6
            END DO ! V_5
               END DO ! V_4
            END DO ! V_3
               END DO ! V_2
            END DO ! V_1

C------------------------------------------------------------------------------------------------------
C- EXACT METHOD FOR NINE MODES
C------------------------------------------------------------------------------------------------------

      ELSEIF (NUMMODES.EQ.9) THEN

         V_1_MAX=INT(E_1_MAX/HNU(1))
         DO V_1=0,V_1_MAX
      WRITE(*,'(A,I2,A)') ' Calculating Vibrational levels (',
     &                            (100*V_1/(V_1_MAX+1)),'%)...'
            E_1=HNU(1)*DFLOAT(V_1)
            E_2_MAX=E_1_MAX-E_1

            V_2_MAX=INT(E_2_MAX/HNU(2))
            DO V_2=0,V_2_MAX
               E_2=HNU(2)*DFLOAT(V_2)
               E_3_MAX=E_2_MAX-E_2

         V_3_MAX=INT(E_3_MAX/HNU(3))
         DO V_3=0,V_3_MAX
            E_3=HNU(3)*DFLOAT(V_3)
            E_4_MAX=E_3_MAX-E_3

            V_4_MAX=INT(E_4_MAX/HNU(4))
            DO V_4=0,V_4_MAX
               E_4=HNU(4)*DFLOAT(V_4)
               E_5_MAX=E_4_MAX-E_4

         V_5_MAX=INT(E_5_MAX/HNU(5))
         DO V_5=0,V_5_MAX
            E_5=HNU(5)*DFLOAT(V_5)
            E_6_MAX=E_5_MAX-E_5

            V_6_MAX=INT(E_6_MAX/HNU(6))
            DO V_6=0,V_6_MAX
               E_6=HNU(6)*DFLOAT(V_6)
               E_7_MAX=E_6_MAX-E_6

         V_7_MAX=INT(E_7_MAX/HNU(7))
         DO V_7=0,V_7_MAX
            E_7=HNU(7)*DFLOAT(V_7)
            E_8_MAX=E_7_MAX-E_7

            V_8_MAX=INT(E_8_MAX/HNU(8))
            DO V_8=0,V_8_MAX
               E_8=HNU(8)*DFLOAT(V_8)
               E_9_MAX=E_8_MAX-E_8

         V_9_MAX=INT(E_9_MAX/HNU(9))
         DO V_9=0,V_9_MAX
            E_9=HNU(9)*DFLOAT(V_9)

               VIBENE=INT((E_1_MAX+E_9-E_9_MAX)/GRID)+1

            END DO ! V_9
               END DO ! V_8
            END DO ! V_7
               END DO ! V_6
            END DO ! V_5
               END DO ! V_4
            END DO ! V_3
               END DO ! V_2
            END DO ! V_1

C------------------------------------------------------------------------------------------------------
C- EXACT METHOD FOR TEN MODES
C------------------------------------------------------------------------------------------------------

      ELSEIF (NUMMODES.EQ.10) THEN

         V_1_MAX=INT(E_1_MAX/HNU(1))
         DO V_1=0,V_1_MAX
      WRITE(*,'(A,I2,A)') ' Calculating Vibrational levels (',
     &                            (100*V_1/(V_1_MAX+1)),'%)...'
            E_1=HNU(1)*DFLOAT(V_1)
            E_2_MAX=E_1_MAX-E_1

            V_2_MAX=INT(E_2_MAX/HNU(2))
            DO V_2=0,V_2_MAX
               E_2=HNU(2)*DFLOAT(V_2)
               E_3_MAX=E_2_MAX-E_2

         V_3_MAX=INT(E_3_MAX/HNU(3))
         DO V_3=0,V_3_MAX
            E_3=HNU(3)*DFLOAT(V_3)
            E_4_MAX=E_3_MAX-E_3

            V_4_MAX=INT(E_4_MAX/HNU(4))
            DO V_4=0,V_4_MAX
               E_4=HNU(4)*DFLOAT(V_4)
               E_5_MAX=E_4_MAX-E_4

         V_5_MAX=INT(E_5_MAX/HNU(5))
         DO V_5=0,V_5_MAX
            E_5=HNU(5)*DFLOAT(V_5)
            E_6_MAX=E_5_MAX-E_5

            V_6_MAX=INT(E_6_MAX/HNU(6))
            DO V_6=0,V_6_MAX
               E_6=HNU(6)*DFLOAT(V_6)
               E_7_MAX=E_6_MAX-E_6

         V_7_MAX=INT(E_7_MAX/HNU(7))
         DO V_7=0,V_7_MAX
            E_7=HNU(7)*DFLOAT(V_7)
            E_8_MAX=E_7_MAX-E_7

            V_8_MAX=INT(E_8_MAX/HNU(8))
            DO V_8=0,V_8_MAX
               E_8=HNU(8)*DFLOAT(V_8)
               E_9_MAX=E_8_MAX-E_8

         V_9_MAX=INT(E_9_MAX/HNU(9))
         DO V_9=0,V_9_MAX
            E_9=HNU(9)*DFLOAT(V_9)
            E_10_MAX=E_9_MAX-E_9

            V_10_MAX=INT(E_10_MAX/HNU(10))
            DO V_10=0,V_10_MAX
               E_10=HNU(10)*DFLOAT(V_10)

               VIBENE=INT((E_1_MAX+E_10-E_10_MAX)/GRID)+1

               END DO ! V_10
            END DO ! V_9
               END DO ! V_8
            END DO ! V_7
               END DO ! V_6
            END DO ! V_5
               END DO ! V_4
            END DO ! V_3
               END DO ! V_2
            END DO ! V_1

C------------------------------------------------------------------------------------------------------
C- EXACT METHOD FOR ELEVEN MODES
C------------------------------------------------------------------------------------------------------

      ELSEIF (NUMMODES.EQ.11) THEN

         V_1_MAX=INT(E_1_MAX/HNU(1))
         DO V_1=0,V_1_MAX
      WRITE(*,'(A,I2,A)') ' Calculating Vibrational levels (',
     &                            (100*V_1/(V_1_MAX+1)),'%)...'
            E_1=HNU(1)*DFLOAT(V_1)
            E_2_MAX=E_1_MAX-E_1

            V_2_MAX=INT(E_2_MAX/HNU(2))
            DO V_2=0,V_2_MAX
               E_2=HNU(2)*DFLOAT(V_2)
               E_3_MAX=E_2_MAX-E_2

         V_3_MAX=INT(E_3_MAX/HNU(3))
         DO V_3=0,V_3_MAX
            E_3=HNU(3)*DFLOAT(V_3)
            E_4_MAX=E_3_MAX-E_3

            V_4_MAX=INT(E_4_MAX/HNU(4))
            DO V_4=0,V_4_MAX
               E_4=HNU(4)*DFLOAT(V_4)
               E_5_MAX=E_4_MAX-E_4

         V_5_MAX=INT(E_5_MAX/HNU(5))
         DO V_5=0,V_5_MAX
            E_5=HNU(5)*DFLOAT(V_5)
            E_6_MAX=E_5_MAX-E_5

            V_6_MAX=INT(E_6_MAX/HNU(6))
            DO V_6=0,V_6_MAX
               E_6=HNU(6)*DFLOAT(V_6)
               E_7_MAX=E_6_MAX-E_6

         V_7_MAX=INT(E_7_MAX/HNU(7))
         DO V_7=0,V_7_MAX
            E_7=HNU(7)*DFLOAT(V_7)
            E_8_MAX=E_7_MAX-E_7

            V_8_MAX=INT(E_8_MAX/HNU(8))
            DO V_8=0,V_8_MAX
               E_8=HNU(8)*DFLOAT(V_8)
               E_9_MAX=E_8_MAX-E_8

         V_9_MAX=INT(E_9_MAX/HNU(9))
         DO V_9=0,V_9_MAX
            E_9=HNU(9)*DFLOAT(V_9)
            E_10_MAX=E_9_MAX-E_9

            V_10_MAX=INT(E_10_MAX/HNU(10))
            DO V_10=0,V_10_MAX
               E_10=HNU(10)*DFLOAT(V_10)
               E_11_MAX=E_10_MAX-E_10

         V_11_MAX=INT(E_11_MAX/HNU(11))
         DO V_11=0,V_11_MAX
            E_11=HNU(11)*DFLOAT(V_11)

               VIBENE=INT((E_1_MAX+E_11-E_11_MAX)/GRID)+1

            END DO ! V_11
               END DO ! V_10
            END DO ! V_9
               END DO ! V_8
            END DO ! V_7
               END DO ! V_6
            END DO ! V_5
               END DO ! V_4
            END DO ! V_3
               END DO ! V_2
            END DO ! V_1

C------------------------------------------------------------------------------------------------------
C- EXACT METHOD FOR TWELVE MODES
C------------------------------------------------------------------------------------------------------

      ELSEIF (NUMMODES.EQ.12) THEN

         V_1_MAX=INT(E_1_MAX/HNU(1))
         DO V_1=0,V_1_MAX
      WRITE(*,'(A,I2,A)') ' Calculating Vibrational levels (',
     &                            (100*V_1/(V_1_MAX+1)),'%)...'
            E_1=HNU(1)*DFLOAT(V_1)
            E_2_MAX=E_1_MAX-E_1

            V_2_MAX=INT(E_2_MAX/HNU(2))
            DO V_2=0,V_2_MAX
               E_2=HNU(2)*DFLOAT(V_2)
               E_3_MAX=E_2_MAX-E_2

         V_3_MAX=INT(E_3_MAX/HNU(3))
         DO V_3=0,V_3_MAX
            E_3=HNU(3)*DFLOAT(V_3)
            E_4_MAX=E_3_MAX-E_3

            V_4_MAX=INT(E_4_MAX/HNU(4))
            DO V_4=0,V_4_MAX
               E_4=HNU(4)*DFLOAT(V_4)
               E_5_MAX=E_4_MAX-E_4

         V_5_MAX=INT(E_5_MAX/HNU(5))
         DO V_5=0,V_5_MAX
            E_5=HNU(5)*DFLOAT(V_5)
            E_6_MAX=E_5_MAX-E_5

            V_6_MAX=INT(E_6_MAX/HNU(6))
            DO V_6=0,V_6_MAX
               E_6=HNU(6)*DFLOAT(V_6)
               E_7_MAX=E_6_MAX-E_6

         V_7_MAX=INT(E_7_MAX/HNU(7))
         DO V_7=0,V_7_MAX
            E_7=HNU(7)*DFLOAT(V_7)
            E_8_MAX=E_7_MAX-E_7

            V_8_MAX=INT(E_8_MAX/HNU(8))
            DO V_8=0,V_8_MAX
               E_8=HNU(8)*DFLOAT(V_8)
               E_9_MAX=E_8_MAX-E_8

         V_9_MAX=INT(E_9_MAX/HNU(9))
         DO V_9=0,V_9_MAX
            E_9=HNU(9)*DFLOAT(V_9)
            E_10_MAX=E_9_MAX-E_9

            V_10_MAX=INT(E_10_MAX/HNU(10))
            DO V_10=0,V_10_MAX
               E_10=HNU(10)*DFLOAT(V_10)
               E_11_MAX=E_10_MAX-E_10

         V_11_MAX=INT(E_11_MAX/HNU(11))
         DO V_11=0,V_11_MAX
            E_11=HNU(11)*DFLOAT(V_11)
            E_12_MAX=E_11_MAX-E_11

            V_12_MAX=INT(E_12_MAX/HNU(12))
            DO V_12=0,V_12_MAX
               E_12=HNU(12)*DFLOAT(V_12)

               VIBENE=INT((E_1_MAX+E_12-E_12_MAX)/GRID)+1

               END DO ! V_12
            END DO ! V_11
               END DO ! V_10
            END DO ! V_9
               END DO ! V_8
            END DO ! V_7
               END DO ! V_6
            END DO ! V_5
               END DO ! V_4
            END DO ! V_3
               END DO ! V_2
            END DO ! V_1

C------------------------------------------------------------------------------------------------------
C- EXACT METHOD FOR THIRTEEN MODES
C------------------------------------------------------------------------------------------------------

      ELSEIF (NUMMODES.EQ.13) THEN

         V_1_MAX=INT(E_1_MAX/HNU(1))
         DO V_1=0,V_1_MAX
      WRITE(*,'(A,I2,A)') ' Calculating Vibrational levels (',
     &                            (100*V_1/(V_1_MAX+1)),'%)...'
            E_1=HNU(1)*DFLOAT(V_1)
            E_2_MAX=E_1_MAX-E_1

            V_2_MAX=INT(E_2_MAX/HNU(2))
            DO V_2=0,V_2_MAX
               E_2=HNU(2)*DFLOAT(V_2)
               E_3_MAX=E_2_MAX-E_2

         V_3_MAX=INT(E_3_MAX/HNU(3))
         DO V_3=0,V_3_MAX
            E_3=HNU(3)*DFLOAT(V_3)
            E_4_MAX=E_3_MAX-E_3

            V_4_MAX=INT(E_4_MAX/HNU(4))
            DO V_4=0,V_4_MAX
               E_4=HNU(4)*DFLOAT(V_4)
               E_5_MAX=E_4_MAX-E_4

         V_5_MAX=INT(E_5_MAX/HNU(5))
         DO V_5=0,V_5_MAX
            E_5=HNU(5)*DFLOAT(V_5)
            E_6_MAX=E_5_MAX-E_5

            V_6_MAX=INT(E_6_MAX/HNU(6))
            DO V_6=0,V_6_MAX
               E_6=HNU(6)*DFLOAT(V_6)
               E_7_MAX=E_6_MAX-E_6

         V_7_MAX=INT(E_7_MAX/HNU(7))
         DO V_7=0,V_7_MAX
            E_7=HNU(7)*DFLOAT(V_7)
            E_8_MAX=E_7_MAX-E_7

            V_8_MAX=INT(E_8_MAX/HNU(8))
            DO V_8=0,V_8_MAX
               E_8=HNU(8)*DFLOAT(V_8)
               E_9_MAX=E_8_MAX-E_8

         V_9_MAX=INT(E_9_MAX/HNU(9))
         DO V_9=0,V_9_MAX
            E_9=HNU(9)*DFLOAT(V_9)
            E_10_MAX=E_9_MAX-E_9

            V_10_MAX=INT(E_10_MAX/HNU(10))
            DO V_10=0,V_10_MAX
               E_10=HNU(10)*DFLOAT(V_10)
               E_11_MAX=E_10_MAX-E_10

         V_11_MAX=INT(E_11_MAX/HNU(11))
         DO V_11=0,V_11_MAX
            E_11=HNU(11)*DFLOAT(V_11)
            E_12_MAX=E_11_MAX-E_11

            V_12_MAX=INT(E_12_MAX/HNU(12))
            DO V_12=0,V_12_MAX
               E_12=HNU(12)*DFLOAT(V_12)
               E_13_MAX=E_12_MAX-E_12

         V_13_MAX=INT(E_13_MAX/HNU(13))
         DO V_13=0,V_13_MAX
            E_13=HNU(13)*DFLOAT(V_13)

               VIBENE=INT((E_1_MAX+E_13-E_13_MAX)/GRID)+1

            END DO ! V_13
               END DO ! V_12
            END DO ! V_11
               END DO ! V_10
            END DO ! V_9
               END DO ! V_8
            END DO ! V_7
               END DO ! V_6
            END DO ! V_5
               END DO ! V_4
            END DO ! V_3
               END DO ! V_2
            END DO ! V_1

C------------------------------------------------------------------------------------------------------
C- EXACT METHOD FOR FOURTEEN MODES
C------------------------------------------------------------------------------------------------------

      ELSEIF (NUMMODES.EQ.14) THEN

         V_1_MAX=INT(E_1_MAX/HNU(1))
         DO V_1=0,V_1_MAX
      WRITE(*,'(A,I2,A)') ' Calculating Vibrational levels (',
     &                            (100*V_1/(V_1_MAX+1)),'%)...'
            E_1=HNU(1)*DFLOAT(V_1)
            E_2_MAX=E_1_MAX-E_1

            V_2_MAX=INT(E_2_MAX/HNU(2))
            DO V_2=0,V_2_MAX
               E_2=HNU(2)*DFLOAT(V_2)
               E_3_MAX=E_2_MAX-E_2

         V_3_MAX=INT(E_3_MAX/HNU(3))
         DO V_3=0,V_3_MAX
            E_3=HNU(3)*DFLOAT(V_3)
            E_4_MAX=E_3_MAX-E_3

            V_4_MAX=INT(E_4_MAX/HNU(4))
            DO V_4=0,V_4_MAX
               E_4=HNU(4)*DFLOAT(V_4)
               E_5_MAX=E_4_MAX-E_4

         V_5_MAX=INT(E_5_MAX/HNU(5))
         DO V_5=0,V_5_MAX
            E_5=HNU(5)*DFLOAT(V_5)
            E_6_MAX=E_5_MAX-E_5

            V_6_MAX=INT(E_6_MAX/HNU(6))
            DO V_6=0,V_6_MAX
               E_6=HNU(6)*DFLOAT(V_6)
               E_7_MAX=E_6_MAX-E_6

         V_7_MAX=INT(E_7_MAX/HNU(7))
         DO V_7=0,V_7_MAX
            E_7=HNU(7)*DFLOAT(V_7)
            E_8_MAX=E_7_MAX-E_7

            V_8_MAX=INT(E_8_MAX/HNU(8))
            DO V_8=0,V_8_MAX
               E_8=HNU(8)*DFLOAT(V_8)
               E_9_MAX=E_8_MAX-E_8

         V_9_MAX=INT(E_9_MAX/HNU(9))
         DO V_9=0,V_9_MAX
            E_9=HNU(9)*DFLOAT(V_9)
            E_10_MAX=E_9_MAX-E_9

            V_10_MAX=INT(E_10_MAX/HNU(10))
            DO V_10=0,V_10_MAX
               E_10=HNU(10)*DFLOAT(V_10)
               E_11_MAX=E_10_MAX-E_10

         V_11_MAX=INT(E_11_MAX/HNU(11))
         DO V_11=0,V_11_MAX
            E_11=HNU(11)*DFLOAT(V_11)
            E_12_MAX=E_11_MAX-E_11

            V_12_MAX=INT(E_12_MAX/HNU(12))
            DO V_12=0,V_12_MAX
               E_12=HNU(12)*DFLOAT(V_12)
               E_13_MAX=E_12_MAX-E_12

         V_13_MAX=INT(E_13_MAX/HNU(13))
         DO V_13=0,V_13_MAX
            E_13=HNU(13)*DFLOAT(V_13)
            E_14_MAX=E_13_MAX-E_13

            V_14_MAX=INT(E_14_MAX/HNU(14))
            DO V_14=0,V_14_MAX
               E_14=HNU(14)*DFLOAT(V_14)

               VIBENE=INT((E_1_MAX+E_14-E_14_MAX)/GRID)+1

               END DO ! V_14
            END DO ! V_13
               END DO ! V_12
            END DO ! V_11
               END DO ! V_10
            END DO ! V_9
               END DO ! V_8
            END DO ! V_7
               END DO ! V_6
            END DO ! V_5
               END DO ! V_4
            END DO ! V_3
               END DO ! V_2
            END DO ! V_1

C------------------------------------------------------------------------------------------------------
C- EXACT METHOD FOR FIFTEEN MODES
C------------------------------------------------------------------------------------------------------

      ELSEIF (NUMMODES.EQ.15) THEN

         V_1_MAX=INT(E_1_MAX/HNU(1))
         DO V_1=0,V_1_MAX
      WRITE(*,'(A,I2,A)') ' Calculating Vibrational levels (',
     &                            (100*V_1/(V_1_MAX+1)),'%)...'
            E_1=HNU(1)*DFLOAT(V_1)
            E_2_MAX=E_1_MAX-E_1

            V_2_MAX=INT(E_2_MAX/HNU(2))
            DO V_2=0,V_2_MAX
               E_2=HNU(2)*DFLOAT(V_2)
               E_3_MAX=E_2_MAX-E_2

         V_3_MAX=INT(E_3_MAX/HNU(3))
         DO V_3=0,V_3_MAX
            E_3=HNU(3)*DFLOAT(V_3)
            E_4_MAX=E_3_MAX-E_3

            V_4_MAX=INT(E_4_MAX/HNU(4))
            DO V_4=0,V_4_MAX
               E_4=HNU(4)*DFLOAT(V_4)
               E_5_MAX=E_4_MAX-E_4

         V_5_MAX=INT(E_5_MAX/HNU(5))
         DO V_5=0,V_5_MAX
            E_5=HNU(5)*DFLOAT(V_5)
            E_6_MAX=E_5_MAX-E_5

            V_6_MAX=INT(E_6_MAX/HNU(6))
            DO V_6=0,V_6_MAX
               E_6=HNU(6)*DFLOAT(V_6)
               E_7_MAX=E_6_MAX-E_6

         V_7_MAX=INT(E_7_MAX/HNU(7))
         DO V_7=0,V_7_MAX
            E_7=HNU(7)*DFLOAT(V_7)
            E_8_MAX=E_7_MAX-E_7

            V_8_MAX=INT(E_8_MAX/HNU(8))
            DO V_8=0,V_8_MAX
               E_8=HNU(8)*DFLOAT(V_8)
               E_9_MAX=E_8_MAX-E_8

         V_9_MAX=INT(E_9_MAX/HNU(9))
         DO V_9=0,V_9_MAX
            E_9=HNU(9)*DFLOAT(V_9)
            E_10_MAX=E_9_MAX-E_9

            V_10_MAX=INT(E_10_MAX/HNU(10))
            DO V_10=0,V_10_MAX
               E_10=HNU(10)*DFLOAT(V_10)
               E_11_MAX=E_10_MAX-E_10

         V_11_MAX=INT(E_11_MAX/HNU(11))
         DO V_11=0,V_11_MAX
            E_11=HNU(11)*DFLOAT(V_11)
            E_12_MAX=E_11_MAX-E_11

            V_12_MAX=INT(E_12_MAX/HNU(12))
            DO V_12=0,V_12_MAX
               E_12=HNU(12)*DFLOAT(V_12)
               E_13_MAX=E_12_MAX-E_12

         V_13_MAX=INT(E_13_MAX/HNU(13))
         DO V_13=0,V_13_MAX
            E_13=HNU(13)*DFLOAT(V_13)
            E_14_MAX=E_13_MAX-E_13

            V_14_MAX=INT(E_14_MAX/HNU(14))
            DO V_14=0,V_14_MAX
               E_14=HNU(14)*DFLOAT(V_14)
               E_15_MAX=E_14_MAX-E_14

         V_15_MAX=INT(E_15_MAX/HNU(15))
         DO V_15=0,V_15_MAX
            E_15=HNU(15)*DFLOAT(V_15)

               VIBENE=INT((E_1_MAX+E_15-E_15_MAX)/GRID)+1

            END DO ! V_15
               END DO ! V_14
            END DO ! V_13
               END DO ! V_12
            END DO ! V_11
               END DO ! V_10
            END DO ! V_9
               END DO ! V_8
            END DO ! V_7
               END DO ! V_6
            END DO ! V_5
               END DO ! V_4
            END DO ! V_3
               END DO ! V_2
            END DO ! V_1


C------------------------------------------------------------------------------------------------------

      ELSE
         WRITE(6,*) 'INVALID NUMBER OF MODES'
         WRITE(6,*) 'ERROR TERMINATION IN VIBRATE SECTION'
         call flush(6)
         STOP
      END IF
      WRITE(*,'(A)') ' Completed Vibrational levels (100%)...'

C------------------------------------------------------------------------------------------------------
C------------------------------------------------------------------------------------------------------
C----- THIS PART FOLDS IN THE VIBRATIONAL STATES ------------------------------------------------------
C------------------------------------------------------------------------------------------------------
C------------------------------------------------------------------------------------------------------

C- FOLD IN ROTATIONAL AND VIBRATIONAL STATES INTO BUILD ARRAY

      DO ROTENE=1,ARRAY
         IF (ROTSTATE(ROTENE).GT.(0.0D0)) THEN
            DO VIBENE=1,(ARRAY-VIBENE+1)
               NOS(ROTENE+VIBENE-1)=ROTSTATE(ROTENE)+VIBSTATE(VIBENE)
               END DO ! VIBENE
            END IF
         END DO ! ROTENE

      RETURN
      END

