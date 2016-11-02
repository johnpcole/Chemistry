      SUBROUTINE DIROTOR(J_VAL,MODE,Z_P_E,DEGEN,J1_CON,K1_CON,
     &                J2_CON,K2_CON,L_CON,NOS,GRID,ARRAY,ECHO)

C- THIS SUBROUTINE CALCULATES THE NUMBER OF
C- STATES ARRAY FOR A GEOMETRY ALONG A MINIMUM
C- ENERGY PATH USING THE UNHINDERED DIROTOR THEORY

C- DEFINE ALL VARIABLES

      IMPLICIT NONE

      REAL*8 Z_P_E                                                    ! ZERO-POINT ENERGY
      REAL*8 GRID                                                     ! ARRAY GRID SIZE
      REAL*8 NOS                                                      ! NUMBER OF STATES ARRAY
      REAL*8 DEGEN                                                    ! THE ELECTRONIC/SYMMETRY DEGENERACY
      REAL*8 E_LIM1,E_LIM2,E_LIM3,E_LIM4,E_LIM5                       ! AVAILABLE ENERGY VALUES
      REAL*8 J1_CON,J2_CON,K1_CON,K2_CON,L_CON                        ! ROTATIONAL CONSTANTS
      REAL*8 L_ENE,J1_ENE,J2_ENE,K1_ENE,K2_ENE                        ! COMPONENT ANGULAR MOMENTUM ENERGIES
      REAL*8 KSQUAR                                                   ! TEMPORARY VARIABLE FOR NEGATIVE Ki_CON CASES
      INTEGER J_VAL,J_BOTH                                            ! TOTAL ANGULAR MOMENTUM QUANTUM NUMBERS
      INTEGER MJ_VAL,M1_VAL                                           ! DEGENERACY COUNTING QUANTUM NUMBERS
      INTEGER L_VAL,L_LIM,L_MIN,L_MAX                                 ! ORBITAL MOTION QUANTUM NUMBERS
      INTEGER J1_VAL,J1_LIM                                           ! FIRST (SMALLER) FRAGMENT QUANTUM NUMBERS
      INTEGER K1_VAL,K1_LIM,K1_MAX                                    ! FIRST (SMALLER) FRAGMENT ORIENTATION QUANTUM NUMBERS
      INTEGER J2_VAL,J2_LIM                                           ! SECOND (LARGER) FRAGMENT QUANTUM NUMBERS
      INTEGER K2_VAL,K2_LIM,K2_MIN,K2_MAX                             ! SECOND (LARGER) FRAGMENT ORIENTATION QUANTUM NUMBERS
      INTEGER MODE                                                    ! THE TYPE OF SYSTEM
      INTEGER ENERGY                                                  ! GRID BASED ENERGY
      INTEGER ARRAY                                                   ! ARRAY SIZE
      INTEGER LEVELS                                                  ! THE DEGENERACY OF M LEVELS
      LOGICAL ECHO


C- DEFINE ARRAYS

      DIMENSION NOS(ARRAY)                                            ! NUMBER OF STATES ARRAY


      E_LIM1=(DFLOAT(ARRAY)*GRID)-Z_P_E                               ! CALCULATE THE AVAILABLE ENERGY FOR ROTATIONAL MOTION

      IF (ECHO) WRITE(6,*) 'FIRST LITTLE J CONSTANT=',J1_CON
      IF (ECHO) WRITE(6,*) 'FIRST LITTLE K CONSTANT=',K1_CON
      IF (ECHO) WRITE(6,*) 'SECOND LITTLE J CONSTANT=',J2_CON
      IF (ECHO) WRITE(6,*) 'SECOND LITTLE K CONSTANT=',K2_CON
      IF (ECHO) WRITE(6,*) 'ORBITAL (L) CONSTANT=',L_CON
      IF (ECHO) WRITE(6,*) 'CURRENT MODE=',MODE
      CALL FLUSH(6)


      ! EXECUTE CODE DEPENDING ON THE DIROTOR SYSTEM:  THE POSSIBLE COMBINATIONS
      ! ARE "ARRANGED" IN THE CONVERT SUBROUTINE TO MINIMISE CODE. ATOM-ATOM=0,
      ! ATOM-LINEAR=1, ATOM-TOP=2, LINEAR-LINEAR=3, LINEAR-TOP=4, TOP-TOP=5


      ! THERE ARE TWO SETS OF CODE: ONE FOR NORMAL SYSTEMS WITH POSITIVE ORBITAL
      ! ROTATIONAL CONSTANTS, AND ONE FOR REAGENT SYSTEMS WITH L CONSTANT=-REDUCED MASS.
      ! THE REDUCED MASS IS REQUIRED FOR CALCULATING THE SPHERICAL TRANSLATIONAL STATES.

                IF (J_VAL.LT.0) THEN

C============================================================================================================================
C============================================================================================================================
C=== PARTICLE IN A BOX / UNCOUPLED BIMOLECULAR REAGENTS =====================================================================
C============================================================================================================================
C============================================================================================================================

      WRITE(6,*) '(USING UNCOUPLED DIROTOR FOR REAGENT SYSTEM)'

      IF (MODE.EQ.1) THEN

C----- ATOM-LINEAR SYSTEM -----------------------------------------------

      J2_LIM=INT(DSQRT(0.25+(E_LIM1/J2_CON))-0.5)                     ! SET ENERGY LIMIT ON J", THE LINEAR FRAGMENT
      DO J2_VAL=0,J2_LIM
      IF (ECHO) WRITE(*,'(A,I2,A)')
     &                        ' Calculating Rotational levels (',
     &                            (100*J2_VAL/(J2_LIM+1)),'%)...'
      CALL FLUSH(6)
         J2_ENE=J2_CON*DFLOAT(J2_VAL*(J2_VAL+1))                      ! CALCULATE J" ENERGY, B"J"(J"+1)
         E_LIM2=E_LIM1-J2_ENE                                         ! CALCULATE AVAILABLE ENERGY MINUS J" ENERGY
               ENERGY=ARRAY+INT(-E_LIM2/GRID)                  ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
            NOS(ENERGY)=NOS(ENERGY)+DFLOAT((2*J2_VAL)+1)            ! ADD M LEVELS TO ENERGY ARRAY
         END DO !J"

C------------------------------------------------------------------------

      ELSEIF (MODE.EQ.2) THEN

C----- ATOM-TOP SYSTEM --------------------------------------------------

      IF (K2_CON.LT.0.0) THEN                                         ! EXECUTE DIFFERENT CODE DEPENDING ON WHETHER TOP IS PROLATE OR OBLATE

      ! NOTE THAT WHEN K2_CON IS NEGATIVE, THE ROTATIONAL ENERGY EXPRESSION
      ! IS WRITTEN AS E= [ BJ(J+1)+(A-C)(J^2) ] + [ (A-C)((K^2)-(J^2)) ]
      ! THIS ALLOWS THE ENERGY TO BE GIVEN AS A BASE VALUE (THE LEFT TERM)
      ! PLUS A POSITIVE K BASED TERM (THE RIGHT TERM)

      J2_LIM=INT(0.5*(DSQRT((J2_CON**2.0)                             ! SET ENERGY LIMIT ON J", THE TOP FRAGMENT
     &                        +(4*(J2_CON+K2_CON)*E_LIM1))
     &                              -J2_CON)/(J2_CON+K2_CON))
      DO J2_VAL=0,J2_LIM
      IF (ECHO) WRITE(*,'(A,I2,A)')
     &                        ' Calculating Rotational levels (',
     &                            (100*J2_VAL/(J2_LIM+1)),'%)...'
      CALL FLUSH(6)
         J2_ENE=(J2_CON*DFLOAT(J2_VAL*(J2_VAL+1)))                    ! CALCULATE J" EQUIVALENT ENERGY, B"J"(J"+1)+(A"-C")(J"^2)
     &                            +(K2_CON*DFLOAT(J2_VAL**2))
         E_LIM3=E_LIM1-J2_ENE                                         ! CALCULATE AVAILABLE ENERGY MINUS J" ENERGY
               ENERGY=ARRAY+INT(-E_LIM3/GRID)                         ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
               LEVELS=(2*J_VAL)+1                                     ! START COUNTING THE M LEVELS
               KSQUAR=DFLOAT(J2_VAL**2)+(E_LIM3/K2_CON)               ! CALCULATE MINIMUM VALUE OF K"^2 WITHIN ENERGY CONSTRAINTS (MAY BE NEGATIVE)
               K2_MIN=INT(0.5+DSQRT(MAX(0.0,KSQUAR)))                 ! SET LOWER LIMIT FOR K" QUANTUM NUMBER
               IF (K2_MIN.LT.1) THEN                                  ! DO K"=0 LEVEL IF ENERGY PERMITS
                  ENERGY=ARRAY+INT(-E_LIM3/GRID)                      ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
            NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))            ! ADD M LEVELS TO ENERGY ARRAY
                  END IF !K"
               LEVELS=LEVELS*2                                        ! STORE LEVELS AS DOUBLEY DEGENERATE VALUE FOR K">0
               DO K2_VAL=K2_MIN,J2_VAL
                  K2_ENE=K2_CON*DFLOAT((K2_VAL**2)-(J2_VAL**2))       ! CALCULATE K" EQUIVALENT ENERGY, (K"^2-J"^2)(A"-C")
                  ENERGY=ARRAY+INT((K2_ENE-E_LIM3)/GRID)              ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
            NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))            ! ADD M LEVELS TO ENERGY ARRAY
                  END DO !K"
         END DO !J"

      ELSE

      J2_LIM=INT(DSQRT(0.25+(E_LIM1/J2_CON))-0.5)                     ! SET ENERGY LIMIT ON J", THE TOP FRAGMENT
      DO J2_VAL=0,J2_LIM
      IF (ECHO) WRITE(*,'(A,I2,A)')
     &                        ' Calculating Rotational levels (',
     &                            (100*J2_VAL/(J2_LIM+1)),'%)...'
         CALL FLUSH(6)
         J2_ENE=J2_CON*DFLOAT(J2_VAL*(J2_VAL+1))                      ! CALCULATE J" ENERGY, B"J"(J"+1)
         E_LIM3=E_LIM1-J2_ENE                                         ! CALCULATE AVAILABLE ENERGY MINUS J" ENERGY
               LEVELS=(2*J_VAL)+1                                     ! START COUNTING THE M LEVELS
               ENERGY=ARRAY+INT(-E_LIM3/GRID)                         ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
            NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))            ! ADD M LEVELS TO ENERGY ARRAY
               K2_LIM=INT(DSQRT(E_LIM3/K2_CON))                       ! SET ENERGY LIMIT ON K"
               K2_MAX=MIN(J2_VAL,K2_LIM)                              ! CALCULATE K" LIMIT WITHIN CLEBSCH-GORDAN SERIES & ENERGY
               IF (K2_MAX.GE.1) THEN                                  ! ONLY PROCEED IF J">0 AND IF ENERGY LIMITS PERMIT
                  LEVELS=LEVELS*2                                     ! STORE LEVELS AS DOUBLEY DEGENERATE VALUE FOR K">0
                  DO K2_VAL=1,K2_MAX
                     K2_ENE=K2_CON*DFLOAT(K2_VAL**2)                  ! CALCULATE K" ENERGY, (K"^2)(A"-C")
                     ENERGY=ARRAY+INT((K2_ENE-E_LIM3)/GRID)           ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
            NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))            ! ADD M LEVELS TO ENERGY ARRAY
                     END DO !K"
                  END IF !K"
         END DO !J"

      END IF

C------------------------------------------------------------------------

      ELSEIF (MODE.EQ.3) THEN

C----- LINEAR-LINEAR SYSTEM ---------------------------------------------

      J1_LIM=INT(DSQRT(0.25+(E_LIM1/J1_CON))-0.5)                     ! SET ENERGY LIMIT J', THE FIRST LINEAR FRAGMENT
      DO J1_VAL=0,J1_LIM
      IF (ECHO) WRITE(*,'(A,I2,A)')
     &                        ' Calculating Rotational levels (',
     &                            (100*J1_VAL/(J1_LIM+1)),'%)...'
      CALL FLUSH(6)
         J1_ENE=J1_CON*DFLOAT(J1_VAL*(J1_VAL+1))                      ! CALCULATE J' ENERGY, B'J'(J'+1)
         E_LIM2=E_LIM1-J1_ENE                                         ! CALCULATE AVAILABLE ENERGY MINUS J' ENERGY
         J2_LIM=INT(DSQRT(0.25+(E_LIM2/J2_CON))-0.5)                  ! SET ENERGY LIMIT J", THE SECOND LINEAR FRAGMENT
         DO J2_VAL=0,J2_LIM
            J2_ENE=J2_CON*DFLOAT(J2_VAL*(J2_VAL+1))                   ! CALCULATE J" ENERGY, B"J"(J"+1)
            E_LIM3=E_LIM2-J2_ENE                                      ! CALCULATE AVAILABLE ENERGY MINUS J" ENERGY
                     ENERGY=ARRAY+INT(-E_LIM3/GRID)            ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
                     LEVELS=((2*J1_VAL)+1)*((2*J2_VAL)+1)                                         ! START COUNTING THE M LEVELS
            NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))            ! ADD M LEVELS TO ENERGY ARRAY
            END DO !J"
         END DO !J'

C------------------------------------------------------------------------

      ELSEIF (MODE.EQ.4) THEN

C----- LINEAR-TOP SYSTEM ------------------------------------------------

      IF (K2_CON.LT.0.0) THEN                                         ! EXECUTE DIFFERENT CODE DEPENDING ON WHETHER TOP IS PROLATE OR OBLATE

      ! NOTE THAT WHEN K2_CON IS NEGATIVE, THE ROTATIONAL ENERGY EXPRESSION
      ! IS WRITTEN AS E= [ BJ(J+1)+(A-C)(J^2) ] + [ (A-C)((K^2)-(J^2)) ]
      ! THIS ALLOWS THE ENERGY TO BE GIVEN AS A BASE VALUE (THE LEFT TERM)
      ! PLUS A POSITIVE K BASED TERM (THE RIGHT TERM)

      J1_LIM=INT(DSQRT(0.25+(E_LIM1/J1_CON))-0.5)                     ! SET ENERGY LIMIT J', THE FIRST LINEAR FRAGMENT
      DO J1_VAL=0,J1_LIM
      IF (ECHO) WRITE(*,'(A,I2,A)')
     &                        ' Calculating Rotational levels (',
     &                            (100*J1_VAL/(J1_LIM+1)),'%)...'
      CALL FLUSH(6)
         J1_ENE=J1_CON*DFLOAT(J1_VAL*(J1_VAL+1))                      ! CALCULATE J' ENERGY, B'J'(J'+1)
         E_LIM2=E_LIM1-J1_ENE                                         ! CALCULATE AVAILABLE ENERGY MINUS J' ENERGY
         J2_LIM=INT(0.5*(DSQRT((J2_CON**2.0)                          ! SET ENERGY LIMIT ON J", THE TOP FRAGMENT
     &                        +(4*(J2_CON+K2_CON)*E_LIM2))
     &                              -J2_CON)/(J2_CON+K2_CON))
         DO J2_VAL=0,J2_LIM
            J2_ENE=(J2_CON*DFLOAT(J2_VAL*(J2_VAL+1)))                 ! CALCULATE J" EQUIVALENT ENERGY, B"J"(J"+1)+(A"-C")(J"^2)
     &                            +(K2_CON*DFLOAT(J2_VAL**2))
            E_LIM4=E_LIM2-J2_ENE                                      ! CALCULATE AVAILABLE ENERGY MINUS J" ENERGY
                     LEVELS=((2*J1_VAL)+1)*((2*J2_VAL)+1)                       ! START COUNTING THE M LEVELS
                     KSQUAR=DFLOAT(J2_VAL**2)+(E_LIM4/K2_CON)         ! CALCULATE MINIMUM VALUE OF K"^2 WITHIN ENERGY CONSTRAINTS (MAY BE NEGATIVE)
                     K2_MIN=INT(0.5+DSQRT(MAX(0.0,KSQUAR)))           ! SET LOWER LIMIT FOR K" QUANTUM NUMBER
                     IF (K2_MIN.LT.1) THEN                            ! DO K"=0 LEVEL IF ENERGY PERMITS
                        ENERGY=ARRAY+INT(-E_LIM4/GRID)                ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
            NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))            ! ADD M LEVELS TO ENERGY ARRAY
                        END IF !K"
                     LEVELS=LEVELS*2                                  ! STORE LEVELS AS DOUBLEY DEGENERATE VALUE FOR K">0
                     DO K2_VAL=K2_MIN,J2_VAL
                        K2_ENE=K2_CON*DFLOAT((K2_VAL**2)-(J2_VAL**2)) ! CALCULATE K" EQUIVALENT ENERGY, (K"^2-J"^2)(A"-C")
                        ENERGY=ARRAY+INT((K2_ENE-E_LIM4)/GRID)        ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
            NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))            ! ADD M LEVELS TO ENERGY ARRAY
                        END DO !K"
            END DO !J"
         END DO !J'

      ELSE

      J1_LIM=INT(DSQRT(0.25+(E_LIM1/J1_CON))-0.5)                     ! SET ENERGY LIMIT J', THE FIRST LINEAR FRAGMENT
      DO J1_VAL=0,J1_LIM
      IF (ECHO) WRITE(*,'(A,I2,A)')
     &                        ' Calculating Rotational levels (',
     &                            (100*J1_VAL/(J1_LIM+1)),'%)...'
      CALL FLUSH(6)
         J1_ENE=J1_CON*DFLOAT(J1_VAL*(J1_VAL+1))                      ! CALCULATE J' ENERGY, B'J'(J'+1)
         E_LIM2=E_LIM1-J1_ENE                                         ! CALCULATE AVAILABLE ENERGY MINUS J' ENERGY
         J2_LIM=INT(DSQRT(0.25+(E_LIM2/J2_CON))-0.5)                  ! SET ENERGY LIMIT J", THE SECOND LINEAR FRAGMENT
         DO J2_VAL=0,J2_LIM
            J2_ENE=J2_CON*DFLOAT(J2_VAL*(J2_VAL+1))                   ! CALCULATE J" ENERGY, B"J"(J"+1)
            E_LIM4=E_LIM2-J2_ENE                                      ! CALCULATE AVAILABLE ENERGY MINUS J" ENERGY
                     LEVELS=((2*J1_VAL)+1)*((2*J2_VAL)+1)                       ! START COUNTING THE M LEVELS
                     ENERGY=ARRAY+INT(-E_LIM4/GRID)                   ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
            NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))            ! ADD M LEVELS TO ENERGY ARRAY
                     K2_LIM=INT(DSQRT(E_LIM4/K2_CON))                 ! SET ENERGY LIMIT ON K"
                     K2_MAX=MIN(J2_VAL,K2_LIM)                        ! CALCULATE K" LIMIT WITHIN CLEBSCH-GORDAN SERIES & ENERGY
                     IF (K2_MAX.GE.1) THEN                            ! ONLY PROCEED IF J">0 AND IF ENERGY LIMITS PERMIT
                     LEVELS=LEVELS*2                                  ! STORE LEVELS AS DOUBLEY DEGENERATE VALUE FOR K">0
                        DO K2_VAL=1,K2_MAX
                           K2_ENE=K2_CON*DFLOAT(K2_VAL**2)            ! CALCULATE K" ENERGY, (K"^2)(A"-C")
                           ENERGY=ARRAY+INT((K2_ENE-E_LIM4)/GRID)     ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
            NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))            ! ADD M LEVELS TO ENERGY ARRAY
                           END DO !K"
                        END IF !K"
            END DO !J"
         END DO !J'


      END IF

C------------------------------------------------------------------------

      ELSEIF (MODE.EQ.5) THEN

C----- TOP-TOP SYSTEM ---------------------------------------------------

      ! IF OBLATE/PROLATE SYSTEM, FRAGMENT 2 MUST HAVE NEGATIVE
      ! A-C (K) CONSTANT. THIS IS DONE IN CONVERT SUBROUTINE

      IF (K2_CON.LT.0.0) THEN                                         ! EXECUTE DIFFERENT CODE DEPENDING ON WHETHER FRAGMENTS ARE PROLATE OR OBLATE

      ! NOTE THAT WHEN K CONST IS NEGATIVE, THE ROTATIONAL ENERGY EXPRESSION
      ! IS WRITTEN AS E= [ BJ(J+1)+(A-C)(J^2) ] + [ (A-C)((K^2)-(J^2)) ]
      ! THIS ALLOWS THE ENERGY TO BE GIVEN AS A BASE VALUE (THE LEFT TERM)
      ! PLUS A POSITIVE K BASED TERM (THE RIGHT TERM)

      IF (K1_CON.LT.0.0) THEN

      WRITE(6,*) 'OBLATE/PROLATE SYSTEM NOT AVAILABLE'
      WRITE(6,*) 'ERROR TERMINATION IN DIROTOR SECTION'
      call flush(6)
      STOP

      ELSE

      WRITE(6,*) 'DOUBLE NEGATIVE K_CON SYSTEM NOT AVAILABLE'
      WRITE(6,*) 'ERROR TERMINATION IN DIROTOR SECTION'
      call flush(6)
      STOP

      END IF

      ELSE

      IF (K1_CON.LT.0.0) THEN

      WRITE(6,*) 'INCORRECTLY ORDERED OBLATE/PROLATE SYSTEM'
      WRITE(6,*) 'ERROR TERMINATION IN DIROTOR SECTION'
      call flush(6)
      STOP

      ELSE

      J1_LIM=INT(DSQRT(0.25+(E_LIM1/J1_CON))-0.5)                     ! SET ENERGY LIMIT J', THE FIRST LINEAR FRAGMENT
      DO J1_VAL=0,J1_LIM
      IF (ECHO) WRITE(*,'(A,I2,A)')
     &                        ' Calculating Rotational levels (',
     &                            (100*J1_VAL/(J1_LIM+1)),'%)...'
      CALL FLUSH(6)
         J1_ENE=J1_CON*DFLOAT(J1_VAL*(J1_VAL+1))                      ! CALCULATE J' ENERGY, B'J'(J'+1)
         E_LIM2=E_LIM1-J1_ENE                                         ! CALCULATE AVAILABLE ENERGY MINUS J' ENERGY
         J2_LIM=INT(DSQRT(0.25+(E_LIM2/J2_CON))-0.5)                  ! SET ENERGY LIMIT J", THE SECOND LINEAR FRAGMENT
         DO J2_VAL=0,J2_LIM
            J2_ENE=J2_CON*DFLOAT(J2_VAL*(J2_VAL+1))                   ! CALCULATE J" ENERGY, B"J"(J"+1)
            E_LIM4=E_LIM2-J2_ENE                                      ! CALCULATE AVAILABLE ENERGY MINUS J" ENERGY
                     LEVELS=((2*J1_VAL)+1)*((2*J2_VAL)+1)                 ! START COUNTING THE M LEVELS
                     ENERGY=ARRAY+INT(-E_LIM4/GRID)                   ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
            NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))            ! ADD M LEVELS TO ENERGY ARRAY
                     K2_LIM=INT(DSQRT(E_LIM4/K2_CON))                 ! SET ENERGY LIMIT ON K"
                     K2_MAX=MIN(J2_VAL,K2_LIM)                        ! CALCULATE K" LIMIT WITHIN CLEBSCH-GORDAN SERIES & ENERGY
                     LEVELS=LEVELS*4
                     DO K2_VAL=0,K2_MAX
                        K2_ENE=K2_CON*DFLOAT(K2_VAL**2)               ! CALCULATE K" ENERGY, (K"^2)(A"-C")
                        E_LIM5=E_LIM4-K2_ENE                          ! CALCULATE AVAILABLE ENERGY MINUS K" ENERGY
                        K1_LIM=INT(DSQRT(E_LIM5/K1_CON))              ! SET ENERGY LIMIT ON K'
                        K1_MAX=MIN(J1_VAL,K1_LIM)                     ! CALCULATE K' LIMIT WITHIN CLEBSCH-GORDAN SERIES & ENERGY
                        DO K1_VAL=0,K1_MAX
                           K1_ENE=K1_CON*DFLOAT(K1_VAL**2)            ! CALCULATE K' ENERGY, (K'^2)(A'-C')
                           ENERGY=ARRAY+INT((K1_ENE-E_LIM5)/GRID)     ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
            NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))            ! ADD M LEVELS TO ENERGY ARRAY
                           END DO !K'
                        END DO !K"
                     LEVELS=LEVELS/2
                     DO K2_VAL=0,K2_MAX
                        K2_ENE=K2_CON*DFLOAT(K2_VAL**2)               ! CALCULATE K" ENERGY, (K"^2)(A"-C")
                        ENERGY=ARRAY+INT((K2_ENE-E_LIM4)/GRID)        ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
            NOS(ENERGY)=NOS(ENERGY)-(DEGEN*DFLOAT(LEVELS))            ! ADD M LEVELS TO ENERGY ARRAY
                        END DO !K": K'=0 OVERCOUNTS REMOVAL
                     K1_LIM=INT(DSQRT(E_LIM4/K1_CON))                 ! SET ENERGY LIMIT ON K' FOR K"=0 SPECIAL CASES
                     K1_MAX=MIN(J1_VAL,K1_LIM)                        ! CALCULATE K' LIMIT WITHIN CLEBSCH-GORDAN SERIES & ENERGY
                     DO K1_VAL=0,K1_MAX
                        K1_ENE=K1_CON*DFLOAT(K1_VAL**2)               ! CALCULATE K' ENERGY, (K'^2)(A'-C')
                        ENERGY=ARRAY+INT((K1_ENE-E_LIM4)/GRID)        ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
            NOS(ENERGY)=NOS(ENERGY)-(DEGEN*DFLOAT(LEVELS))            ! ADD M LEVELS TO ENERGY ARRAY
                        END DO !K': K"=0 OVERCOUNTS REMOVAL
            END DO !J"
         END DO !J'

      END IF

      END IF

C------------------------------------------------------------------------

      ELSE

C----- INVALID SYSTEM ---------------------------------------------------

         WRITE(6,*) 'INVALID DIROTOR MODE'
         WRITE(6,*) 'ERROR TERMINATION IN DIROTOR SECTION'
         call flush(6)
         STOP

      END IF

C============================================================================================================================
C============================================================================================================================
C=== END OF PARTICLE IN A BOX / UNCOUPLED BIMOLECULAR REAGENTS SECTION ======================================================
C============================================================================================================================
C============================================================================================================================

                ELSE

          IF (L_CON.LT.(0.0D0)) THEN
             WRITE(6,*) 'NEGATIVE ORBITAL CONSTANT NOT ALLOWED'
             WRITE(6,*) 'ERROR TERMINATION IN DIROTOR SECTION'
             call flush(6)
             STOP
             END IF

C============================================================================================================================
C============================================================================================================================
C=== NORMAL SYSTEMS HAVING COUPLED ANGULAR MOMENTA ==========================================================================
C============================================================================================================================
C============================================================================================================================

      IF (MODE.EQ.1) THEN

C----- ATOM-LINEAR SYSTEM -----------------------------------------------

      J2_LIM=INT(DSQRT(0.25+(E_LIM1/J2_CON))-0.5)                     ! SET ENERGY LIMIT ON J", THE LINEAR FRAGMENT
      DO J2_VAL=0,J2_LIM
      IF (ECHO) WRITE(*,'(A,I2,A)')
     &                        ' Calculating Rotational levels (',
     &                            (100*J2_VAL/(J2_LIM+1)),'%)...'
      CALL FLUSH(6)
         J2_ENE=J2_CON*DFLOAT(J2_VAL*(J2_VAL+1))                      ! CALCULATE J" ENERGY, B"J"(J"+1)
         E_LIM2=E_LIM1-J2_ENE                                         ! CALCULATE AVAILABLE ENERGY MINUS J" ENERGY
         L_LIM=INT(DSQRT(0.25+(E_LIM2/L_CON))-0.5)                    ! SET ENERGY LIMIT ON L, THE ORBITAL MOTION
         L_MIN=ABS(J_VAL-J2_VAL)                                      ! CALCULATE MINIMUM L VALUE WITHIN CLEBSCH-GORDAN SERIES
         IF (L_LIM.GE.L_MIN) THEN                                     ! ONLY PROCEED IF ENERGY LIMIT IS WITHIN CLEBSCH-GORDAN SERIES
            L_MAX=MIN(L_LIM,J_VAL+J2_VAL)                             ! CALCULATE MAXIMUM L VALUE WITHIN CLEBSCH-GORDAN SERIES & ENERGY
            DO L_VAL=L_MIN,L_MAX
               L_ENE=L_CON*DFLOAT(L_VAL*(L_VAL+1))                    ! CALCULATE L ENERGY, BL(L+1)
               ENERGY=ARRAY+INT((L_ENE-E_LIM2)/GRID)                  ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
               LEVELS=(2*J_VAL)+1                                     ! START COUNTING THE M LEVELS
               DO MJ_VAL=-J_VAL,J_VAL
                  LEVELS=LEVELS+(2*MIN(J2_VAL,L_VAL+MJ_VAL))
                  END DO !MJ
            NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))            ! ADD M LEVELS TO ENERGY ARRAY
               END DO !L
            END IF !L
         END DO !J"

C------------------------------------------------------------------------

      ELSEIF (MODE.EQ.2) THEN

C----- ATOM-TOP SYSTEM --------------------------------------------------


      IF (K2_CON.LT.0.0) THEN                                         ! EXECUTE DIFFERENT CODE DEPENDING ON WHETHER TOP IS PROLATE OR OBLATE

      ! NOTE THAT WHEN K2_CON IS NEGATIVE, THE ROTATIONAL ENERGY EXPRESSION
      ! IS WRITTEN AS E= [ BJ(J+1)+(A-C)(J^2) ] + [ (A-C)((K^2)-(J^2)) ]
      ! THIS ALLOWS THE ENERGY TO BE GIVEN AS A BASE VALUE (THE LEFT TERM)
      ! PLUS A POSITIVE K BASED TERM (THE RIGHT TERM)

      J2_LIM=INT(0.5*(DSQRT((J2_CON**2.0)                             ! SET ENERGY LIMIT ON J", THE TOP FRAGMENT
     &                        +(4*(J2_CON+K2_CON)*E_LIM1))
     &                              -J2_CON)/(J2_CON+K2_CON))
      DO J2_VAL=0,J2_LIM
      IF (ECHO) WRITE(*,'(A,I2,A)')
     &                        ' Calculating Rotational levels (',
     &                            (100*J2_VAL/(J2_LIM+1)),'%)...'
      CALL FLUSH(6)
         J2_ENE=(J2_CON*DFLOAT(J2_VAL*(J2_VAL+1)))                    ! CALCULATE J" EQUIVALENT ENERGY, B"J"(J"+1)+(A"-C")(J"^2)
     &                            +(K2_CON*DFLOAT(J2_VAL**2))
         E_LIM2=E_LIM1-J2_ENE                                         ! CALCULATE AVAILABLE ENERGY MINUS J" ENERGY
         L_LIM=INT(DSQRT(0.25+(E_LIM2/L_CON))-0.5)                    ! SET ENERGY LIMIT ON L, THE ORBITAL MOTION
         L_MIN=ABS(J_VAL-J2_VAL)                                      ! CALCULATE MINIMUM L VALUE WITHIN CLEBSCH-GORDAN SERIES
         IF (L_LIM.GE.L_MIN) THEN                                     ! ONLY PROCEED IF ENERGY LIMIT IS WITHIN CLEBSCH-GORDAN SERIES
            L_MAX=MIN(L_LIM,J_VAL+J2_VAL)                             ! CALCULATE MAXIMUM L VALUE WITHIN CLEBSCH-GORDAN SERIES & ENERGY
            DO L_VAL=L_MIN,L_MAX
               L_ENE=L_CON*DFLOAT(L_VAL*(L_VAL+1))                    ! CALCULATE L ENERGY, BL(L+1)
               E_LIM3=E_LIM2-L_ENE                                    ! CALCULATE AVAILABLE ENERGY MINUS L ENERGY
               ENERGY=ARRAY+INT(-E_LIM3/GRID)                         ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
               LEVELS=(2*J_VAL)+1                                     ! START COUNTING THE M LEVELS
               DO MJ_VAL=-J_VAL,J_VAL
                  LEVELS=LEVELS+(2*MIN(J2_VAL,L_VAL+MJ_VAL))
                  END DO !MJ
               KSQUAR=DFLOAT(J2_VAL**2)+(E_LIM3/K2_CON)               ! CALCULATE MINIMUM VALUE OF K"^2 WITHIN ENERGY CONSTRAINTS (MAY BE NEGATIVE)
               K2_MIN=INT(0.5+DSQRT(MAX(0.0,KSQUAR)))                 ! SET LOWER LIMIT FOR K" QUANTUM NUMBER
               IF (K2_MIN.LT.1) THEN                                  ! DO K"=0 LEVEL IF ENERGY PERMITS
                  ENERGY=ARRAY+INT(-E_LIM3/GRID)                      ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
           NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))             ! ADD M LEVELS TO ENERGY ARRAY FOR K"=0 CASE
                  END IF !K"
               LEVELS=LEVELS*2                                        ! STORE LEVELS AS DOUBLEY DEGENERATE VALUE FOR K">0
               DO K2_VAL=K2_MIN,J2_VAL
                  K2_ENE=K2_CON*DFLOAT((K2_VAL**2)-(J2_VAL**2))       ! CALCULATE K" EQUIVALENT ENERGY, (K"^2-J"^2)(A"-C")
                  ENERGY=ARRAY+INT((K2_ENE-E_LIM3)/GRID)              ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
           NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))             ! ADD M LEVELS TO ENERGY ARRAY FOR K">0 CASE
                  END DO !K"
               END DO !L
            END IF !L
         END DO !J"

      ELSE

      J2_LIM=INT(DSQRT(0.25+(E_LIM1/J2_CON))-0.5)                     ! SET ENERGY LIMIT ON J", THE TOP FRAGMENT
      DO J2_VAL=0,J2_LIM
      IF (ECHO) WRITE(*,'(A,I2,A)')
     &                        ' Calculating Rotational levels (',
     &                            (100*J2_VAL/(J2_LIM+1)),'%)...'
      CALL FLUSH(6)
         J2_ENE=J2_CON*DFLOAT(J2_VAL*(J2_VAL+1))                      ! CALCULATE J" ENERGY, B"J"(J"+1)
         E_LIM2=E_LIM1-J2_ENE                                         ! CALCULATE AVAILABLE ENERGY MINUS J" ENERGY
         L_LIM=INT(DSQRT(0.25+(E_LIM2/L_CON))-0.5)                    ! SET ENERGY LIMIT ON L, THE ORBITAL MOTION
         L_MIN=ABS(J_VAL-J2_VAL)                                      ! CALCULATE MINIMUM L VALUE WITHIN CLEBSCH-GORDAN SERIES
         IF (L_LIM.GE.L_MIN) THEN                                     ! ONLY PROCEED IF ENERGY LIMIT IS WITHIN CLEBSCH-GORDAN SERIES
            L_MAX=MIN(L_LIM,J_VAL+J2_VAL)                             ! CALCULATE MAXIMUM L VALUE WITHIN CLEBSCH-GORDAN SERIES & ENERGY
            DO L_VAL=L_MIN,L_MAX
               L_ENE=L_CON*DFLOAT(L_VAL*(L_VAL+1))                    ! CALCULATE L ENERGY, BL(L+1)
               E_LIM3=E_LIM2-L_ENE                                    ! CALCULATE AVAILABLE ENERGY MINUS L ENERGY
               LEVELS=(2*J_VAL)+1                                     ! START COUNTING THE M LEVELS
               DO MJ_VAL=-J_VAL,J_VAL
                  LEVELS=LEVELS+(2*MIN(J2_VAL,L_VAL+MJ_VAL))
                  END DO !MJ
               ENERGY=ARRAY+INT(-E_LIM3/GRID)                         ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
         NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))               ! ADD M LEVELS TO ENERGY ARRAY FOR K"=0 SPECIAL CASE
               K2_LIM=INT(DSQRT(E_LIM3/K2_CON))                       ! SET ENERGY LIMIT ON K"
               K2_MAX=MIN(J2_VAL,K2_LIM)                              ! CALCULATE K" LIMIT WITHIN CLEBSCH-GORDAN SERIES & ENERGY
               IF (K2_MAX.GE.1) THEN                                  ! ONLY PROCEED IF J">0 AND IF ENERGY LIMITS PERMIT
                  LEVELS=LEVELS*2                                     ! STORE LEVELS AS DOUBLEY DEGENERATE VALUE FOR K">0
                  DO K2_VAL=1,K2_MAX
                     K2_ENE=K2_CON*DFLOAT(K2_VAL**2)                  ! CALCULATE K" ENERGY, (K"^2)(A"-C")
                     ENERGY=ARRAY+INT((K2_ENE-E_LIM3)/GRID)           ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
         NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))               ! ADD M LEVELS TO ENERGY ARRAY FOR K">0 CASE
                     END DO !K"
                  END IF !K"
               END DO !L
            END IF !L
         END DO !J"

      END IF

C------------------------------------------------------------------------

      ELSEIF (MODE.EQ.3) THEN

C----- LINEAR-LINEAR SYSTEM ---------------------------------------------

      J1_LIM=INT(DSQRT(0.25+(E_LIM1/J1_CON))-0.5)                     ! SET ENERGY LIMIT J', THE FIRST LINEAR FRAGMENT
      DO J1_VAL=0,J1_LIM
      IF (ECHO) WRITE(*,'(A,I2,A)')
     &                        ' Calculating Rotational levels (',
     &                            (100*J1_VAL/(J1_LIM+1)),'%)...'
      CALL FLUSH(6)
         J1_ENE=J1_CON*DFLOAT(J1_VAL*(J1_VAL+1))                      ! CALCULATE J' ENERGY, B'J'(J'+1)
         E_LIM2=E_LIM1-J1_ENE                                         ! CALCULATE AVAILABLE ENERGY MINUS J' ENERGY
         J2_LIM=INT(DSQRT(0.25+(E_LIM2/J2_CON))-0.5)                  ! SET ENERGY LIMIT J", THE SECOND LINEAR FRAGMENT
         DO J2_VAL=0,J2_LIM
            J2_ENE=J2_CON*DFLOAT(J2_VAL*(J2_VAL+1))                   ! CALCULATE J" ENERGY, B"J"(J"+1)
            E_LIM3=E_LIM2-J2_ENE                                      ! CALCULATE AVAILABLE ENERGY MINUS J" ENERGY
            L_LIM=INT(DSQRT(0.25+(E_LIM3/L_CON))-0.5)                 ! SET ENERGY LIMIT L, THE ORBITAL MOTION OF THE FRAGMENTS
            DO J_BOTH=ABS(J1_VAL-J2_VAL),J1_VAL+J2_VAL
               L_MIN=ABS(J_BOTH+J_VAL)                                ! SET MINIMUM L VALUE (CLEBSCH-GORDAN)
               IF (L_LIM.GE.L_MIN) THEN                               ! ONLY PROCEED IF ENERGY LIMIT IS WITHIN CLEBSCH-GORDAN SERIES
                  L_MAX=MIN(L_LIM,J_BOTH+J_VAL)                       ! SET LIMIT ON L FOR ENERGY AND CLEBSCH-GORDAN RESTRICTIONS
                  DO L_VAL=L_MIN,L_MAX
                     L_ENE=L_CON*DFLOAT(L_VAL*(L_VAL+1))              ! CALCULATE L ENERGY, BL(L+1)
                     ENERGY=ARRAY+INT((L_ENE-E_LIM3)/GRID)            ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
                     LEVELS=0                                         ! START COUNTING THE M LEVELS
                     DO MJ_VAL=-J_VAL,J_VAL
                        DO M1_VAL=-J1_VAL,J1_VAL
                           LEVELS=LEVELS+MAX(MIN(J_BOTH,L_VAL+MJ_VAL,
     &                                J2_VAL+M1_VAL)+MIN(J_BOTH,L_VAL
     &                                    -MJ_VAL,J2_VAL-M1_VAL)+1,0)
                          END DO !M1
                       END DO !MJ
           NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))             ! ADD M LEVELS TO ENERGY ARRAY
                     END DO !L
                  END IF !L
               END DO !J_BOTH
            END DO !J"
         END DO !J'

C------------------------------------------------------------------------

      ELSEIF (MODE.EQ.4) THEN

C----- LINEAR-TOP SYSTEM ------------------------------------------------

      IF (K2_CON.LT.0.0) THEN                                         ! EXECUTE DIFFERENT CODE DEPENDING ON WHETHER TOP IS PROLATE OR OBLATE

      ! NOTE THAT WHEN K2_CON IS NEGATIVE, THE ROTATIONAL ENERGY EXPRESSION
      ! IS WRITTEN AS E= [ BJ(J+1)+(A-C)(J^2) ] + [ (A-C)((K^2)-(J^2)) ]
      ! THIS ALLOWS THE ENERGY TO BE GIVEN AS A BASE VALUE (THE LEFT TERM)
      ! PLUS A POSITIVE K BASED TERM (THE RIGHT TERM)

      J1_LIM=INT(DSQRT(0.25+(E_LIM1/J1_CON))-0.5)                     ! SET ENERGY LIMIT J', THE FIRST LINEAR FRAGMENT
      DO J1_VAL=0,J1_LIM
      IF (ECHO) WRITE(*,'(A,I2,A)')
     &                        ' Calculating Rotational levels (',
     &                            (100*J1_VAL/(J1_LIM+1)),'%)...'
      CALL FLUSH(6)
         J1_ENE=J1_CON*DFLOAT(J1_VAL*(J1_VAL+1))                      ! CALCULATE J' ENERGY, B'J'(J'+1)
         E_LIM2=E_LIM1-J1_ENE                                         ! CALCULATE AVAILABLE ENERGY MINUS J' ENERGY
         J2_LIM=INT(0.5*(DSQRT((J2_CON**2.0)                          ! SET ENERGY LIMIT ON J", THE TOP FRAGMENT
     &                        +(4*(J2_CON+K2_CON)*E_LIM2))
     &                              -J2_CON)/(J2_CON+K2_CON))
         DO J2_VAL=0,J2_LIM
            J2_ENE=(J2_CON*DFLOAT(J2_VAL*(J2_VAL+1)))                 ! CALCULATE J" EQUIVALENT ENERGY, B"J"(J"+1)+(A"-C")(J"^2)
     &                            +(K2_CON*DFLOAT(J2_VAL**2))
            E_LIM3=E_LIM2-J2_ENE                                      ! CALCULATE AVAILABLE ENERGY MINUS J" ENERGY
            L_LIM=INT(DSQRT(0.25+(E_LIM3/L_CON))-0.5)                 ! SET ENERGY LIMIT L, THE ORBITAL MOTION OF THE FRAGMENTS
            DO J_BOTH=ABS(J1_VAL-J2_VAL),J1_VAL+J2_VAL
               L_MIN=ABS(J_BOTH+J_VAL)                                ! SET MINIMUM L VALUE (CLEBSCH-GORDAN)
               IF (L_LIM.GE.L_MIN) THEN                               ! ONLY PROCEED IF ENERGY LIMIT IS WITHIN CLEBSCH-GORDAN SERIES
                  L_MAX=MIN(L_LIM,J_BOTH+J_VAL)                       ! SET LIMIT ON L FOR ENERGY AND CLEBSCH-GORDAN RESTRICTIONS
                  DO L_VAL=L_MIN,L_MAX
                     L_ENE=L_CON*DFLOAT(L_VAL*(L_VAL+1))              ! CALCULATE L ENERGY, BL(L+1)
                     E_LIM4=E_LIM3-L_ENE                              ! CALCULATE AVAILABLE ENERGY MINUS L ENERGY
                     LEVELS=0                                         ! START COUNTING THE M LEVELS
                     DO MJ_VAL=-J_VAL,J_VAL
                        DO M1_VAL=-J1_VAL,J1_VAL
                           LEVELS=LEVELS+MAX(MIN(J_BOTH,L_VAL+MJ_VAL,
     &                                J2_VAL+M1_VAL)+MIN(J_BOTH,L_VAL
     &                                    -MJ_VAL,J2_VAL-M1_VAL)+1,0)
                           END DO !M1
                        END DO !MJ
                     KSQUAR=DFLOAT(J2_VAL**2)+(E_LIM4/K2_CON)         ! CALCULATE MINIMUM VALUE OF K"^2 WITHIN ENERGY CONSTRAINTS (MAY BE NEGATIVE)
                     K2_MIN=INT(0.5+DSQRT(MAX(0.0,KSQUAR)))           ! SET LOWER LIMIT FOR K" QUANTUM NUMBER
                     IF (K2_MIN.LT.1) THEN                            ! DO K"=0 LEVEL IF ENERGY PERMITS
                        ENERGY=ARRAY+INT(-E_LIM4/GRID)                ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
             NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))           ! ADD M LEVELS TO ENERGY ARRAY FOR K"=0 CASE
                        END IF !K"
                     LEVELS=LEVELS*2                                  ! STORE LEVELS AS DOUBLEY DEGENERATE VALUE FOR K">0
                     DO K2_VAL=K2_MIN,J2_VAL
                        K2_ENE=K2_CON*DFLOAT((K2_VAL**2)-(J2_VAL**2)) ! CALCULATE K" EQUIVALENT ENERGY, (K"^2-J"^2)(A"-C")
                        ENERGY=ARRAY+INT((K2_ENE-E_LIM4)/GRID)        ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
             NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))           ! ADD M LEVELS TO ENERGY ARRAY FOR K">0 CASE
                        END DO !K"
                     END DO !L
                  END IF !L
               END DO !J_BOTH
            END DO !J"
         END DO !J'

      ELSE

      J1_LIM=INT(DSQRT(0.25+(E_LIM1/J1_CON))-0.5)                     ! SET ENERGY LIMIT J', THE FIRST LINEAR FRAGMENT
      DO J1_VAL=0,J1_LIM
      IF (ECHO) WRITE(*,'(A,I2,A)')
     &                        ' Calculating Rotational levels (',
     &                            (100*J1_VAL/(J1_LIM+1)),'%)...'
      CALL FLUSH(6)
         J1_ENE=J1_CON*DFLOAT(J1_VAL*(J1_VAL+1))                      ! CALCULATE J' ENERGY, B'J'(J'+1)
         E_LIM2=E_LIM1-J1_ENE                                         ! CALCULATE AVAILABLE ENERGY MINUS J' ENERGY
         J2_LIM=INT(DSQRT(0.25+(E_LIM2/J2_CON))-0.5)                  ! SET ENERGY LIMIT J", THE SECOND LINEAR FRAGMENT
         DO J2_VAL=0,J2_LIM
            J2_ENE=J2_CON*DFLOAT(J2_VAL*(J2_VAL+1))                   ! CALCULATE J" ENERGY, B"J"(J"+1)
            E_LIM3=E_LIM2-J2_ENE                                      ! CALCULATE AVAILABLE ENERGY MINUS J" ENERGY
            L_LIM=INT(DSQRT(0.25+(E_LIM3/L_CON))-0.5)                 ! SET ENERGY LIMIT L, THE ORBITAL MOTION OF THE FRAGMENTS
            DO J_BOTH=ABS(J1_VAL-J2_VAL),J1_VAL+J2_VAL
               L_MIN=ABS(J_BOTH+J_VAL)                                ! SET MINIMUM L VALUE (CLEBSCH-GORDAN)
               IF (L_LIM.GE.L_MIN) THEN                               ! ONLY PROCEED IF ENERGY LIMIT IS WITHIN CLEBSCH-GORDAN SERIES
                  L_MAX=MIN(L_LIM,J_BOTH+J_VAL)                       ! SET LIMIT ON L FOR ENERGY AND CLEBSCH-GORDAN RESTRICTIONS
                  DO L_VAL=L_MIN,L_MAX
                     L_ENE=L_CON*DFLOAT(L_VAL*(L_VAL+1))              ! CALCULATE L ENERGY, BL(L+1)
                     E_LIM4=E_LIM3-L_ENE                              ! CALCULATE AVAILABLE ENERGY MINUS L ENERGY
                     LEVELS=0                                         ! START COUNTING THE M LEVELS
                     DO MJ_VAL=-J_VAL,J_VAL
                        DO M1_VAL=-J1_VAL,J1_VAL
                           LEVELS=LEVELS+MAX(MIN(J_BOTH,L_VAL+MJ_VAL,
     &                                J2_VAL+M1_VAL)+MIN(J_BOTH,L_VAL
     &                                    -MJ_VAL,J2_VAL-M1_VAL)+1,0)
                           END DO !M1
                        END DO !MJ
                     ENERGY=ARRAY+INT(-E_LIM4/GRID)                   ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
          NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))              ! ADD M LEVELS TO ENERGY ARRAY FOR K"=0 SPECIAL CASE
                     K2_LIM=INT(DSQRT(E_LIM4/K2_CON))                 ! SET ENERGY LIMIT ON K"
                     K2_MAX=MIN(J2_VAL,K2_LIM)                        ! CALCULATE K" LIMIT WITHIN CLEBSCH-GORDAN SERIES & ENERGY
                     IF (K2_MAX.GE.1) THEN                            ! ONLY PROCEED IF J">0 AND IF ENERGY LIMITS PERMIT
                     LEVELS=LEVELS*2                                  ! STORE LEVELS AS DOUBLEY DEGENERATE VALUE FOR K">0
                        DO K2_VAL=1,K2_MAX
                           K2_ENE=K2_CON*DFLOAT(K2_VAL**2)            ! CALCULATE K" ENERGY, (K"^2)(A"-C")
                           ENERGY=ARRAY+INT((K2_ENE-E_LIM3)/GRID)     ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
          NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))              ! ADD M LEVELS TO ENERGY ARRAY FOR K">0 CASE
                           END DO !K"
                        END IF !K"
                     END DO !L
                  END IF !L
               END DO !J_BOTH
            END DO !J"
         END DO !J'


      END IF

C------------------------------------------------------------------------

      ELSEIF (MODE.EQ.5) THEN

C----- TOP-TOP SYSTEM ---------------------------------------------------

      ! IF OBLATE/PROLATE SYSTEM, FRAGMENT 2 MUST HAVE NEGATIVE
      ! A-C (K) CONSTANT. THIS IS DONE IN CONVERT SUBROUTINE

      IF (K2_CON.LT.0.0) THEN                                         ! EXECUTE DIFFERENT CODE DEPENDING ON WHETHER FRAGMENTS ARE PROLATE OR OBLATE

      ! NOTE THAT WHEN K CONST IS NEGATIVE, THE ROTATIONAL ENERGY EXPRESSION
      ! IS WRITTEN AS E= [ BJ(J+1)+(A-C)(J^2) ] + [ (A-C)((K^2)-(J^2)) ]
      ! THIS ALLOWS THE ENERGY TO BE GIVEN AS A BASE VALUE (THE LEFT TERM)
      ! PLUS A POSITIVE K BASED TERM (THE RIGHT TERM)

      IF (K1_CON.LT.0.0) THEN

      WRITE(6,*) 'OBLATE/PROLATE SYSTEM NOT AVAILABLE'
      WRITE(6,*) 'ERROR TERMINATION IN DIROTOR SECTION'
      call flush(6)
      STOP

      ELSE

      WRITE(6,*) 'DOUBLE NEGATIVE K_CON SYSTEM NOT AVAILABLE'
      WRITE(6,*) 'ERROR TERMINATION IN DIROTOR SECTION'
      call flush(6)
      STOP

      END IF

      ELSE

      IF (K1_CON.LT.0.0) THEN

      WRITE(6,*) 'INCORRECTLY ORDERED OBLATE/PROLATE SYSTEM'
      WRITE(6,*) 'ERROR TERMINATION IN DIROTOR SECTION'
      call flush(6)
      STOP

      ELSE

      J1_LIM=INT(DSQRT(0.25+(E_LIM1/J1_CON))-0.5)                     ! SET ENERGY LIMIT J', THE FIRST LINEAR FRAGMENT
      DO J1_VAL=0,J1_LIM
      IF (ECHO) WRITE(*,'(A,I2,A)')
     &                        ' Calculating Rotational levels (',
     &                            (100*J1_VAL/(J1_LIM+1)),'%)...'
      CALL FLUSH(6)
         J1_ENE=J1_CON*DFLOAT(J1_VAL*(J1_VAL+1))                      ! CALCULATE J' ENERGY, B'J'(J'+1)
         E_LIM2=E_LIM1-J1_ENE                                         ! CALCULATE AVAILABLE ENERGY MINUS J' ENERGY
         J2_LIM=INT(DSQRT(0.25+(E_LIM2/J2_CON))-0.5)                  ! SET ENERGY LIMIT J", THE SECOND LINEAR FRAGMENT
         DO J2_VAL=0,J2_LIM
            J2_ENE=J2_CON*DFLOAT(J2_VAL*(J2_VAL+1))                   ! CALCULATE J" ENERGY, B"J"(J"+1)
            E_LIM3=E_LIM2-J2_ENE                                      ! CALCULATE AVAILABLE ENERGY MINUS J" ENERGY
            L_LIM=INT(DSQRT(0.25+(E_LIM3/L_CON))-0.5)                 ! SET ENERGY LIMIT L, THE ORBITAL MOTION OF THE FRAGMENTS
            DO J_BOTH=ABS(J1_VAL-J2_VAL),J1_VAL+J2_VAL
               L_MIN=ABS(J_BOTH+J_VAL)                                ! SET MINIMUM L VALUE (CLEBSCH-GORDAN)
               IF (L_LIM.GE.L_MIN) THEN                               ! ONLY PROCEED IF ENERGY LIMIT IS WITHIN CLEBSCH-GORDAN SERIES
                  L_MAX=MIN(L_LIM,J_BOTH+J_VAL)                       ! SET LIMIT ON L FOR ENERGY AND CLEBSCH-GORDAN RESTRICTIONS
                  DO L_VAL=L_MIN,L_MAX
                     L_ENE=L_CON*DFLOAT(L_VAL*(L_VAL+1))              ! CALCULATE L ENERGY, BL(L+1)
                     E_LIM4=E_LIM3-L_ENE                              ! CALCULATE AVAILABLE ENERGY MINUS L ENERGY
                     LEVELS=0                                         ! START COUNTING THE M LEVELS
                     DO MJ_VAL=-J_VAL,J_VAL
                        DO M1_VAL=-J1_VAL,J1_VAL
                           LEVELS=LEVELS+MAX(MIN(J_BOTH,L_VAL+MJ_VAL,
     &                                J2_VAL+M1_VAL)+MIN(J_BOTH,L_VAL
     &                                    -MJ_VAL,J2_VAL-M1_VAL)+1,0)
                           END DO !M1
                        END DO !MJ
                     ENERGY=ARRAY+INT(-E_LIM4/GRID)                   ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
           NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))             ! ADD M LEVELS TO ENERGY ARRAY FOR K'=0,K"=0 SPECIAL CASE
                     K2_LIM=INT(DSQRT(E_LIM4/K2_CON))                 ! SET ENERGY LIMIT ON K"
                     K2_MAX=MIN(J2_VAL,K2_LIM)                        ! CALCULATE K" LIMIT WITHIN CLEBSCH-GORDAN SERIES & ENERGY
                     LEVELS=LEVELS*4
                     DO K2_VAL=0,K2_MAX
                        K2_ENE=K2_CON*DFLOAT(K2_VAL**2)               ! CALCULATE K" ENERGY, (K"^2)(A"-C")
                        E_LIM5=E_LIM4-K2_ENE                          ! CALCULATE AVAILABLE ENERGY MINUS K" ENERGY
                        K1_LIM=INT(DSQRT(E_LIM5/K1_CON))              ! SET ENERGY LIMIT ON K'
                        K1_MAX=MIN(J1_VAL,K1_LIM)                     ! CALCULATE K' LIMIT WITHIN CLEBSCH-GORDAN SERIES & ENERGY
                        DO K1_VAL=0,K1_MAX
                           K1_ENE=K1_CON*DFLOAT(K1_VAL**2)            ! CALCULATE K' ENERGY, (K'^2)(A'-C')
                           ENERGY=ARRAY+INT((K1_ENE-E_LIM5)/GRID)     ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
           NOS(ENERGY)=NOS(ENERGY)+(DEGEN*DFLOAT(LEVELS))             ! ADD 4 LOTS OF M LEVELS TO ENERGY ARRAY FOR ALL K',K" CASES
                           END DO !K'
                        END DO !K"
                     LEVELS=LEVELS/2
                     DO K2_VAL=0,K2_MAX
                        K2_ENE=K2_CON*DFLOAT(K2_VAL**2)               ! CALCULATE K" ENERGY, (K"^2)(A"-C")
                        ENERGY=ARRAY+INT((K2_ENE-E_LIM4)/GRID)        ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
           NOS(ENERGY)=NOS(ENERGY)-(DEGEN*DFLOAT(LEVELS))             ! REMOVE 2 LOTS OF M LEVELS TO ENERGY ARRAY FOR ALL K'=0 SPECIAL CASES
                        END DO !K": K'=0 OVERCOUNTS REMOVAL
                     K1_LIM=INT(DSQRT(E_LIM4/K1_CON))                 ! SET ENERGY LIMIT ON K' FOR K"=0 SPECIAL CASES
                     K1_MAX=MIN(J1_VAL,K1_LIM)                        ! CALCULATE K' LIMIT WITHIN CLEBSCH-GORDAN SERIES & ENERGY
                     DO K1_VAL=0,K1_MAX
                        K1_ENE=K1_CON*DFLOAT(K1_VAL**2)               ! CALCULATE K' ENERGY, (K'^2)(A'-C')
                        ENERGY=ARRAY+INT((K1_ENE-E_LIM4)/GRID)        ! CALCULATE TOTAL ENERGY AND CONVERT TO GRID BASED UNITS
           NOS(ENERGY)=NOS(ENERGY)-(DEGEN*DFLOAT(LEVELS))             ! REMOVE 2 LOTS OF M LEVELS TO ENERGY ARRAY FOR ALL K"=0 SPECIAL CASES
                        END DO !K': K"=0 OVERCOUNTS REMOVAL
                     END DO !L
                  END IF !L
               END DO !J_BOTH
            END DO !J"
         END DO !J'

      END IF

      END IF

C------------------------------------------------------------------------

      ELSE

C----- INVALID SYSTEM ---------------------------------------------------

         WRITE(6,*) 'INVALID DIROTOR MODE'
         WRITE(6,*) 'ERROR TERMINATION IN DIROTOR SECTION'
         call flush(6)
         STOP

      END IF

C============================================================================================================================
C============================================================================================================================
C=== END OF NORMAL SYSTEMS HAVING COUPLED ANGULAR MOMENTA SECTION ===========================================================
C============================================================================================================================
C============================================================================================================================

                   END IF

      RETURN
      END
