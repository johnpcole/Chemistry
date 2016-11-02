      SUBROUTINE LEVELS(DOSSOS,ROTCON,VIBNUM,VIBCON,ZERO_E,ROTFRA,
     &               GEOSTA,GEONUM,SYSMAX,SYSTYP,ECHO,ARRAY,J_VAL,
     &             USE_DI,GRID,ELEDEG,SYMNUM,OPTISO,A_EDEG,A_SNUM,
     &                A_OPTI,SYSLIM,GEOLIM,ORBCON,VIBMODE,ROTMODE,
     &                                            PATHFLAG,KINCON)

C- THIS SUBROUTINE CALCULATES THE NUMBER OF STATES ARRAY FOR
C- EACH SYSTEM IN TURN, AND CONVERTS IT TO A DENSITY OF STATES
C- IF AN INTERMEDIATE, OR A SUM OF STATES IF A TRANSITION STATE

C- DEFINE ALL VARIABLES

      IMPLICIT NONE

      REAL*8 ZERO_E,MODZPE,Z_P_E                           ! ZERO-POINT ENERGIES
      REAL*8 DOSSOS,NOS,GRID                               ! STATES ARRAYS
      REAL*8 ROTCON,ROTFRA,VIBCON,ORBCON                   ! GEOMETRY CONSTANTS
      REAL*8 J_ENE,K_ENE                                   ! REAL ROTATIONAL ENERGIES
      REAL*8 DEGEN,JDEGEN                                  ! REAL DEGENERACY FACTOR
      INTEGER F_TYPE                                       ! FLAG TO DESCRIBE DIROTOR THEORY
      INTEGER R_MODE,RUNMAX,GEOM,SYSTEM,CONST,RUNMIN       ! COUNTERS FOR DO LOOPS
      INTEGER GEOSTA,GEONUM,SYSMAX,VIBNUM,SYSMIN,SYSEND    ! MARKERS
      INTEGER ELEDEG,SYMNUM,OPTISO                         ! GEOMETRY DEGENERACY FACTORS
      INTEGER A_EDEG,A_SNUM,A_OPTI                         ! DIROTOR ASYMPTOTIC DEGENERACY FACTORS
      INTEGER ARRAY,SYSLIM,GEOLIM                          ! ARRAY LIMITS
      INTEGER J_VAL,K_VAL                                  ! ROTATIONAL QUANTUM NUMBERS
      INTEGER ENERGY                                       ! GRID BASED ENERGIES
      LOGICAL SYSTYP,USE_DI,ECHO,PATHFLAG                  ! GEOMETRY TYPE & TREATMENT
      LOGICAL VIBMODE,ROTMODE
      INTEGER VIBSTART                                     ! COUNTER TO EXCLUDE TRANSITIONAL MODES
      INTEGER V_MODE,V_ENE,V_MAX,V_VAL,V_GAP               ! B-S ALGORITHM VARIABLES
      REAL*8 KINCON,KINSTATE                               ! KINETIC ENERGY CONSTANT FOR BIMOLECULAR SYSTEM
      INTEGER T_ENE                                        ! TRANSLATION ENERGY
      REAL*8 T_UPPER,T_LOWER                               ! TRANSLATION LEVELS

C- DEFINE ARRAYS

      DIMENSION ORBCON(0:GEOLIM)            ! ORBITAL CONSTANTS, UNIQUE FOR EACH GEOMETRY
      DIMENSION ROTCON(0:GEOLIM,3)          ! ROTATIONAL CONSTANTS, UNIQUE FOR EACH GEOMETRY
      DIMENSION VIBCON(0:GEOLIM,15)         ! VIBRATIONAL CONSTANTS, UNIQUE FOR EACH GEOMETRY
      DIMENSION ZERO_E(0:GEOLIM)            ! ADJUSTED ZERO POINT ENERGY, UNIQUE FOR EACH GEOMETRY
      DIMENSION SYSTYP(0:SYSLIM)            ! TYPE OF GEOMETRY (TRANSITION STATE OR INTERMEDIATE)
      DIMENSION ROTFRA(0:SYSLIM,2,3)        ! ROTATIONAL CONSTANTS OF ASYMPTOTICALLY FREE FRAGMENTS
      DIMENSION USE_DI(0:SYSLIM)            ! DIROTOR THEORY FLAG FOR EACH MEP
      DIMENSION GEOSTA(0:SYSLIM)            ! STARTING GEOMETRY FOR AN MEP
      DIMENSION GEONUM(0:SYSLIM)            ! NUMBER OF GEOMETRIES ON A MEP
      DIMENSION VIBNUM(0:SYSLIM)            ! NUMBER OF VIBRATIONAL MODES, COMMON FOR ALL POINTS ON MEP
      DIMENSION ELEDEG(0:SYSLIM)            ! ELECTRONIC DEGENERACY
      DIMENSION SYMNUM(0:SYSLIM)            ! SYMMETRY NUMBER
      DIMENSION OPTISO(0:SYSLIM)            ! OPTICAL ISOMERS
      DIMENSION A_EDEG(0:SYSLIM)            ! ASYMPTOTIC ELECTRONIC DEGENERACY
      DIMENSION A_SNUM(0:SYSLIM)            ! ASYMPTOTIC SYMMETRY NUMBER
      DIMENSION A_OPTI(0:SYSLIM)            ! ASYMPTOTIC OPTICAL ISOMERS
      DIMENSION DOSSOS(0:SYSLIM,ARRAY)      ! DENSITY/SUM ARRAYS FOR EACH SYSTEM
      DIMENSION NOS(ARRAY)                  ! NUMBER OF STATES ARRAY
      DIMENSION KINSTATE(ARRAY)             ! KINETIC LEVELS ARRAY
      DIMENSION KINCON(2)                   ! KINETIC LEVELS CONSTANTS

C------------------------------------------------------------------------------------------------------
C- LOOP OVER SYSTEMS & GEOMETRIES, INITIALISING VARIABLES AND ARRAYS EACH TIME ------------------------
C------------------------------------------------------------------------------------------------------

C- LOOP OVER SYSTEMS

      IF (J_VAL.LT.0) THEN

         SYSMIN=0                     ! THIS IS A SPECIAL PARTICLE IN A BOX CALCULATION
         SYSEND=0                     ! FOR BIMOLECULAR REAGENTS, FOR ALL POSSIBLE Js

         ELSE

         SYSEND=SYSMAX
         IF (PATHFLAG) THEN           ! IF THIS IS A PATH CALCULATION, DON'T WASTE
            SYSMIN=1                  ! TIME CALCULATING REAGENT DENSITY OF STATES
            ELSE
            SYSMIN=0                  ! OTHERWISE, INCLUDE REAGENT DENSITY OF STATES
            END IF

         END IF

      DO SYSTEM=SYSMIN,SYSEND

C- FOR PATHS EMPLOYING DIROTOR THEORY, FLAG THE TYPE OF SYSTEM
C- ATOM-ATOM=0, ATOM-LINEAR=1, ATOM-TOP=2, LINEAR-LINEAR=3, LINEAR-TOP=4, TOP-TOP=5
C- THE NUMBERS DENOTE HOW MANY VIBRATIONS ARE TRANSITIONAL MODES
C- THIS REQUIRES FRAGMENT 1 TO BE "LOWER ORDER" THAN FRAGMENT 2

      IF (USE_DI(SYSTEM)) THEN
         IF (ROTFRA(SYSTEM,2,2).GE.0.0) THEN
            IF (ROTFRA(SYSTEM,2,1).GE.0.0) THEN
               IF (ROTFRA(SYSTEM,1,2).GE.0.0) THEN
                  IF (ROTFRA(SYSTEM,1,1).GE.0.0) THEN
                     F_TYPE=5                                  ! TOP-TOP SYSTEM
                     ELSE
                     F_TYPE=4                                  ! LINEAR-TOP SYSTEM
                     END IF
                  ELSE
                  F_TYPE=2                                     ! ATOM-TOP SYSTEM
                  END IF
               ELSE
               IF (ROTFRA(SYSTEM,1,2).GE.0.0) THEN
                  F_TYPE=3                                     ! LINEAR-LINEAR SYSTEM
                  ELSE
                  F_TYPE=1                                     ! ATOM-LINEAR SYSTEM
                  END IF
               END IF
            ELSE
            F_TYPE=0                                           ! ATOM-ATOM SYSTEM
            END IF
         END IF

C- LOOP OVER GEOMETRIES

      DO GEOM=GEOSTA(SYSTEM),(GEOSTA(SYSTEM)+GEONUM(SYSTEM)-1)

         IF (ECHO) THEN
            IF (J_VAL.GE.0) THEN
               WRITE(6,'(A,I3,A,I2,A,I3)')
     &                 ' J=',J_VAL,',   System=',SYSTEM,',   Geometry=',
     &                                           (GEOM-GEOSTA(SYSTEM)+1)
               ELSE
               WRITE(6,'(A,I2,A,I3)')
     &                 ' All J,   System=',SYSTEM,',   Geometry=',
     &                                           (GEOM-GEOSTA(SYSTEM)+1)
               END IF
            call flush(6)
            END IF



C- IF EMPLOYING DIROTOR THEORY, LOOP TWICE: THE FIRST TIME USING
C- STANDARD VIBRATION THEORY, THE SECOND USING UNHINDERED ROTORS THEORY

      RUNMIN=0                                                ! THE DEFAULT IS TO ONLY USE STANDARD MODEL
      RUNMAX=0
      MODZPE=ZERO_E(GEOM)                                     ! DEFAULT THE DIROTOR ZERO POINT ENERGY TO THE STANDARD ONE

      IF (SYSTYP(SYSTEM)) THEN                                ! FOR MINIMUM ENERGY PATHS (TRANSITION STATES)
         IF ((USE_DI(SYSTEM)).AND.(ROTMODE)) THEN             ! IF THE SYSTEM AND PROGRAM ARE BOTH DIROTOR
            DO CONST=1,F_TYPE                                 ! ENABLED, THE SMALLEST VIBRATIONAL CONSTANTS
               MODZPE=MODZPE-(0.5*VIBCON(GEOM,CONST))         ! (THE TRANSITIONAL ONES) ARE REMOVED
               END DO
            IF (MODZPE.GE.GRID) THEN
               IF (ECHO) WRITE(6,*) '      (Dirotor model will be used)'
               RUNMAX=1
               ELSE
               IF (ECHO) WRITE(*,*) '     (Dirotor model to be ignored)'
               RUNMAX=0
               END IF
            END IF

         ELSE                                                 ! FOR INTERMEDIATES (AND REAGENTS)
         IF ((USE_DI(SYSTEM)).AND.(SYSTEM.EQ.0)) THEN         ! IF THE SYSTEM IS A BIMOLECULAR REAGENT SYSTEM
            IF (ECHO) WRITE(*,*) '    (Dirotor used for reagent system)'
            RUNMIN=1
            RUNMAX=1
            END IF
         END IF


      CALL FLUSH(6)

C------------------------------------------------------------------------------------------------------
C- LOOP OVER RUN TYPE, 0 IS FOR STANDARD THEORY, 1 IS FOR DIROTOR THEORY
C------------------------------------------------------------------------------------------------------

      DO R_MODE=RUNMIN,RUNMAX

C- CLEAR THE NOS ARRAY

         DO ENERGY=1,ARRAY
            NOS(ENERGY)=0
            END DO

C- SET THE STARTING VIBRATIONAL MODE AND ZERO POINT ENERGY

      IF ((R_MODE.EQ.1).AND.(RUNMIN.EQ.0)) THEN
         VIBSTART=VIBSTART+F_TYPE                   ! THIS MISSES OUT THE TRANSITIONAL MODES FOR DIROTOR TREATMENT
         Z_P_E=MODZPE                               ! USE THE REDUCED ZERO POINT ENERGY
         ELSE
         VIBSTART=1                                 ! INCLUDE THE TRANSITIONAL MODES FOR STANDARD THEORY (AND REAGENT SYSTEMS)
         Z_P_E=ZERO_E(GEOM)                         ! USE THE STANDARD ZERO POINT ENERGY
         END IF

C------------------------------------------------------------------------------------------------------
C- CREATE THE ROTATIONAL STATES USING EITHER STANDARD OR DIROTOR THEORIES -----------------------------
C------------------------------------------------------------------------------------------------------

         IF (R_MODE.EQ.0) THEN

C- INSERT ROTATIONAL STATES USING STANDARD VIBRATION THEORY

            IF (J_VAL.LT.0) THEN
            WRITE(6,*) 'CANT CREATE ROTATIONAL LEVELS WITH NEGATIVE J'
            WRITE(6,*) 'ERROR TERMINATION IN LEVELS SECTION'
            call flush(6)
            STOP
            END IF

            DEGEN=DFLOAT(OPTISO(SYSTEM))*DFLOAT(ELEDEG(SYSTEM))
     &                                           *DFLOAT(SYMNUM(SYSTEM))
            J_ENE=(ROTCON(GEOM,2)*J_VAL*(J_VAL+1))+Z_P_E
            JDEGEN=((2.0*DFLOAT(J_VAL))+1.0)*DEGEN
            ENERGY=INT(J_ENE/GRID)
            IF (ENERGY.LE.ARRAY) THEN
               NOS(ENERGY)=NOS(ENERGY)+JDEGEN
               END IF

            IF ((ROTCON(GEOM,1).GE.0.0D+0).AND.(J_VAL.GT.0)) THEN   ! THE MOLECULE IS NON-LINEAR
               JDEGEN=JDEGEN*2.0                                    ! AND J IS GREATER THAN ZERO
               DO K_VAL=1,J_VAL
                  K_ENE=ROTCON(GEOM,3)*K_VAL*K_VAL
                  ENERGY=INT((J_ENE+K_ENE)/GRID)
                  IF (ENERGY.LE.ARRAY) THEN
                     NOS(ENERGY)=NOS(ENERGY)+JDEGEN
                     END IF
                  END DO
               END IF

            ELSE

C- INSERT ROTATIONAL STATES USING UNHINDERED DIROTOR THEORY SUBROUTINE

            DEGEN=DFLOAT(A_OPTI(SYSTEM))*DFLOAT(A_EDEG(SYSTEM))
     &                                           /DFLOAT(A_SNUM(SYSTEM))

            CALL DIROTOR(J_VAL,F_TYPE,Z_P_E,DEGEN,
     &                         ROTFRA(SYSTEM,1,2),ROTFRA(SYSTEM,1,3),
     &                         ROTFRA(SYSTEM,2,2),ROTFRA(SYSTEM,2,3),
     &                              ORBCON(GEOM),NOS,GRID,ARRAY,ECHO)

C------------------------------------------------------------------------------------------------------
C- CREATE THE TRANSLATIONAL STATES FOR BIMOLECULAR REACTION -------------------------------------------
C------------------------------------------------------------------------------------------------------

            IF (SYSTEM.EQ.0) THEN

               IF (J_VAL.GE.0) THEN             ! PARTICLE IN SPHERE

                  T_UPPER=0.0D0
                  DO ENERGY=1,ARRAY
                     DOSSOS(0,ENERGY)=NOS(ENERGY)
                     NOS(ENERGY)=0.0D0
                     T_LOWER=T_UPPER
                     T_UPPER=DSQRT(DFLOAT(ENERGY)*GRID)
                     KINSTATE(ENERGY)=KINCON(2)*(T_UPPER-T_LOWER)
                  END DO

                  ELSE                          ! PARTICLE IN BOX

                  T_UPPER=0.0D0
                  DO ENERGY=1,ARRAY
                     DOSSOS(0,ENERGY)=NOS(ENERGY)
                     NOS(ENERGY)=0.0D0
                     T_LOWER=T_UPPER
                     T_UPPER=DSQRT((DFLOAT(ENERGY)*GRID)**3.0)
                     KINSTATE(ENERGY)=KINCON(1)*(T_UPPER-T_LOWER)
                  END DO

                  END IF                        ! BOX/SPHERE

               DO ENERGY=1,ARRAY
                  IF (ECHO) THEN
                     IF (MOD(ENERGY,1000).EQ.0) WRITE(*,'(A,I2,A)')
     &                        ' Calculating Translational levels (',
     &                            (100*ENERGY/ARRAY),'%)...'
                     CALL FLUSH(6)
                     END IF !ECHO
                  IF (DOSSOS(0,ENERGY).GT.(0.0D0)) THEN
                     DO T_ENE=ENERGY,ARRAY
                        NOS(T_ENE)=NOS(T_ENE)+(DOSSOS(0,ENERGY)
     &                                        *KINSTATE(T_ENE-ENERGY+1))
                        END DO ! T_ENE
                     END IF
                  END DO ! ENERGY

               END IF ! TRANSLATIONAL STATES

            END IF    ! DIROTOR METHOD

C------------------------------------------------------------------------------------------------------
C- CREATE THE VIBRATIONAL STATES ARRAY ----------------------------------------------------------------
C------------------------------------------------------------------------------------------------------

      IF (VIBMODE) THEN

C- USE EXACT METHOD OF CALCULATING VIBRATIONAL LEVELS

         CALL VIBRATE(ARRAY,NOS,VIBSTART,SYSTEM,
     &                     VIBNUM,VIBCON,GEOLIM,
     &                   SYSLIM,GEOM,GRID,Z_P_E)

         ELSE

C- USE QUICK CONVOLUTION METHODS

         DO V_MODE=VIBSTART,VIBNUM(SYSTEM)
            V_ENE=INT(VIBCON(GEOM,V_MODE)/GRID)

            IF (V_ENE.LT.ARRAY) THEN

            IF (V_ENE.GT.0) THEN  ! THIS SETS THE SWITCH OVER VALUE

C- USE THE BEYER-SWINEHART ALGORITHM TO FOLD IN STATES

               DO ENERGY=MIN(V_ENE+1,ARRAY),ARRAY
                  NOS(ENERGY)=NOS(ENERGY)+NOS(ENERGY-V_ENE)
                  END DO ! ENERGY

               ELSE

C- USE A MODIFIED ALGORITHM FOR VERY SMALL FREQUENCIES

               DO ENERGY=ARRAY,MIN(V_ENE+1,ARRAY),-1
                  V_MAX=(DFLOAT(ENERGY)*GRID)/VIBCON(GEOM,V_MODE)
                  DO V_VAL=0,V_MAX
                     V_GAP=INT((VIBCON(GEOM,V_MODE)*V_VAL)/GRID)
                     NOS(ENERGY)=NOS(ENERGY)+NOS(ENERGY-V_GAP)
                     END DO ! V_VAL
                   END DO ! ENERGY

               END IF

               END IF

            END DO ! V_MODE

         END IF


C------------------------------------------------------------------------------------------------------
C- NOW CONVERT ARRAY TO DOS OR MINIMISED SOS ARRAYS FOR USE BY CALCRATE -------------------------------
C------------------------------------------------------------------------------------------------------

         IF (SYSTYP(SYSTEM)) THEN

C- IF THE SYSTEM IS A TRANSITION STATE, CONVERT
C- TO SUM OF STATES IN THE STATES ARRAY

            DO ENERGY=2,ARRAY
               NOS(ENERGY)=NOS(ENERGY)+NOS(ENERGY-1)
               END DO

C- IF THIS IS THE FIRST GEOMETRY & RUN FOR A SYSTEM, COPY
C- ACROSS TO THE DOSSOS ARRAY

            IF ((GEOSTA(SYSTEM).EQ.GEOM).AND.(R_MODE.EQ.0)) THEN
               DO ENERGY=1,ARRAY
                  DOSSOS(SYSTEM,ENERGY)=NOS(ENERGY)
                  END DO

               ELSE

C- OTHERWISE, COPY OVER TO DOSSOS ONLY IF THE VALUE IS LOWER

               DO ENERGY=1,ARRAY
                  DOSSOS(SYSTEM,ENERGY)
     &                        =MIN(DOSSOS(SYSTEM,ENERGY),NOS(ENERGY))
                  END DO
               END IF

            ELSE

C- IF THE SYSTEM IS AN INTERMEDIATE, CONVERT TO DENSITY OF STATES
C- COPYING DIRECTLY ACROSS TO THE DOSSOS ARRAY

            DO ENERGY=1,ARRAY
               DOSSOS(SYSTEM,ENERGY)=NOS(ENERGY)/GRID
               END DO

            END IF


C------------------------------------------------------------------------------------------------------
C------------------------------------------------------------------------------------------------------
C------------------------------------------------------------------------------------------------------


         END DO      ! GEOM     (GEOMETRY)
         END DO      ! R_MODE   (THEORY)
         END DO      ! SYSTEM   (SYSTEM)

      RETURN
      END
