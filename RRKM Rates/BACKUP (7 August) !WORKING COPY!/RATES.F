      PROGRAM RATES

C- DEFINE ALL VARIABLES

      IMPLICIT NONE

      REAL*8 OUT_MIN,OUT_MAX,OUT_STEP              ! TEMPERATURE/ENERGY OUTPUT VARIABLES
      REAL*8 GRID                                  ! OTHER VARIABLES
      REAL*8 DOSSOS,T_RATE,P_FUNC                  ! DENSITY/SUM ARRAYS AND RATE CONSTANTS
      REAL*8 H,C,A,KB,PI                           ! PHYSICAL CONSTANTS
      REAL*8 ROTCON,VIBCON,ZERO_E,ROTFRA,ORBCON    ! GEOMETRY DATA
      REAL*8 MASS,KINCON                           ! REAGENT MASS DATA
      INTEGER GEOSTA,GEONUM,VIBNUM                 ! GEOMETRY DATA
      INTEGER SYSMAX,ELEDEG,SYMNUM,OPTISO          ! GEOMETRY DATA
      INTEGER A_EDEG,A_SNUM,A_OPTI                 ! OTHER DATA
      INTEGER MISC,J_VAL,INDEX                     ! ENERGY GRID SIZE AND OTHER VARIABLES
      INTEGER ARRAY,SYSLIM,GEOLIM,J_MAX,J_MIN      ! PROGRAM LIMITS
      LOGICAL SYSTYP                               ! SYSTEM TYPE: TRUE=TRANSITION STATE, FALSE=INTERMEDIATE
      LOGICAL USE_DI                               ! DIROTOR FLAG: TRUE=USE DIROTOR THEORY, FALSE=DONT
      LOGICAL FILETYPE                             ! OPERATING SYSTEM: TRUE=MS-DOS, FALSE=UNIX
      LOGICAL THERM                                ! CALCULATION TYPE: TRUE=THERMAL, FALSE=EJ
      LOGICAL STATES                               ! CALCULATION TYPE: TRUE=STATES, FALSE=RATES
      LOGICAL PATHS                                ! CALCULATION TYPE: TRUE=WHOLE PATH, FALSE=MINIMISED VALUES
                                                   !   OR FOR THERMAL: TRUE=P-IN-BOX, FALSE=P-IN-SPHERE
      LOGICAL VIBMODE                              ! VIBRATION METHOD: TRUE=EXACT, FALSE=BEYER-SWINEHART
      LOGICAL ROTMODE                              ! ROTOR METHOD: TRUE=BOTH, FALSE=STANDARD ONLY
      LOGICAL ECHO                                 ! PRINTOUT MODE: TRUE=LOTS, FALSE=LITTLE
      LOGICAL FINAL                                ! FLAG FOR CALCRATE ON LAST J_VAL

C- DEFINE CONSTANTS

      PARAMETER(H=6.6260755D-34,C=2.99792458D+8)
      PARAMETER(A=6.022137D+23,KB=1.380658D-23)
      PARAMETER(GRID=10.0,ARRAY=100001)
      PARAMETER(GEOLIM=1000,SYSLIM=100)
      PARAMETER(FILETYPE=.FALSE.)

C- DEFINE ARRAYS

      DIMENSION ORBCON(0:GEOLIM)        ! ORBITAL CONSTANTS, UNIQUE FOR EACH GEOMETRY
      DIMENSION ROTCON(0:GEOLIM,3)      ! ROTATIONAL CONSTANTS, UNIQUE FOR EACH GEOMETRY
      DIMENSION VIBCON(0:GEOLIM,15)     ! VIBRATIONAL CONSTANTS, UNIQUE FOR EACH GEOMETRY
      DIMENSION ZERO_E(0:GEOLIM)        ! ADJUSTED ZERO POINT ENERGY, UNIQUE FOR EACH GEOMETRY
      DIMENSION SYSTYP(0:SYSLIM)        ! TYPE OF GEOMETRY (TRANSITION STATE OR INTERMEDIATE)
      DIMENSION USE_DI(0:SYSLIM)        ! FLAG FOR WHETHER TO USE DIROTOR THEORY
      DIMENSION ROTFRA(0:SYSLIM,2,3)    ! ROTATIONAL CONSTANTS OF ASYMPTOTICALLY FREE FRAGMENTS
      DIMENSION GEOSTA(0:SYSLIM)        ! STARTING GEOMETRY FOR AN MEP
      DIMENSION GEONUM(0:SYSLIM)        ! NUMBER OF GEOMETRIES ON A MEP
      DIMENSION VIBNUM(0:SYSLIM)        ! NUMBER OF VIBRATIONAL MODES, COMMON FOR ALL POINTS ON MEP
      DIMENSION ELEDEG(0:SYSLIM)        ! ELECTRONIC DEGENERACY OF SPECIES
      DIMENSION SYMNUM(0:SYSLIM)        ! SYMMETRY NUMBER OF SPECIES
      DIMENSION OPTISO(0:SYSLIM)        ! OPTICAL ISOMERS OF SPECIES
      DIMENSION DOSSOS(0:SYSLIM,ARRAY)  ! DENSITY/SUM ARRAYS FOR EACH SYSTEM
      DIMENSION T_RATE(1000,10)         ! RATE CONSTANTS FOR A GIVEN TEMPERATURE
      DIMENSION P_FUNC(1000)            ! REAGENT PARTIION FUNCTION FOR A GIVEN TEMPERATURE
      DIMENSION A_EDEG(0:SYSLIM)        ! ASYMPTOTIC ELECTRONIC DEGENERACY
      DIMENSION A_SNUM(0:SYSLIM)        ! ASYMPTOTIC SYMMETRY NUMBER
      DIMENSION A_OPTI(0:SYSLIM)        ! ASYMPTOTIC OPTICAL ISOMERS
      DIMENSION MASS(2)                 ! REAGENT MASSES
      DIMENSION KINCON(2)               ! PARTICLE IN BOX/SPHERE CONSTANTS

      PI=4.0D+0*ATAN(1.0D+0)
      J_MIN=0

C------------------------------------------------------------------------------------------------------
C- START PROGRAM
C------------------------------------------------------------------------------------------------------

      IF (FILETYPE) THEN
c         OPEN(6,FILE=('B:\RATES.LOG'))
         ELSE
         OPEN(6,FILE=('rates.log'))
         END IF

      WRITE(6,*) 'RATES PROGRAM'
      WRITE(6,'(71(''=''))')
      WRITE(6,'(A,F7.1,A)') ' Energy precision: ',GRID,' J/mol'
      WRITE(6,'(A,F7.1,A)') '     Energy limit: ',(GRID*ARRAY/1000.0),
     &                                                         ' kJ/mol'
      WRITE(6,'(71(''=''))')


C------------------------------------------------------------------------------------------------------
C- READ IN THE DATA
C------------------------------------------------------------------------------------------------------

      CALL INPUT(OUT_MIN,OUT_MAX,OUT_STEP,USE_DI,MASS,STATES,ROTMODE,
     &                     PATHS,SYSMAX,SYSTYP,GEOSTA,VIBMODE,GEONUM,
     &                      ZERO_E,VIBNUM,VIBCON,ROTCON,THERM,ELEDEG,
     &                   SYMNUM,OPTISO,A_EDEG,A_SNUM,FILETYPE,A_OPTI,
     &                        ROTFRA,SYSLIM,GEOLIM,ORBCON,J_MAX,ECHO)

      CALL FLUSH(6)

C------------------------------------------------------------------------------------------------------
C- CONVERT DATA INTO CONSISTENT QUANTITIES
C------------------------------------------------------------------------------------------------------

      CALL CONVERT(MASS,ECHO,ZERO_E,VIBNUM,VIBCON,ROTCON,
     &            ROTFRA,ARRAY,USE_DI,GEOSTA,GRID,SYSMAX,
     &       GEONUM,H,C,A,PI,SYSLIM,GEOLIM,ORBCON,KINCON)

      CALL FLUSH(6)

C------------------------------------------------------------------------------------------------------
C- CLEAR THE THERMAL RATES ARRAY, K(T), THE PARTITION FUNCTIONS Q(T),
C------------------------------------------------------------------------------------------------------

      IF (THERM) THEN
         P_FUNC(1)=(DFLOAT(ARRAY)*GRID)-ZERO_E(0)
         P_FUNC(2)=DEXP((-P_FUNC(1))/(KB*OUT_MAX*A))

         WRITE (6,'(A)') '   BOLTZMANN DISTRIBUTION CHECK'
         WRITE (6,'(71(''~''))')
         WRITE (6,'(A,I10,A)')  '           Maximum Temperature:',
     &                                            INT(OUT_MAX),' K'
         WRITE (6,'(A,I10,A)')  ' Maximum Energy above Reagents:',
     &                              INT(P_FUNC(1)/1000.0),' kJ/mol'
         WRITE (6,'(A,E10.3,A)') '     Boltzmann Factor at Limit:',
     &                                       (P_FUNC(2)*100.0),' %'

         DO INDEX=1,1000
            P_FUNC(INDEX)=0.0D+0
            DO MISC=1,10
               T_RATE(INDEX,MISC)=0.0D+0
               END DO
            END DO

C------------------------------------------------------------------------------------------------------
C- FOR BIMOLECULAR THERMAL RATES, SEPARATELY CALCULATE UNCOUPLED
C- REAGENT DENSITY OF STATES FOR THE REAGENT PARTITION FUNCTION
C------------------------------------------------------------------------------------------------------

         IF (USE_DI(0)) J_MIN=-1

         END IF          ! THERM

C------------------------------------------------------------------------------------------------------
C- >> CALCULATE RATES <<
C------------------------------------------------------------------------------------------------------

C- LOOP OVER J_VALUES

      FINAL=(.FALSE.)

      DO J_VAL=J_MIN,J_MAX

         IF (J_VAL.GT.J_MIN) WRITE(6,'(35(''- ''),''-'')')
         IF (J_VAL.GE.0) WRITE(*,'(A,I3)')
     &                               ' Calculating states for J=',J_VAL
         IF (J_VAL.LT.0) WRITE(*,'(A)')
     &                            ' Calculating states for all J values'

         IF (J_VAL.EQ.J_MAX) FINAL=(.TRUE.)

C------------------------------------------------------------------------------------------------------
C- CREATE SUM/DENSITY OF STATES ARRAY ALL SYSTEMS
C------------------------------------------------------------------------------------------------------

         CALL LEVELS(DOSSOS,ROTCON,VIBNUM,VIBCON,ZERO_E,ROTFRA,
     &            GEOSTA,GEONUM,SYSMAX,SYSTYP,ECHO,ARRAY,J_VAL,
     &          USE_DI,GRID,ELEDEG,SYMNUM,OPTISO,A_EDEG,A_SNUM,
     &       A_OPTI,SYSLIM,GEOLIM,ORBCON,VIBMODE,ROTMODE,PATHS,KINCON)

C------------------------------------------------------------------------------------------------------
C- CALCULATE RATE CONSTANTS / SUM OF STATES / CRITICAL GEOMETRIES
C------------------------------------------------------------------------------------------------------

         WRITE(*,*) 'Calculating Rate Constants...'

         CALL OUTPUT(THERM,STATES,PATHS,OUT_MIN,OUT_MAX,
     &                     OUT_STEP,T_RATE,ARRAY,SYSLIM,
     &              SYSMAX,GRID,ZERO_E(0),P_FUNC,DOSSOS,
     &       KB,H,A,J_VAL,FILETYPE,FINAL,ECHO,USE_DI(0))

         END DO       ! LOOP OVER J_VALUES

C------------------------------------------------------------------------------------------------------
C- FINISH!
C------------------------------------------------------------------------------------------------------

      WRITE(6,'(71(''-''))')
      WRITE(*,*) 'Rates calculation complete'
      WRITE(6,'(71(''=''))')
      CALL FLUSH(6)
      CLOSE(6)

      END


