      SUBROUTINE INPUT(OUT_MIN,OUT_MAX,OUT_STEP,USE_DI,MASS,STATES,
     &                  ROTMODE,PATHS,SYSMAX,SYSTYP,GEOSTA,VIBMODE,
     &                    GEONUM,ZERO_E,VIBNUM,VIBCON,ROTCON,THERM,
     &                 ELEDEG,SYMNUM,OPTISO,A_EDEG,A_SNUM,FILETYPE,
     &       A_OPTI,ROTFRA,SYSLIM,GEOLIM,ORBCON,J_MIN,J_MAX,J_STEP,ECHO)

C- THIS SUBROUTINE READS IN ALL OF THE DATA
C- NOTE: ENERGIES ARE IN HATREES
C-       VIBRATIONAL CONSTANTS ARE IN WAVENUMBERS
C-       ROTATIONAL CONSTANTS ARE IN GIGAHERTZ
C-                            (INFINITY IS REPRESENTED BY ANY NEGATIVE NUMBER)
C-       MASSES ARE IN ATOMIC MASS UNITS



C- DEFINE ALL VARIABLES

      IMPLICIT NONE

      REAL*8 OUT_MIN,OUT_MAX,OUT_STEP
      REAL*8 ROTCON,VIBCON,ZERO_E,ROTFRA,ORBCON
      REAL*8 R_ENER,R_VIBC,R_ROTC,MASS
      INTEGER GEOSTA,GEONUM,VIBNUM,SYSTEM,GEOM,R_VIBN,CONST,J_MAX
      INTEGER SYSMAX,ELEDEG,SYMNUM,OPTISO,INDEX,R_OPTI,SYSLIM,J_MIN
      INTEGER R_EDEG,R_SNUM,R_MAX,A_EDEG,A_SNUM,A_OPTI,GEOLIM,J_STEP
      CHARACTER*12 FILE,R_FILE
      CHARACTER*50 TITLE,FLAG
      LOGICAL SYSTYP,USE_DI,BI_MOL,FILETYPE
      LOGICAL THERM,VIBMODE,ROTMODE,STATES,ECHO,PATHS

C- DEFINE ARRAYS

      DIMENSION ORBCON(0:GEOLIM)        ! ORBITAL CONSTANTS, UNIQUE FOR EACH GEOMETRY
      DIMENSION ROTCON(0:GEOLIM,3)      ! ROTATIONAL CONSTANTS, UNIQUE FOR EACH GEOMETRY
      DIMENSION VIBCON(0:GEOLIM,15)     ! VIBRATIONAL CONSTANTS, UNIQUE FOR EACH GEOMETRY
      DIMENSION ZERO_E(0:GEOLIM)        ! ADJUSTED ZERO POINT ENERGY, UNIQUE FOR EACH GEOMETRY
      DIMENSION SYSTYP(0:SYSLIM)        ! TYPE OF GEOMETRY (TRANSITION,INTERMEDIATE,REACTANT)
      DIMENSION USE_DI(0:SYSLIM)        ! WHETHER TO USE DIROTOR THEORY
      DIMENSION ROTFRA(0:SYSLIM,2,3)    ! ROTATIONAL CONSTANTS OF ASYMPTOTICALLY FREE FRAGMENTS
      DIMENSION GEOSTA(0:SYSLIM)        ! STARTING GEOMETRY FOR AN MEP
      DIMENSION GEONUM(0:SYSLIM)        ! NUMBER OF GEOMETRIES ON A MEP
      DIMENSION VIBNUM(0:SYSLIM)        ! NUMBER OF VIBRATIONAL MODES, COMMON FOR ALL POINTS ON MEP
      DIMENSION ELEDEG(0:SYSLIM)        ! ELECTRONIC DEGENERACY OF SPECIES
      DIMENSION SYMNUM(0:SYSLIM)        ! SYMMETRY NUMBER OF SPECIES
      DIMENSION OPTISO(0:SYSLIM)        ! OPTICAL ISOMERS OF SPECIES
      DIMENSION FILE(SYSLIM)            ! FILE NAMES
      DIMENSION MASS(2)                 ! REAGENT MASSES
      DIMENSION R_FILE(2)               ! REAGENT FILE NAMES
      DIMENSION R_ENER(2)               ! REAGENT ENERGIES
      DIMENSION R_VIBN(2)               ! REAGENT VIBRATIONAL MODE NUMBER
      DIMENSION R_VIBC(2,15)            ! REAGENT VIBRATIONAL CONSTANTS
      DIMENSION R_ROTC(2,3)             ! REAGENT ROTATIONAL CONSTANT
      DIMENSION R_EDEG(2)               ! REAGENT ELECTRONIC DEGENERACY
      DIMENSION R_SNUM(2)               ! REAGENT SYMMETRY NUMBER
      DIMENSION R_OPTI(2)               ! REAGENT OPTICAL ISOMERS
      DIMENSION A_EDEG(0:SYSLIM)        ! ASYMPTOTIC ELECTRONIC DEGENERACY
      DIMENSION A_SNUM(0:SYSLIM)        ! ASYMPTOTIC SYMMETRY NUMBER
      DIMENSION A_OPTI(0:SYSLIM)        ! ASYMPTOTIC OPTICAL ISOMERS

C- OPEN MASTER DATA FILE AND INPUT.LOG

      IF (FILETYPE) THEN
c         OPEN(11,FILE=('B:\INPUT.TXT'),STATUS='OLD')
         ELSE
         OPEN(11,FILE=('input.txt'))
         END IF

      READ(11,*) TITLE

C- READ IN PRINTOUT MODE

      READ(11,*) FLAG
      IF ((FLAG(1:1).EQ.'V').OR.(FLAG(1:1).EQ.'v')) THEN
         ECHO=.TRUE.
      ELSEIF ((FLAG(1:1).EQ.'B').OR.(FLAG(1:1).EQ.'b')) THEN
         ECHO=.FALSE.
      ELSE
         WRITE(6,*) 'PRINTOUT MODE INVALID'
         WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
         call flush(6)
         STOP
      END IF

C- READ IN VIBRATIONAL METHOD

      READ(11,*) FLAG
      IF ((FLAG(1:1).EQ.'E').OR.(FLAG(1:1).EQ.'e')) THEN
         VIBMODE=.TRUE.
      ELSEIF ((FLAG(1:1).EQ.'Q').OR.(FLAG(1:1).EQ.'q')) THEN
         VIBMODE=.FALSE.
      ELSE
         WRITE(6,*) 'VIBRATIONAL METHOD INVALID'
         WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
         call flush(6)
         STOP
      END IF

C- READ IN ROTATIONAL METHOD

      READ(11,*) FLAG
      IF ((FLAG(1:1).EQ.'S').OR.(FLAG(1:1).EQ.'s')) THEN
         ROTMODE=.FALSE.
      ELSEIF ((FLAG(1:1).EQ.'D').OR.(FLAG(1:1).EQ.'d')) THEN
         ROTMODE=.TRUE.
      ELSE
         WRITE(6,*) 'ROTATIONAL METHOD INVALID'
         WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
         call flush(6)
         STOP
      END IF

C- CHECK FOR THERMAL OR MICROCANONICAL TREATMENT

      READ(11,*) FLAG
      IF ((FLAG(1:1).EQ.'T').OR.(FLAG(1:1).EQ.'t')) THEN
         THERM=.TRUE.
         STATES=.FALSE.
         PATHS=.FALSE.
      ELSEIF ((FLAG(1:1).EQ.'A').OR.(FLAG(1:1).EQ.'a')) THEN
         THERM=.TRUE.
         STATES=.TRUE.
         PATHS=.FALSE.
      ELSEIF ((FLAG(1:1).EQ.'M').OR.(FLAG(1:1).EQ.'m')) THEN
         THERM=.FALSE.
         STATES=.FALSE.
         PATHS=.FALSE.
      ELSEIF ((FLAG(1:1).EQ.'S').OR.(FLAG(1:1).EQ.'s')) THEN
         THERM=.FALSE.
         STATES=.TRUE.
         PATHS=.FALSE.
      ELSEIF ((FLAG(1:1).EQ.'P').OR.(FLAG(1:1).EQ.'p')) THEN
         THERM=.FALSE.
         STATES=.TRUE.
         PATHS=.TRUE.
      ELSE
         WRITE(6,*)
     &               'TYPE OF CALCULATION INVALID ',
     &               '(THERMAL/MICROCANONICAL/ALLJ/STATES/PATH)'
         WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
         call flush(6)
         STOP
      END IF

C- READ IN TEMPERATURE OR ENERGY RANGE

      READ(11,*) OUT_MIN,OUT_MAX,OUT_STEP

      IF ((THERM).AND.(.NOT.(STATES))) THEN

      IF ((OUT_MIN.GT.OUT_MAX).OR.(MIN(OUT_MIN,OUT_STEP).LT.0.0)) THEN
         WRITE(6,*) 'TEMPERATURE RANGE INVALID'
         WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
         call flush(6)
         STOP
         END IF
      IF (OUT_STEP.EQ.0.0) THEN
         INDEX=1
         ELSE
         INDEX=INT((OUT_MAX-OUT_MIN)/OUT_STEP)+1
         IF (INDEX.GT.1000) THEN
            WRITE(6,*) 'TOO MANY TEMPERATURES'
            WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
            call flush(6)
            STOP
            END IF
         END IF
      OUT_MAX=OUT_MIN+(OUT_STEP*DFLOAT(INDEX-1))

      ELSE

      IF ((OUT_MIN.GT.OUT_MAX).OR.(OUT_STEP.LE.0.0)) THEN
         WRITE(6,*) 'ENERGY RANGE INVALID'
         WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
         call flush(6)
         STOP
         END IF

      END IF

C- READ IN J_RANGE

      IF (THERM) THEN

         J_MIN=0
         J_STEP=1
         READ(11,*) J_MAX
         IF (J_MAX.LT.0) THEN
            WRITE(6,*) 'J LIMIT INVALID'
            WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
            call flush(6)
            STOP
            END IF

         ELSE

         READ(11,*) J_MIN,J_MAX,J_STEP
         IF ((MIN(J_MAX,J_MIN,J_STEP).LT.0).OR.(J_MAX.LT.J_MIN)) THEN
            WRITE(6,*) 'J RANGE INVALID'
            WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
            call flush(6)
            STOP
            END IF

         END IF

C- READ IN STRING WHICH DETERMINES UNI/BI-MOLECULAR REACTION

      READ(11,*) FLAG
      IF ((FLAG(1:1).EQ.'U').OR.(FLAG(1:1).EQ.'u')) THEN
         BI_MOL=.FALSE.
         R_MAX=1
      ELSEIF ((FLAG(1:1).EQ.'B').OR.(FLAG(1:1).EQ.'b')) THEN
         BI_MOL=.TRUE.
         R_MAX=2
         IF (THERM) J_MIN=-1  ! DO THE SPECIAL ALL-J DENSITY OF STATES FOR THE REAGENTS
      ELSE
         WRITE(6,*) 'TYPE OF REACTION INVALID (NUMBER OF REAGENTS)'
         WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
         call flush(6)
         STOP
      END IF

C- READ IN REAGENT DATAFILE NAMES

      DO SYSTEM=1,R_MAX
         READ(11,*) R_FILE(SYSTEM)
         END DO

C- READ IN NUMBER OF GEOMETRY FILES

      READ(11,*) SYSMAX
      IF ((SYSMAX.GT.SYSLIM).OR.(SYSMAX.LT.1)) THEN
         WRITE(6,*) 'NUMBER OF SYSTEMS INVALID'
         WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
         call flush(6)
         STOP
         END IF

C- READ IN DATA FILE NAMES

      DO SYSTEM=1,SYSMAX
         READ(11,*) FILE(SYSTEM)
         END DO

C- CLOSE MASTER DATA FILE

      CLOSE(11)

C-------------------------------------------------------------------------------------
C- PRINT MASTER DATA OUT AS CHECK

      WRITE(6,'(A,A)') ' Reaction scheme title: ',TITLE

      IF (BI_MOL) THEN
         FLAG='Bimolecular Collision'
         ELSE
         FLAG='Unimolecular Reaction'
         END IF
      WRITE(6,'(A,I2,A)')
     &                               ' A '//FLAG(1:21)//' involving a ',
     &                             (SYSMAX+1),' system reaction scheme'

      IF (THERM) THEN
         IF (STATES) THEN
            WRITE(6,'(A,A)')   ' This is a microcanonical calculation,',
     &                                              ' summed over all J'
            ELSE
            WRITE(6,'(A)')              ' This is a thermal calculation'
            END IF
         ELSE
         IF (STATES) THEN
            WRITE (6,'(A)')
     &                    ' This is a density/sum of states calculation'
            IF (PATHS) THEN
               WRITE (6,'(A)')
     &               ' (Sum of states of each geometry will be printed)'
               ELSE
               WRITE (6,'(A)')
     &         ' (Minimised sum of states of each path will be printed)'
               END IF
            ELSE
            WRITE (6,'(A)')      ' This is a microcanonical calculation'
            END IF
         END IF

      IF ((THERM).AND.(.NOT.(STATES))) THEN
         WRITE(6,'(A,I4,A,I4,A,I3,A,I4,A)')
     &                         '  Temperature range: ',INT(OUT_MIN),
     &                          ' K to ',INT(OUT_MAX),' K, in ',INDEX,
     &                                 ' steps of ',INT(OUT_STEP),' K'
         ELSE

         WRITE(6,'(A,I4,A,I4,A)')
     &                      '       Energy range: ',INT(OUT_MIN/1000.0),
     &                      ' kJ/mol to ',INT(OUT_MAX/1000.0),' kJ/mol,'
         WRITE(6,'(A,A,F7.3,A)')
     &                            '                                   ',
     &                          'in steps of ',(OUT_STEP/1000),' kJ/mol'
         END IF

      WRITE(6,'(A,I3,A,I3,A,I3)')  '            J range: ',MAX(J_MIN,0),
     &                               ' to ',J_MAX,' in steps of ',J_STEP

      IF (VIBMODE) THEN
         WRITE(6,'(A)') ' (Exact calculation of vibrational levels)'
         ELSE
         WRITE(6,'(A)')
     &            ' (Beyer-Swinehart calculation of vibrational levels)'
         END IF 

      IF (ROTMODE) THEN
         WRITE(6,'(A)')
     &       ' (Dirotor treatment of transitional modes where required)'
         ELSE
         WRITE(6,'(A)')
     &                ' (Standard treatment of transitional modes only)'
         END IF      


      WRITE(6,'(71(''-''))')
C-------------------------------------------------------------------------------------

C- READ IN REAGENT DATA FILES

      DO SYSTEM=1,R_MAX

         IF (FILETYPE) THEN
c            OPEN(33,FILE=('B:\'//R_FILE(SYSTEM)),STATUS='OLD')
            ELSE
            OPEN(33,FILE=R_FILE(SYSTEM))
            END IF
         READ(33,*) TITLE

C- READ IN REAGENT MASS IN AMUS

         READ(33,*) MASS(SYSTEM)
         IF (MASS(SYSTEM).LE.0.0) THEN
            WRITE(6,*) 'INVALID REAGENT MASS'
            WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
            call flush(6)
            STOP
            END IF

C- READ IN THE NUMBER OF REAGENT VIBRATIONAL MODES

         READ(33,*) R_VIBN(SYSTEM)
         IF ((R_VIBN(SYSTEM).LT.0).OR.(R_VIBN(SYSTEM).GT.15)) THEN
            WRITE(6,*) 'INVALID NUMBER OF VIBRATIONAL MODES'
            WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
            call flush(6)
            STOP
            END IF

C- READ IN REAGENT DEGENERACIES

         READ(33,*) R_EDEG(SYSTEM),R_SNUM(SYSTEM),R_OPTI(SYSTEM)
         IF (((R_EDEG(SYSTEM).LE.0).OR.(R_SNUM(SYSTEM).LE.0)).OR.
     &                                       (R_OPTI(SYSTEM).LE.0)) THEN
            WRITE(6,*) 'INVALID DEGENERACY/SYMMETRY VALUE'
            WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
            call flush(6)
            STOP
            END IF

C- READ IN REAGENT ELECTRONIC ENERGIES

         READ(33,*) R_ENER(SYSTEM)

C- READ IN REAGENT ROTATIONAL CONSTANTS

         READ(33,*) (R_ROTC(SYSTEM,CONST),CONST=1,3)

C- READ IN REAGENT VIBRATIONAL CONSTANTS

         IF (R_VIBN(SYSTEM).GT.0) THEN
            DO INDEX=1,INT((R_VIBN(SYSTEM)+4)/5)
               READ(33,*) (R_VIBC(SYSTEM,CONST),CONST=
     &              (((INDEX-1)*5)+1),MIN(R_VIBN(SYSTEM),(INDEX*5)))
               END DO
            DO CONST=1,R_VIBN(SYSTEM)
               IF (R_VIBC(SYSTEM,CONST).LE.0.0) THEN
                  WRITE(6,*) 'INVALID VIBRATIONAL CONSTANT'
                  WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
                  call flush(6)
                  STOP
                  END IF
               END DO
            END IF

C-------------------------------------------------------------------------------------
C- PRINT REAGENT DATA OUT AS CHECK

         WRITE(6,'(A,I1,A,A)')
     &                        ' Reagent ',SYSTEM,' title: ',TITLE
         WRITE(6,'(A,I3,A,F10.5,A)')
     &                                  ' Mass: ',INT(MASS(SYSTEM)+0.5),
     &       ' Amus,     Electronic energy: ',R_ENER(SYSTEM),' Hartrees'
         WRITE(6,'(A,I1,A,I2,A,I2)')
     &                                   ' Elec degen: ',R_EDEG(SYSTEM),
     &                             ',        Symm num: ',R_SNUM(SYSTEM),
     &                           ',        Opt isomers: ',R_OPTI(SYSTEM)
         WRITE(6,'(A,3(F6.2,'' GHz   ''))')
     &       ' Rotational constants: ',(R_ROTC(SYSTEM,CONST),CONST=1,3)
         IF (R_VIBN(SYSTEM).EQ.0) THEN
            WRITE(6,*) 'No vibrational modes (Reagent is an atom)'
            ELSE
            WRITE(6,'(I3,A,3(I4,'' cm-1    ''))')
     &                           R_VIBN(SYSTEM),' vibrational modes:  ',
     &         (INT(R_VIBC(SYSTEM,CONST)),CONST=1,MIN(3,R_VIBN(SYSTEM)))
            IF (R_VIBN(SYSTEM).GT.3) THEN
            WRITE(6,'(11('' ''),4(I4,'' cm-1    ''))')
     &         (INT(R_VIBC(SYSTEM,CONST)),CONST=4,MIN(7,R_VIBN(SYSTEM)))
            IF (R_VIBN(SYSTEM).GT.7) THEN
            WRITE(6,'(11('' ''),4(I4,'' cm-1    ''))')
     &        (INT(R_VIBC(SYSTEM,CONST)),CONST=8,MIN(11,R_VIBN(SYSTEM)))
            IF (R_VIBN(SYSTEM).GT.11) THEN
            WRITE(6,'(11('' ''),4(I4,'' cm-1    ''))')
     &         (INT(R_VIBC(SYSTEM,CONST)),CONST=12,R_VIBN(SYSTEM))
            END IF
            END IF
            END IF
            END IF
         WRITE(6,'(71(''-''))')
C-------------------------------------------------------------------------------------

C- LOOP OVER REAGENTS

         END DO

C- CONVERT REAGENT DATA INTO STANDARD FORMAT

      GEOSTA(0)=0                                                ! REAGENT GEOMETRY INDEX
      GEONUM(0)=1                                                ! ONE GEOMETRY REQUIRED
      SYSTYP(0)=(.FALSE.)                                        ! SYSTEM BEHAVES LIKE AN INTERMEDIATE
      ORBCON(0)=-999.9                                           ! If bimolecular, this value will be set in convert
      ZERO_E(0)=R_ENER(1)                                        ! READ IN REAGENT ENERGY
      VIBNUM(0)=R_VIBN(1)                                        ! READ IN NUMBER OF MODES
      IF (R_VIBN(1).GT.0) THEN                                   ! READ IN VIBRATIONAL CONSTANTS
         DO CONST=1,R_VIBN(1)
            VIBCON(0,CONST)=R_VIBC(1,CONST)
            END DO
         END IF

      IF (BI_MOL) THEN

         USE_DI(0)=(.TRUE.)
         ZERO_E(0)=ZERO_E(0)+R_ENER(2)                           ! READ IN MORE REAGENT ENERGY
         DO CONST=1,3                                            ! SET UNIMOLECULAR ROTATIONAL
            ROTCON(0,CONST)=-99.99                               ! CONSTANTS TO INFINITY
            END DO
         A_OPTI(0)=R_OPTI(1)*R_OPTI(2)                           ! READ IN BOTH SETS OF DEGENERACY
         A_SNUM(0)=R_SNUM(1)*R_SNUM(2)                           ! VALUES INTO ASYMPTOTIC DATA
         A_EDEG(0)=R_EDEG(1)*R_EDEG(2)
         DO SYSTEM=1,2                                           ! READ BIMOLECULAR ROTATIONAL
            DO CONST=1,3                                         ! CONSTANTS INTO FRAGMENT DATA
               ROTFRA(0,SYSTEM,CONST)=R_ROTC(SYSTEM,CONST)
               END DO
            END DO

         VIBNUM(0)=VIBNUM(0)+R_VIBN(2)                           ! READ IN MORE NUMBER OF MODES
         IF ((VIBNUM(0).GT.15).OR.(VIBNUM(0).LT.1)) THEN
            WRITE(6,*) 'INVALID NUMBER OF REAGENT VIBRATIONAL MODES'
            WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
            call flush(6)
            STOP
            END IF
         IF (R_VIBN(2).GT.0) THEN                                ! READ IN MORE VIBRATIONAL CONSTANTS
            DO CONST=1,R_VIBN(2)
               VIBCON(0,CONST+R_VIBN(1))=R_VIBC(2,CONST)
               END DO
            END IF

         ELSE

         USE_DI(0)=(.FALSE.)
         DO CONST=1,3                                            ! READ UNIMOLECULAR ROTATIONAL
            ROTCON(0,CONST)=R_ROTC(1,CONST)                      ! CONSTANTS INTO SYSTEM DATA
            END DO
         ELEDEG(0)=R_EDEG(1)                                     ! READ IN DEGENERACY VALUES
         OPTISO(0)=R_OPTI(1)                                     ! INTO SYSTEM DATA
         SYMNUM(0)=R_SNUM(1)

         END IF

C-------------------------------------------------------------------------------------

C- READ IN INTERMEDIATE/TRANSITION STATE DATA FILES

      DO SYSTEM=1,SYSMAX

         IF (FILETYPE) THEN
c            OPEN(33,FILE=('B:\'//FILE(SYSTEM)),STATUS='OLD')
            ELSE
            OPEN(33,FILE=FILE(SYSTEM))
            END IF

         READ(33,*) TITLE

C- CHECK WHAT SORT OF SYSTEM IT IS

         READ(33,*) FLAG
         IF ((FLAG(1:1).EQ.'T').OR.(FLAG(1:1).EQ.'t')) THEN
            SYSTYP(SYSTEM)=(.TRUE.)
            ELSEIF ((FLAG(1:1).EQ.'I').OR.(FLAG(1:1).EQ.'i')) THEN
            SYSTYP(SYSTEM)=(.FALSE.)
            ELSE
            WRITE(6,*) 'SYSTEM TYPE INVALID'
            WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
            call flush(6)
            STOP
            END IF

C- READ IN THE NUMBER OF VIBRATIONAL MODES

         READ(33,*) VIBNUM(SYSTEM)
         IF ((VIBNUM(SYSTEM).LT.1).OR.(VIBNUM(SYSTEM).GT.15)) THEN
            WRITE(6,*) 'INVALID NUMBER OF VIBRATIONAL MODES'
            WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
            call flush(6)
            STOP
            END IF

C- READ IN DEGENERACIES

         READ(33,*) ELEDEG(SYSTEM),SYMNUM(SYSTEM),OPTISO(SYSTEM)
         IF (((ELEDEG(SYSTEM).LE.0).OR.(SYMNUM(SYSTEM).LE.0))
     &                                  .OR.(OPTISO(SYSTEM).LE.0)) THEN
            WRITE(6,*) 'INVALID DEGENERACY/SYMMETRY VALUE'
            WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
            call flush(6)
            STOP
            END IF

C- IF SYSTEM IS A TRANSITION STATE, READ IN WHICH MODEL TO USE
C- (STANDARD OR DIROTOR). THEN READ IN FRAGMENT ROTATIONAL
C- CONSTANTS AND THE MODIFIED SYMMETRY NUMBERS IF DIROTOR
C- MODEL IS TO BE USED. FINALLY READ IN THE NUMBER OF GEOMETRIES.

         USE_DI(SYSTEM)=.FALSE.
         IF (SYSTYP(SYSTEM)) THEN
            READ(33,*) FLAG
            IF ((FLAG(1:1).EQ.'D').OR.(FLAG(1:1).EQ.'d')) THEN
               USE_DI(SYSTEM)=.TRUE.
               READ(33,*) (ROTFRA(SYSTEM,1,CONST),CONST=1,3)
               READ(33,*) (ROTFRA(SYSTEM,2,CONST),CONST=1,3)
               READ(33,*) A_EDEG(SYSTEM),A_SNUM(SYSTEM),A_OPTI(SYSTEM)
               IF (((A_EDEG(SYSTEM).LE.0).OR.(A_SNUM(SYSTEM).LE.0))
     &                                   .OR.(A_OPTI(SYSTEM).LE.0)) THEN
                  WRITE(6,*) 'INVALID DEGENERACY/SYMMETRY VALUE'
                  WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
                  call flush(6)
                  STOP
                  END IF
               END IF
            READ(33,*) GEONUM(SYSTEM)
               IF (GEONUM(SYSTEM).LT.1) THEN
                  WRITE(6,*) 'INVALID NUMBER OF GEOMETRIES'
                  WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
                  call flush(6)
                  STOP
               END IF
            ELSE
            GEONUM(SYSTEM)=1
            END IF

C-------------------------------------------------------------------------------------
C- PRINT OVERALL SYSTEM DATA OUT AS CHECK

         WRITE(6,'(A,I2,A,A)')
     &                        ' System ',SYSTEM,' title: ',TITLE
         IF (SYSTYP(SYSTEM)) THEN
            IF (GEONUM(SYSTEM).EQ.1) THEN
               WRITE(6,*) 'This is a transition state (Single point)'
               ELSE
               WRITE(6,'(A,I3,A)')
     &         ' This is a ',GEONUM(SYSTEM),' point minimum energy path'
               END IF
            ELSE
            WRITE(6,*) 'This is an intermediate'
            END IF
            WRITE(6,'(A,I1,A,I2,A,I2)')
     &                                   ' Elec degen: ',ELEDEG(SYSTEM),
     &                             ',        Symm num: ',SYMNUM(SYSTEM),
     &                           ',        Opt isomers: ',OPTISO(SYSTEM)

         IF (USE_DI(SYSTEM)) THEN
            WRITE(6,*) 'This pathway will employ the dirotor model.....'
            WRITE(6,'(A,3(F6.2,'' GHz   ''))')
     &                        ' Fragment 1 constants: ',
     &                               (ROTFRA(SYSTEM,1,CONST),CONST=1,3)
            WRITE(6,'(A,3(F6.2,'' GHz   ''))')
     &                        ' Fragment 2 constants: ',
     &                               (ROTFRA(SYSTEM,2,CONST),CONST=1,3)
            WRITE(6,'(A,I1,A,I2,A,I2)')
     &                                   ' Elec degen: ',A_EDEG(SYSTEM),
     &                             ',        Symm num: ',A_SNUM(SYSTEM),
     &                           ',        Opt isomers: ',A_OPTI(SYSTEM)
            END IF

C-------------------------------------------------------------------------------------

C- SET THE STARTING POINT GEOMETRY INDEX

         GEOSTA(SYSTEM)=GEOSTA(SYSTEM-1)+GEONUM(SYSTEM-1)

C- NOW READ IN THE GEOMETRY(S)

         DO GEOM=GEOSTA(SYSTEM),(GEOSTA(SYSTEM)+GEONUM(SYSTEM)-1)
            IF (GEOM.GT.GEOLIM) THEN
               WRITE(6,*) 'TOO MANY GEOMETRIES'
               WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
               call flush(6)
               STOP
               END IF

C--------------ELECTRONIC ENERGY

            READ(33,*) ZERO_E(GEOM)

C--------------ROTATIONAL CONSTANTS

            IF (USE_DI(SYSTEM)) THEN
               READ(33,*) (ROTCON(GEOM,CONST),CONST=1,3),ORBCON(GEOM)
               ELSE
               READ(33,*) (ROTCON(GEOM,CONST),CONST=1,3)
               END IF

C--------------VIBRATIONAL CONSTANTS

            DO INDEX=1,INT((VIBNUM(SYSTEM)+4)/5)
               READ(33,*) (VIBCON(GEOM,CONST),CONST=
     &                 (((INDEX-1)*5)+1),MIN(VIBNUM(SYSTEM),(INDEX*5)))
               END DO
            DO CONST=1,VIBNUM(SYSTEM)
               IF (VIBCON(GEOM,CONST).LE.0.0) THEN
                  WRITE(6,*) 'INVALID VIBRATIONAL CONSTANT'
                  WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
                  call flush(6)
                  STOP
                  END IF
               END DO

C-------------------------------------------------------------------------------------
C- PRINT INDIVIDUAL GEOMETRY DATA OUT AS CHECK

         IF (GEONUM(SYSTEM).GT.1) THEN
            WRITE(6,'(35(''- ''),''-'')')
            WRITE(6,'(A,I3,A,F10.5,A)')
     &                             ' Geometry ',(GEOM-GEOSTA(SYSTEM)+1),
     &            ',       Electronic energy: ',ZERO_E(GEOM),' Hartrees'       
            ELSE
            WRITE(6,'(A,F10.5,A)')
     &                   ' Electronic energy: ',ZERO_E(GEOM),' Hartrees'
            END IF
         WRITE(6,'(A,3(F6.2,'' GHz   ''))')
     &          ' Rotational constants: ',(ROTCON(GEOM,CONST),CONST=1,3)
         WRITE(6,'(I3,A,3(I4,'' cm-1    ''))')
     &                           VIBNUM(SYSTEM),' vibrational modes:  ',
     &           (INT(VIBCON(GEOM,CONST)),CONST=1,MIN(3,VIBNUM(SYSTEM)))
         IF (VIBNUM(SYSTEM).GT.3) THEN
            WRITE(6,'(11('' ''),4(I4,'' cm-1    ''))')
     &           (INT(VIBCON(GEOM,CONST)),CONST=4,MIN(7,VIBNUM(SYSTEM)))
            IF (VIBNUM(SYSTEM).GT.7) THEN
            WRITE(6,'(11('' ''),4(I4,'' cm-1    ''))')
     &          (INT(VIBCON(GEOM,CONST)),CONST=8,MIN(11,VIBNUM(SYSTEM)))
            IF (VIBNUM(SYSTEM).GT.11) THEN
            WRITE(6,'(11('' ''),4(I4,'' cm-1    ''))')
     &                 (INT(VIBCON(GEOM,CONST)),CONST=12,VIBNUM(SYSTEM))
            END IF
            END IF
            END IF
         IF (USE_DI(SYSTEM)) WRITE(6,'(A,F6.2,'' GHz   '')')
     &          '     Orbital constant: ',ORBCON(GEOM)

C-------------------------------------------------------------------------------------

            END DO               ! END OF GEOM LOOP  (GEOMETRIES)

C- CLOSE DATA FILE

         CLOSE(33)
         WRITE(6,'(71(''-''))')

         END DO                  ! END OF SYSTEM LOOP (SYSTEMS)


C------------------------------------------------------------------------------------------------------
C- FOR PATH CALCULATION, CONVERT EACH GEOMETRY INTO A SEPARATE SYSTEM
C------------------------------------------------------------------------------------------------------

      IF (PATHS) THEN

         IF (SYSMAX.GT.1) THEN
            WRITE(6,*) 'ONLY ONE SYSTEM ALLOWED FOR PATH CALCULATION'
            WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
            call flush(6)
            STOP
            END IF
         IF (.NOT.(SYSTYP(1))) THEN
            WRITE(6,*) 'WRONG TYPE OF SYSTEM FOR PATH CALCULATION'
            WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
            call flush(6)
            STOP
            END IF

         IF (GEONUM(1).GT.1) THEN
            SYSMAX=GEONUM(1)
            IF (SYSMAX.GT.SYSLIM) THEN
               WRITE(6,*) 'NUMBER OF SYSTEMS INVALID (PATH CONVERSION)'
               WRITE(6,*) 'ERROR TERMINATION IN INPUT SECTION'
               call flush(6)
               STOP
               END IF
            DO GEOM=2,SYSMAX
               GEOSTA(GEOM)=GEOM
               GEONUM(GEOM)=1
               SYSTYP(GEOM)=SYSTYP(1)
               VIBNUM(GEOM)=VIBNUM(1)
               ELEDEG(GEOM)=ELEDEG(1)
               SYMNUM(GEOM)=SYMNUM(1)
               OPTISO(GEOM)=OPTISO(1)
               USE_DI(GEOM)=USE_DI(1)
               ROTFRA(GEOM,1,1)=ROTFRA(1,1,1)
               ROTFRA(GEOM,1,2)=ROTFRA(1,1,2)
               ROTFRA(GEOM,1,3)=ROTFRA(1,1,3)
               ROTFRA(GEOM,2,1)=ROTFRA(1,2,1)
               ROTFRA(GEOM,2,2)=ROTFRA(1,2,2)
               ROTFRA(GEOM,2,3)=ROTFRA(1,2,3)
               A_EDEG(GEOM)=A_EDEG(1)
               A_SNUM(GEOM)=A_SNUM(1)
               A_OPTI(GEOM)=A_OPTI(1)
               END DO ! GEOM
            GEONUM(1)=1
            END IF

         END IF

      WRITE(6,*) 'Finished reading data in'
      WRITE(6,'(71(''=''))')

C-------------------------------------------------------------------------------------

      RETURN
      END       
