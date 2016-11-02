      SUBROUTINE CONVERT(MASS,ECHO,ZERO_E,VIBNUM,VIBCON,ROTCON,
     &                  ROTFRA,ARRAY,USE_DI,GEOSTA,GRID,SYSMAX,
     &             GEONUM,H,C,A,PI,SYSLIM,GEOLIM,ORBCON,KINCON)



C- THIS SUBROUTINE READS CONVERTS ALL DATA INTO JOULES PER MOLE
C- CALCULATES THE ZERO-POINT ENERGIES, SHIFTS THEM TO A RELATIVE ZERO,
C- AND THEN CALCULATES THE GEOMETRIC MEAN OF THE INACTIVE MOMENTS OF
C- INERTIAS AS WELL. CALCULATES THE REDUCED MASS.

      IMPLICIT NONE
      
      REAL*8 H,C,A,PI,BASE_E,FACTOR,RED_MASS
      REAL*8 ROTCON,VIBCON,MASS,ZERO_E,ROTFRA,GRID,ORBCON,KINCON
      REAL*8 LARGE,MEDIUM,SMALL
      INTEGER GEONUM,SYSMAX,COUNT,SYSTEM
      INTEGER GEOSTA,GEOM,CONST,VIBNUM,ARRAY,F_TYPE,SYSLIM,GEOLIM
      LOGICAL USE_DI,ECHO

      DIMENSION ORBCON(0:GEOLIM)        ! ORBITAL CONSTANTS, UNIQUE FOR EACH GEOMETRY
      DIMENSION ROTCON(0:GEOLIM,3)      ! ROTATIONAL CONSTANTS, UNIQUE FOR EACH GEOMETRY
      DIMENSION VIBCON(0:GEOLIM,15)     ! VIBRATIONAL CONSTANTS, UNIQUE FOR EACH GEOMETRY
      DIMENSION ZERO_E(0:GEOLIM)        ! ADJUSTED ZERO POINT ENERGY, UNIQUE FOR EACH GEOMETRY
      DIMENSION ROTFRA(0:SYSLIM,2,3)    ! ROTATIONAL CONSTANTS OF ASYMPTOTICALLY FREE FRAGMENTS
      DIMENSION GEONUM(0:SYSLIM)        ! NUMBER OF GEOMETRIES ON A MEP
      DIMENSION GEOSTA(0:SYSLIM)        ! GEOMETRY STARTING INDEX
      DIMENSION VIBNUM(0:SYSLIM)        ! NUMBER OF VIBRATIONAL MODES, COMMON FOR ALL POINTS ON MEP
      DIMENSION F_TYPE(2)
      DIMENSION USE_DI(0:SYSLIM)
      DIMENSION MASS(2)
      DIMENSION KINCON(2)

C==============================================================================
C                  VIBRATIONAL CONSTANTS
C==============================================================================

C- SORT VIBRATIONAL CONSTANTS INTO ASCENDING ORDER USING THE RATHER CRAP BUBBLE SORT
C   REAGENTS/INTERMEDIATES/TRANSITION STATES
      DO SYSTEM=0,SYSMAX
         DO GEOM=GEOSTA(SYSTEM),(GEOSTA(SYSTEM)+GEONUM(SYSTEM)-1)
            DO COUNT=1,(VIBNUM(SYSTEM)-1)
               DO CONST=1,(VIBNUM(SYSTEM)-1)
                  IF (VIBCON(GEOM,CONST).GT.VIBCON(GEOM,CONST+1)) THEN
                     FACTOR=VIBCON(GEOM,CONST+1)
                     VIBCON(GEOM,CONST+1)=VIBCON(GEOM,CONST)
                     VIBCON(GEOM,CONST)=FACTOR
                     END IF
                  END DO
               END DO
            END DO
         END DO

C- CONVERT VIBRATIONAL CONSTANTS FROM WAVENUMBERS TO JOULES PER MOLE
      FACTOR=H*C*A*100.0
      WRITE(6,*) 'Converting vibrational modes into J/mol'
C   REAGENTS/INTERMEDIATES/TRANSITION STATES
      DO SYSTEM=0,SYSMAX
         DO GEOM=GEOSTA(SYSTEM),(GEOSTA(SYSTEM)+GEONUM(SYSTEM)-1)
            DO CONST=1,VIBNUM(SYSTEM)
               VIBCON(GEOM,CONST)=VIBCON(GEOM,CONST)*FACTOR
               END DO
            IF (ECHO) THEN
               IF (GEOM.GT.0) THEN
                  WRITE(6,'(A,I2,A,I3,A,7(I5,''   ''))')
     &                                  ' System ',SYSTEM,', geometry ',
     &                                    (GEOM-GEOSTA(SYSTEM)+1),':  ',
     &                                         (INT(VIBCON(GEOM,CONST)),
     &                                    CONST=1,MIN(6,VIBNUM(SYSTEM)))
                  ELSE
                  WRITE(6,'(A,7(I5,''   ''))')
     &                                  '          Reagent System:  ',
     &                                         (INT(VIBCON(GEOM,CONST)),
     &                                    CONST=1,MIN(6,VIBNUM(SYSTEM)))
                  END IF
               IF (VIBNUM(SYSTEM).GT.6)
     &                             WRITE(6,'(11('' ''),15(I5,''   ''))')
     &                                         (INT(VIBCON(GEOM,CONST)),
     &                                           CONST=7,VIBNUM(SYSTEM))
               END IF
            IF (VIBCON(GEOM,1).LT.(10.0*GRID)) THEN
               WRITE(6,'(A,I2,A,I3,A)')
     &                         ' WARNING: System ',SYSTEM,', geometry ',
     &                                          (GEOM-GEOSTA(SYSTEM)+1),
     &                         ' has very small vibrational constant(s)'
               END IF
            END DO
         END DO
      WRITE(6,'(71(''-''))')

C==============================================================================
C                  ROTATIONAL CONSTANTS
C==============================================================================

C- SORT ROTATIONAL CONSTANTS INTO ASCENDING ORDER USING THE RATHER CRAP BUBBLE SORT
C   UNIMOLECULAR REAGENTS/INTERMEDIATES/TRANSITION STATES
      DO GEOM=0,(GEOSTA(SYSMAX)+GEONUM(SYSMAX)-1)
         DO COUNT=1,2
            DO CONST=1,2
               IF (ROTCON(GEOM,CONST).GT.ROTCON(GEOM,CONST+1)) THEN
                  FACTOR=ROTCON(GEOM,CONST+1)
                  ROTCON(GEOM,CONST+1)=ROTCON(GEOM,CONST)
                  ROTCON(GEOM,CONST)=FACTOR
                  END IF
               END DO
            END DO
         END DO
C   BIMOLECULAR REAGENTS/ASYMPTOTIC REGIONS
      DO SYSTEM=0,SYSMAX
         IF (USE_DI(SYSTEM)) THEN
            DO GEOM=1,2
               DO COUNT=1,2
                  DO CONST=1,2
                     IF (ROTFRA(SYSTEM,GEOM,CONST).GT.
     &                                 ROTFRA(SYSTEM,GEOM,CONST+1)) THEN
                        FACTOR=ROTFRA(SYSTEM,GEOM,CONST+1)
                        ROTFRA(SYSTEM,GEOM,CONST+1)=
     &                                         ROTFRA(SYSTEM,GEOM,CONST)
                        ROTFRA(SYSTEM,GEOM,CONST)=FACTOR
                        END IF
                     END DO
                  END DO
               END DO
            END IF
         END DO

C- CONVERT ROTATIONAL CONSTANTS FROM GHZ TO JOULES PER MOLE
      FACTOR=H*A*1.0D+9
      WRITE(6,*) 'Converting rotational constants into J/mol'
C   REAGENTS/INTERMEDIATES/TRANSITION STATES
      DO GEOM=0,(GEOSTA(SYSMAX)+GEONUM(SYSMAX)-1)
         DO CONST=1,3
            ROTCON(GEOM,CONST)=ROTCON(GEOM,CONST)*FACTOR
            END DO
         END DO
C   ASYMPTOTIC REGIONS
      DO SYSTEM=0,SYSMAX
         IF (USE_DI(SYSTEM)) THEN
            DO GEOM=1,2
               DO CONST=1,3
                  ROTFRA(SYSTEM,GEOM,CONST)=
     &                                  ROTFRA(SYSTEM,GEOM,CONST)*FACTOR
                  END DO
               END DO
            END IF
         END DO
C   ORBITAL CONSTANTS
      DO SYSTEM=0,SYSMAX
         IF (USE_DI(SYSTEM)) THEN
            DO GEOM=GEOSTA(SYSTEM),(GEOSTA(SYSTEM)+GEONUM(SYSTEM)-1)
               ORBCON(GEOM)=ORBCON(GEOM)*FACTOR
               END DO
            END IF
         END DO

C- TRANSFORM THE THREE ROTATIONAL CONSTANTS [SMALL,MEDIUM,LARGE]
C- INTO [SINGLY DEGENERATE,DOUBLY DEGENERATE,SINGLE-DOUBLE VALUE]
C   UNIMOLECULAR REAGENTS/INTERMEDIATES/TRANSITION STATES
      DO GEOM=0,(GEOSTA(SYSMAX)+GEONUM(SYSMAX)-1)
         LARGE=ROTCON(GEOM,3)
         MEDIUM=ROTCON(GEOM,2)
         SMALL=ROTCON(GEOM,1)
         IF (ECHO.AND.(((GEOM.GT.0).OR.(.NOT.(USE_DI(0)))))) THEN
            WRITE(6,'(35(''- ''),''-'')')
            WRITE(6,'(A,I3,A,3F10.2)')
     &                      ' Geometry ',GEOM,' rotational constants: ',
     &                                    (ROTCON(GEOM,CONST),CONST=1,3)
            END IF

         IF (SMALL.LT.0.0) THEN
            IF (MEDIUM.LT.0.0) THEN                                             ! THIS IS AN ATOM
               IF ((GEOM.EQ.0).AND.(USE_DI(0))) THEN
                  ROTCON(GEOM,1)=-1.0
                  ROTCON(GEOM,2)=-1.0
                  ROTCON(GEOM,3)=-1.0
                  ELSE
                  IF (ECHO) WRITE(6,'(A,I3,A)')
     &                                   ' Geometry ',GEOM,' is an atom'
                  WRITE(6,*) 'INTERMEDIATE/TRANSITION GEOMETRY IS ATOM'
                  WRITE(6,*) 'ERROR TERMINATION IN CONVERT SECTION'
                  call flush(6)
                  STOP
                  END IF
               ELSE                                                             ! THIS IS A LINEAR TOP
               IF (ECHO) WRITE(6,'(A,I3,A)')
     &                              ' Geometry ',GEOM,' is a linaer top'
               ROTCON(GEOM,1)=-1.0
               ROTCON(GEOM,2)=DSQRT(MEDIUM*LARGE)
               ROTCON(GEOM,3)=-1.0
               IF (ECHO) WRITE(6, '(A,F6.2)')
     &                    ' Doubly degenerate constant: ',ROTCON(GEOM,2)
               END IF
            ELSE
            IF ((LARGE-SMALL).LT.1.0) THEN                                      ! THIS IS A SYMMETRIC TOP
               IF (ECHO) WRITE(6,'(A,I3,A)')
     &                           ' Geometry ',GEOM,' is a symmetric top'
               FACTOR=(LARGE*MEDIUM*SMALL)**(1.0/3.0)
               ROTCON(GEOM,1)=FACTOR
               ROTCON(GEOM,2)=FACTOR
               ROTCON(GEOM,3)=0.0
               IF (ECHO) WRITE(6, '(A,F6.2)')
     &                    ' Triply degenerate constant: ',ROTCON(GEOM,2)
               ELSE
               FACTOR=((2*MEDIUM)-LARGE-SMALL)/(LARGE-SMALL)
               IF ((FACTOR.GT.-0.9).AND.(FACTOR.LT.0.9)) THEN
                  WRITE(6,'(A,I3,A)')
     &                  ' WARNING: Geometry ',GEOM,' is very asymmetric'
                  END IF
               IF (FACTOR.GT.0.0) THEN                                          ! THIS IS A (NEAR) OBLATE TOP
                  ROTCON(GEOM,1)=SMALL
                  ROTCON(GEOM,2)=DSQRT(MEDIUM*LARGE)
                  IF (ECHO) WRITE(6,'(A,I3,A,F4.2,A)')
     &          ' Geometry ',GEOM,' is an oblate top (Kappa=',FACTOR,')'

                  ELSE                                                          ! THIS IS A (NEAR) PROLATE TOP
                  ROTCON(GEOM,1)=LARGE
                  ROTCON(GEOM,2)=DSQRT(MEDIUM*SMALL)
                  IF (ECHO) WRITE(6,'(A,I3,A,F5.2,A)')
     &          ' Geometry ',GEOM,' is a prolate top (Kappa=',FACTOR,')'
                  END IF
               ROTCON(GEOM,3)=ROTCON(GEOM,1)-ROTCON(GEOM,2)
               IF (ECHO) WRITE(6, '(A,F6.2,A,F6.2)')
     &                   ' Singly degenerate constant: ',ROTCON(GEOM,1),
     &                   ', Doubly degenerate constant: ',ROTCON(GEOM,2)
               END IF
            END IF
         IF (ECHO) THEN
            IF ((GEOM.GT.0).OR.(.NOT.(USE_DI(0))))
     &                                        WRITE(6,'(A,I3,A,3F10.2)')
     &                      ' Geometry ',GEOM,' rotational constants: ',
     &                                    (ROTCON(GEOM,CONST),CONST=1,3)
            END IF
         END DO

C   BIMOLECULAR REAGENTS/ASYMPTOTIC REGIONS
      DO SYSTEM=0,SYSMAX
         IF (USE_DI(SYSTEM)) THEN
         DO GEOM=1,2
         LARGE=ROTFRA(SYSTEM,GEOM,3)
         MEDIUM=ROTFRA(SYSTEM,GEOM,2)
         SMALL=ROTFRA(SYSTEM,GEOM,1)
         IF (ECHO) THEN
            WRITE(6,'(35(''- ''),''-'')')
            WRITE(6,'(A,I3,A,3F10.2)')
     &             ' Fragment ',SYSTEM,' rotational constants: ',
     &                            (ROTFRA(SYSTEM,GEOM,CONST),CONST=1,3)
            END IF
         IF (SMALL.LT.0.0) THEN
            IF (MEDIUM.LT.0.0) THEN                                             ! THIS IS AN ATOM
               IF (ECHO) WRITE(6,'(A,I3,A)')
     &                           ' Fragment ',SYSTEM,' is an atom'
               ROTFRA(SYSTEM,GEOM,1)=-1.0
               ROTFRA(SYSTEM,GEOM,2)=-1.0
               ROTFRA(SYSTEM,GEOM,3)=-1.0
               ELSE                                                             ! THIS IS A LINEAR TOP
               IF (ECHO) WRITE(6,'(A,I3,A)')
     &                      ' Fragment ',SYSTEM,' is a linear top'
               ROTFRA(SYSTEM,GEOM,1)=-1.0
               ROTFRA(SYSTEM,GEOM,2)=DSQRT(MEDIUM*LARGE)
               ROTFRA(SYSTEM,GEOM,3)=-1.0
               IF (ECHO) WRITE(6,'(A,F6.2)')
     &            ' Doubly degenerate constant: ',ROTFRA(SYSTEM,GEOM,2)
               END IF
            ELSE
            IF ((LARGE-SMALL).LT.1.0) THEN                                      ! THIS IS A SYMMETRIC TOP
               IF (ECHO) WRITE(6,'(A,I3,A)')
     &                         ' Fragment ',SYSTEM,' is a symmetric top'
               FACTOR=(LARGE*MEDIUM*SMALL)**(1.0/3.0)
               ROTFRA(SYSTEM,GEOM,1)=FACTOR
               ROTFRA(SYSTEM,GEOM,2)=FACTOR
               ROTFRA(SYSTEM,GEOM,3)=0.0
               IF (ECHO) WRITE(6, '(A,F6.2)')
     &             ' Triply degenerate constant: ',ROTFRA(SYSTEM,GEOM,2)
               ELSE
               FACTOR=((2*MEDIUM)-LARGE-SMALL)/(LARGE-SMALL)
               IF ((FACTOR.GT.-0.9).AND.(FACTOR.LT.0.9)) THEN
                  WRITE(6,'(A,I3,A)')
     &               ' WARNING: Fragment ',SYSTEM,' is very asymmetric'
                  END IF
               IF (FACTOR.GT.0.0) THEN                                          ! THIS IS A (NEAR) OBLATE TOP
                  ROTFRA(SYSTEM,GEOM,1)=SMALL
                  ROTFRA(SYSTEM,GEOM,2)=DSQRT(MEDIUM*LARGE)
                  IF (ECHO) WRITE(6,'(A,I3,A,F4.2,A)')
     &        ' Fragment ',SYSTEM,' is an oblate top (Kappa=',FACTOR,')'

                  ELSE                                                          ! THIS IS A (NEAR) PROLATE TOP
                  ROTFRA(SYSTEM,GEOM,1)=LARGE
                  ROTFRA(SYSTEM,GEOM,2)=DSQRT(MEDIUM*SMALL)
                  IF (ECHO) WRITE(6,'(A,I3,A,F5.2,A)')
     &        ' Fragment ',SYSTEM,' is a prolate top (Kappa=',FACTOR,')'
                  END IF
               ROTFRA(SYSTEM,GEOM,3)=
     &                       ROTFRA(SYSTEM,GEOM,1)-ROTFRA(SYSTEM,GEOM,2)
               IF (ECHO) WRITE(6, '(A,F6.2,A,F6.2)')
     &            ' Singly degenerate constant: ',ROTFRA(SYSTEM,GEOM,1),
     &            ', Doubly degenerate constant: ',ROTFRA(SYSTEM,GEOM,2)
               END IF
            END IF
         IF (ECHO) WRITE(6,'(A,I3,A,3F10.2)')
     &                    ' Fragment ',SYSTEM,' rotational constants: ',
     &                             (ROTFRA(SYSTEM,GEOM,CONST),CONST=1,3)
         END DO !GEOM

C    THIS MAKES SURE THAT FRAGMENT 1 IS "LOWER ORDER" THAN FRAGMENT 2
C    "ORDER" IS GIVEN BY ATOM < LINEAR < 3D-TOP

C    IF BOTH FRAGMENTS ARE TOPS AND ONE IS PROLATE,
C    THE OTHER OBLATE, THEN MAKE SURE THE ONE WITH
C    THE NEGATIVE A-C CONSTANT IS THE SECOND FRAGMENT

         DO GEOM=1,2
            IF (ROTFRA(SYSTEM,GEOM,2).GE.0.0) THEN
               IF (ROTFRA(SYSTEM,GEOM,1).GE.0.0) THEN
                  F_TYPE(GEOM)=3 !TOP
                  ELSE
                  F_TYPE(GEOM)=2 !LINEAR
                  END IF
               ELSE
               F_TYPE(GEOM)=1 !ATOM
               END IF
            END DO
         IF (F_TYPE(1).GT.F_TYPE(2)) THEN
            DO CONST=1,3
               FACTOR=ROTFRA(SYSTEM,1,CONST)
               ROTFRA(SYSTEM,1,CONST)=ROTFRA(SYSTEM,2,CONST)
               ROTFRA(SYSTEM,2,CONST)=FACTOR
               END DO
            END IF
         IF (F_TYPE(1)+F_TYPE(2).EQ.6) THEN
            IF (ROTFRA(SYSTEM,1,3).LT.0.0) THEN
               IF (ROTFRA(SYSTEM,2,3).GE.0.0) THEN
                  DO CONST=1,3
                     FACTOR=ROTFRA(SYSTEM,1,CONST)
                     ROTFRA(SYSTEM,1,CONST)=ROTFRA(SYSTEM,2,CONST)
                     ROTFRA(SYSTEM,2,CONST)=FACTOR
                     END DO
                  END IF
               END IF
            END IF
         END IF !USE_DI
         END DO !SYSTEM

      WRITE(6,'(71(''-''))')

C==============================================================================
C                  ENERGIES
C==============================================================================

C- CONVERT ENERGIES FROM HARTREES TO JOULES PER MOLE
      FACTOR=2625.5*1000.0
      WRITE(6,*) 'Converting electronic energies into J/mol'
C   REAGENTS/INTERMEDIATES/TRANSITION STATES
      DO GEOM=0,(GEOSTA(SYSMAX)+GEONUM(SYSMAX)-1)
         ZERO_E(GEOM)=ZERO_E(GEOM)*FACTOR
         END DO
C- CONVERT ELECTRONIC ENERGIES TO ZERO-POINT ENERGIES
C   REAGENTS/INTERMEDIATES/TRANSITION STATES
      DO SYSTEM=0,SYSMAX
         DO GEOM=GEOSTA(SYSTEM),(GEOSTA(SYSTEM)+GEONUM(SYSTEM)-1)
            DO CONST=1,VIBNUM(SYSTEM)
               ZERO_E(GEOM)=ZERO_E(GEOM)+(0.5*VIBCON(GEOM,CONST))
               END DO
            END DO
         END DO

C- FIND THE LOWEST ENERGY SYSTEM
      BASE_E=ZERO_E(0)
      DO GEOM=1,(GEOSTA(SYSMAX)+GEONUM(SYSMAX)-1)
         BASE_E=MIN(BASE_E,ZERO_E(GEOM))
         END DO
      BASE_E=BASE_E-GRID

C- SHIFT ALL ENERGIES TO THIS

      DO GEOM=0,(GEOSTA(SYSMAX)+GEONUM(SYSMAX)-1)
         ZERO_E(GEOM)=ZERO_E(GEOM)-BASE_E
         IF (ECHO) THEN
            IF (GEOM.GT.0) THEN
               WRITE(6,'(A,I3,A,F10.2,F20.2)')
     &                               '  Geometry ',GEOM,' energy: ',
     &                         ZERO_E(GEOM),(ZERO_E(GEOM)-ZERO_E(0))
               ELSE
               WRITE(6,'(A,F10.2,F20.2)')
     &                                    ' Total Reagent energy: ',
     &                         ZERO_E(GEOM),(ZERO_E(GEOM)-ZERO_E(0))
               END IF
            END IF

         IF (ZERO_E(GEOM).GT.(DFLOAT(ARRAY)*GRID)) THEN
            IF (GEOM.GT.0) THEN
               WRITE(6,'(A,I3,A)')           ' WARNING: Geometry ',GEOM,
     &                      ' zero point energy lies above energy limit'
               ELSE
               WRITE(6,*) 'REAGENT ENERGY IS ABOVE LIMIT'
               WRITE(6,*) 'ERROR TERMINATION IN CONVERT SECTION'
               call flush(6)
               STOP
               END IF
            END IF
         END DO
      WRITE(6,'(71(''-''))')

C==============================================================================
C                  MASSES
C==============================================================================

C- CONVERT MASSES FROM AMUS TO KILOGRAMS

C- FOR BIMOLECULAR SYSTEMS, CALCULATE THE REDUCED MASS AND CALCULATE
C- THE ORBITAL ROTATIONAL CONSTANT FOR A SPHERE WITH 1m^3 VOLUME

      IF (USE_DI(0)) THEN
         FACTOR=1.0/(A*1000.0)
         WRITE(6,*) 'Converting masses into kg'
         MASS(1)=FACTOR*MASS(1)
         MASS(2)=FACTOR*MASS(2)
         RED_MASS=MASS(1)*MASS(2)/(MASS(1)+MASS(2))        ! REDUCED MASS
         IF (ECHO) WRITE(6,'(A,E15.5,A)')
     &                          ' Reagent reduced mass: ',RED_MASS,' kg'

         FACTOR=((H/PI)**2.0)*A/8.0                                    ! ORBITAL ROTATIONAL CONSTANT
         ORBCON(0)=FACTOR/(RED_MASS*((0.75*PI)**(2.0/3.0)))

C- NOW CALCULATE THE KINETIC LEVELS CONSTANT, THE FORMULA DEPENDING
C- ON WHETHER THIS IS A PARTICLE IN A BOX OF A SPHERE SYSTEM

         KINCON(1)=((2.0*RED_MASS/A)**1.5)*4.0*PI/(3.0*(H**3.0))       ! 3D KINETIC CONSTANT
         KINCON(2)=DSQRT(8.0*RED_MASS/A)*((0.75/PI)**(1.0/3.0))/H      ! RADIAL KINETIC CONSTANT

         WRITE(6,'(71(''-''))')

         END IF

C==============================================================================

      WRITE(6,*) 'Finished converting data'
      WRITE(6,'(71(''=''))')

      RETURN
      END
