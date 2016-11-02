      SUBROUTINE MAKE_VIB(VIBCONST,E_LIMIT,VIBSTATE,STATEMAX
     &                                              ,GEOMAX,GEONUM)

C-    THIS SUBROUTINE CREATES THE VIBRATIONAL DATA TABLE

      IMPLICIT NONE

      REAL*8 E1_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 1
      REAL*8 E2_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 2
      REAL*8 E3_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 3
      REAL*8 E4_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 4
      REAL*8 E5_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 5
      REAL*8 E6_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 6
      REAL*8 E_LIMIT      ! AMOUNT OF AVAILABLE ENERGY
      INTEGER INDEX       ! VIBRATIONAL STATES TABLE INDEX
      INTEGER P,T         ! DATA TABLE FLAGS
      REAL*8 P_ENE        ! PRODUCT VIBRATIONAL ENERGY
      REAL*8 P1_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 1
      REAL*8 P2_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 2
      REAL*8 P3_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 3
      REAL*8 P4_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 4
      REAL*8 P5_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 5
      REAL*8 P6_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 6
      INTEGER STATEMAX    ! VIBRATIONAL STATES TABLE SIZE
      REAL*8 T_ENE        ! TRANSITION STATE ENERGY
      REAL*8 T1_ENE       ! ENERGY OF TS VIBRATIONAL MODE 1
      REAL*8 T2_ENE       ! ENERGY OF TS VIBRATIONAL MODE 2
      REAL*8 T3_ENE       ! ENERGY OF TS VIBRATIONAL MODE 3
      REAL*8 T4_ENE       ! ENERGY OF TS VIBRATIONAL MODE 4
      REAL*8 T5_ENE       ! ENERGY OF TS VIBRATIONAL MODE 5
      REAL*8 T6_ENE       ! ENERGY OF TS VIBRATIONAL MODE 6
      REAL*8 V_ENE        ! M.E.P. VIBRATIONAL ENERGIES
      INTEGER V1_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 1
      INTEGER V2_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 2
      INTEGER V3_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 3
      INTEGER V4_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 4
      INTEGER V5_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 5
      INTEGER V6_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 6
      INTEGER V1_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 1
      INTEGER V2_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 2
      INTEGER V3_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 3
      INTEGER V4_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 4
      INTEGER V5_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 5
      INTEGER V6_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 6
      REAL*8 VIBCONST     ! VIBRATIONAL CONSTANTS DATA
      REAL*8 VIBSTATE     ! VIBRATIONAL STATES TABLE
      INTEGER PATH        ! MINIMUM ENERGY PATH POINTER
      INTEGER GEOMAX
      INTEGER GEONUM

      DIMENSION E_LIMIT(-1:GEOMAX)                ! AMOUNT OF AVAILABLE ENERGY
      DIMENSION VIBCONST(-1:GEOMAX,1:6)           ! VIBRATIONAL CONSTANTS DATA
      DIMENSION VIBSTATE(-1:GEOMAX,0:STATEMAX)    ! VIBRATIONAL STATES TABLE

      PARAMETER (P=-1,T=0)                        ! DATA TABLE FLAGS

C-    INITIALISE THE VIBRATIONAL STATES TABLE

      DO PATH=-1,GEONUM
         DO INDEX=0,STATEMAX
            VIBSTATE(PATH,INDEX)=-1.0D0
            END DO
         END DO

C-    BUILD UP THE VIBRATIONAL STATES TABLE

      INDEX=0
      E1_LIM=E_LIMIT(P)
      V1_MAX=INT((E1_LIM/VIBCONST(P,1))-0.5)
      DO V1_VAL=0,V1_MAX
         T1_ENE=VIBCONST(T,1)*(DFLOAT(V1_VAL)+0.5)
         P1_ENE=VIBCONST(P,1)*(DFLOAT(V1_VAL)+0.5)

      WRITE (*,'(A,F6.2,A)')     ' CREATING VIBRATIONAL STATES TABLE :',
     &                      (100.0D0*DFLOAT(1+V1_VAL)/DFLOAT(1+V1_MAX)),
     &                                  '% DONE OF OUTER LOOP'

         E2_LIM=E1_LIM-P1_ENE
         V2_MAX=INT((E2_LIM/VIBCONST(P,2))-0.5)
         DO V2_VAL=0,V2_MAX
            T2_ENE=VIBCONST(T,2)*(DFLOAT(V2_VAL)+0.5)
            P2_ENE=VIBCONST(P,2)*(DFLOAT(V2_VAL)+0.5)

            E3_LIM=E2_LIM-P2_ENE
            V3_MAX=INT((E3_LIM/VIBCONST(P,3))-0.5)
            DO V3_VAL=0,V3_MAX
               T3_ENE=VIBCONST(T,3)*(DFLOAT(V3_VAL)+0.5)
               P3_ENE=VIBCONST(P,3)*(DFLOAT(V3_VAL)+0.5)

               E4_LIM=E3_LIM-P3_ENE
               V4_MAX=INT((E4_LIM/VIBCONST(P,4))-0.5)
               DO V4_VAL=0,V4_MAX
                  T4_ENE=VIBCONST(T,4)*(DFLOAT(V4_VAL)+0.5)
                  P4_ENE=VIBCONST(P,4)*(DFLOAT(V4_VAL)+0.5)

                  E5_LIM=E4_LIM-P4_ENE
                  V5_MAX=INT((E5_LIM/VIBCONST(P,5))-0.5)
                  DO V5_VAL=0,V5_MAX
                     T5_ENE=VIBCONST(T,5)*(DFLOAT(V5_VAL)+0.5)
                     P5_ENE=VIBCONST(P,5)*(DFLOAT(V5_VAL)+0.5)

                     E6_LIM=E5_LIM-P5_ENE
                     V6_MAX=INT((E6_LIM/VIBCONST(P,6))-0.5)
                     DO V6_VAL=0,V6_MAX
                        T6_ENE=VIBCONST(T,6)*(DFLOAT(V6_VAL)+0.5)
                        P6_ENE=VIBCONST(P,6)*(DFLOAT(V6_VAL)+0.5)

                        T_ENE=T1_ENE+T2_ENE+T3_ENE+T4_ENE+T5_ENE+T6_ENE
                        P_ENE=P1_ENE+P2_ENE+P3_ENE+P4_ENE+P5_ENE+P6_ENE

                        IF (T_ENE.LE.E_LIMIT(T)) THEN
                           VIBSTATE(P,INDEX)=P_ENE
                           VIBSTATE(T,INDEX)=T_ENE

                           DO PATH=1,GEONUM
                              V_ENE=VIBCONST(PATH,1)
     &                                             *(DFLOAT(V1_VAL)+0.5)
                              V_ENE=V_ENE+VIBCONST(PATH,2)
     &                                             *(DFLOAT(V2_VAL)+0.5)
                              V_ENE=V_ENE+VIBCONST(PATH,3)
     &                                             *(DFLOAT(V3_VAL)+0.5)
                              V_ENE=V_ENE+VIBCONST(PATH,4)
     &                                             *(DFLOAT(V4_VAL)+0.5)
                              V_ENE=V_ENE+VIBCONST(PATH,5)
     &                                             *(DFLOAT(V5_VAL)+0.5)
                              V_ENE=V_ENE+VIBCONST(PATH,6)
     &                                             *(DFLOAT(V6_VAL)+0.5)
                              VIBSTATE(PATH,INDEX)=V_ENE
                              END DO

                           INDEX=INDEX+1
                           IF (INDEX.GT.STATEMAX) THEN
                              PAUSE 'VIB STATES ARRAY NOT LARGE ENOUGH'
                              STOP
                              END IF
                           END IF

                        END DO ! V6_VAL
                     END DO ! V5_VAL
                  END DO ! V4_VAL
               END DO ! V3_VAL
            END DO ! V2_VAL
         END DO ! V1_VAL

      RETURN
      END

