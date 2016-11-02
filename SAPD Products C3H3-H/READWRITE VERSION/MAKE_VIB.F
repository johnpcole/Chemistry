      SUBROUTINE MAKE_VIB(VIBCONST,E_LIMIT,GEOMAX,GEONUM,VIBCOUNT)

C-    THIS SUBROUTINE CREATES THE VIBRATIONAL DATA TABLE

      IMPLICIT NONE

      REAL*8 E1_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 1
      REAL*8 E2_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 2
      REAL*8 E3_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 3
      REAL*8 E4_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 4
      REAL*8 E5_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 5
      REAL*8 E6_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 6
      REAL*8 E7_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 7
      REAL*8 E8_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 8
      REAL*8 E9_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 9
      REAL*8 E10_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 10
      REAL*8 E11_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 11
      REAL*8 E12_LIM       ! ENERGY LIMIT ON TS VIBRATIONAL MODE 12
      REAL*8 E_LIMIT      ! AMOUNT OF AVAILABLE ENERGY
      INTEGER P,T         ! DATA TABLE FLAGS
      REAL*8 P_ENE        ! PRODUCT VIBRATIONAL ENERGY
      REAL*8 P1_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 1
      REAL*8 P2_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 2
      REAL*8 P3_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 3
      REAL*8 P4_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 4
      REAL*8 P5_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 5
      REAL*8 P6_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 6
      REAL*8 P7_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 7
      REAL*8 P8_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 8
      REAL*8 P9_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 9
      REAL*8 P10_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 10
      REAL*8 P11_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 11
      REAL*8 P12_ENE       ! ENERGY OF PRODUCT VIBRATIONAL MODE 12
      REAL*8 T_ENE        ! TRANSITION STATE ENERGY
      REAL*8 T1_ENE       ! ENERGY OF TS VIBRATIONAL MODE 1
      REAL*8 T2_ENE       ! ENERGY OF TS VIBRATIONAL MODE 2
      REAL*8 T3_ENE       ! ENERGY OF TS VIBRATIONAL MODE 3
      REAL*8 T4_ENE       ! ENERGY OF TS VIBRATIONAL MODE 4
      REAL*8 T5_ENE       ! ENERGY OF TS VIBRATIONAL MODE 5
      REAL*8 T6_ENE       ! ENERGY OF TS VIBRATIONAL MODE 6
      REAL*8 T7_ENE       ! ENERGY OF TS VIBRATIONAL MODE 7
      REAL*8 T8_ENE       ! ENERGY OF TS VIBRATIONAL MODE 8
      REAL*8 T9_ENE       ! ENERGY OF TS VIBRATIONAL MODE 9
      REAL*8 T10_ENE       ! ENERGY OF TS VIBRATIONAL MODE 10
      REAL*8 T11_ENE       ! ENERGY OF TS VIBRATIONAL MODE 11
      REAL*8 T12_ENE       ! ENERGY OF TS VIBRATIONAL MODE 12
      REAL*8 V_ENE        ! M.E.P. VIBRATIONAL ENERGIES
      INTEGER V1_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 1
      INTEGER V2_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 2
      INTEGER V3_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 3
      INTEGER V4_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 4
      INTEGER V5_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 5
      INTEGER V6_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 6
      INTEGER V7_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 7
      INTEGER V8_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 8
      INTEGER V9_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 9
      INTEGER V10_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 10
      INTEGER V11_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 11
      INTEGER V12_MAX      ! QUANTUM NUMBER LIMIT ON VIBRATIONAL MODE 12
      INTEGER V1_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 1
      INTEGER V2_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 2
      INTEGER V3_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 3
      INTEGER V4_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 4
      INTEGER V5_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 5
      INTEGER V6_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 6
      INTEGER V7_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 7
      INTEGER V8_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 8
      INTEGER V9_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 9
      INTEGER V10_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 10
      INTEGER V11_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 11
      INTEGER V12_VAL      ! QUANTUM NUMBER OF VIBRATIONAL MODE 12
      REAL*8 VIBCONST     ! VIBRATIONAL CONSTANTS DATA
      REAL*8 VIBSTATE     ! VIBRATIONAL STATES TABLE
      INTEGER PATH        ! MINIMUM ENERGY PATH POINTER
      INTEGER GEOMAX
      INTEGER GEONUM
      INTEGER INDEX
      INTEGER VIBCOUNT

      DIMENSION E_LIMIT(-1:GEOMAX)                ! AMOUNT OF AVAILABLE ENERGY
      DIMENSION VIBCONST(-1:GEOMAX,1:12)           ! VIBRATIONAL CONSTANTS DATA
      DIMENSION VIBSTATE(-1:GEOMAX)               ! VIBRATIONAL STATES TABLE

      PARAMETER (P=-1,T=0)                        ! DATA TABLE FLAGS

      OPEN (72,FILE=('VIBRATE.DAT'),STATUS=('REPLACE'))
      VIBCOUNT=-1

C-    BUILD UP THE VIBRATIONAL STATES TABLE

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

                     E7_LIM=E6_LIM-P6_ENE
                     V7_MAX=INT((E7_LIM/VIBCONST(P,7))-0.5)
                     DO V7_VAL=0,V7_MAX
                        T7_ENE=VIBCONST(T,7)*(DFLOAT(V7_VAL)+0.5)
                        P7_ENE=VIBCONST(P,7)*(DFLOAT(V7_VAL)+0.5)

                     E8_LIM=E7_LIM-P7_ENE
                     V8_MAX=INT((E8_LIM/VIBCONST(P,8))-0.5)
                     DO V8_VAL=0,V8_MAX
                        T8_ENE=VIBCONST(T,8)*(DFLOAT(V8_VAL)+0.5)
                        P8_ENE=VIBCONST(P,8)*(DFLOAT(V8_VAL)+0.5)

                     E9_LIM=E8_LIM-P8_ENE
                     V9_MAX=INT((E9_LIM/VIBCONST(P,9))-0.5)
                     DO V9_VAL=0,V9_MAX
                        T9_ENE=VIBCONST(T,9)*(DFLOAT(V9_VAL)+0.5)
                        P9_ENE=VIBCONST(P,9)*(DFLOAT(V9_VAL)+0.5)

                     E10_LIM=E9_LIM-P9_ENE
                     V10_MAX=INT((E10_LIM/VIBCONST(P,10))-0.5)
                     DO V10_VAL=0,V10_MAX
                        T10_ENE=VIBCONST(T,10)*(DFLOAT(V10_VAL)+0.5)
                        P10_ENE=VIBCONST(P,10)*(DFLOAT(V10_VAL)+0.5)

                     E11_LIM=E10_LIM-P10_ENE
                     V11_MAX=INT((E11_LIM/VIBCONST(P,11))-0.5)
                     DO V11_VAL=0,V11_MAX
                        T11_ENE=VIBCONST(T,11)*(DFLOAT(V11_VAL)+0.5)
                        P11_ENE=VIBCONST(P,11)*(DFLOAT(V11_VAL)+0.5)

                     E12_LIM=E11_LIM-P11_ENE
                     V12_MAX=INT((E12_LIM/VIBCONST(P,12))-0.5)
                     DO V12_VAL=0,V12_MAX
                        T12_ENE=VIBCONST(T,12)*(DFLOAT(V12_VAL)+0.5)
                        P12_ENE=VIBCONST(P,12)*(DFLOAT(V12_VAL)+0.5)

                        T_ENE=T1_ENE+T2_ENE+T3_ENE+T4_ENE+T5_ENE+T6_ENE
     &                     +T7_ENE+T8_ENE+T9_ENE+T10_ENE+T11_ENE+T12_ENE
                        P_ENE=P1_ENE+P2_ENE+P3_ENE+P4_ENE+P5_ENE+P6_ENE
     &                     +P7_ENE+P8_ENE+P9_ENE+P10_ENE+P11_ENE+P12_ENE

                        IF (T_ENE.LE.E_LIMIT(T)) THEN
                           VIBSTATE(P)=P_ENE
                           VIBSTATE(T)=T_ENE

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
                              V_ENE=V_ENE+VIBCONST(PATH,7)
     &                                             *(DFLOAT(V7_VAL)+0.5)
                              V_ENE=V_ENE+VIBCONST(PATH,8)
     &                                             *(DFLOAT(V8_VAL)+0.5)
                              V_ENE=V_ENE+VIBCONST(PATH,9)
     &                                             *(DFLOAT(V9_VAL)+0.5)
                              V_ENE=V_ENE+VIBCONST(PATH,10)
     &                                            *(DFLOAT(V10_VAL)+0.5)
                              V_ENE=V_ENE+VIBCONST(PATH,11)
     &                                            *(DFLOAT(V11_VAL)+0.5)
                              V_ENE=V_ENE+VIBCONST(PATH,12)
     &                                            *(DFLOAT(V12_VAL)+0.5)
                              VIBSTATE(PATH)=V_ENE
                              END DO

                        VIBCOUNT=VIBCOUNT+1
                        WRITE(72,'(100(E25.18))')
     &                                 (VIBSTATE(INDEX),INDEX=-1,GEONUM)

                           END IF


                                          END DO ! V12_VAL
                                       END DO ! V11_VAL
                                    END DO ! V10_VAL
                                 END DO ! V9_VAL
                              END DO ! V8_VAL
                           END DO ! V7_VAL
                        END DO ! V6_VAL
                     END DO ! V5_VAL
                  END DO ! V4_VAL
               END DO ! V3_VAL
            END DO ! V2_VAL
         END DO ! V1_VAL

      CLOSE(72)

      RETURN
      END

