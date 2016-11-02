      SUBROUTINE MAKE_VIB(VIBCONST,E_LIMIT,GEOMAX,GEONUM,
     &                    ROTSTATE,STATEMAX,GRID,PROBMAX,
     &                PROB_KIN,PROB_VIB,PROB_ROT,ROT_TOP)



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
      real*8 Vx_ENE
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
      real*8 prob_rot,prob_vib,prob_kin
      real*8 grid
      integer statemax,probmax,rot_top
      real*8 rotstate

      DIMENSION E_LIMIT(-1:GEOMAX)                ! AMOUNT OF AVAILABLE ENERGY
      DIMENSION VIBCONST(-1:GEOMAX,1:12)           ! VIBRATIONAL CONSTANTS DATA
      DIMENSION VIBSTATE(-1:GEOMAX)               ! VIBRATIONAL STATES TABLE
      DIMENSION Vx_ENE(-1:GEOMAX,1:12)
      DIMENSION ROTSTATE(-2:GEOMAX,0:STATEMAX)! ROTATIONAL STATES TABLE
      DIMENSION PROB_KIN(0:PROBMAX)           ! KINETIC PROBABILITY DISTRIBUTION
      DIMENSION PROB_ROT(0:PROBMAX)           ! ROTATIONAL PROBABILITY DISTRIBUTION
      DIMENSION PROB_VIB(0:PROBMAX)           ! VIBRATIONAL PROBABILITY DISTRIBUTION

      PARAMETER (P=-1,T=0)                        ! DATA TABLE FLAGS

C-    BUILD UP THE VIBRATIONAL STATES TABLE

      E1_LIM=E_LIMIT(P)
      V1_MAX=INT((E1_LIM/VIBCONST(P,1))-0.5)
      DO V1_VAL=0,V1_MAX
         do path=-1,geonum
            vx_ene(path,1)=vibconst(path,1)
     &                     *(dfloat(v1_val)+0.5)
            end do

      WRITE (*,'(A,F6.2,A)')     ' CREATING VIBRATIONAL STATES TABLE :',
     &                      (100.0D0*DFLOAT(1+V1_VAL)/DFLOAT(1+V1_MAX)),
     &                                  '% DONE OF OUTER LOOP'

      call flush(6)

         E2_LIM=E1_LIM-vx_ene(p,1)
         V2_MAX=INT((E2_LIM/VIBCONST(P,2))-0.5)
         DO V2_VAL=0,V2_MAX
            do path=-1,geonum
               vx_ene(path,2)=vibconst(path,2)
     &                        *(dfloat(v2_val)+0.5)
               end do

            E3_LIM=E2_LIM-vx_ene(p,2)
            V3_MAX=INT((E3_LIM/VIBCONST(P,3))-0.5)
            DO V3_VAL=0,V3_MAX
               do path=-1,geonum
                  vx_ene(path,3)=vibconst(path,3)
     &                           *(dfloat(v3_val)+0.5)
                  end do

               E4_LIM=E3_LIM-vx_ene(p,3)
               V4_MAX=INT((E4_LIM/VIBCONST(P,4))-0.5)
               DO V4_VAL=0,V4_MAX
                  do path=-1,geonum
                     vx_ene(path,4)=vibconst(path,4)
     &                              *(dfloat(v4_val)+0.5)
                     end do

                  E5_LIM=E4_LIM-vx_ene(p,4)
                  V5_MAX=INT((E5_LIM/VIBCONST(P,5))-0.5)
                  DO V5_VAL=0,V5_MAX
                     do path=-1,geonum
                        vx_ene(path,5)=vibconst(path,5)
     &                                 *(dfloat(v5_val)+0.5)
                        end do

                     E6_LIM=E5_LIM-vx_ene(p,5)
                     V6_MAX=INT((E6_LIM/VIBCONST(P,6))-0.5)
                     DO V6_VAL=0,V6_MAX
                        do path=-1,geonum
                           vx_ene(path,6)=vibconst(path,6)
     &                                    *(dfloat(v6_val)+0.5)
                           end do

                     E7_LIM=E6_LIM-vx_ene(p,6)
                     V7_MAX=INT((E7_LIM/VIBCONST(P,7))-0.5)
                     DO V7_VAL=0,V7_MAX
                        do path=-1,geonum
                           vx_ene(path,7)=vibconst(path,7)
     &                                    *(dfloat(v7_val)+0.5)
                           end do

                     E8_LIM=E7_LIM-vx_ene(p,7)
                     V8_MAX=INT((E8_LIM/VIBCONST(P,8))-0.5)
                     DO V8_VAL=0,V8_MAX
                        do path=-1,geonum
                           vx_ene(path,8)=vibconst(path,8)
     &                                    *(dfloat(v8_val)+0.5)
                           end do

                     E9_LIM=E8_LIM-vx_ene(p,8)
                     V9_MAX=INT((E9_LIM/VIBCONST(P,9))-0.5)
                     DO V9_VAL=0,V9_MAX
                        do path=-1,geonum
                           vx_ene(path,9)=vibconst(path,9)
     &                                    *(dfloat(v9_val)+0.5)
                           end do

                     E10_LIM=E9_LIM-vx_ene(p,9)
                     V10_MAX=INT((E10_LIM/VIBCONST(P,10))-0.5)
                     DO V10_VAL=0,V10_MAX
                        do path=-1,geonum
                           vx_ene(path,10)=vibconst(path,10)
     &                                    *(dfloat(v10_val)+0.5)
                           end do

                     E11_LIM=E10_LIM-vx_ene(p,10)
                     V11_MAX=INT((E11_LIM/VIBCONST(P,11))-0.5)
                     DO V11_VAL=0,V11_MAX
                        do path=-1,geonum
                           vx_ene(path,11)=vibconst(path,11)
     &                                    *(dfloat(v11_val)+0.5)
                           end do

                     E12_LIM=E11_LIM-vx_ene(p,11)
                     V12_MAX=INT((E12_LIM/VIBCONST(P,12))-0.5)
                     DO V12_VAL=0,V12_MAX
                        do path=-1,geonum
                           vx_ene(path,12)=vibconst(path,12)
     &                                    *(dfloat(v12_val)+0.5)
                           end do

c-----------------------------------------------------------------
c calculate total vibrational energies at each point along path
c-----------------------------------------------------------------

                     do path=-1,geonum
                        vibstate(path)=vx_ene(path,1)
                        do index=2,12
                           vibstate(path)=vibstate(path)
     &                                         +vx_ene(path,index)
                           end do
                        end do


      call MAKE_RHO(ROTSTATE,STATEMAX,E_LIMIT,GRID,
     &                PROBMAX,PROB_KIN,PROB_VIB,PROB_ROT,
     &                GEOMAX,GEONUM,VIBSTATE,ROT_TOP)


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

      RETURN
      END

