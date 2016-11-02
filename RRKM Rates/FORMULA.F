      SUBROUTINE FORMULA(ENERGY,DOSSOS,ARRAY,SYSLIM,EJRATE,H,A,ratenum)

C- THIS IS THE USER DEFINED SUBROUTINE WHICH CALCULATES THE RATES
C- FROM DENSITY/SUM OF STATES CALCULATED FROM THE INPUT DATA

C- ENERGY IS THE CURRENT ENERGY POINTER USED BY THE MAIN PROGRAM
C- DOSSOS IS THE SUM/DENSITY OF STATES ARRAYS CREATED FOR EACH SYSTEM
C- ARRAY  IS THE SIZE OF THE DOSSOS ARRAYS
C- EJRATE IS THE OUTPUT FROM THIS SUBROUTINE, MAXIMUM 5 RATES

C- REMEMBER UNITS !!!
C- UNIMOLECULAR DENSITIES OF STATES ARE IN mol/J
C- UNIMOLECULAR RATES SHOULD HAVE UNITS /s
C- MULTIPLY UNIMOLECULAR DENSITIES BY H*A

C- BIMOLECULAR DENSITIES OF STATES ARE IN mol/J/m^3
C- BIMOLECULAR RATES SHOULD HAVE UNITS m^3/mol/s
C- MULTIPLY BIMOLECULAR DENSITIES BY H


      IMPLICIT NONE

      REAL*8 DOSSOS,EJRATE
      INTEGER ENERGY,ARRAY,SYSLIM,ratenum
      real*8 H,A

      INTEGER MODE,POINTIT

      real*8 w1,w2,w3t,w3c,w4e,w4s,w5te,w5ts,w5ce,w5cs,w6,w7
      real*8 rhoREA,rhoNIT,rhoTE,rhoTS,rhoCS,rhoCE
      real*8 k1,k2,k3t,k3c,k4e,k4s,k5te,k5ts,k5ce,k5cs,k6,k7
      real*8 km1,km2,km3t,km3c,km4e,km4s
      real*8 Wnit,Wce,Wcs,Wte,Wts,S2,S3t,S3c,S4s,S4e
      real*8 Isodiff,Pcis,Ptra,Psta,Pecl
      real*8 Sum1,Sum2,Sum3,Sum4,Sum5,Sum6
      real*8 Determin,Adjoint1,Adjoint2,Adjoint3,Adjoint4,Adjoint5
      real*8 uni_nh2,uni_n2h,uni_h2o,uni_n2o,bi_rate

      DIMENSION DOSSOS(0:SYSLIM,ARRAY)
      DIMENSION EJRATE(ratenum)

C ===================================================
C Set which kind of calculation you wish to carry out
C ===================================================
C    1 = SIMPLE (FORWARD) RATES
C    2 = SIMPLE (REVERSE) RATES
C    3 = COMBINED RATES
C ===================================================
      PARAMETER(MODE=3)
C ===================================================


C ===================================================
C    1 = SIMPLE (FORWARD) RATES
C    2 = SIMPLE (REVERSE) RATES
C ===================================================

      IF (MODE.LT.3) THEN

         rhoREA=H*DOSSOS(0,ENERGY)
         rhoNIT=H*A*DOSSOS(1,ENERGY)*2.0   ! Optically active (sort of)
         rhoTE=H*A*DOSSOS(2,ENERGY)
         rhoTS=H*A*DOSSOS(3,ENERGY)
         rhoCS=H*A*DOSSOS(4,ENERGY)
         rhoCE=H*A*DOSSOS(5,ENERGY)
         W1=DOSSOS(6,ENERGY)
         W2=DOSSOS(7,ENERGY)
         W3t=DOSSOS(8,ENERGY)*2.0  ! Rotomerisations
         W3c=DOSSOS(9,ENERGY)*2.0  ! are doubly degenerate
         W4s=DOSSOS(10,ENERGY)
         W4e=DOSSOS(11,ENERGY)
         W5te=DOSSOS(12,ENERGY)
         W5ts=DOSSOS(13,ENERGY)
         W5ce=0.0d+0
         W5cs=0.0d+0
         W6=DOSSOS(14,ENERGY)
         W7=DOSSOS(15,ENERGY)

         if (rhoREA.gt.(0.0d0)) then
            k1=W1/rhoREA
            else
            k1=0.0d0
            end if

         if (rhoNIT.gt.(0.0d0)) then
            km1=W1/rhoNIT
            k2=W2/rhoNIT
            else
            km1=0.0d0
            k2=0.0d0
            end if

         if (rhoTE.gt.(0.0d0)) then
            km2=W2/rhoTE
            k3t=W3t/rhoTE
            k4e=W4e/rhoTE
            k5te=W5te/rhoTE
            else
            km2=0.0d0
            k3t=0.0d0
            k4e=0.0d0
            k5te=0.0d0
            end if

         if (rhoTS.gt.(0.0d0)) then
            km3t=W3t/rhoTS
            k4s=W4s/rhoTS
            k5ts=W5ts/rhoTS
            else
            km3t=0.0d0
            k4s=0.0d0
            k5ts=0.0d0
            end if

         if (rhoCE.gt.(0.0d0)) then
            k3c=W3c/rhoCE
            km4e=W4e/rhoCE
            k5ce=W5ce/rhoCE
            k7=W7/rhoCE
            else
            k3c=0.0d0
            km4e=0.0d0
            k5ce=0.0d0
            k7=0.0d0
            end if

         if (rhoCS.gt.(0.0d0)) then
            km3c=W3c/rhoCS
            km4s=W4s/rhoCS
            k5cs=W5cs/rhoCS
            k6=W6/rhoCS
            else
            km3c=0.0d0
            km4s=0.0d0
            k5cs=0.0d0
            k6=0.0d0
            end if

         if (mode.eq.1) then

            EJRATE(1)=k1
            EJRATE(2)=k2
            EJRATE(3)=k3t
            EJRATE(4)=k3c
            EJRATE(5)=k4s
            EJRATE(6)=k4e
            EJRATE(7)=k5te
            EJRATE(8)=k5ts
            EJRATE(9)=k6
            EJRATE(10)=k7

            else

            EJRATE(1)=km1
            EJRATE(2)=km2
            EJRATE(3)=km3t
            EJRATE(4)=km3c
            EJRATE(5)=km4s
            EJRATE(6)=km4e
            EJRATE(7)=0.0d0
            EJRATE(8)=0.0d0
            EJRATE(9)=0.0d0
            EJRATE(10)=0.0d0

            end if

         END IF

C ===================================================
C    3 = COMBINED RATES
C ===================================================

      IF (MODE.EQ.3) THEN

         rhoREA=H*DOSSOS(0,ENERGY)
         W1=DOSSOS(1,ENERGY)
         W2=DOSSOS(2,ENERGY)
         W3t=DOSSOS(3,ENERGY)*2.0   ! Rotomerisations
         W3c=DOSSOS(4,ENERGY)*2.0   ! are doubly degenerate
         W4s=DOSSOS(5,ENERGY)
         W4e=DOSSOS(6,ENERGY)
         W5te=DOSSOS(7,ENERGY)
         W5ts=DOSSOS(8,ENERGY)
         W5ce=0.0d+0
         W5cs=0.0d+0
         W6=DOSSOS(9,ENERGY)
         W7=DOSSOS(10,ENERGY)

         Wnit=W1+W2                      ! Combined nitrosamine depleting pathways
         Wce=W3c+W4e+W5ce+W7             ! Combined cis-eclipsed depleting pathways
         Wcs=W3c+W4s+W5cs+W6             ! Combined cis-staggered depleting pathways
         Wte=W2+W3t+W4e+W5te             ! Combined trans-eclipsed depleting pathways
         Wts=W3t+W4s+W5ts                ! Combined trans-staggered depleting pathways

         S2=W2*W2                        ! Squared sum of states
         S3t=W3t*W3t                     ! Squared sum of states
         S3c=W3c*W3c                     ! Squared sum of states
         S4s=W4s*W4s                     ! Squared sum of states
         S4e=W4e*W4e                     ! Squared sum of states

         Isodiff=(W3t*W3c)-(W4e*W4s)     ! Difference between rotomerisation and bending isomerisations

         Pcis=Wce*Wcs                    ! Product of combined cis depleting pathways
         Ptra=Wte*Wts                    ! Product of combined trans depleting pathways
         Psta=Wcs*Wts                    ! Product of combined staggered depleting pathways
         Pecl=Wce*Wte                    ! Product of combined eclipsed depleting pathways

         Sum1=(Wts*S3c)+(Wce*S4s)-(Pcis*Wts)
         Sum2=(Pcis*W3t)-(W3c*Isodiff)
         Sum3=-(S4e-Pecl)*(S4s-Psta)
         Sum4=(W3c*W4e*W3t*W4s)+(S3c*Ptra)
         Sum5=(W4e*Psta)+(W4s*Isodiff)
         Sum6=(W3t*W4s*Wce)+(W3c*W4e*Wts)

C   Calculate the determinant

         Determin=(S2*Sum1)-(Wnit*((W3t*Sum2)+Sum3+Sum4))

         IF (Determin.EQ.(0.0D0)) then

            DO POINTIT=1,10
               EJRATE(POINTIT)=0.0D0
               END DO

            ELSE

C   Calculate adjoint elements

            Adjoint1=-(W3t*Sum2)-Sum3-Sum4
            Adjoint2=-W2*Sum1
            Adjoint3=W2*Sum2
            Adjoint4=W2*Sum5
            Adjoint5=W2*Sum6

C   Calculate unimolecular nitrosamine rates

            uni_nh2=((Adjoint1*W1/Determin)-1.0d0)
            uni_n2h=((Adjoint2*W5te)+(Adjoint3*W5ts)+(Adjoint4*W5ce)
     &                                        +(Adjoint5*W5cs))/Determin
            uni_h2o=Adjoint5*W6/Determin
            uni_n2o=Adjoint4*W7/Determin

            EJRATE(1)=uni_nh2
            EJRATE(2)=uni_n2h
            EJRATE(3)=uni_h2o
            EJRATE(4)=uni_n2o
            EJRATE(5)=uni_nh2+uni_n2h+uni_h2o+uni_n2o

            IF (rhoREA.EQ.(0.0d0)) THEN

               EJRATE(6)=0.0D0
               EJRATE(7)=0.0D0
               EJRATE(8)=0.0D0
               EJRATE(9)=0.0D0
               EJRATE(10)=0.0D0

               ELSE

               bi_rate=W1/rhoREA

               EJRATE(6)=bi_rate
               EJRATE(7)=uni_nh2*bi_rate
               EJRATE(8)=uni_n2h*bi_rate
               EJRATE(9)=uni_h2o*bi_rate
               EJRATE(10)=uni_n2o*bi_rate

               END IF

            END IF

         END IF

C ===================================================

      RETURN
      END
