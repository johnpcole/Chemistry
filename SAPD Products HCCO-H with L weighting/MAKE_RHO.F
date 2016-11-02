      SUBROUTINE MAKE_RHO(VIBSTATE,ROTSTATE,STATEMAX,E_LIMIT,GRID,
     &      PROBMAX,PROB_KIN,PROB_VIB,PROB_ROT,GEOMAX,GEONUM,tstate)

C- THIS SUBROUTINE CREATES THE KINETIC-NONKINETIC
C- NORMALISED PROBABILITY DISTRIBUTION ARRAY

      IMPLICIT NONE

      REAL*8 E_LIMIT      ! AMOUNT OF AVAILABLE ENERGY
      REAL*8 E_ROT        ! ROTATIONAL ENERGY OF FRAGMENT
      REAL*8 E_INT        ! INTERNAL ENERGY ON PATH
      REAL*8 E_STR        ! STRETCH ENERGY ON PATH
      REAL*8 GRID         ! INTEGRATION COARSENESS VALUE
      INTEGER INDEX       ! COUNTING FLAG
      INTEGER D,R         ! DATA TABLE FLAGS
      REAL*8 LEVELS       ! ROTATIONAL-KINETIC DEGENERACY
      REAL*8 NORMAL       ! NORMALISATION CONSTANT
      INTEGER P,T         ! DATA TABLE FLAGS
      REAL*8 P_KIN        ! PRODUCT KINETIC ENERGY
      REAL*8 P_NONVIB     ! PRODUCT NONVIB ENERGY
      REAL*8 P_TOTAL      ! PRODUCT TOTAL ENERGY
      REAL*8 P_VIB        ! PRODUCT VIBRATIONAL ENERGY
      REAL*8 PROB_KIN     ! KINETIC PROBABILITY ARRAY
      REAL*8 PROB_ROT     ! ROTATIONAL PROBABILITY ARRAY
      REAL*8 PROB_VIB     ! VIBRATIONAL PROBABILITY ARRAY
      INTEGER PROBMAX     ! PROBABILITY ARRAY SIZE
      INTEGER ROT_MARK    ! ROTATIONAL STATES TABLE INDEX
      INTEGER ROT_TOP     ! TOP OF ROTATIONAL STATES TABLE
      REAL*8 ROTSTATE     ! ROTATIONAL STATES TABLE
      INTEGER STATEMAX    ! DATA TABLES SIZE
      INTEGER VIB_MARK    ! VIBRATIONAL STATES TABLE INDEX
      INTEGER VIB_TOP     ! TOP OF VIBRATIONAL STATES TABLE
      REAL*8 VIBSTATE     ! VIBRATIONAL STATES TABLE
      REAL*8 T_KIN        ! TRANSITION STATE KINETIC ENERGY
      REAL*8 T_NONVIB     ! TRANSITION STATE NONVIB ENERGY
      REAL*8 T_ORB        ! TRANSITION STATE ORBITAL ENERGY
      REAL*8 T_STR        ! TRANSITION STATE STRETCH ENERGY
      REAL*8 T_TOTAL      ! TRANSITION STATE TOTAL ENERGY
      REAL*8 T_VIB        ! TRANSITION STATE VIBRATIONAL ENERGY
      LOGICAL ALLOW       ! FLAG FOR CHECKING ALONG PATH
      INTEGER PATH        ! MINIMUM ENERGY PATH POINTER
      INTEGER GEOMAX
      INTEGER GEONUM
      INTEGER ECHOMARK,ECHOTOP
      real*8 tstate
      integer t_mark
      integer t_top

      PARAMETER (R=-1,D=-2)                   ! DATA TABLE FLAGS
      PARAMETER (P=-1,T=0)                    ! DATA TABLE FLAGS

      DIMENSION PROB_KIN(0:PROBMAX)           ! KINETIC PROBABILITY ARRAY
      DIMENSION PROB_ROT(0:PROBMAX)           ! ROTATIONAL PROBABILITY ARRAY
      DIMENSION PROB_VIB(0:PROBMAX)           ! VIBRATIONAL PROBABILITY ARRAY
      DIMENSION VIBSTATE(-1:GEOMAX,0:STATEMAX)    ! VIBRATIONAL STATES TABLE
      DIMENSION ROTSTATE(-2:GEOMAX,0:STATEMAX)    ! ROTATIONAL STATES TABLE
      DIMENSION E_LIMIT(-1:GEOMAX)                ! AVAILABLE ENERGY
      dimension tstate(0:statemax)

C-    INITIALISE THE PROBABILITY ARRAY

      DO INDEX=0,PROBMAX
         PROB_KIN(INDEX)=0.0D0
         PROB_ROT(INDEX)=0.0D0
         PROB_VIB(INDEX)=0.0D0
         END DO

C-    FIND STATE TABLE LIMITS

      ROT_TOP=STATEMAX
      VIB_TOP=STATEMAX
      t_top=statemax
      DO INDEX=STATEMAX,1,-1
         IF (ROTSTATE(T,INDEX).LT.(0.0D0)) ROT_TOP=INDEX-1
         IF (VIBSTATE(T,INDEX).LT.(0.0D0)) VIB_TOP=INDEX-1
         IF (tSTATE(INDEX).LT.(0.0D0)) t_TOP=INDEX-1
         END DO

C-    SET ENERGY LIMITS

      T_TOTAL=E_LIMIT(T)
      P_TOTAL=E_LIMIT(P)

      IF (INT(P_TOTAL/GRID).GT.PROBMAX) THEN
         PAUSE 'PROBAB ARRAY NOT BIG ENOUGH'
         STOP
         END IF

C-    BUILD UP PROBABILITY ARRAY

      DO ECHOMARK=0,VIB_TOP,100

         WRITE (*,'(A,F6.2,A)')
     &                          ' CREATING PROBABILITY DISTRIBUTIONS: ',
     &                   (100.0D0*DFLOAT(ECHOMARK+1)/DFLOAT(VIB_TOP+1)),
     &                                            '% DONE OF OUTER LOOP'

      ECHOTOP=MIN(VIB_TOP,ECHOMARK+99)

      DO VIB_MARK=ECHOMARK,ECHOTOP

         T_VIB=VIBSTATE(T,VIB_MARK)           ! VIBRATIONAL ENERGY AT T.S.
         P_VIB=VIBSTATE(P,VIB_MARK)           ! VIBRATIONAL ENERGY AT PRODUCT
         T_NONVIB=T_TOTAL-T_VIB               ! NON-VIBRATIONAL ENERGY AT T.S.
         P_NONVIB=P_TOTAL-P_VIB               ! NON-VIBRATIONAL ENERGY AT PRODUCT

         DO ROT_MARK=0,ROT_TOP

            E_ROT=ROTSTATE(R,ROT_MARK)        ! ROTATIONAL ENERGY (AT ALL POINTS)
            T_KIN=T_NONVIB-E_ROT              ! KINETIC ENERGY AT T.S.
            P_KIN=P_NONVIB-E_ROT              ! KINETIC ENERGY AT PRODUCT
            T_ORB=ROTSTATE(T,ROT_MARK)        ! ORBITAL ENERGY AT T.S.
            T_STR=T_KIN-T_ORB                 ! R.C. TRANSLATIONAL ENERGY AT T.S.

            IF (P_KIN.GE.(0.0D0)) THEN        ! LEVEL IS ALLOWED AT PRODUCT
               IF (T_STR.GE.(0.0D0)) THEN     ! LEVEL IS ALLOWED AT T.S.

                  ALLOW=.TRUE.

                  DO PATH=1,GEONUM
                     E_INT=E_ROT+VIBSTATE(PATH,VIB_MARK)
     &                                          +ROTSTATE(PATH,ROT_MARK)
                     E_STR=E_LIMIT(PATH)-E_INT
                     IF (E_STR.LT.(0.0D0)) ALLOW=.FALSE.
                     END DO

                  IF (ALLOW) THEN
 
                     LEVELS=0.0d+0

                     do t_mark=0,t_top
                        if (tstate(t_mark).le.t_str) then
                           if (tstate(t_mark).ge.(t_orb+e_rot)) then

               add a bit of probability here based on t_orb+e_rot and tstate(t_mark)

                              end if
                           end if

                        end do

                     LEVELS=LEVELS*ROTSTATE(D,ROT_MARK)

                     PROB_KIN(INT(P_KIN/GRID))=PROB_KIN(INT(P_KIN/GRID))
     &                                                           +LEVELS
                     PROB_VIB(INT(P_VIB/GRID))=PROB_VIB(INT(P_VIB/GRID))
     &                                                           +LEVELS
                     PROB_ROT(INT(E_ROT/GRID))=PROB_ROT(INT(E_ROT/GRID))
     &                                                           +LEVELS
                     END IF
                  END IF
               END IF

            END DO ! ROT

         END DO ! VIB         

         END DO !ECHO

C-    NORMALISE PROBABILITY ARRAY

      NORMAL=0.0D0
      DO INDEX=0,PROBMAX
         NORMAL=NORMAL+PROB_KIN(INDEX)
         NORMAL=NORMAL+PROB_ROT(INDEX)
         NORMAL=NORMAL+PROB_VIB(INDEX)
         END DO
      NORMAL=NORMAL/3.0D0
      DO INDEX=0,PROBMAX
         PROB_KIN(INDEX)=PROB_KIN(INDEX)/NORMAL
         PROB_ROT(INDEX)=PROB_ROT(INDEX)/NORMAL
         PROB_VIB(INDEX)=PROB_VIB(INDEX)/NORMAL
         END DO

      RETURN
      END

