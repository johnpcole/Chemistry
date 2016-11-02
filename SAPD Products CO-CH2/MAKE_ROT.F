      SUBROUTINE MAKE_ROT(BIG_J,ROT_J,ROT_K,E_LIMIT,ROTSTATE,STATEMAX,
     &                   ORBCONST,ROT_I,ORBSTATE,DIASTATE,GEOMAX)

C-    THIS SUBROUTINE CREATES THE ROTATIONAL DATA TABLE

      IMPLICIT NONE

      INTEGER B_MAX       ! COMBINATION QUANTUM NUMBER UPPER LIMIT
      INTEGER B_MIN       ! COMBINATION QUANTUM NUMBER LOWER LIMIT
      INTEGER B_VAL       ! COMBINATION QUANTUM NUMBER
      INTEGER BIG_J       ! TOTAL ANGULAR MOMENTUM QUANTUM NUMBER
      INTEGER DEGEN       ! ROTATIONAL DEGENERACY FACTOR
      REAL*8 E_LIMIT      ! AMOUNT OF AVAILABLE ENERGY
      INTEGER INDEX       ! ROTATIONAL STATES TABLE INDEX
      REAL*8 I_ENE        ! LINEAR ROTATIONAL ENERGY
      INTEGER I_MAX       ! LINEAR ROTATIONAL QUANTUM NUMBER LIMIT
      INTEGER I_VAL       ! LINEAR ROTATIONAL QUANTUM NUMBER
      REAL*8 J_ENE        ! ROTATIONAL ENERGY
      INTEGER J_MAX       ! ROTATIONAL QUANTUM NUMBER LIMIT
      INTEGER J_VAL       ! ROTATIONAL QUANTUM NUMBER
      REAL*8 K_ENE        ! ROTATIONAL ENERGY
      INTEGER K_MAX       ! ROTATIONAL QUANTUM NUMBER LIMIT
      INTEGER K_VAL       ! ROTATIONAL QUANTUM NUMBER
      REAL*8 L_ENE        ! ORBITAL ENERGY
      INTEGER L_MAX       ! ORBITAL QUANTUM NUMBER UPPER LIMIT
      INTEGER L_MIN       ! ORBITAL QUANTUM NUMBER LOWER LIMIT
      INTEGER L_VAL       ! ORBITAL QUANTUM NUMBER
      INTEGER M_VAL       ! DEGENERACY COUNTING QUANTUM NUMBER
      INTEGER N_VAL       ! DEGENERACY COUNTING QUANTUM NUMBER
      INTEGER P,T         ! DATA TABLE FLAGS
      INTEGER R,D         ! DATA TABLE FLAGS
      REAL*8 R_ENE        ! PRODUCT ENERGY (EXCLUDES L_ENE)
      REAL*8 ROT_I        ! ROTATIONAL CONSTANT OF LINEAR FRAGMENT
      REAL*8 ROT_J        ! ROTATIONAL CONSTANT
      REAL*8 ROT_K        ! ROTATIONAL CONSTANT
      REAL*8 ROTSTATE     ! ROTATIONAL STATES TABLE
      INTEGER STATEMAX    ! ROTATIONAL STATES TABLE SIZE
      REAL*8 ORBCONST     ! ORBITAL CONSTANTS
      INTEGER PATH        ! MINIMUM ENERGY PATH POINTER
      INTEGER ORBSTATE
      INTEGER DIASTATE
      INTEGER TERM_1
      INTEGER TERM_2
      INTEGER L_LIMIT
      INTEGER GEOMAX

      DIMENSION E_LIMIT(-1:GEOMAX)            ! AMOUNT OF AVAILABLE ENERGY
      DIMENSION ROTSTATE(-2:0,0:STATEMAX)     ! ROTATIONAL STATES TABLE
      DIMENSION ORBCONST(0:GEOMAX)
      DIMENSION ORBSTATE(0:STATEMAX)          ! L_VAL TABLE
      DIMENSION DIASTATE(0:STATEMAX)          ! I_VAL TABLE

      PARAMETER (R=-1,D=-2)                   ! DATA TABLE FLAGS
      PARAMETER (P=-1,T=0)                    ! DATA TABLE FLAGS

C-    INITIALISE THE ROTATIONAL STATES TABLE

      DO INDEX=0,STATEMAX
         ORBSTATE(INDEX)=-1.0D0
         DIASTATE(INDEX)=-1.0D0
         DO PATH=-2,0
            ROTSTATE(PATH,INDEX)=-1.0D0
         END DO
      END DO

C-    BUILD UP THE ROTATIONAL STATES TABLE

      INDEX=0
      J_MAX=INT(DSQRT(0.25+(E_LIMIT(P)/ROT_J))-0.5)

      DO J_VAL=0,J_MAX

         WRITE (*,'(A,F6.2,A)') ' CREATING ROTATIONAL STATES TABLE: ',
     &    (100.0*DFLOAT(J_VAL+1)/DFLOAT(J_MAX+1)),'% DONE OF OUTER LOOP'

         J_ENE=ROT_J*DFLOAT(J_VAL*(J_VAL+1))
         I_MAX=INT(DSQRT(0.25+((E_LIMIT(P)-J_ENE)/ROT_I))-0.5)

         DO I_VAL=0,I_MAX

            I_ENE=ROT_I*DFLOAT(I_VAL*(I_VAL+1))
            L_LIMIT=INT(DSQRT(0.25+((E_LIMIT(T)-(J_ENE+I_ENE))
     &                                               /ORBCONST(T)))-0.5)
            B_MIN=ABS(J_VAL-I_VAL)
            B_MAX=J_VAL+I_VAL

            DO L_VAL=0,L_LIMIT

               L_ENE=ORBCONST(T)*DFLOAT(L_VAL*(L_VAL+1))
               K_MAX=MIN(
     &               INT(DSQRT((E_LIMIT(P)-(J_ENE+I_ENE))/ROT_K)),J_VAL,
     &               INT(DSQRT((E_LIMIT(T)-(J_ENE+I_ENE+L_ENE))/ROT_K)))

               DO K_VAL=0,K_MAX

                  K_ENE=ROT_K*DFLOAT(K_VAL*K_VAL)
                  R_ENE=I_ENE+J_ENE+K_ENE              ! ROTATIONAL ENERGY AT PRODUCTS
                  ROTSTATE(D,INDEX)=0.0D0              ! SET DEGENERACY TO ZERO

                  DO B_VAL=B_MIN,B_MAX

                     L_MIN=ABS(BIG_J-B_VAL)
                     L_MAX=BIG_J+B_VAL

                     IF ((L_VAL.GE.L_MIN).AND.(L_VAL.LE.L_MAX)) THEN

                        DEGEN=0
                        DO M_VAL=-BIG_J,BIG_J
                           DO N_VAL=-J_VAL,J_VAL
                              TERM_1=MIN(B_VAL,L_VAL+M_VAL,I_VAL+N_VAL)
                              TERM_2=MIN(B_VAL,L_VAL-M_VAL,I_VAL-N_VAL)
                              DEGEN=DEGEN+MAX(TERM_1+TERM_2+1,0)
                              END DO
                           END DO
                        IF (K_VAL.GT.0) DEGEN=DEGEN*2

                        ROTSTATE(D,INDEX)=ROTSTATE(D,INDEX)
     &                                                    +DFLOAT(DEGEN)
                        END IF

                     END DO ! B_VAL

                     IF (ROTSTATE(D,INDEX).GT.(0.0D0)) THEN
                        ROTSTATE(R,INDEX)=R_ENE              ! ROTATIONAL ENERGY
                        ROTSTATE(T,INDEX)=L_ENE              ! ORBITAL ENERGY AT T.S.
                        ORBSTATE(INDEX)=L_VAL*(L_VAL+1)
                        DIASTATE(INDEX)=MIN(I_VAL,1000)
                        INDEX=INDEX+1
                        IF (INDEX.GT.STATEMAX) THEN
                           PAUSE 'ROT STATES ARRAY NOT LARGE ENOUGH'
                           STOP
                           END IF
                        END IF

                  END DO ! K_VAL

               END DO ! L_VAL

            END DO ! I_VAL

         END DO ! J_VAL

      RETURN
      END

