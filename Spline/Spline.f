      PROGRAM SPLINE

C- THIS PROGRAM CREATES A SET OF NATURAL CUBIC
C- SPLINE INTERPOLATION EXPRESSIONS FOR A GIVEN
C- SET OF 2 DIMENSIONAL CARTESIAN COORDINATES

C- Y = A*Y1 + B*Y2 + C*Y1'' + D*Y2''

C- THE X VALUES MUST BE IN INCREASING VALUE

      USE MSFLIB

C- DECLARE VARIABLES, CONSTANTS & ARRAYS

      IMPLICIT NONE

	INTEGER ARRAY,MAXNUM,INDEX
      REAL*8 X_VAL,Y_VAL,SECDER,GRAD,MATRIX,TEMP

	PARAMETER(ARRAY=10000)

	DIMENSION X_VAL(ARRAY)
	DIMENSION Y_VAL(ARRAY)
	DIMENSION SECDER(ARRAY)
      DIMENSION GRAD(ARRAY-1)
      DIMENSION MATRIX(ARRAY-2,6)

C- OPEN INPUT & OUTPUT FILES

      INDEX=DELFILESQQ('Spline.LOG')
      OPEN(22,FILE='Spline.LOG',STATUS='NEW')

C- READ INPUT DATA IN

      OPEN(11,FILE='Spline.DAT',STATUS='OLD')
      MAXNUM=0
      WRITE(22,*) 'X & Y VALUES'
      DO WHILE ((.NOT.EOF(11)).AND.(MAXNUM.LE.ARRAY))
         MAXNUM=MAXNUM+1
         READ(11,*) X_VAL(MAXNUM),Y_VAL(MAXNUM)
         WRITE(22,*) MAXNUM,X_VAL(MAXNUM),Y_VAL(MAXNUM)
         END DO

      WRITE(22,*) 'MAXNUM OF DATA POINTS:',MAXNUM

C- SET THE TWO END SECOND-DERIVATIVES TO ZERO

      SECDER(1)=0.0D+0
	SECDER(MAXNUM)=0.0D+0

C- CALCULATE THE LINEAR GRADIENTS BETWEEN CONSECUTIVE POINTS

      WRITE(22,*) '----------------------------------------------------'
      WRITE(22,*) 'LINEAR GRADIENT VALUES'
      DO INDEX=1,MAXNUM-1
	   GRAD(INDEX)=(Y_VAL(INDEX+1)-Y_VAL(INDEX))
     &                                 /(X_VAL(INDEX+1)-X_VAL(INDEX))
         WRITE(22,*) INDEX,INDEX+1,GRAD(INDEX)
         END DO

C---------------------------------------------------------------------------------------------------
C- THE SET OF SECOND DERIVATIVES IS FOUND BY SOLVING
C- (MAXNUM-2) SIMILTANEOUS EQUATIONS WHICH PRODUCE
C- CONTINUOUS FIRST-DERIVATIVES USING THE FORMULA

C- X2-X1          X3-X1          X3-X2          Y3-Y2   Y2-Y1
C- ~~~~~ * Y1'' + ~~~~~ * Y2'' + ~~~~~ * Y3'' = ~~~~~ - ~~~~~
C-   6              3              6            X3-X2   X2-X1

C- THE SYSTEM FORMS A TRIDIAGONAL MATRIX, WHICH IS SOLVED HERE
C- BY LU DECOMPOSITION, FORWARD- AND BACKWARD-SUBSTITUTION

      DO INDEX=1,MAXNUM-2
	   MATRIX(INDEX,1)=(X_VAL(INDEX+1)-X_VAL(INDEX))/6.0       ! A(N): Y1'' COEFFICIENTS
	   MATRIX(INDEX,2)=(X_VAL(INDEX+3)-X_VAL(INDEX))/3.0       ! B(N): Y2'' COEFFICIENTS
	   MATRIX(INDEX,3)=(X_VAL(INDEX+3)-X_VAL(INDEX+2))/6.0     ! C(N): Y3'' COEFFICIENTS
	   MATRIX(INDEX,4)=0.0                                     ! U(N): Y'' VALUES
	   MATRIX(INDEX,5)=GRAD(INDEX+1)-GRAD(INDEX)               ! R(N): GRADIENT DIFFERENCES
         END DO

      TEMP=MATRIX(1,2)                                           ! TEMP=B(1)
      IF (TEMP.EQ.0.0) THEN
	   WRITE(22,*) 'TRIDIAGONAL ALGORITHM ERROR'
         CALL FLUSH(22)
	   STOP
         END IF
      MATRIX(1,4)=MATRIX(1,5)/TEMP                               ! U(1)=R(1)/B(1)

      DO INDEX=2,MAXNUM-2                                        ! DECOMPOSITION & FORWARD SUBSTITUTION
         MATRIX(INDEX,6)=MATRIX(INDEX-1,3)/TEMP                  ! G(N)=C(N-1)/TEMP
         TEMP=MATRIX(INDEX,2)-(MATRIX(INDEX,1)*MATRIX(INDEX,6))  ! TEMP=B(N)-[A(N)*G(N)]
         IF (TEMP.EQ.0.0) THEN
	      WRITE(22,*) 'TRIDIAGONAL ALGORITHM ERROR'
            CALL FLUSH(22)
	      STOP
            END IF
         MATRIX(INDEX,4)=(MATRIX(INDEX,5)-(MATRIX(INDEX,1)       ! U(N)=[R(N)-[A(N)*U(N-1)]]/TEMP
     &                                       *MATRIX(INDEX-1,4)))/TEMP
         END DO

      DO INDEX=MAXNUM-2,1,-1                                     ! BACKSUBSTITUTION
	   MATRIX(INDEX,4)=MATRIX(INDEX,4)-(MATRIX(INDEX+1,6)      ! U(N)=U(N)-[G(N+1)*U(N+1)]
     &                                       *MATRIX(INDEX+1,4))
	   END DO

      WRITE(22,*) '----------------------------------------------------'
      WRITE(22,*) 'SECOND DERIVATIVES'
      DO INDEX=2,MAXNUM-1
         SECDER(INDEX)=MATRIX(INDEX-1,4)
	   WRITE(22,*) INDEX,SECDER(INDEX)
	END DO

C---------------------------------------------------------------------------------------------------

      WRITE(22,*) '----------------------------------------------------'
      WRITE(22,*) 'RESULTS'
      DO INDEX=1,MAXNUM
	   WRITE(22,*) X_VAL(INDEX),',',Y_VAL(INDEX),',',SECDER(INDEX)
         CALL FLUSH(22)
	END DO


      END