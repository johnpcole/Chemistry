      Program potential

C NX,NY : Dimensions of grid.
C NCL   : Number of contouring levels.

      PARAMETER (NX=81,NY=501,NCL=100,IUNDEF=9999)
      PARAMETER (OFFSET=300)
      PARAMETER (RUNDEF=999.999)

      real rawdata(100)
      integer rawenergy,geometry,energy
      REAL VV(NX,NY),ZCL(2),WI(NCL),HI(NCL)
      real v1(nx,ny),v2(nx,ny)
      real x1(nx),y1(ny)
      real x,y,z
      real vsc
      INTEGER ISTYLE(NCL)


      topvalue=offset+NY-1

C Smallest contouring level and distance between levels.

      DATA CMIN,STEP / 0.0 , 0.5/

C Define contour line widths to be in X mm.

      DATA WI /NCL*0.5 /

C Contour line styles. all defined as solid.all defined as solid. 

      DATA ISTYLE / NCL*0 /

C.....SET RANGES OF GRID IN 2 DIRECTIONS.
      XMIN=1.75
      XMAX=5.75
      YMIN=dfloat(offset)
      YMAX=dfloat(topvalue)
C.....SET LOWEST and HIGHEST CONTOURS. UNITS of 0.1ev.
      ZMIN= 0.0
      ZMAX= 20.0

C.....INPUT THE FILE PRODUCED BY THE (MODIFIED) RATES PROGRAM

      do geometry=1,NX
         x1(geometry)=1.7+(0.05*dfloat(geometry))
         end do

      open(23,file='data.csv')
      read(23,*) (rawdata(i),i=1,(NX+2))
      do energy=0,800
         read(23,*) (rawdata(i),i=1,(NX+2))
         if ((energy.ge.offset).and.(energy.le.topvalue)) then
            do geometry=1,NX
               y1(energy+1-offset)=dfloat(energy)
               v1(geometry,(energy+1-offset))=rawdata(geometry+2)
               end do
            end if
         end do

      close(23)

C -------------------------------------------------------------
C OLD CODE
C -------------------------------------------------------------
c      open(23,file='data.csv')
c      do geometry=1,NX
c         read(23,'(A)') temptext
c         read(23,'(A)') temptext
c         x1(geometry)=1.7+(0.05*dfloat(geometry))
c
c         do energy=0,995,5
c            read(23,*) rawenergy,(rawdata(i),i=1,5)
c
c            do i=1,5
c               if (((energy+i-1).ge.offset)
c     &                   .and.((energy+i-1).le.topvalue)) then
c                  y1(energy+i-offset)=dfloat(energy+i-1)
c                  v1(geometry,(energy+i-offset))=rawdata(i)
c                  end if
c               end do
c
c            end do
c
c         read(23,'(A)') temptext
c
c         end do
c
c      close(23)
C -------------------------------------------------------------

C Generate a grid.

      vsc = 0.0d0

      do i=1,NY
         do j=1,NX

         if (v1(j,i).gt.(0.0d0)) then
            vv(j,i)=LOG10(v1(j,i))
            else
            vv(j,i)=-100000.0
            end if

            write(7,*)i,j,x1(j),y1(i),vv(j,i)

            call flush(7)

            end do
         end do

C Open UNIRAS.

      CALL GROUTE(' ')
      CALL GOPEN

C Set limits and viewport. The viewport is given the
C same aspect ratio as the user coordinate rectangle.
C It is placed in the middle of the plot area in the X-
C direction, and slightly to the right in the Y-direction.
C This is to make room for the left margin.
C
C     Get the size of the display surface for the nominated device.
      CALL GRPSIZ (XSIZMM, YSIZMM)
C     Calculate the required scale to adjust A4 onto display surface.
      SCALE = AMIN1 (XSIZMM / 190.0, YSIZMM / 277.0)
C     Calculate required position and size of graph.
      XOR = 27.5 * SCALE
      YOR = 20.0 * SCALE
      XSIZE = 150.0 * SCALE
      YSIZE = 200.0 * SCALE
C
C SET ESCAPE CHARACTERS:
      CALL RTXESC(1,'<')     
      CALL RTXESC(2,'>')  
      CALL RTXESC(8,'^')  
C
C DRAW AXES.
C
C     Specify position and size of graph.
      CALL GVPORT (XOR, YOR, XSIZE, YSIZE)
      CALL GWBOX (XSIZE, YSIZE, 0.0)
C     Specify extreme values of axes.
      CALL GLIMIT (XMIN, XMAX, YMIN, YMAX, 0.0, 0.0)
C     Apply this scale.
      CALL GSCALE
C
C     Calculate required height of axis text.
      TXH3 = 5.0 * SCALE
C     Calculate required height of axis labels.
      TXH4 = 3.0 * SCALE
C     Calculate required height for legend text.
      TXH5 = 4.0 * SCALE
C     Specify font for axis text and labels.
      CALL RTXFON ('SIMP', 0)
C     Calculate required marker size.
      RMKH1 = 4.0 * SCALE
C
C     Enable, and specify axis text.
      CALL RAXDIS (6, 1, 0)
      CALL RAXTEX (6, -1, 'Bond Separation (Angstroms)', 999.999, 
     $ 999.999, TXH3)
C     Specify tickmark interval.
      CALL RAXBTI (9999, 999.999, 999.999, 999.999)
C     Draw the X axis at Y = YMIN.
      CALL RAXIS (1, YMIN, TXH4, 1)
C.....Draw reverse X axis
      CALL RAXIS (1, YMAX, TXH4, 2)
C
C     Enable, and specify axis text.
      CALL RAXDIS (6, 1, 0)
      CALL RAXTEX (6,-1,'Energy (kJ/mol)',999.999,999.999,
     $ TXH3)
C     Specify tickmark interval.
      CALL RAXBTI (9999, 999.999, 999.999, 999.999)
C     Draw the Y axis at X = XMIN.
      CALL RAXIS (2, XMIN, TXH4, 1)
C     Draw reverse Y axis.
      CALL RAXIS (2, XMAX, TXH4, 2)
C
C Character height for contour line labels, numeric
C axis labels, and posting of elevations.
C
      HEIGHT = 0.012*MIN(XSIZE,YSIZE)
C
C Set contour levels, line widths, and colors.
C
      ZCL(1) = CMIN
      ZCL(2) = STEP
      CALL RCLASS(ZCL,NCL,5)
C
C Define contour line widths.
C
      CALL GCONWI(WI,NCL)
C
C Set contour line annotation attributes and text font.
C
C                Character height
C                 |   Number of decimals
C                 |   |  Distance between labels
C                 |   |  |         Move and overlay
C                 |   |  |         options active
C                 |   |  |         |
      CALL GCONA(TXH4,1, 0.3*XSIZE,3)
C
C Set character heights for contour line labels for
C each contour value.
C
      DO 100 I=1,NCL
        HI(I) = HEIGHT
100   CONTINUE
      CALL GCONAH(HI,NCL)
C
C Set line styles for contours.
C
      CALL GCONDA(ISTYLE,NCL)
C
C Plot a box plotted around each contour line label.
C           
C                  Color of box
C                  | Turn plotting of box edge on
C                  | |
       CALL GCONAB(0,1)
C
C Set text font for contour line labels.
C
      CALL RTXFON('SWIM',1)
C
C Do the contour plot.
C
      CALL GCNR2V(VV,NX,NY)     
C
       
      CALL GSCALE              
C
C
C Give the plot a title.
C
C     Calculate text heights for title lines.
      TXH1 = 6.0 * SCALE
      TXH2 = 4.0 * SCALE
C     Calculate horizontal position for title, centered on X axis.
      TXPOSX = (XMIN+XMAX)/2.0
C     Calculate vertical position for first title line, above graph.
      TXPOSY = YMAX + (YMAX-YMIN)*0.05
C
       CALL RTXFON('SWIM',1)
       CALL RTXJUS(1,3)
C     Specify font, height and justifiction for first title line.
c      CALL RTXHEI (TXH1)
c       CALL RTX(-1,' Li-F-H  theta=74, energy units 0.1eV',
c     $          TXPOSX,TXPOSY)
      CALL GCLOSE
      STOP
      END
C
C
      Subroutine grid(V,N,XMIN,XMAX,YMIN,YMAX)
C.....this subroutine sets up an N x N grid of points representing an
C.....Li + HF reactive potential energy surface.
      DOUBLE PRECISION VV,XX,YY,DXX,DYY
      DIMENSION V(N,N)
      DXX = (XMAX-XMIN)/FLOAT(N-1)
      DYY = (YMAX-YMIN)/FLOAT(N-1)
      XX=XMIN
      DO I=1,N
      YY=YMIN
      DO J=1,N
      call EVEV(XX,YY,VV)
C.....Convert units from AU to 0.1*EV.
      V(I,J)=VV*272.12
      YY=YY+DYY
      END DO
      XX=XX+DXX
      END DO
      RETURN
      END
C
      SUBROUTINE EVEV(XX,YY,VV)
C.....SUBROUTINE TO EVALUATE LIFH POTENTIAL AT FIXED LI-F-H ANGLE.
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION RIJ(3)
      DATA PI/3.141592653589793D0/
C.....FIRST FIX LI-F-H ANGLE THETA
      THETA=74.0D0*PI/180.0D0
      CTHETA=DCOS(THETA)
C.....FIX MAXIMUM VALUE OF POTENTIAL FOR PLOTTING.
      VMAX=5.D0/27.212
C.....  RIJ(1) = F--H
C.....  RIJ(2) = LI--H
C.....  RIJ(3) = LI--F
      RR1=YY
          IF(RR1.LT.0.6D0)GO TO 100
        RR3=XX
          IF(RR3.LT.1.0D0)GO TO 100
          Z = RR1**2 + RR3**2 - 2.0D0*RR1*RR3*CTHETA
          RR2=DSQRT(Z)
          IF(RR2.LT.1.0D0)GO TO 100
          RIJ(1)=RR1
          RIJ(2)=RR2
          RIJ(3)=RR3
          CALL LIFH(VV,RIJ)
          IF(VV.GT.VMAX) VV=VMAX
      RETURN
100   VV=VMAX
      RETURN
      END
C
C
C-----------------------------------------------------------------------
C THIS ROUTINE SUPPLIES THE POTENTIAL ENERGY SURFACE FOR THE
C REACTIVE SCATTERING CODES.
C
C ON ENTERING: THE ARRARY RIJ CONTAINS THE INTERNUCLEAR DISTANCES
C              IN UNITS OF BOHR (NOT MASS SCALED).
C
C              THE COMMON /PES/ MUST CONTAIN A CHARACTER STRING
C              OF LENGTH 20 THAT PROPERLY DESCRIBES THE POTENTIAL.
C
C ON EXIT:     THE VARIABLE V IS THE VALUE OF THE POTENTIAL ENERGY
C              SURFACE IN HARTREE ATOMIC UNITS.
C
      SUBROUTINE LIFH(EN,R)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
       DIMENSION R(3)
       DATA AUANG,AUCCLM,VBASE/.52917706E0,1.59359E-3,141.2/
       DATA NCOUNT/0/
C
       IF(NCOUNT.GT.0)GOTO100
       NCOUNT=1
       CALL PREPOT
100    CONTINUE
       RAB=AUANG*R(3)
       RBC=AUANG*R(1)
       RAC=AUANG*R(2)
       CALL POTBO(RAB,RBC,RAC,V)
C.....CONVERT KCALS/MOLE TO A.U.
       EN=(V+141.2)*AUCCLM
C      WRITE(6,*)'IN SURF',RAB,RBC ,RAC,EN
       RETURN
       END
C
C  THIS DECK POTBO CONTAINS:
C            THE INTERFACE ROUTINE POT
C                       VBASE ADJUSTS THE ENERGY ZERO
C                       CONVR CONVERTS INTERNUCLEAR DISTANCES TO A
C                       CONVV CONVERTS POTENTIAL VALUES TO KCAL.MOL
C                       R ARE INTERNUCL. DISTANCES IN THE ORDER RAC,RAB,rbc
C                       VV IS THE OUTPUT POTENTIAL VALUE IN DESIRED UNITs
C            THE ROUTINE PREPOT FOR READING POTENTIAL PARAMETERS
C            THE ROUTINE POTBO FOR CALCULATING BO POTENTIALS
C               AS A FUNCTION OF RAB,RBC,RAC (IN A). IT RETURNS AS V THE
C               VALUE OF THE POTENTIAL IN KCALMOL. THE ENERGY ZERO IS SEt
C               AT TOTAL DISSOCIATION.
C           THE SOUBROUTINE POL3 FOR CALCULATING THE BO POLYNOMIAL
C
C
      SUBROUTINE PREPOT
C
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/B21/ DE1,DE2,DE3,RE1,RE2,RE3
     >   ,B1,B2,B3
      COMMON/B23/COF(65),IE(65),JE(65),KE(65),NT
      DIMENSION COEF(65),IEXP(65),JEXP(65),KEXP(65)
      COMMON/B22/A11,A12,A13,A21,A22,A23,A31,A32,A33
     >   ,A14,A24,A34
      COMMON/B25/ PG
       COMMON/BTANH/REF1,REF2,REF3,SL1,SL2,SL3,R01,R02,R03
       COMMON/REPULS/A,N1,N2,N3
       COMMON/REFINE/AGB,CGBX,CGBY,CGBZ,AGW,CGWX,CGWY,CGWZ,
     >               XN0B,YN0B,ZN0B,XN0W,YN0W,ZN0W
C
       DATA COEF /
     *   0.9224052E+03,  0.3272860E+03,  0.3381706E+03, -0.1048643E+04,
     *  -0.2962569E+03, -0.3839140E+03, -0.7478068E+03, -0.7533124E+03,
     *  -0.1715470E+03, -0.2134407E+01,  0.5508481E+03,  0.1134382E+04,
     *  -0.1160042E+04,  0.3557390E+03,  0.1017086E+02,  0.6969160E+03,
     *   0.1258877E+04,  0.5197582E+02,  0.4328018E+03, -0.1062101E+03,
     *   0.5471942E+02, -0.3211568E+02, -0.1333515E+03, -0.4993582E+03,
     *  -0.3928734E+03,  0.1143333E+04, -0.2252141E+03,  0.2641766E+03,
     *   0.9964729E+02, -0.3031654E+03, -0.9919408E+03,  0.7123589E+02,
     *  -0.5026490E+03, -0.2019268E+03,  0.1085769E+03, -0.3689024E+02,
     *   0.1259192E+03,  0.1398510E+02, -0.5489551E+01, -0.2721741E+02,
     *  -0.8672702E+01,  0.9243725E+02,  0.9455495E+02,  0.4378558E+02,
     *  -0.3012895E+03,  0.5875433E+02, -0.7644942E+01, -0.1544242E+03,
     *   0.1174672E+01,  0.1881099E+02,  0.2699340E+03, -0.3803897E+02,
     *   0.2211474E+02,  0.1713199E+03,  0.6709845E+02, -0.2999395E+02,
     *   0.1285414E+02,  0.5457174E+02, -0.9745477E+02, -0.7197123E+01,
     *  -0.1377454E+01, -0.2120028E+02,  0.2071087E+02,  0.7003032E+00,
     *   0.4229195E+01/
       DATA IEXP/
     * 1, 1, 0, 2, 1, 2, 1, 0, 1, 0, 3, 2, 1, 3, 2, 1, 0, 2, 1, 0,
     * 1, 0, 4, 3, 2, 1, 4, 3, 2, 1, 0, 3, 2, 1, 0, 2, 1, 0, 1, 0,
     * 5, 4, 3, 2, 1, 5, 4, 3, 2, 1, 0, 4, 3, 2, 1, 0, 3, 2, 1, 0,
     * 2, 1, 0, 1, 0/
       DATA JEXP/
     * 1, 0, 1, 1, 2, 0, 1, 2, 0, 1, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2,
     * 0, 1, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 0, 1, 2, 0, 1,
     * 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 0, 1, 2, 3,
     * 0, 1, 2, 0, 1/
       DATA KEXP/
     * 0, 1, 1, 0, 0, 1, 1, 1, 2, 2, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2,
     * 3, 3, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4,
     * 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3,
     * 4, 4, 4, 5, 5/
C
       DATA VVRE,VVR0,EPS/1.0E-4,.01,.001/
*      DATA AGB,TAILBX,TAILBY,TAILBZ/-0.1,.111,.037,.078/
       DATA TAILBX,TAILBY,TAILBZ/.111,.037,.078/
*      DATA AGW,TAILWX,TAILWY,TAILWZ/-1.8,.088,.096,.042/
       DATA TAILWX,TAILWY,TAILWZ/.088,.096,.042/
       DATA X0B,Y0B,ANGB/1.6523,1.3715,72.5235/
       DATA X0W,Y0W,ANGW/1.8848,0.9363,106.929/
C
    2 FORMAT(I2)
    3 FORMAT(E15.7,3I3)
C
      PG=3.1415926536E0
C
C   DISSOCIATION ENERGIES
       DE1=137.59E0
       DE2=141.20E0
       DE3=58.E0
C
C   B PARAMETERS
C
       B1=.970755E0
       B2=2.194185
       B3=1.17086
C
C   EQUILIBRIUM DISTANCES
      RE1=1.563864E0
      RE2=0.916808E0
      RE3=1.5957E0
C
C    DIATOMIC PARAMETERS
       A11=2.30439E0
       A12=-2.30172E0
       A13=1.69028E0
       A14=-.69295E0
       A21=2.07810E0
       A22=-1.25667E0
       A23=0.27905E0
       A24=-0.10047E0
       A31=2.15123E0
       A32=-1.38219E0
       A33=0.31067E0
       A34=-.07972E0
C
C     FINDS THE LARGEST SHORT RANGE DISTANCE AT WHICH DIATOMIC
C     POTENTIALS ARE ZERO (R01,R02,R03)
      DO 1 I=1,3
       IF(I.NE.1)GOTO 63
       B=B1
       RE=RE1
       DE=DE1
       A1=A11
       A2=A12
       A3=A13
       A4=A14
       GOTO67
63     CONTINUE
       IF(I.NE.2)GOTO65
       B=B2
       RE=RE2
       DE=DE2
       A1=A21
        A2=A22
       A3=A23
        A4=A24
        GOTO 67
65      CONTINUE
       B=B3
       RE=RE3
       DE=DE3
       A1=A31
       A2=A32
       A3=A33
       A4=A34
67     CONTINUE
       DX=-.1
       X=RE-DX
70     CONTINUE
       X=X+DX
       EX=EXP(-B*(X-RE))
       V=-DE*EX*(A1+EX*(A2+EX*(A3+EX*A4)))
       IF(V.LT.0D0)GOTO70
       IF(ABS(DX).LT.0.0001)GOTO110
        X=X-DX
        DX=.1*DX
       GOTO70
110    CONTINUE
       IF(I.EQ.1)R01=X
       IF(I.EQ.2)R02=X
       IF(I.EQ.3)R03=X
1      CONTINUE
C
C    DETERMINES THE VALUE AT WHICH TANH HAS A FLEX
       REF1=.49*R01+.51*RE1
       REF2=.49*R02+.51*RE2
       REF3=.49*R03+.51*RE3
C
C   DETERMINES THE SLOPE OF TANH AT THE FLEX
       ALNEPS=LOG(1.0E0/EPS-1.0E0)
       SL1=.5E0*ALNEPS/(RE1-REF1)
       SL2=.5E0*ALNEPS/(RE2-REF2)
       SL3=.5E0*ALNEPS/(RE3-REF3)
C
C  DETERMINES THE PARAMETERS FOR THE ADDITIONAL REPULSIVE INNER
C  DIATOMIC TERM THE COEFFICIENT A AND THE POWERS N1,N2,N3
C  TERMS OF DIATOMS (GIVEN AS D*A*BO**N)
C
       A=VVRE
       N1=LOG(VVR0/VVRE)/(-B1*(R01-RE1))
       N2=LOG(VVR0/VVRE)/(-B2*(R02-RE2))
       N3=LOG(VVR0/VVRE)/(-B3*(R03-RE3))
C
C  CALCULATES THE PARAMETERS FOR GAUSSIAN BOND ORDER CORRECTIONS TO
C  THE BARRIER AND THE WELL
C
       GAMB=ANGB*PG/180.
       GAMW=ANGW*PG/180.
       Z0B=SQRT(X0B*X0B+Y0B*Y0B-2.*X0B*Y0B*COS(GAMB))
       Z0W=SQRT(X0W*X0W+Y0W*Y0W-2.*X0W*Y0W*COS(GAMW))
       XN0B=EXP(-B1*(X0B-RE1))
       YN0B=EXP(-B2*(Y0B-RE2))
       ZN0B=EXP(-B3*(Z0B-RE3))
       XN0W=EXP(-B1*(X0W-RE1))
       YN0W=EXP(-B2*(Y0W-RE2))
       ZN0W=EXP(-B3*(Z0W-RE3))
       CL1=LOG(.5)
       CGBX=-CL1/(TAILBX*TAILBX)
       CL1=LOG(.5)
       CGBY=-CL1/(TAILBY*TAILBY)
       CL1=LOG(.5)
       CGBZ=-CL1/(TAILBZ*TAILBZ)
       CL1=LOG(.5)
       CGWX=-CL1/(TAILWX*TAILWX)
       CL1=LOG(.5)
       CGWY=-CL1/(TAILWY*TAILWY)
       CL1=LOG(.5)
       CGWZ=-CL1/(TAILWZ*TAILWZ)
C
C  COEFFICIENTS FOR THE THREE-BODY TERM
C
       NT=65
      DO 10 I=1,NT
      COF(I)=COEF(I)
      IE(I)=1+IEXP(I)
      JE(I)=1+JEXP(I)
      KE(I)=1+KEXP(I)
C     WRITE(6,*)'PREPOT', COF(I),IE(I),JE(I),KE(I)
   10 CONTINUE
C
      RETURN
      END
C
      SUBROUTINE POTBO(RAB,RBC,RAC,V)
C
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/B21/ DE1,DE2,DE3,RE1,RE2,RE3
     >   ,B1,B2,B3
       COMMON/B22/A11,A12,A13,A21,A22,A23,A31,A32,A33,A14,A24,A34
       COMMON/BTANH/REF1,REF2,REF3,SL1,SL2,SL3,R01,R02,R03
       COMMON/REPULS/A,N1,N2,N3
       COMMON/REFINE/AGB,CGBX,CGBY,CGBZ,AGW,CGWX,CGWY,CGWZ,
     >               XN0B,YN0B,ZN0B,XN0W,YN0W,ZN0W
C
      X=RAB
      Y=RBC
      Z=RAC
C
      IF(X.LE.0.E0)X=1.E-4
      IF(Y.LE.0.E0)Y=1.E-4
      IF(Z.LE.0.E0)Z=1.E-4
C
C HYPERBOLIC TANGENT DAMPING FUNCTIONS
C
C      W1=.5E0*(1.0E0+TANH(SL1*(X-REF1)))
C      W2=.5E0*(1.0E0+TANH(SL2*(Y-REF2)))
C      W3=.5E0*(1.0E0+TANH(SL2*(Z-REF3)))
C      WW=W1*W2*W3
C
C  DIATOMIC TERMS AND THEIR CORRECTIONS
C
      V=0.
       EX=EXP(-B1*(X-RE1))
       EY=EXP(-B2*(Y-RE2))
       EZ=EXP(-B3*(Z-RE3))
       VXX=-DE1*EX*(A11+EX*(A12+EX*(A13+EX*A14)))
       VYY=-DE2*EY*(A21+EY*(A22+EY*(A23+EY*A24)))
       VZZ=-DE3*EZ*(A31+EZ*(A32+EZ*(A33+EZ*A34)))
       TX=DE1*EX**N1
       TY=DE2*EY**N2
       TZ=DE3*EZ**N3
       VXX=VXX+A*TX
       VYY=VYY+A*TY
       VZZ=VZZ+A*TZ
       VBIAT=VXX+VYY+VZZ
C       WRITE(6,*)N1,N2,N3,TX,TY,TX
C
C  THE TRIATOMIC TERM AND ITS CORRECTIONS
C
        CALL POL3(X,Y,Z,V3XYZ)
C
C      IF(X.LT.RE1)X=RE1
C      IF(Y.LT.RE2)Y=RE2
C      IF(Z.LT.RE3)Z=RE3
C      CALL POL3(X,Y,Z,V3RE)
C
C      VTRIAT=WW*V3XYZ+(1.E0-WW)*V3RE
       VCBAR=EXP(-CGBX*(EX-XN0B)*(EX-XN0B))*
     > EXP(-CGBY*(EY-YN0B)*(EY-YN0B))*
     > EXP(-CGBZ*(EZ-ZN0B)*(EZ-ZN0B))
       VCWEL=EXP(-CGWX*(EX-XN0W)*(EX-XN0W))*
     > EXP(-CGWY*(EY-YN0W)*(EY-YN0W))*
     > EXP(-CGWZ*(EZ-ZN0W)*(EZ-ZN0W))
       VTRIAT=.9935*V3XYZ + AGB*VCBAR + AGW*VCWEL
C      IF(VCBAR.GT..001.OR.VCWEL.GT..001)WRITE(6,*)AGB,VCBAR,AGW,VCWEL
       V=VBIAT+VTRIAT
       IF(V.GT.331.)V=331.0+LOG(V-331.0+1.0)
       VSHOW=V+DE2
C     WRITE(8,11)X,Y,Z,SX,SY,SZ,VXX,VYY,VZZ,VTRIAT,VSHOW
11      FORMAT(6F5.2,5F8.2)
      END
C
      SUBROUTINE POL3(X,Y,Z,V3)
      IMPLICIT REAL*8(A-H,O-Z)
C
       COMMON/B21/DE1,DE2,DE3,RE1,RE2,RE3,B1,B2,B3
      COMMON/B23/ COEF(65),IEXP(65),JEXP(65),KEXP(65),NT
      DIMENSION TX(6),TY(6),TZ(6)
C
      TX(1)=1.E0
      TY(1)=1.E0
      TZ(1)=1.E0
       EX=EXP(-B1*(X-RE1))
       EY=EXP(-B2*(Y-RE2))
       EZ=EXP(-B3*(Z-RE3))
      DO 10 I=2,6
      II=I-1
       TX(I)=TX(II)*EX
       TY(I)=TY(II)*EY
       TZ(I)=TZ(II)*EZ
   10 CONTINUE
      W=0.E0
      DO 20 I=1,NT
      IX=IEXP(I)
      JX=JEXP(I)
      KX=KEXP(I)
      W=W+COEF(I)*TX(IX)*TY(JX)*TZ(KX)
   20 CONTINUE
      V3=W
      RETURN
      END
***** IBM DEPENDENT BLOCK
      BLOCK DATA
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
       COMMON/REFINE/AGB,CGBX,CGBY,CGBZ,AGW,CGWX,CGWY,CGWZ,
     >               XN0B,YN0B,ZN0B,XN0W,YN0W,ZN0W
       DATA AGB/-0.1/
       DATA AGW/-1.8/
      END
