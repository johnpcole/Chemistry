      PROGRAM PRODUCTS

      IMPLICIT NONE

      INTEGER BIG_J       ! TOTAL ANGULAR MOMENTUM
      INTEGER BIG_K       ! ANGULAR MOMENTUM
      REAL*8 E_LIMIT      ! AMOUNT OF AVAILABLE ENERGY
      REAL*8 GRID         ! DELTA E OR INTEGRATION COARSENESS
      REAL*8 ROT_J        ! ROTATIONAL CONSTANT
      REAL*8 ROT_K        ! ROTATIONAL CONSTANT
      INTEGER INDEX       ! COUNTING FLAG
      INTEGER P,T         ! DATA TABLE FLAGS
      INTEGER PROBMAX     ! PROBABILITY DISTRIBUTION ARRAY SIZE
      REAL*8 ROTSTATE     ! ROTATIONAL DATA TABLE
      INTEGER STATEMAX    ! SIZE OF DATA TABLES
      REAL*8 VIBCONST     ! VIBRATIONAL CONSTANTS
      REAL*8 VIBSTATE     ! VIBRATIONAL DATA TABLE
      REAL*8 PROB_KIN     ! KINETIC PROBABILITY DISTRIBUTION
      REAL*8 PROB_ROT     ! ROTATIONAL PROBABILITY DISTRIBUTION
      REAL*8 PROB_VIB     ! VIBRATIONAL PROBABILITY DISTRIBUTION
      REAL*8 ORBCONST
      INTEGER GEOMAX
      INTEGER GEONUM
      integer thisrun,maxruns


      PARAMETER (STATEMAX=300000)             ! THE SIZE OF THE DATA TABLES
      PARAMETER (GRID=10.0)                   ! THE COARSENESS OF INTEGRATION
      PARAMETER (PROBMAX=500000)              ! PROBABILITY DISTRIBUTION ARRAY SIZE
      PARAMETER (P=-1,T=0)                    ! DATA TABLE FLAGS
      PARAMETER (GEOMAX=100)                  ! NUMBER OF POINTS ON MINIMUM ENERGY PATH

      DIMENSION VIBCONST(-1:GEOMAX,1:6)       ! VIBRATIONAL CONSTANTS
      DIMENSION VIBSTATE(-1:GEOMAX,0:STATEMAX)! VIBRATIONAL STATES TABLE
      DIMENSION ROTSTATE(-2:GEOMAX,0:STATEMAX)! ROTATIONAL STATES TABLE
      DIMENSION E_LIMIT(-1:GEOMAX)            ! AVAILABLE ENERGY DATA
      DIMENSION PROB_KIN(0:PROBMAX)           ! KINETIC PROBABILITY DISTRIBUTION
      DIMENSION PROB_ROT(0:PROBMAX)           ! ROTATIONAL PROBABILITY DISTRIBUTION
      DIMENSION PROB_VIB(0:PROBMAX)           ! VIBRATIONAL PROBABILITY DISTRIBUTION
      DIMENSION ORBCONST(0:GEOMAX)            ! ORBITAL CONSTANTS

      WRITE (*,'(A)') ' KINETIC PROGRAM'
      WRITE (*,'(A)') ' ---------------'

      open(48,FILE="RUNS.INP")
      read(48,*) maxruns

      do thisrun=1,maxruns
      read(48,*) big_j,big_k
      write (*,'(A,I3,A,I2,A,I2)')
     &                   'Run ',thisrun,':  J=',big_j,', K=',big_k


C----------------------------------------------------------------------------
C===== GET DATA =============================================================
C----------------------------------------------------------------------------
      CALL GET_DATA(VIBCONST,E_LIMIT,ROT_J,ROT_K,
     &                          BIG_J,BIG_K,ORBCONST,GEOMAX,GEONUM)
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



C----------------------------------------------------------------------------
C===== CREATE VIBRATIONAL DENSITY OF STATES ARRAY ===========================
C----------------------------------------------------------------------------
      CALL MAKE_VIB(VIBCONST,E_LIMIT,VIBSTATE,STATEMAX,GEOMAX,GEONUM)
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



C----------------------------------------------------------------------------
C===== CREATE ROTATIONAL DATA ARRAY =========================================
C----------------------------------------------------------------------------
      CALL MAKE_ROT(BIG_J,ROT_J,ROT_K,E_LIMIT,ROTSTATE,STATEMAX,
     &                                          ORBCONST,GEOMAX,GEONUM)
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



C----------------------------------------------------------------------------
C===== CREATE DISTRIBUTION ARRAYS ===========================================
C----------------------------------------------------------------------------
      CALL MAKE_RHO(VIBSTATE,ROTSTATE,STATEMAX,E_LIMIT,GRID,
     &                 PROBMAX,PROB_KIN,PROB_VIB,PROB_ROT,GEOMAX,GEONUM)
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



      if (thisrun.eq.1) OPEN (20,FILE=('RUN_1.CSV'))
      if (thisrun.eq.2) OPEN (20,FILE=('RUN_2.CSV'))
      if (thisrun.eq.3) OPEN (20,FILE=('RUN_3.CSV'))
      if (thisrun.eq.4) OPEN (20,FILE=('RUN_4.CSV'))
      if (thisrun.eq.5) OPEN (20,FILE=('RUN_5.CSV'))
      if (thisrun.eq.6) OPEN (20,FILE=('RUN_6.CSV'))
      if (thisrun.eq.7) OPEN (20,FILE=('RUN_7.CSV'))
      if (thisrun.eq.8) OPEN (20,FILE=('RUN_8.CSV'))
      if (thisrun.eq.9) OPEN (20,FILE=('RUN_9.CSV'))
      if (thisrun.eq.10) OPEN (20,FILE=('RUN_10.CSV'))
      if (thisrun.eq.11) OPEN (20,FILE=('RUN_11.CSV'))
      if (thisrun.eq.12) OPEN (20,FILE=('RUN_12.CSV'))
      if (thisrun.eq.13) OPEN (20,FILE=('RUN_13.CSV'))
      if (thisrun.eq.14) OPEN (20,FILE=('RUN_14.CSV'))
      if (thisrun.eq.15) OPEN (20,FILE=('RUN_15.CSV'))
      if (thisrun.eq.16) OPEN (20,FILE=('RUN_16.CSV'))
      if (thisrun.eq.17) OPEN (20,FILE=('RUN_17.CSV'))
      if (thisrun.eq.18) OPEN (20,FILE=('RUN_18.CSV'))
      if (thisrun.eq.19) OPEN (20,FILE=('RUN_19.CSV'))
      if (thisrun.eq.20) OPEN (20,FILE=('RUN_20.CSV'))
      if (thisrun.eq.21) OPEN (20,FILE=('RUN_21.CSV'))
      if (thisrun.eq.22) OPEN (20,FILE=('RUN_22.CSV'))
      if (thisrun.eq.23) OPEN (20,FILE=('RUN_23.CSV'))
      if (thisrun.eq.24) OPEN (20,FILE=('RUN_24.CSV'))
      if (thisrun.eq.25) OPEN (20,FILE=('RUN_25.CSV'))
      if (thisrun.eq.26) OPEN (20,FILE=('RUN_26.CSV'))
      if (thisrun.eq.27) OPEN (20,FILE=('RUN_27.CSV'))
      if (thisrun.eq.28) OPEN (20,FILE=('RUN_28.CSV'))
      if (thisrun.eq.29) OPEN (20,FILE=('RUN_29.CSV'))
      if (thisrun.eq.30) OPEN (20,FILE=('RUN_30.CSV'))
      if (thisrun.eq.31) OPEN (20,FILE=('RUN_31.CSV'))
      if (thisrun.eq.32) OPEN (20,FILE=('RUN_32.CSV'))
      if (thisrun.eq.33) OPEN (20,FILE=('RUN_33.CSV'))
      if (thisrun.eq.34) OPEN (20,FILE=('RUN_34.CSV'))
      if (thisrun.eq.35) OPEN (20,FILE=('RUN_35.CSV'))
      if (thisrun.eq.36) OPEN (20,FILE=('RUN_36.CSV'))
      if (thisrun.eq.37) OPEN (20,FILE=('RUN_37.CSV'))
      if (thisrun.eq.38) OPEN (20,FILE=('RUN_38.CSV'))
      if (thisrun.eq.39) OPEN (20,FILE=('RUN_39.CSV'))
      if (thisrun.eq.40) OPEN (20,FILE=('RUN_40.CSV'))
      if (thisrun.eq.41) OPEN (20,FILE=('RUN_41.CSV'))
      if (thisrun.eq.42) OPEN (20,FILE=('RUN_42.CSV'))
      if (thisrun.eq.43) OPEN (20,FILE=('RUN_43.CSV'))
      if (thisrun.eq.44) OPEN (20,FILE=('RUN_44.CSV'))
      if (thisrun.eq.45) OPEN (20,FILE=('RUN_45.CSV'))
      if (thisrun.eq.46) OPEN (20,FILE=('RUN_46.CSV'))
      if (thisrun.eq.47) OPEN (20,FILE=('RUN_47.CSV'))
      if (thisrun.eq.48) OPEN (20,FILE=('RUN_48.CSV'))
      if (thisrun.eq.49) OPEN (20,FILE=('RUN_49.CSV'))
      if (thisrun.eq.50) OPEN (20,FILE=('RUN_50.CSV'))
      if (thisrun.eq.51) OPEN (20,FILE=('RUN_51.CSV'))
      if (thisrun.eq.52) OPEN (20,FILE=('RUN_52.CSV'))
      if (thisrun.eq.53) OPEN (20,FILE=('RUN_53.CSV'))
      if (thisrun.eq.54) OPEN (20,FILE=('RUN_54.CSV'))
      if (thisrun.eq.55) OPEN (20,FILE=('RUN_55.CSV'))
      if (thisrun.eq.56) OPEN (20,FILE=('RUN_56.CSV'))
      if (thisrun.eq.57) OPEN (20,FILE=('RUN_57.CSV'))
      if (thisrun.eq.58) OPEN (20,FILE=('RUN_58.CSV'))
      if (thisrun.eq.59) OPEN (20,FILE=('RUN_59.CSV'))
      if (thisrun.eq.60) OPEN (20,FILE=('RUN_60.CSV'))
      if (thisrun.eq.61) OPEN (20,FILE=('RUN_61.CSV'))
      if (thisrun.eq.62) OPEN (20,FILE=('RUN_62.CSV'))
      if (thisrun.eq.63) OPEN (20,FILE=('RUN_63.CSV'))
      if (thisrun.eq.64) OPEN (20,FILE=('RUN_64.CSV'))
      if (thisrun.eq.65) OPEN (20,FILE=('RUN_65.CSV'))
      if (thisrun.eq.66) OPEN (20,FILE=('RUN_66.CSV'))
      if (thisrun.eq.67) OPEN (20,FILE=('RUN_67.CSV'))
      if (thisrun.eq.68) OPEN (20,FILE=('RUN_68.CSV'))
      if (thisrun.eq.69) OPEN (20,FILE=('RUN_69.CSV'))
      if (thisrun.eq.70) OPEN (20,FILE=('RUN_70.CSV'))
      if (thisrun.eq.71) OPEN (20,FILE=('RUN_71.CSV'))
      if (thisrun.eq.72) OPEN (20,FILE=('RUN_72.CSV'))
      if (thisrun.eq.73) OPEN (20,FILE=('RUN_73.CSV'))
      if (thisrun.eq.74) OPEN (20,FILE=('RUN_74.CSV'))
      if (thisrun.eq.75) OPEN (20,FILE=('RUN_75.CSV'))
      if (thisrun.eq.76) OPEN (20,FILE=('RUN_76.CSV'))
      if (thisrun.eq.77) OPEN (20,FILE=('RUN_77.CSV'))
      if (thisrun.eq.78) OPEN (20,FILE=('RUN_78.CSV'))
      if (thisrun.eq.79) OPEN (20,FILE=('RUN_79.CSV'))
      if (thisrun.eq.80) OPEN (20,FILE=('RUN_80.CSV'))
      if (thisrun.eq.81) OPEN (20,FILE=('RUN_81.CSV'))
      if (thisrun.eq.82) OPEN (20,FILE=('RUN_82.CSV'))
      if (thisrun.eq.83) OPEN (20,FILE=('RUN_83.CSV'))
      if (thisrun.eq.84) OPEN (20,FILE=('RUN_84.CSV'))
      if (thisrun.eq.85) OPEN (20,FILE=('RUN_85.CSV'))
      if (thisrun.eq.86) OPEN (20,FILE=('RUN_86.CSV'))
      if (thisrun.eq.87) OPEN (20,FILE=('RUN_87.CSV'))
      if (thisrun.eq.88) OPEN (20,FILE=('RUN_88.CSV'))
      if (thisrun.eq.89) OPEN (20,FILE=('RUN_89.CSV'))
      if (thisrun.eq.90) OPEN (20,FILE=('RUN_90.CSV'))
      if (thisrun.eq.91) OPEN (20,FILE=('RUN_91.CSV'))
      if (thisrun.eq.92) OPEN (20,FILE=('RUN_92.CSV'))
      if (thisrun.eq.93) OPEN (20,FILE=('RUN_93.CSV'))
      if (thisrun.eq.94) OPEN (20,FILE=('RUN_94.CSV'))
      if (thisrun.eq.95) OPEN (20,FILE=('RUN_95.CSV'))
      if (thisrun.eq.96) OPEN (20,FILE=('RUN_96.CSV'))
      if (thisrun.eq.97) OPEN (20,FILE=('RUN_97.CSV'))
      if (thisrun.eq.98) OPEN (20,FILE=('RUN_98.CSV'))
      if (thisrun.eq.99) OPEN (20,FILE=('RUN_99.CSV'))
      if (thisrun.eq.100) OPEN (20,FILE=('RUN_100.CSV'))
      if (thisrun.eq.101) OPEN (20,FILE=('RUN_101.CSV'))
      if (thisrun.eq.102) OPEN (20,FILE=('RUN_102.CSV'))
      if (thisrun.eq.103) OPEN (20,FILE=('RUN_103.CSV'))
      if (thisrun.eq.104) OPEN (20,FILE=('RUN_104.CSV'))
      if (thisrun.eq.105) OPEN (20,FILE=('RUN_105.CSV'))
      if (thisrun.eq.106) OPEN (20,FILE=('RUN_106.CSV'))
      if (thisrun.eq.107) OPEN (20,FILE=('RUN_107.CSV'))
      if (thisrun.eq.108) OPEN (20,FILE=('RUN_108.CSV'))
      if (thisrun.eq.109) OPEN (20,FILE=('RUN_109.CSV'))
      if (thisrun.eq.110) OPEN (20,FILE=('RUN_110.CSV'))
      if (thisrun.eq.111) OPEN (20,FILE=('RUN_111.CSV'))
      if (thisrun.eq.112) OPEN (20,FILE=('RUN_112.CSV'))
      if (thisrun.eq.113) OPEN (20,FILE=('RUN_113.CSV'))
      if (thisrun.eq.114) OPEN (20,FILE=('RUN_114.CSV'))
      if (thisrun.eq.115) OPEN (20,FILE=('RUN_115.CSV'))
      if (thisrun.eq.116) OPEN (20,FILE=('RUN_116.CSV'))
      if (thisrun.eq.117) OPEN (20,FILE=('RUN_117.CSV'))
      if (thisrun.eq.118) OPEN (20,FILE=('RUN_118.CSV'))
      if (thisrun.eq.119) OPEN (20,FILE=('RUN_119.CSV'))
      if (thisrun.eq.120) OPEN (20,FILE=('RUN_120.CSV'))
      if (thisrun.eq.121) OPEN (20,FILE=('RUN_121.CSV'))
      if (thisrun.eq.122) OPEN (20,FILE=('RUN_122.CSV'))
      if (thisrun.eq.123) OPEN (20,FILE=('RUN_123.CSV'))
      if (thisrun.eq.124) OPEN (20,FILE=('RUN_124.CSV'))
      if (thisrun.eq.125) OPEN (20,FILE=('RUN_125.CSV'))
      if (thisrun.eq.126) OPEN (20,FILE=('RUN_126.CSV'))
      if (thisrun.eq.127) OPEN (20,FILE=('RUN_127.CSV'))
      if (thisrun.eq.128) OPEN (20,FILE=('RUN_128.CSV'))
      if (thisrun.eq.129) OPEN (20,FILE=('RUN_129.CSV'))
      if (thisrun.eq.130) OPEN (20,FILE=('RUN_130.CSV'))
      if (thisrun.eq.131) OPEN (20,FILE=('RUN_131.CSV'))
      if (thisrun.eq.132) OPEN (20,FILE=('RUN_132.CSV'))
      if (thisrun.eq.133) OPEN (20,FILE=('RUN_133.CSV'))
      if (thisrun.eq.134) OPEN (20,FILE=('RUN_134.CSV'))
      if (thisrun.eq.135) OPEN (20,FILE=('RUN_135.CSV'))
      if (thisrun.eq.136) OPEN (20,FILE=('RUN_136.CSV'))
      if (thisrun.eq.137) OPEN (20,FILE=('RUN_137.CSV'))
      if (thisrun.eq.138) OPEN (20,FILE=('RUN_138.CSV'))
      if (thisrun.eq.139) OPEN (20,FILE=('RUN_139.CSV'))
      if (thisrun.eq.140) OPEN (20,FILE=('RUN_140.CSV'))
      if (thisrun.eq.141) OPEN (20,FILE=('RUN_141.CSV'))
      if (thisrun.eq.142) OPEN (20,FILE=('RUN_142.CSV'))
      if (thisrun.eq.143) OPEN (20,FILE=('RUN_143.CSV'))
      if (thisrun.eq.144) OPEN (20,FILE=('RUN_144.CSV'))
      if (thisrun.eq.145) OPEN (20,FILE=('RUN_145.CSV'))
      if (thisrun.eq.146) OPEN (20,FILE=('RUN_146.CSV'))
      if (thisrun.eq.147) OPEN (20,FILE=('RUN_147.CSV'))
      if (thisrun.eq.148) OPEN (20,FILE=('RUN_148.CSV'))
      if (thisrun.eq.149) OPEN (20,FILE=('RUN_149.CSV'))
      if (thisrun.eq.150) OPEN (20,FILE=('RUN_150.CSV'))
      if (thisrun.eq.151) OPEN (20,FILE=('RUN_151.CSV'))
      if (thisrun.eq.152) OPEN (20,FILE=('RUN_152.CSV'))
      if (thisrun.eq.153) OPEN (20,FILE=('RUN_153.CSV'))
      if (thisrun.eq.154) OPEN (20,FILE=('RUN_154.CSV'))
      if (thisrun.eq.155) OPEN (20,FILE=('RUN_155.CSV'))
      if (thisrun.eq.156) OPEN (20,FILE=('RUN_156.CSV'))
      if (thisrun.eq.157) OPEN (20,FILE=('RUN_157.CSV'))
      if (thisrun.eq.158) OPEN (20,FILE=('RUN_158.CSV'))
      if (thisrun.eq.159) OPEN (20,FILE=('RUN_159.CSV'))
      if (thisrun.eq.160) OPEN (20,FILE=('RUN_160.CSV'))
      if (thisrun.eq.161) OPEN (20,FILE=('RUN_161.CSV'))
      if (thisrun.eq.162) OPEN (20,FILE=('RUN_162.CSV'))
      if (thisrun.eq.163) OPEN (20,FILE=('RUN_163.CSV'))
      if (thisrun.eq.164) OPEN (20,FILE=('RUN_164.CSV'))
      if (thisrun.eq.165) OPEN (20,FILE=('RUN_165.CSV'))
      if (thisrun.eq.166) OPEN (20,FILE=('RUN_166.CSV'))
      if (thisrun.eq.167) OPEN (20,FILE=('RUN_167.CSV'))
      if (thisrun.eq.168) OPEN (20,FILE=('RUN_168.CSV'))
      if (thisrun.eq.169) OPEN (20,FILE=('RUN_169.CSV'))
      if (thisrun.eq.170) OPEN (20,FILE=('RUN_170.CSV'))
      if (thisrun.eq.171) OPEN (20,FILE=('RUN_171.CSV'))
      if (thisrun.eq.172) OPEN (20,FILE=('RUN_172.CSV'))
      if (thisrun.eq.173) OPEN (20,FILE=('RUN_173.CSV'))
      if (thisrun.eq.174) OPEN (20,FILE=('RUN_174.CSV'))
      if (thisrun.eq.175) OPEN (20,FILE=('RUN_175.CSV'))
      if (thisrun.eq.176) OPEN (20,FILE=('RUN_176.CSV'))
      if (thisrun.eq.177) OPEN (20,FILE=('RUN_177.CSV'))
      if (thisrun.eq.178) OPEN (20,FILE=('RUN_178.CSV'))
      if (thisrun.eq.179) OPEN (20,FILE=('RUN_179.CSV'))
      if (thisrun.eq.180) OPEN (20,FILE=('RUN_180.CSV'))
      if (thisrun.eq.181) OPEN (20,FILE=('RUN_181.CSV'))
      if (thisrun.eq.182) OPEN (20,FILE=('RUN_182.CSV'))
      if (thisrun.eq.183) OPEN (20,FILE=('RUN_183.CSV'))
      if (thisrun.eq.184) OPEN (20,FILE=('RUN_184.CSV'))
      if (thisrun.eq.185) OPEN (20,FILE=('RUN_185.CSV'))
      if (thisrun.eq.186) OPEN (20,FILE=('RUN_186.CSV'))
      if (thisrun.eq.187) OPEN (20,FILE=('RUN_187.CSV'))
      if (thisrun.eq.188) OPEN (20,FILE=('RUN_188.CSV'))
      if (thisrun.eq.189) OPEN (20,FILE=('RUN_189.CSV'))
      if (thisrun.eq.190) OPEN (20,FILE=('RUN_190.CSV'))
      if (thisrun.eq.191) OPEN (20,FILE=('RUN_191.CSV'))
      if (thisrun.eq.192) OPEN (20,FILE=('RUN_192.CSV'))
      if (thisrun.eq.193) OPEN (20,FILE=('RUN_193.CSV'))
      if (thisrun.eq.194) OPEN (20,FILE=('RUN_194.CSV'))
      if (thisrun.eq.195) OPEN (20,FILE=('RUN_195.CSV'))
      if (thisrun.eq.196) OPEN (20,FILE=('RUN_196.CSV'))
      if (thisrun.eq.197) OPEN (20,FILE=('RUN_197.CSV'))
      if (thisrun.eq.198) OPEN (20,FILE=('RUN_198.CSV'))
      if (thisrun.eq.199) OPEN (20,FILE=('RUN_199.CSV'))
      if (thisrun.eq.200) OPEN (20,FILE=('RUN_200.CSV'))

      WRITE (20,'(A)')              'ENERGY,KIN-PROB,VIB-PROB,ROT-PROB'
      DO INDEX=0,MIN(INT(E_LIMIT(P)/GRID)+10,PROBMAX)
         WRITE (20,'(F15.5,3(A,E20.13))')
     &                     DFLOAT(INDEX)*GRID,',',PROB_KIN(INDEX),',',
     &                        PROB_VIB(INDEX),',',PROB_ROT(INDEX)
         END DO

      CLOSE(20)


      end do

      END

