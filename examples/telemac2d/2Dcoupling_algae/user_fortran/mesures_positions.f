!
      MODULE MESURES_POSITIONS
!**********************************************************************
! DECLARATION OF VARIABLES USED TO RECORD RESULTS
!**********************************************************************
! RESULT FILES
      CHARACTER (LEN=50) :: F_MES_ALG
      LOGICAL :: INIT_MES
      DATA INIT_MES /.TRUE./
! COORDINATES OF THE POLYGONS
      INTEGER:: NSOM1, NSOM2, NSOM3, NSOM4
      INTEGER, PARAMETER:: NSOM=40
      DOUBLE PRECISION:: XSOM1(NSOM), YSOM1(NSOM)
      DOUBLE PRECISION:: XSOM2(NSOM), YSOM2(NSOM)
      DOUBLE PRECISION:: XSOM3(NSOM), YSOM3(NSOM)
      DOUBLE PRECISION:: XSOM4(NSOM), YSOM4(NSOM)
! RECORDED VARIABLES IN EACH POLYGON
      INTEGER:: N_1
      INTEGER:: N_2
      INTEGER:: N_3
      INTEGER:: N_4
      INTEGER,DIMENSION(:,:),ALLOCATABLE:: PRESENCE
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: X_INI
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: Y_INI
!
      CONTAINS
!                       **********************
                        SUBROUTINE MESURES_ALG
!                       **********************
!
     &(NP_TOT,N_A,X_A,Y_A,TAG,LT,DT,DTREAL,NCSIZE,IPID,POLY_ID)
!
!***********************************************************************
!
! SUBROUTINE USED TO RECORD PARTICLE POSITIONS
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
! APPELE PAR : TELMAC
!
! SOUS-PROGRAMME APPELE : NEANT
!
!***********************************************************************
!
!
      USE BIEF, ONLY : INPOLY
      USE INTERFACE_PARALLEL, ONLY : P_SUM, P_MAX
      IMPLICIT NONE
! INPUT VARIABLES
      INTEGER         ,INTENT(IN)     :: NP_TOT,N_A,NCSIZE,IPID
      DOUBLE PRECISION,INTENT(IN)     :: X_A(NP_TOT)
      DOUBLE PRECISION,INTENT(IN)     :: Y_A(NP_TOT)
      INTEGER         ,INTENT(IN)     :: TAG(NP_TOT)
      INTEGER         ,INTENT(IN)     :: LT
      DOUBLE PRECISION,INTENT(IN)     :: DT
      DOUBLE PRECISION,INTENT(IN)     :: DTREAL
      INTEGER, INTENT(IN)             :: POLY_ID
! VARIABLES FOR LOOPS
      INTEGER                         :: I_A
      INTEGER                         :: I_Q
! INTERMEDIATE VARIABLES
      DOUBLE PRECISION:: XDIF,YDIF
! VARIABLES USED TO WRITE RESULT FILES
      INTEGER:: N_1_W
      INTEGER:: N_2_W
      INTEGER:: N_3_W
      INTEGER:: N_4_W
!
!-----------------------------------------------------------------------
! DEFINE THE QUARTERS
!-----------------------------------------------------------------------
! AT THE FIRST TIME STEP
      IF(INIT_MES)THEN
        INIT_MES=.FALSE.
! DEFINE THE AREAS OF MEASUREMENT
!
        NSOM1 = 15
        XSOM1(1)  = 312.541984
        XSOM1(2)  = 336.445120
        XSOM1(3)  = 379.470764
        XSOM1(4)  = 417.715782
        XSOM1(5)  = 451.180172
        XSOM1(6)  = 469.107524
        XSOM1(7)  = 473.290573
        XSOM1(8)  = 463.131740
        XSOM1(9)  = 419.508517
        XSOM1(10) = 378.873186
        XSOM1(11) = 350.787001
        XSOM1(12) = 325.688708
        XSOM1(13) = 309.554092
        XSOM1(14) = 301.187994
        XSOM1(15) = 304.773465
!
        YSOM1(1)  = 97.651361
        YSOM1(2)  = 94.065890
        YSOM1(3)  = 95.261047
        YSOM1(4)  = 101.236831
        YSOM1(5)  = 114.981134
        YSOM1(6)  = 128.725438
        YSOM1(7)  = 156.811622
        YSOM1(8)  = 184.897807
        YSOM1(9)  = 192.068748
        YSOM1(10) = 192.666326
        YSOM1(11) = 186.092964
        YSOM1(12) = 176.531709
        YSOM1(13) = 165.775298
        YSOM1(14) = 148.445525
        YSOM1(15) = 125.139967
!
        NSOM2 = 31
        XSOM2(1)  = 478.668778
        XSOM2(2)  = 426.679458
        XSOM2(3)  = 400.386008
        XSOM2(4)  = 395.605381
        XSOM2(5)  = 400.386008
        XSOM2(6)  = 418.313360
        XSOM2(7)  = 445.801966
        XSOM2(8)  = 467.912367
        XSOM2(9)  = 490.620346
        XSOM2(10) = 512.730747
        XSOM2(11) = 536.036304
        XSOM2(12) = 564.720067
        XSOM2(13) = 579.659527
        XSOM2(14) = 598.184458
        XSOM2(15) = 624.477907
        XSOM2(16) = 656.747140
        XSOM2(17) = 671.089022
        XSOM2(18) = 684.833325
        XSOM2(19) = 691.406687
        XSOM2(20) = 694.992158
        XSOM2(21) = 694.992158
        XSOM2(22) = 694.394579
        XSOM2(23) = 686.626060
        XSOM2(24) = 660.332611
        XSOM2(25) = 610.136025
        XSOM2(26) = 576.074057
        XSOM2(27) = 552.768499
        XSOM2(28) = 533.645991
        XSOM2(29) = 525.877472
        XSOM2(30) = 518.108952
        XSOM2(31) = 494.205816
!
        YSOM2(1)  = 117.969026
        YSOM2(2)  = 93.468312
        YSOM2(3)  = 84.504636
        YSOM2(4)  = 70.760333
        YSOM2(5)  = 60.601500
        YSOM2(6)  = 54.028138
        YSOM2(7)  = 55.223295
        YSOM2(8)  = 58.808765
        YSOM2(9)  = 70.760333
        YSOM2(10) = 86.894950
        YSOM2(11) = 101.236831
        YSOM2(12) = 120.359340
        YSOM2(13) = 133.506065
        YSOM2(14) = 139.481849
        YSOM2(15) = 138.884270
        YSOM2(16) = 139.481849
        YSOM2(17) = 140.677005
        YSOM2(18) = 144.262476
        YSOM2(19) = 150.238260
        YSOM2(20) = 164.580141
        YSOM2(21) = 179.519601
        YSOM2(22) = 184.897807
        YSOM2(23) = 189.678434
        YSOM2(24) = 193.861483
        YSOM2(25) = 191.471169
        YSOM2(26) = 190.276012
        YSOM2(27) = 186.092964
        YSOM2(28) = 176.531709
        YSOM2(29) = 161.592249
        YSOM2(30) = 145.457633
        YSOM2(31) = 129.323016
!
        NSOM3 = 10
        XSOM3(1)  = 758.335468
        XSOM3(2)  = 781.641025
        XSOM3(3)  = 794.787750
        XSOM3(4)  = 792.397437
        XSOM3(5)  = 776.262820
        XSOM3(6)  = 758.933046
        XSOM3(7)  = 734.432332
        XSOM3(8)  = 727.858970
        XSOM3(9)  = 731.444440
        XSOM3(10) = 742.200851
!
        YSOM3(1)  = 49.247511
        YSOM3(2)  = 55.820873
        YSOM3(3)  = 68.967598
        YSOM3(4)  = 85.699793
        YSOM3(5)  = 93.468312
        YSOM3(6)  = 92.870734
        YSOM3(7)  = 85.102215
        YSOM3(8)  = 74.345803
        YSOM3(9)  = 61.796657
        YSOM3(10) = 49.845089
!
        F_MES_ALG='../polygon_particles.txt'
        IF(IPID.EQ.0)THEN
          WRITE(POLY_ID,'(A)')
     &    'Number of un-mobilised particles in polygon areas'
          WRITE(POLY_ID, '(A)')
     &    'Time, Area 1, Area 2, Area 3'
        ENDIF
!
! INITIALISE THE VARIABLES
        N_1=0
        N_2=0
        N_3=0
        N_4=0
        IF(ALLOCATED(PRESENCE))DEALLOCATE(PRESENCE)
        IF(ALLOCATED(X_INI))DEALLOCATE(X_INI)
        IF(ALLOCATED(Y_INI))DEALLOCATE(Y_INI)
        ALLOCATE(PRESENCE(NP_TOT,4))
        ALLOCATE(X_INI(NP_TOT))
        ALLOCATE(Y_INI(NP_TOT))
        DO I_A=1,NP_TOT
          DO I_Q=1,4
            PRESENCE(I_A,I_Q)=0
          END DO
          X_INI(I_A) = -1.D10
          Y_INI(I_A) = -1.D10
        END DO
!-----------------------------------------------------------------------
! CALCULATE THE VARIABLES FOR EACH QUARTER (TIME ZERO)
!-----------------------------------------------------------------------
!
        DO I_A=1,N_A
          IF(INPOLY(X_A(I_A),Y_A(I_A),XSOM1,YSOM1,NSOM1)) THEN
            IF(PRESENCE(TAG(I_A),1).EQ.0)THEN
              N_1=N_1+1
              PRESENCE(TAG(I_A),1)=PRESENCE(TAG(I_A),1)+1
            END IF
          END IF
!
          IF(INPOLY(X_A(I_A),Y_A(I_A),XSOM2,YSOM2,NSOM2)) THEN
            IF(PRESENCE(TAG(I_A),2).EQ.0)THEN
              N_2=N_2+1
              PRESENCE(TAG(I_A),2)=PRESENCE(TAG(I_A),2)+1
            END IF
          END IF
!
          IF(INPOLY(X_A(I_A),Y_A(I_A),XSOM3,YSOM3,NSOM3)) THEN
            IF(PRESENCE(TAG(I_A),3).EQ.0)THEN
              N_3=N_3+1
              PRESENCE(TAG(I_A),3)=PRESENCE(TAG(I_A),3)+1
            END IF
          END IF
!
          X_INI(TAG(I_A))=X_A(I_A)
          Y_INI(TAG(I_A))=Y_A(I_A)
!
        END DO
!-----------------------------------------------------------------------
! COMMUNICATE RESULTS FOUND IN EACH PROCESSOR (TIME ZERO)
!-----------------------------------------------------------------------
!
        IF(NCSIZE.GT.0)THEN
          DO I_A=1,NP_TOT
            PRESENCE(I_A,1)=P_MAX(PRESENCE(I_A,1))
            PRESENCE(I_A,2)=P_MAX(PRESENCE(I_A,2))
            PRESENCE(I_A,3)=P_MAX(PRESENCE(I_A,3))
            X_INI(I_A)=P_MAX(X_INI(I_A))
            Y_INI(I_A)=P_MAX(Y_INI(I_A))
          END DO
        END IF
!
      END IF
!
!-----------------------------------------------------------------------
! THE REST IS DONE EVERY TIME STEP
!-----------------------------------------------------------------------
! CHECK FOR PARTICLES THAT WERE IN QUADRANTS INITIALLY
!-----------------------------------------------------------------------
      N_1=0
      N_2=0
      N_3=0
!
      DO I_A=1,N_A
        IF(INPOLY(X_A(I_A),Y_A(I_A),XSOM1,YSOM1,NSOM1)) THEN
          IF(PRESENCE(TAG(I_A),1).EQ.1)THEN
            XDIF = ABS(X_A(I_A)-X_INI(TAG(I_A)))
            YDIF = ABS(Y_A(I_A)-Y_INI(TAG(I_A)))
            IF ((XDIF.LT.1D-12).AND.(YDIF.LT.1.D-12)) THEN
              N_1=N_1+1
            ENDIF
          END IF
        END IF
        IF(INPOLY(X_A(I_A),Y_A(I_A),XSOM2,YSOM2,NSOM2)) THEN
          IF(PRESENCE(TAG(I_A),2).EQ.1)THEN
            XDIF = ABS(X_A(I_A)-X_INI(TAG(I_A)))
            YDIF = ABS(Y_A(I_A)-Y_INI(TAG(I_A)))
            IF ((XDIF.LT.1D-12).AND.(YDIF.LT.1.D-12)) THEN
              N_2=N_2+1
            ENDIF
          END IF
        END IF
!
        IF(INPOLY(X_A(I_A),Y_A(I_A),XSOM3,YSOM3,NSOM3)) THEN
          IF(PRESENCE(TAG(I_A),3).EQ.1)THEN
            XDIF = ABS(X_A(I_A)-X_INI(TAG(I_A)))
            YDIF = ABS(Y_A(I_A)-Y_INI(TAG(I_A)))
            IF ((XDIF.LT.1D-12).AND.(YDIF.LT.1.D-12)) THEN
              N_3=N_3+1
            ENDIF
          END IF
        END IF
!
      END DO
!-----------------------------------------------------------------------
! CALCULATE THE VARIABLES USED FOR THE RESULT FILE
!-----------------------------------------------------------------------
! IN PARALLEL
      IF(NCSIZE.GT.0)THEN
        N_1_W=P_SUM(N_1)
        N_2_W=P_SUM(N_2)
        N_3_W=P_SUM(N_3)
      ELSE
        N_1_W=N_1
        N_2_W=N_2
        N_3_W=N_3
      ENDIF
!
!-----------------------------------------------------------------------
! WRITE RESULTS
!-----------------------------------------------------------------------
!
      IF(IPID.EQ.0)THEN
        IF (1*(LT/1).EQ.LT) THEN
          WRITE(POLY_ID,'(1F10.2,1X,3(I0,1X))') LT*DT,
     &           N_1_W, N_2_W, N_3_W
        ENDIF
      END IF
!
      RETURN
      END SUBROUTINE MESURES_ALG
!
      END MODULE
