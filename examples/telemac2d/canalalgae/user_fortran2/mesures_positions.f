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
      INTEGER, PARAMETER:: NSOM=20
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
     &(NP_TOT,N_A,X_A,Y_A,TAG,LT,DT,DTREAL,NCSIZE,IPID, POLY_ID)
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
      INTEGER, INTENT(IN) :: POLY_ID
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
        NSOM1 = 11
        XSOM1(1) = 0.106795
        XSOM1(2) = 0.197477
        XSOM1(3) = 0.260000
        XSOM1(4) = 0.276044
        XSOM1(5) = 0.296971
        XSOM1(6) = 0.294034
        XSOM1(7) = 0.273841
        XSOM1(8) = 0.252547
        XSOM1(9) = 0.233456
        XSOM1(10) = 0.142040
        XSOM1(11) = 0.083666
!
        YSOM1(1) = 0.414802
        YSOM1(2) = 0.395344
        YSOM1(3) = 0.410000
        YSOM1(4) = 0.420677
        YSOM1(5) = 0.462163
        YSOM1(6) = 0.498876
        YSOM1(7) = 0.525310
        YSOM1(8) = 0.521638
        YSOM1(9) = 0.532285
        YSOM1(10) = 0.513194
        YSOM1(11) = 0.464366
!
        NSOM2 = 11
        XSOM2(1) = 0.235443
        XSOM2(2) = 0.275489
        XSOM2(3) = 0.320542
        XSOM2(4) = 0.390624
        XSOM2(5) = 0.450694
        XSOM2(6) = 0.545805
        XSOM2(7) = 0.580846
        XSOM2(8) = 0.603373
        XSOM2(9) = 0.570835
        XSOM2(10) = 0.468215
        XSOM2(11) = 0.313033
!
        YSOM2(1) = 0.553493
        YSOM2(2) = 0.500932
        YSOM2(3) = 0.495926
        YSOM2(4) = 0.523458
        YSOM2(5) = 0.533470
        YSOM2(6) = 0.550990
        YSOM2(7) = 0.591037
        YSOM2(8) = 0.636090
        YSOM2(9) = 0.666125
        YSOM2(10) = 0.663622
        YSOM2(11) = 0.638593
!
        NSOM3 = 12
        XSOM3(1) = 0.370601
        XSOM3(2) = 0.373103
        XSOM3(3) = 0.485735
        XSOM3(4) = 0.643419
        XSOM3(5) = 0.741033
        XSOM3(6) = 0.826133
        XSOM3(7) = 0.813618
        XSOM3(8) = 0.723513
        XSOM3(9) = 0.670952
        XSOM3(10) = 0.625899
        XSOM3(11) = 0.573337
        XSOM3(12) = 0.468215
!
        YSOM3(1) = 0.460885
        YSOM3(2) = 0.395809
        YSOM3(3) = 0.353259
        YSOM3(4) = 0.373283
        YSOM3(5) = 0.420838
        YSOM3(6) = 0.591037
        YSOM3(7) = 0.648605
        YSOM3(8) = 0.666125
        YSOM3(9) = 0.631084
        YSOM3(10) = 0.566008
        YSOM3(11) = 0.513447
        YSOM3(12) = 0.485914
!
        NSOM4 = 4
        XSOM4(1) = 0.175 - 0.05
        XSOM4(2) = 0.175 - 0.05
        XSOM4(3) = 0.175 + 0.05
        XSOM4(4) = 0.175 + 0.05
!
        YSOM4(1) = 0.850 - 0.05
        YSOM4(2) = 0.850 + 0.05
        YSOM4(3) = 0.850 + 0.05
        YSOM4(4) = 0.850 - 0.05
!
        F_MES_ALG='../polygon_particles.txt'
        IF(IPID.EQ.0)THEN
          WRITE(POLY_ID,'(A)')
     &    'Number of un-mobilised particles in polygon areas'
          WRITE(POLY_ID,'(A)')
     &    'Time, Area 1, Area 2, Area 3, Area 4'
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
          IF(INPOLY(X_A(I_A),Y_A(I_A),XSOM4,YSOM4,NSOM4)) THEN
            IF(PRESENCE(TAG(I_A),4).EQ.0)THEN
              N_4=N_4+1
              PRESENCE(TAG(I_A),4)=PRESENCE(TAG(I_A),4)+1
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
            PRESENCE(I_A,4)=P_MAX(PRESENCE(I_A,4))
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
      N_4=0
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
        IF(INPOLY(X_A(I_A),Y_A(I_A),XSOM4,YSOM4,NSOM4)) THEN
          IF(PRESENCE(TAG(I_A),4).EQ.1)THEN
            XDIF = ABS(X_A(I_A)-X_INI(TAG(I_A)))
            YDIF = ABS(Y_A(I_A)-Y_INI(TAG(I_A)))
            IF ((XDIF.LT.1D-12).AND.(YDIF.LT.1.D-12)) THEN
              N_4=N_4+1
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
        N_4_W=P_SUM(N_4)
      ELSE
        N_1_W=N_1
        N_2_W=N_2
        N_3_W=N_3
        N_4_W=N_4
      ENDIF
!
!-----------------------------------------------------------------------
! WRITE RESULTS
!-----------------------------------------------------------------------
!
      IF(IPID.EQ.0)THEN
        IF (40*(LT/40).EQ.LT) THEN
          WRITE(POLY_ID,'(1F10.2,1X,4(I0,1X))') LT*DT,
     &            N_1_W, N_2_W, N_3_W, N_4_W
        ENDIF
      END IF
!
      RETURN
      END SUBROUTINE MESURES_ALG
!
      END MODULE
