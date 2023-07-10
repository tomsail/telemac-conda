      MODULE MESURES_POSITIONS
!**********************************************************************
! DECLARATION OF VARIABLES USED TO RECORD RESULTS
!**********************************************************************
! PARAMETRES USED TO DESCRIBE THE RECORDED CASE
        USE DECLARATIONS_TELEMAC2D, ONLY : T2D_FILES, T2DRF1, T2DRF2,
     &                              T2DRF3, T2DRF4, T2DRF5, T2DRF6
      INTEGER,PARAMETER  :: MESURE=2
      INTEGER,PARAMETER  :: LACHE=2
      INTEGER,PARAMETER  :: VIT=1
! TO DEFINE THE INITIAL PARTICLE POSITION
      DOUBLE PRECISION:: X_INI
      DOUBLE PRECISION:: Y_INI
! RESULT FILES
      CHARACTER (LEN=50) :: F_PROF_VIT
      LOGICAL :: INIT_MES
      DATA INIT_MES /.TRUE./
      LOGICAL :: INIT_PROF
      DATA INIT_PROF /.TRUE./
! POSITIONS OF THE QUADRANTS
      DOUBLE PRECISION:: X_HD_1
      DOUBLE PRECISION:: X_HD_2
      DOUBLE PRECISION:: Y_HD_1
      DOUBLE PRECISION:: Y_HD_2
!
      DOUBLE PRECISION:: X_BD_1
      DOUBLE PRECISION:: X_BD_2
      DOUBLE PRECISION:: Y_BD_1
      DOUBLE PRECISION:: Y_BD_2
!
      DOUBLE PRECISION:: X_HG_1
      DOUBLE PRECISION:: X_HG_2
      DOUBLE PRECISION:: Y_HG_1
      DOUBLE PRECISION:: Y_HG_2
!
      DOUBLE PRECISION:: X_BG_1
      DOUBLE PRECISION:: X_BG_2
      DOUBLE PRECISION:: Y_BG_1
      DOUBLE PRECISION:: Y_BG_2
! RECORDED VARIABLES IN EACH QUADRANT
      INTEGER:: N_HD
      INTEGER:: N_BD
      INTEGER:: N_HG
      INTEGER:: N_BG
      INTEGER,DIMENSION(:,:),ALLOCATABLE:: PRESENCE
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: RESID
      DOUBLE PRECISION:: T_START
      DOUBLE PRECISION:: T_END
      INTEGER:: N_TOT
      LOGICAL:: YES_START
! VARIABLES USED TO RECORD VELOCITY PROFILES
      DOUBLE PRECISION                   ::X_PROF
      INTEGER         ,PARAMETER         ::NY=10
      DOUBLE PRECISION                   ::Y_PROF(NY)
      DOUBLE PRECISION                   ::DY_PROF
      INTEGER                            ::N_PROF(NY)
      DOUBLE PRECISION                   ::UX_PROF(NY)
      DOUBLE PRECISION                   ::UY_PROF(NY)
      DOUBLE PRECISION                   ::VX_PROF(NY)
      DOUBLE PRECISION                   ::VY_PROF(NY)
! VARIABLES USED TO WRITE THE RESULTS
      INTEGER                            ::N_WRI(NY)
      DOUBLE PRECISION                   ::UX_WRI(NY)
      DOUBLE PRECISION                   ::UY_WRI(NY)
      DOUBLE PRECISION                   ::VX_WRI(NY)
      DOUBLE PRECISION                   ::VY_WRI(NY)
      INTEGER :: FILE_ID

      CONTAINS

!                       **********************
                        SUBROUTINE MESURES_ALG
!                       **********************
!
     &(NP_TOT,N_A,X_A,Y_A,TAG,LT,DT,DTREAL,NCSIZE,IPID)
!
!***********************************************************************
!  ANTOINE JOLY
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
! VARIABLES FOR LOOPS
      INTEGER                         :: I_A
      INTEGER                         :: I_Q
! VARIABLES USED TO WRITE RESULT FILES
      INTEGER:: N_HD_W
      INTEGER:: N_BD_W
      INTEGER:: N_HG_W
      INTEGER:: N_BG_W
      DOUBLE PRECISION:: RESID_W(4)
      DOUBLE PRECISION:: X_DEC(10),Y_DEC(10)
      DOUBLE PRECISION:: N_HD_N
      DOUBLE PRECISION:: N_BD_N
      DOUBLE PRECISION:: N_HG_N
      DOUBLE PRECISION:: N_BG_N
      DOUBLE PRECISION:: RSD_HD_N
      DOUBLE PRECISION:: RSD_BD_N
      DOUBLE PRECISION:: RSD_HG_N
      DOUBLE PRECISION:: RSD_BG_N
      INTEGER:: SUM_N
      DOUBLE PRECISION:: SUM_RSD
      DOUBLE PRECISION:: MAX_N
      DOUBLE PRECISION:: MAX_RSD

!
!-----------------------------------------------------------------------
! DEFINE THE QUARTERS
!-----------------------------------------------------------------------
! AT THE FIRST TIME STEP
      IF(INIT_MES)THEN
        INIT_MES=.FALSE.
! DEFINE RELEASE POINT
        IF(LACHE.EQ.1)THEN
          X_INI=-1.3D0
          Y_INI=0.19D0
        ELSEIF(LACHE.EQ.2)THEN
          X_INI=0.175D0
          Y_INI=0.45D0
        ELSEIF(LACHE.EQ.3)THEN
          X_INI=-0.19D0
          Y_INI=0.25D0
        ELSEIF(LACHE.EQ.4)THEN
          X_INI=0.98D0
          Y_INI=0.55D0
        END IF
! DEFINE THE WINDOW OF MEASUREMENT
        IF(MESURE.EQ.1)THEN
          X_HG_1=-1.05D0
          X_HG_2=-0.55D0
          Y_HG_1=0.5D0
          Y_HG_2=1.0D0
!
          X_BG_1=-1.05D0
          X_BG_2=-0.55D0
          Y_BG_1=0.D0
          Y_BG_2=0.5D0
!
          X_HD_1=-0.55D0
          X_HD_2=-0.05D0
          Y_HD_1=0.5D0
          Y_HD_2=1.0D0
!
          X_BD_1=-0.55D0
          X_BD_2=-0.05D0
          Y_BD_1=0.D0
          Y_BD_2=0.5D0
!
        ELSEIF(MESURE.EQ.2)THEN
          X_HG_1=0.225D0
          X_HG_2=0.725D0
          Y_HG_1=0.5D0
          Y_HG_2=1.0D0
!
          X_BG_1=0.225D0
          X_BG_2=0.725D0
          Y_BG_1=0.D0
          Y_BG_2=0.5D0
!
          X_HD_1=0.725D0
          X_HD_2=1.225D0
          Y_HD_1=0.5D0
          Y_HD_2=1.0D0
!
          X_BD_1=0.725D0
          X_BD_2=1.225D0
          Y_BD_1=0.D0
          Y_BD_2=0.5D0
!
        ELSEIF(MESURE.EQ.3)THEN
          X_HG_1=1.4D0
          X_HG_2=1.9D0
          Y_HG_1=0.5D0
          Y_HG_2=1.0D0
!
          X_BG_1=1.4D0
          X_BG_2=1.9D0
          Y_BG_1=0.D0
          Y_BG_2=0.5D0
!
          X_HD_1=1.9D0
          X_HD_2=2.4D0
          Y_HD_1=0.5D0
          Y_HD_2=1.0D0
!
          X_BD_1=1.9D0
          X_BD_2=2.4D0
          Y_BD_1=0.D0
          Y_BD_2=0.5D0
        END IF
! OPEN RESULT FILE
        IF(MESURE.EQ.1.AND.LACHE.EQ.1)THEN
          FILE_ID = T2D_FILES(T2DRF1)%LU
        ELSEIF(MESURE.EQ.2.AND.LACHE.EQ.2)THEN
          FILE_ID = T2D_FILES(T2DRF2)%LU
        ELSEIF(MESURE.EQ.2.AND.LACHE.EQ.3)THEN
          FILE_ID = T2D_FILES(T2DRF3)%LU
        ELSEIF(MESURE.EQ.3.AND.LACHE.EQ.2)THEN
          FILE_ID = T2D_FILES(T2DRF4)%LU
        ELSEIF(MESURE.EQ.3.AND.LACHE.EQ.4)THEN
          FILE_ID = T2D_FILES(T2DRF5)%LU
        END IF
! INITIALISE THE VARIABLES
        N_HD=0
        N_BD=0
        N_HG=0
        N_BG=0
        IF(ALLOCATED(PRESENCE))DEALLOCATE(PRESENCE)
        ALLOCATE(PRESENCE(NP_TOT,4))
        IF(ALLOCATED(RESID))DEALLOCATE(RESID)
        ALLOCATE(RESID(4))
        DO I_A=1,NP_TOT
          DO I_Q=1,4
!             PRESENCE(I_A,I_Q)=.FALSE.
            PRESENCE(I_A,I_Q)=0
          END DO
        END DO
        DO I_Q=1,4
          RESID(I_Q)=0.D0
        END DO
!
        YES_START=.TRUE.
        T_START=1.D10
        T_END=0.D0
      END IF
!-----------------------------------------------------------------------
! CALCULATE THE VARIABLES FOR EACH QUARTER
!-----------------------------------------------------------------------
!
      N_TOT=0
!
      DO I_A=1,N_A
        IF(X_A(I_A).GE.X_HD_1.AND.X_A(I_A).LE.X_HD_2.AND.
     &     Y_A(I_A).GE.Y_HD_1.AND.Y_A(I_A).LE.Y_HD_2)THEN
          IF(PRESENCE(TAG(I_A),1).EQ.0)THEN
            N_HD=N_HD+1
            PRESENCE(TAG(I_A),1)=PRESENCE(TAG(I_A),1)+1
          ELSE
            RESID(1)=RESID(1)+DT
          END IF
          IF(YES_START)THEN
            T_START=LT*DTREAL
            YES_START=.FALSE.
          END IF
          IF(LT*DTREAL.GT.T_END)T_END=LT*DTREAL
        END IF
!
        IF(X_A(I_A).GE.X_BD_1.AND.X_A(I_A).LE.X_BD_2.AND.
     &     Y_A(I_A).GE.Y_BD_1.AND.Y_A(I_A).LE.Y_BD_2)THEN
          IF(PRESENCE(TAG(I_A),2).EQ.0)THEN
            N_BD=N_BD+1
            PRESENCE(TAG(I_A),2)=PRESENCE(TAG(I_A),2)+1
          ELSE
            RESID(2)=RESID(2)+DT
          END IF
          IF(YES_START)THEN
            T_START=LT*DTREAL
            YES_START=.FALSE.
          END IF
          IF(LT*DTREAL.GT.T_END)T_END=LT*DTREAL
        END IF
!
        IF(X_A(I_A).GE.X_HG_1.AND.X_A(I_A).LE.X_HG_2.AND.
     &     Y_A(I_A).GE.Y_HG_1.AND.Y_A(I_A).LE.Y_HG_2)THEN
          IF(PRESENCE(TAG(I_A),3).EQ.0)THEN
            N_HG=N_HG+1
            PRESENCE(TAG(I_A),3)=PRESENCE(TAG(I_A),3)+1
          ELSE
            RESID(3)=RESID(3)+DT
          END IF
          IF(YES_START)THEN
            T_START=LT*DTREAL
            YES_START=.FALSE.
          END IF
          IF(LT*DTREAL.GT.T_END)T_END=LT*DTREAL
        END IF
!
        IF(X_A(I_A).GE.X_BG_1.AND.X_A(I_A).LE.X_BG_2.AND.
     &     Y_A(I_A).GE.Y_BG_1.AND.Y_A(I_A).LE.Y_BG_2)THEN
          IF(PRESENCE(TAG(I_A),4).EQ.0)THEN
            N_BG=N_BG+1
            PRESENCE(TAG(I_A),4)=PRESENCE(TAG(I_A),4)+1
          ELSE
            RESID(4)=RESID(4)+DT
          END IF
          IF(YES_START)THEN
            T_START=LT*DTREAL
            YES_START=.FALSE.
          END IF
          IF(LT*DTREAL.GT.T_END)T_END=LT*DTREAL
        END IF
      END DO
!
!-----------------------------------------------------------------------
! COMMUNICATE RESULTS FOUND IN EACH PROCESSOR
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.0)THEN
        DO I_A=1,NP_TOT
          PRESENCE(I_A,1)=P_MAX(PRESENCE(I_A,1))
          PRESENCE(I_A,2)=P_MAX(PRESENCE(I_A,2))
          PRESENCE(I_A,3)=P_MAX(PRESENCE(I_A,3))
          PRESENCE(I_A,4)=P_MAX(PRESENCE(I_A,4))
!
          IF(PRESENCE(I_A,1).GT.0.OR.PRESENCE(I_A,2).GT.0.OR.
     &       PRESENCE(I_A,3).GT.0.OR.PRESENCE(I_A,4).GT.0)THEN
            N_TOT=N_TOT+1
          END IF
        END DO
      ELSE
        DO I_A=1,NP_TOT
          IF(PRESENCE(I_A,1).GT.0.OR.PRESENCE(I_A,2).GT.0.OR.
     &       PRESENCE(I_A,3).GT.0.OR.PRESENCE(I_A,4).GT.0)THEN
            N_TOT=N_TOT+1
          END IF
        END DO
      END IF
!
!-----------------------------------------------------------------------
! CALCULATE THE VARIABLES USED FOR THE RESULT FILE
!-----------------------------------------------------------------------
! IN PARRALLEL
      IF(NCSIZE.GT.0)THEN
        N_HD_W=P_SUM(N_HD)
        N_BD_W=P_SUM(N_BD)
        N_HG_W=P_SUM(N_HG)
        N_BG_W=P_SUM(N_BG)
        RESID_W(1)=P_SUM(RESID(1))
        RESID_W(2)=P_SUM(RESID(2))
        RESID_W(3)=P_SUM(RESID(3))
        RESID_W(4)=P_SUM(RESID(4))

        IF(IPID.EQ.0)THEN
! DEFINE THE COORDINATES OF THE QUARTERS
          X_DEC(1)=X_BG_1
          Y_DEC(1)=Y_BG_1
          X_DEC(2)=X_BD_2
          Y_DEC(2)=Y_BD_1
          X_DEC(3)=X_HD_2
          Y_DEC(3)=Y_HD_2
          X_DEC(4)=X_HG_1
          Y_DEC(4)=Y_HG_2
          X_DEC(5)=X_BG_1
          Y_DEC(5)=Y_BG_1
          X_DEC(6)=X_BG_2
          Y_DEC(6)=Y_BG_1
          X_DEC(7)=X_HG_2
          Y_DEC(7)=Y_HG_2
          X_DEC(8)=X_HG_1
          Y_DEC(8)=Y_HG_2
          X_DEC(9)=X_HG_1
          Y_DEC(9)=Y_HG_1
          X_DEC(10)=X_HD_2
          Y_DEC(10)=Y_HD_1
! CALCULATE THE SUMS
          SUM_N=N_HD_W+N_BD_W+N_HG_W+N_BG_W
          SUM_RSD=RESID_W(1)+RESID_W(2)+RESID_W(3)+RESID_W(4)
! CALCULATE THE NON-DIMENSIONNAL PROPORTION AND TIME OF RESIDENCE
          IF(SUM_N.NE.0)THEN
            N_BG_N=REAL(N_BG_W)/REAL(SUM_N)
            N_HG_N=REAL(N_HG_W)/REAL(SUM_N)
            N_HD_N=REAL(N_HD_W)/REAL(SUM_N)
            N_BD_N=REAL(N_BD_W)/REAL(SUM_N)
          ELSE
            N_BG_N=0.D0
            N_HG_N=0.D0
            N_HD_N=0.D0
            N_BD_N=0.D0
          END IF

          IF(SUM_RSD.NE.0)THEN
            IF(N_BG_N.GT.0.D0)THEN
              RSD_BG_N=RESID_W(4)/SUM_RSD/N_BG_N
            ELSE
              RSD_BG_N=0.D0
            ENDIF
            IF(N_HG_N.GT.0.D0)THEN
              RSD_HG_N=RESID_W(3)/SUM_RSD/N_HG_N
            ELSE
              RSD_HG_N=0.D0
            ENDIF
            IF(N_HD_N.GT.0.D0)THEN
              RSD_HD_N=RESID_W(1)/SUM_RSD/N_HD_N
            ELSE
              RSD_HD_N=0.D0
            ENDIF
            IF(N_BD_N.GT.0.D0)THEN
              RSD_BD_N=RESID_W(2)/SUM_RSD/N_BD_N
            ELSE
              RSD_BD_N=0.D0
            ENDIF
          ELSE
            RSD_BG_N=0.D0
            RSD_HG_N=0.D0
            RSD_HD_N=0.D0
            RSD_BD_N=0.D0
          END IF
        END IF
! IN SEQUENTIAL
      ELSE
! DEFINE THE COORDINATES OF THE QUARTERS
        X_DEC(1)=X_BG_1
        Y_DEC(1)=Y_BG_1
        X_DEC(2)=X_BD_2
        Y_DEC(2)=Y_BD_1
        X_DEC(3)=X_HD_2
        Y_DEC(3)=Y_HD_2
        X_DEC(4)=X_HG_1
        Y_DEC(4)=Y_HG_2
        X_DEC(5)=X_BG_1
        Y_DEC(5)=Y_BG_1
        X_DEC(6)=X_BG_2
        Y_DEC(6)=Y_BG_1
        X_DEC(7)=X_HG_2
        Y_DEC(7)=Y_HG_2
        X_DEC(8)=X_HG_1
        Y_DEC(8)=Y_HG_2
        X_DEC(9)=X_HG_1
        Y_DEC(9)=Y_HG_1
        X_DEC(10)=X_HD_2
        Y_DEC(10)=Y_HD_1
! CALCULATE THE SUMS
        SUM_N=N_HD+N_BD+N_HG+N_BG
        SUM_RSD=RESID(1)+RESID(2)+RESID(3)+RESID(4)
! CALCULATE THE NON-DIMENSIONNAL PROPORTION AND TIME OF RESIDENCE
        IF(SUM_N.NE.0)THEN
          N_BG_N=REAL(N_BG)/REAL(SUM_N)
          N_HG_N=REAL(N_HG)/REAL(SUM_N)
          N_HD_N=REAL(N_HD)/REAL(SUM_N)
          N_BD_N=REAL(N_BD)/REAL(SUM_N)
        ELSE
          N_BG_N=0.D0
          N_HG_N=0.D0
          N_HD_N=0.D0
          N_BD_N=0.D0
        END IF

        IF(SUM_RSD.NE.0)THEN
          IF(N_BG_N.GT.0.D0)THEN
            RSD_BG_N=RESID(4)/SUM_RSD/N_BG_N
          ELSE
            RSD_BG_N=0.D0
          ENDIF
          IF(N_HG_N.GT.0.D0)THEN
            RSD_HG_N=RESID(3)/SUM_RSD/N_HG_N
          ELSE
            RSD_HG_N=0.D0
          ENDIF
          IF(N_HD_N.GT.0.D0)THEN
            RSD_HD_N=RESID(1)/SUM_RSD/N_HD_N
          ELSE
            RSD_HD_N=0.D0
          ENDIF
          IF(N_BD_N.GT.0.D0)THEN
            RSD_BD_N=RESID(2)/SUM_RSD/N_BD_N
          ELSE
            RSD_BD_N=0.D0
          ENDIF
        ELSE
          RSD_BG_N=0.D0
          RSD_HG_N=0.D0
          RSD_HD_N=0.D0
          RSD_BD_N=0.D0
        END IF
      END IF
!-----------------------------------------------------------------------
! WRITE RESULTS
!-----------------------------------------------------------------------
      IF(IPID.EQ.0)THEN
! DEFINE THE SCALLING FACTORS
          MAX_N=0.5D0
          MAX_RSD=2.5D0
!
 9401 FORMAT(A,1X,A,1X,A,1X,A,1X,A,1X,A,1X,A,1X,A)
        WRITE(FILE_ID,'(A)')'# canal_chatou.f'
        WRITE(FILE_ID,'(A)')'# Maximum used to rescale values:'
        WRITE(FILE_ID,'(A,F3.1)')'# Max N_part = ',MAX_N
        WRITE(FILE_ID,'(A,F3.1)')'# Max t_resid = ',MAX_RSD
        WRITE(FILE_ID,'(A)')'# Maximum calculated for this case:'
        WRITE(FILE_ID,'(A,F5.3)')'# Max N_part = ',
     &        MAX(N_BG_N,N_HG_N,N_HD_N,N_BD_N)
        WRITE(FILE_ID,'(A,F5.3)')'# Max t_resid = ',
     &        MAX(RSD_BG_N,RSD_HG_N,RSD_HD_N,RSD_BD_N)
!
        WRITE(FILE_ID,9401) 'x_decoup','y_decoup','x_np',
     &        'y_np','x_rsd','y_rsd','x_rel','y_rel'
        WRITE(FILE_ID,'(8F12.5)') X_DEC(1),Y_DEC(1),
     &      X_BG_2-N_BG_N/MAX_N*(X_BG_2-X_BG_1),
     &      Y_BG_2-N_BG_N/MAX_N*(Y_BG_2-Y_BG_1),
     &      X_BG_2-RSD_BG_N/MAX_RSD*(X_BG_2-X_BG_1),
     &      Y_BG_2-RSD_BG_N/MAX_RSD*(Y_BG_2-Y_BG_1),
     &      X_INI,Y_INI
        WRITE(FILE_ID,'(6F12.5)') X_DEC(2),Y_DEC(2),
     &      X_HG_2-N_HG_N/MAX_N*(X_HG_2-X_HG_1),
     &      Y_HG_1+N_HG_N/MAX_N*(Y_HG_2-Y_HG_1),
     &      X_HG_2-RSD_HG_N/MAX_RSD*(X_HG_2-X_HG_1),
     &      Y_HG_1+RSD_HG_N/MAX_RSD*(Y_HG_2-Y_HG_1)
        WRITE(FILE_ID,'(6F12.5)') X_DEC(3),Y_DEC(3),
     &      X_HD_1+N_HD_N/MAX_N*(X_HD_2-X_HD_1),
     &      Y_HD_1+N_HD_N/MAX_N*(Y_HD_2-Y_HD_1),
     &      X_HD_1+RSD_HD_N/MAX_RSD*(X_HD_2-X_HD_1),
     &      Y_HD_1+RSD_HD_N/MAX_RSD*(Y_HD_2-Y_HD_1)
        WRITE(FILE_ID,'(6F12.5)') X_DEC(4),Y_DEC(4),
     &      X_BD_1+N_BD_N/MAX_N*(X_BD_2-X_BD_1),
     &      Y_BD_2-N_BD_N/MAX_N*(Y_BD_2-Y_BD_1),
     &      X_BD_1+RSD_BD_N/MAX_RSD*(X_BD_2-X_BD_1),
     &      Y_BD_2-RSD_BD_N/MAX_RSD*(Y_BD_2-Y_BD_1)
        WRITE(FILE_ID,'(6F12.5)') X_DEC(5),Y_DEC(5),
     &      X_BG_2-N_BG_N/MAX_N*(X_BG_2-X_BG_1),
     &      Y_BG_2-N_BG_N/MAX_N*(Y_BG_2-Y_BG_1),
     &      X_BG_2-RSD_BG_N/MAX_RSD*(X_BG_2-X_BG_1),
     &      Y_BG_2-RSD_BG_N/MAX_RSD*(Y_BG_2-Y_BG_1)
        WRITE(FILE_ID,'(2F12.5)') X_DEC(6),Y_DEC(6)
        WRITE(FILE_ID,'(2F12.5)') X_DEC(7),Y_DEC(7)
        WRITE(FILE_ID,'(2F12.5)') X_DEC(8),Y_DEC(8)
        WRITE(FILE_ID,'(2F12.5)') X_DEC(9),Y_DEC(9)
        WRITE(FILE_ID,'(2F12.5)') X_DEC(10),Y_DEC(10)
      END IF
!
      RETURN
      END SUBROUTINE MESURES_ALG


!                       ***************************
                        SUBROUTINE MESURES_PROF_VIT
!                       ***************************
!
     &(NCSIZE,IPID,NP_TOT,N_A,X_A,Y_A,DX,DY,V_X,V_Y,V_X_0,V_Y_0,U_X,
     & U_Y,U_X_0,U_Y_0)
!
!***********************************************************************
!  ANTOINE JOLY
!
!***********************************************************************
!
! SUBROUTINE USED TO RECORD THE PARTICLE VELOCITY PROFILES
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
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
! INPUT VARIABLES
      INTEGER         ,INTENT(IN)        ::NCSIZE,IPID
! INPUT VARIABLES FOR THE BODIES
      INTEGER         ,INTENT(IN)        ::NP_TOT
      INTEGER         ,INTENT(IN)        ::N_A
      DOUBLE PRECISION,INTENT(IN)        ::X_A(NP_TOT)
      DOUBLE PRECISION,INTENT(IN)        ::Y_A(NP_TOT)
      DOUBLE PRECISION,INTENT(IN)        ::DX(NP_TOT)
      DOUBLE PRECISION,INTENT(IN)        ::DY(NP_TOT)
      DOUBLE PRECISION                   ::X_A_0
      DOUBLE PRECISION                   ::Y_A_0
      DOUBLE PRECISION,INTENT(IN)        ::V_X(NP_TOT)
      DOUBLE PRECISION,INTENT(IN)        ::V_Y(NP_TOT)
      DOUBLE PRECISION,INTENT(IN)        ::V_X_0(NP_TOT)
      DOUBLE PRECISION,INTENT(IN)        ::V_Y_0(NP_TOT)
      DOUBLE PRECISION,INTENT(IN)        ::U_X(NP_TOT)
      DOUBLE PRECISION,INTENT(IN)        ::U_Y(NP_TOT)
      DOUBLE PRECISION,INTENT(IN)        ::U_X_0(NP_TOT)
      DOUBLE PRECISION,INTENT(IN)        ::U_Y_0(NP_TOT)
! VARIABLES USED IN LOOPS
      INTEGER                         :: I_A
      INTEGER                         :: I_Y
! VARIABLES USED TO RECORD THE PROFILES
      DOUBLE PRECISION                   ::X_MULT
      INTEGER:: SUM_N, FILE_ID
!-----------------------------------------------------------------------
! DEFINE THE VARIABLES FOR THE VELOCITY PROFILES
!-----------------------------------------------------------------------
      IF(INIT_PROF)THEN
        INIT_PROF=.FALSE.
!
        IF(VIT.EQ.1)THEN
          X_PROF=0.55D0
        END IF
!
        DY_PROF=2.D0/REAL(NY)
!
        DO I_Y=1,NY
          Y_PROF(I_Y)=REAL(I_Y)*DY_PROF-DY_PROF/2.D0
!
          N_PROF(I_Y)=0
          UX_PROF(I_Y)=0.D0
          UY_PROF(I_Y)=0.D0
          VX_PROF(I_Y)=0.D0
          VY_PROF(I_Y)=0.D0
        END DO
      END IF
!-----------------------------------------------------------------------
! CALCULATE THE VELOCITY PROFILES
!-----------------------------------------------------------------------

      DO I_A=1,N_A
        X_A_0=X_A(I_A)-DX(I_A)
        Y_A_0=Y_A(I_A)-DY(I_A)
        IF((X_A_0.LT.X_PROF.AND.X_A(I_A).GE.X_PROF).OR.
     &     (X_A_0.GE.X_PROF.AND.X_A(I_A).LT.X_PROF))THEN
          IF(X_A(I_A).EQ.X_A_0)THEN
            X_MULT=0.D0
          ELSE
            X_MULT=(X_PROF-X_A_0)/(X_A(I_A)-X_A_0)
          END IF
!
          DO I_Y=1,NY
            IF(Y_PROF(I_Y)-DY_PROF/2.D0.LT.
     &         Y_A_0+X_MULT*(Y_A(I_A)-Y_A_0).AND.
     &         Y_PROF(I_Y)+DY_PROF/2.D0.GE.
     &         Y_A_0+X_MULT*(Y_A(I_A)-Y_A_0))THEN

              N_PROF(I_Y)=N_PROF(I_Y)+1
!
              IF(U_X_0(I_A).GE.1000000.D0)THEN
              ! THE PARTICLE WAS IN ANOTHER PROCESSOR AT TIME LT_0 (OBSOLESCENT)
                UX_PROF(I_Y)=UX_PROF(I_Y)+U_X(I_A)
                UY_PROF(I_Y)=UY_PROF(I_Y)+U_Y(I_A)
                VX_PROF(I_Y)=VX_PROF(I_Y)+V_X(I_A)
                VY_PROF(I_Y)=VY_PROF(I_Y)+V_Y(I_A)
              ELSE
                UX_PROF(I_Y)=UX_PROF(I_Y)+U_X_0(I_A)+X_MULT*(U_X(I_A)
     &                     -U_X_0(I_A))
                UY_PROF(I_Y)=UY_PROF(I_Y)+U_Y_0(I_A)+X_MULT*(U_Y(I_A)
     &                     -U_Y_0(I_A))
                VX_PROF(I_Y)=VX_PROF(I_Y)+V_X_0(I_A)+X_MULT*(V_X(I_A)
     &                     -V_X_0(I_A))
                VY_PROF(I_Y)=VY_PROF(I_Y)+V_Y_0(I_A)+X_MULT*(V_Y(I_A)
     &                     -V_Y_0(I_A))
              END IF

            END IF
          END DO
        END IF
      END DO

!-----------------------------------------------------------------------
! SEND THESE INFORMATION TO PROCESSOR 0
!-----------------------------------------------------------------------
      IF(NCSIZE.GT.1) THEN
        SUM_N=0
        DO I_Y=1,NY
          N_WRI(I_Y)=P_SUM(N_PROF(I_Y))
          UX_WRI(I_Y)=P_SUM(UX_PROF(I_Y))
          UY_WRI(I_Y)=P_SUM(UY_PROF(I_Y))
          VX_WRI(I_Y)=P_SUM(VX_PROF(I_Y))
          VY_WRI(I_Y)=P_SUM(VY_PROF(I_Y))
          !
          SUM_N=SUM_N+N_WRI(I_Y)
        END DO
      ELSE
        SUM_N=0
        DO I_Y=1,NY
          N_WRI(I_Y)=N_PROF(I_Y)
          UX_WRI(I_Y)=UX_PROF(I_Y)
          UY_WRI(I_Y)=UY_PROF(I_Y)
          VX_WRI(I_Y)=VX_PROF(I_Y)
          VY_WRI(I_Y)=VY_PROF(I_Y)
          !
          SUM_N=SUM_N+N_WRI(I_Y)
        END DO
      END IF
! IGNORE VALUES THAT ARE TO LOW (NUMERICAL NOISE)
      DO I_Y=1,NY
        IF(SUM_N.NE.0)THEN
          IF(N_WRI(I_Y)/REAL(SUM_N).LE.0.001)THEN
            N_WRI(I_Y)=0
            UX_WRI(I_Y)=0.D0
            UY_WRI(I_Y)=0.D0
            VX_WRI(I_Y)=0.D0
            VY_WRI(I_Y)=0.D0
          END IF
        END IF
      END DO
!
!-----------------------------------------------------------------------
! WRITE THE RESULTS
!-----------------------------------------------------------------------
!
      IF(IPID.EQ.0)THEN
        FILE_ID = T2D_FILES(T2DRF6)%LU
 9401   FORMAT(A)
 9402   FORMAT(6F16.8)
!
        WRITE(FILE_ID,9401) '#canal_chatou.f'
        WRITE(FILE_ID,'(A,F5.3)') '#Velocity profil at x =',X_PROF
        WRITE(FILE_ID,'(A,I6)') '#Sum_N = ',SUM_N
        WRITE(FILE_ID,9401) 'Y N U_X U_Y V_X V_Y'
        DO I_Y=1,NY
          IF(N_WRI(I_Y).NE.0)THEN
            WRITE(FILE_ID,9402)Y_PROF(I_Y),REAL(N_WRI(I_Y))/REAL(SUM_N),
     &       UX_WRI(I_Y)/REAL(N_WRI(I_Y)),UY_WRI(I_Y)/REAL(N_WRI(I_Y)),
     &       VX_WRI(I_Y)/REAL(N_WRI(I_Y)),VY_WRI(I_Y)/REAL(N_WRI(I_Y))
     &
          ELSE
            WRITE(9994,9402) Y_PROF(I_Y),0.D0,0.D0,0.D0,0.D0,0.D0
          END IF
        END DO
!
      END IF
!
      RETURN
      END SUBROUTINE
!

      END MODULE
