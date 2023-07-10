      MODULE PNTS_MESURE
!**********************************************************************
! DECLARATION OF VARIABLES USED TO RECORD THE FLOW
!**********************************************************************
      USE DECLARATIONS_TELEMAC2D, ONLY : T2D_FILES, T2DRFO
      INTEGER         ,PARAMETER      :: NY_PR=101
      INTEGER         ,PARAMETER      :: NX_PR=7
      DOUBLE PRECISION                :: X_PROF(NX_PR,NY_PR)
      DOUBLE PRECISION                :: Y_PROF(NX_PR,NY_PR)
      INTEGER                         :: ELEM_PROF(NX_PR,NY_PR)
      DOUBLE PRECISION                :: DET_PROF(3,NX_PR,NY_PR)
      INTEGER                         :: PROC_PROF(NX_PR,NY_PR)
!
      DOUBLE PRECISION                :: U_MES(NX_PR,NY_PR)
      DOUBLE PRECISION                :: V_MES(NX_PR,NY_PR)
      DOUBLE PRECISION                :: K_MES(NX_PR,NY_PR)
      DOUBLE PRECISION                :: EPS_MES(NX_PR,NY_PR)
      DOUBLE PRECISION                :: H_MES(NX_PR,NY_PR)
      END MODULE



!                       ************************
                        SUBROUTINE ENR_FLU
!                       *************************
!
     &(IKLE,N_NODE)
!
!***********************************************************************
!  ANTOINE JOLY
!
!***********************************************************************
!
! SUBROUTINE USED TO RECORD FLUID VELOCITY PROFILES
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
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D,ONLY:X,Y,LT,NELEM,MESH,UCONV,VCONV,AK,
     &                                EP,H
      USE PNTS_MESURE
!
      IMPLICIT NONE
! IMPORT MESH VARIABLES
      INTEGER         ,INTENT(IN)     :: N_NODE
      INTEGER         ,INTENT(IN)     :: IKLE(NELEM,N_NODE)
! VARIABLES USED TO FIND ELEMENT VALUES OF THE MESUREMENT POINT
      INTEGER                         :: IELEM,N1,N2,N3
      DOUBLE PRECISION                :: DET1,DET2,DET3
      DOUBLE PRECISION                :: A,B
! VARIABLES USED TO INTERPOLATE FLUID VARIABLES
      DOUBLE PRECISION                :: LAMBDA1,LAMBDA2,LAMBDA3
      DOUBLE PRECISION,DIMENSION(:),POINTER:: U
      DOUBLE PRECISION,DIMENSION(:),POINTER:: V
      DOUBLE PRECISION,DIMENSION(:),POINTER:: K
      DOUBLE PRECISION,DIMENSION(:),POINTER:: EPS
      DOUBLE PRECISION,DIMENSION(:),POINTER:: H2
! VARIABLES USED IN LOOPS
      INTEGER                         :: IX,IY,IPROC
! TEMPORARY VARIABLES
      CHARACTER(LEN=30)               :: TEMP
      INTEGER                         :: IOS
      INTEGER                         :: I_TEMP
      INTEGER ID_1,ID_2,ID_3
      U=>UCONV%R
      V=>VCONV%R
      K=>AK%R
      EPS=>EP%R
      H2=>H%R
!***********************************************************************
!
!-----------------------------------------------------------------------
! DEFINE THE MESUREMENT POINTS
!-----------------------------------------------------------------------
      IF(LT.EQ.0)THEN
        DO IX=1,NX_PR
          DO IY=1,NY_PR
            IF(IX.EQ.1)X_PROF(IX,IY)=-2.D0
            IF(IX.EQ.2)X_PROF(IX,IY)=0.D0
            IF(IX.EQ.3)X_PROF(IX,IY)=1.D0
            IF(IX.EQ.4)X_PROF(IX,IY)=2.D0
            IF(IX.EQ.5)X_PROF(IX,IY)=3.D0
            IF(IX.EQ.6)X_PROF(IX,IY)=4.D0
            IF(IX.EQ.7)X_PROF(IX,IY)=5.D0
!
            Y_PROF(IX,IY)=REAL(IY-1)*2.D0/REAL(NY_PR-1)
            IF(IY.EQ.1)Y_PROF(IX,IY)=0.01D0
            IF(IY.EQ.NY_PR)Y_PROF(IX,IY)=1.99D0

            PROC_PROF(IX,IY)=-1
            ELEM_PROF(IX,IY)=-1
!
            DO IELEM=1,NELEM
              N1=IKLE(IELEM,1)
              N2=IKLE(IELEM,2)
              N3=IKLE(IELEM,3)
              A=(X(N3)-X(N2))*(Y_PROF(IX,IY)-Y(N2))
              B=(Y(N3)-Y(N2))*(X_PROF(IX,IY)-X(N2))
              DET1=A-B
              A=(X(N1)-X(N3))*(Y_PROF(IX,IY)-Y(N3))
              B=(Y(N1)-Y(N3))*(X_PROF(IX,IY)-X(N3))
              DET2=A-B
              A=(X(N2)-X(N1))*(Y_PROF(IX,IY)-Y(N1))
              B=(Y(N2)-Y(N1))*(X_PROF(IX,IY)-X(N1))
              DET3=A-B
              IF(DET1.GE.0.D0.AND.DET2.GE.0.D0.AND.DET3.GE.0.D0)THEN
                PROC_PROF(IX,IY)=IPID
                ELEM_PROF(IX,IY)=IELEM
                DET_PROF(1,IX,IY)=DET1
                DET_PROF(2,IX,IY)=DET2
                DET_PROF(3,IX,IY)=DET3
              END IF
            END DO
          END DO
        END DO
      END IF
!-----------------------------------------------------------------------
! INTERPOLATE FLUID VARIABLES AT MEASUREMENT POINTS
!-----------------------------------------------------------------------
      IF(LT.GT.0)THEN
        DO IX=1,NX_PR
          DO IY=1,NY_PR
            IF(ELEM_PROF(IX,IY).EQ.-1)THEN
              PROC_PROF(IX,IY)=0
              U_MES(IX,IY)=0.D0
              V_MES(IX,IY)=0.D0
              K_MES(IX,IY)=0.D0
              EPS_MES(IX,IY)=0.D0
              H_MES(IX,IY)=0.D0
            ELSE
              N1=IKLE(ELEM_PROF(IX,IY),1)
              N2=IKLE(ELEM_PROF(IX,IY),2)
              N3=IKLE(ELEM_PROF(IX,IY),3)
              LAMBDA1=DET_PROF(1,IX,IY)*MESH%SURDET%R(ELEM_PROF(IX,IY))
              LAMBDA2=DET_PROF(2,IX,IY)*MESH%SURDET%R(ELEM_PROF(IX,IY))
              LAMBDA3=DET_PROF(3,IX,IY)*MESH%SURDET%R(ELEM_PROF(IX,IY))
              U_MES(IX,IY)=U(N1)*LAMBDA1+U(N2)*LAMBDA2+U(N3)*LAMBDA3
              V_MES(IX,IY)=V(N1)*LAMBDA1+V(N2)*LAMBDA2+V(N3)*LAMBDA3
              K_MES(IX,IY)=K(N1)*LAMBDA1+K(N2)*LAMBDA2+K(N3)*LAMBDA3
              EPS_MES(IX,IY)=EPS(N1)*LAMBDA1+EPS(N2)*LAMBDA2
     &                    +EPS(N3)*LAMBDA3
              H_MES(IX,IY)=H2(N1)*LAMBDA1+H2(N2)*LAMBDA2+H2(N3)*LAMBDA3
            END IF
          END DO
        END DO
!-----------------------------------------------------------------------
! COMMUNICATE WITH OTHER PROCESSORS IN PARRALLEL
!-----------------------------------------------------------------------
! WRITE THE VALUES IN TEMPORARY FILES
        IF(NCSIZE.GT.1) THEN
          IF(IPID.NE.0)THEN
            WRITE(TEMP,'(A,I3.3,A)')'./prof_cour_temp_p',IPID,'.txt'
            CALL GET_FREE_ID(ID_1)
            OPEN(ID_1,FILE=TEMP,FORM='UNFORMATTED')
            WRITE(ID_1)0
            DO IX=1,NX_PR
              DO IY=1,NY_PR
                IF(PROC_PROF(IX,IY).EQ.IPID)THEN
                  WRITE(ID_1)IX,IY
                  WRITE(ID_1)U_MES(IX,IY),V_MES(IX,IY),K_MES(IX,IY),
     &                       EPS_MES(IX,IY),H_MES(IX,IY)
                END IF
              END DO
            END DO
            CLOSE(ID_1)
          END IF
! SYNCHRONISE THE PROCESSORS
          CALL P_SYNC()
! READ THE VALUES IN TEMPORARY FILES
          IF(IPID.EQ.0)THEN
            DO IPROC=1,NCSIZE-1
              WRITE(TEMP,'(A,I3.3,A)')'./prof_cour_temp_p',IPROC,'.txt'
              CALL GET_FREE_ID(ID_2)
              OPEN(ID_2,FILE=TEMP,FORM='UNFORMATTED')
              READ(ID_2)I_TEMP
              IOS=0
              DO WHILE(IOS.EQ.0)
                READ(ID_2,IOSTAT=IOS)IX,IY
                IF(IOS.NE.0)EXIT
                READ(ID_2)U_MES(IX,IY),V_MES(IX,IY),K_MES(IX,IY),
     &                    EPS_MES(IX,IY),H_MES(IX,IY)
              END DO
              CLOSE(ID_2)
            END DO
          END IF
        END IF
      END IF
!-----------------------------------------------------------------------
! WRITE THE RESULT FILE
!-----------------------------------------------------------------------
      IF(LT.GT.0.AND.IPID.EQ.0)THEN
        ID_3 = T2D_FILES(T2DRFO)%LU
! WRITE THE HEADER
        WRITE(ID_3,'(A)') '# canal_chatou.f'
        WRITE(ID_3,'(A)') '# k=10*k'
        WRITE(ID_3,'(A)') '# eps=25*eps'
 21     FORMAT(A,I1,A,1X)
        DO IX=1,NX_PR
          WRITE(ID_3,21,ADVANCE='NO') 'u_plus_x_',IX,'_tel'
          WRITE(ID_3,21,ADVANCE='NO') 'k_plus_x_',IX,'_tel'
          WRITE(ID_3,21,ADVANCE='NO') 'eps_plus_x_',IX,'_tel'
          WRITE(ID_3,21,ADVANCE='NO') 'y_',IX,'_tel'
        END DO
        WRITE(ID_3,'(1X)')
! WRITE THE RESULTS
 22     FORMAT(F12.6)
        DO IY=1,NY_PR
          DO IX=1,NX_PR
            WRITE(ID_3,22,ADVANCE='NO')X_PROF(IX,IY)+U_MES(IX,IY)
            WRITE(ID_3,22,ADVANCE='NO')X_PROF(IX,IY)+K_MES(IX,IY)*10.D0
            WRITE(ID_3,22,ADVANCE='NO')X_PROF(IX,IY)
     &                                +EPS_MES(IX,IY)*25.D0
            WRITE(ID_3,22,ADVANCE='NO')Y_PROF(IX,IY)
          END DO
          WRITE(ID_3,'(1X)')
        END DO
      END IF
!
      RETURN
      END SUBROUTINE ENR_FLU
