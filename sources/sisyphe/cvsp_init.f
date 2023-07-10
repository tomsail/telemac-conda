!                   ********************
                    SUBROUTINE CVSP_INIT
!                   ********************
!
!***********************************************************************
! SISYPHE   V7P2                                   16/05/2017
!***********************************************************************
!
!brief   INITS A VERTICAL SORTING PROFILE BY USING HIRANO LAYERS
!        OR USER CODING
!
!history UWE MERKEL
!+        20/07/2011
!+        V6P2
!+
!
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   Cleaning, cosmetic
!
!history U. MERKEL, R.KOPMANN (BAW)
!+        21/07/2016
!+        V6P3, V7P2
!+   Integrating init_from_layers in this subroutine
!
!history R.KOPMANN (BAW)
!+        19/02/2019
!+        V7P2
!+   Initial volume calculated with CVSP variables
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX OF A POINT IN MESH
!| I              |<--| INDEX OF A FRACTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE
      USE CVSP_OUTPUTFILES, ONLY: CP
      USE INTERFACE_PARALLEL, ONLY : P_DSUM
!
      IMPLICIT NONE
!
      INTEGER  I,J,K,L,M
      DOUBLE PRECISION DEPTH
      LOGICAL RET, CVSP_CHECK_L, CVSP_CHECK_F
!
!-----------------------------------------------------------------------
!
      IF(CP) WRITE(LU,*) 'CVSP_INIT'

      DO J=1,NPOIN
        DO K=1,NOMBLAY
          RET =  CVSP_CHECK_L(J,K,' CVSP_INIT:  ')
        ENDDO
      ENDDO
!
      ALLOCATE(PRO_D(NPOIN,PRO_MAX_MAX,NSICLA))
      ALLOCATE(PRO_F(NPOIN,PRO_MAX_MAX,NSICLA))
      ALLOCATE(PRO_MAX(NPOIN))
!
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN
        DEPTH = 0.D0                 ! INIT DEPTH OF THE VSP
        PRO_MAX(J) =  2* NLAYER%I(J) ! 2 SECTION POINTS PER LAYER
        L = PRO_MAX(J)+1
!
!-----------------------------------------------------------------------
! BUILDING SECTIONS
!-----------------------------------------------------------------------
!
        DO M=1,NLAYER%I(J)
          IF (ABS(ES(J,M)).GT.0.D0) THEN
            L = L - 1
            DO I=1,NSICLA
              PRO_D(J,L,I) = ZF%R(J) - DEPTH
              PRO_F(J,L,I) = AVAIL(J,M,I)
            ENDDO
            DEPTH = DEPTH + ES(J,M)
            L = L - 1
            DO I=1,NSICLA
              PRO_D(J,L,I) = ZF%R(J) - DEPTH
              IF(M==NLAYER%I(J)) THEN
                PRO_F(J,L,I) = AVAIL(J,M,I)
              ELSE
                PRO_F(J,L,I) = AVAIL(J,M+1,I)
              ENDIF
            ENDDO
            RET =  CVSP_CHECK_F(J,L,'FromLay:  ')
          ENDIF
        ENDDO

!-----------------------------------------------------------------------
! USER CODING
!-----------------------------------------------------------------------
!        PRO_MAX(J) = PRO_MAX_MAX
!        DO K=1,PRO_MAX(J)
!          DO I=1,NSICLA
!            PRO_D(J,K,I) = (ZF%R(J)-ZF%R(J))/PRO_MAX(J)*K
!            PRO_F(J,K,1) = 1.D0 / NSICLA
!          ENDDO
!-----------------------------------------------------------------------
! FINAL CHECKS
!-----------------------------------------------------------------------
        IF (ABS(ZF%R(J) - ZR%R(J) - DEPTH).GT.10.D-6) THEN
          WRITE(LU,*)'Depth Synchro Error for Point J: ',J
          WRITE(LU,*)ZF%R(J),ZR%R(J),DEPTH,ABS(ZF%R(J)-ZR%R(J)-DEPTH)
        ENDIF
!
      ENDDO !J=1,NPOIN
!-----------------------------------------------------------------------
      DO J=1,NPOIN
        DO L=1,PRO_MAX(J)
          RET =  CVSP_CHECK_F(J,L,'AfterLAY:  ')
        END DO
        CALL CVSP_CHECK_STEADY(J)
      END DO

!-----------------------------------------------------------------------
      CALL CVSP_CHECK_ANYTHING()

!INITIALISIATION OUTPUT TO SERAFIN FILE
      IF (CVSM_OUT_FULL) CALL CVSP_OUTPUT_INIT()
      IF (CVSM_OUT_FULL) CALL CVSP_WRITE_PROFILE()
!
! CALCULATING THE VOLUME SEEN FROM CVSM MODEL
      DO I = 1, NSICLA
        VOLTOT(I) = 0.D0
      ENDDO
!
      DO I=1,NSICLA
        DO J=1,NPOIN
          DO K=1,PRO_MAX(J)-1
            VOLTOT(I) = VOLTOT(I) + (PRO_F(J,K,I)+PRO_F(J,K+1,I))/2.D0
     &           *(PRO_D(J,K+1,I)-PRO_D(J,K,I))*VOLU2D%R(J)
          ENDDO
        ENDDO
      ENDDO
      IF(NCSIZE.GT.1) THEN
        DO I=1,NSICLA
          VOLTOT(I) = P_DSUM(VOLTOT(I))
        ENDDO
      ENDIF
!
      DO I=1,NSICLA
        VOLINI(I) = VOLTOT(I)
      ENDDO
!
      RETURN
      END SUBROUTINE CVSP_INIT
