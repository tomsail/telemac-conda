!                   ********************************
                    SUBROUTINE CVSP_INIT_FROM_LAYERS
!                   ********************************
!
!
!***********************************************************************
! SISYPHE V6P3                                   14/03/2013
!***********************************************************************
!
!brief   INITS A VERTICAL SORTING PROFILE USING HIRANO LAYERS
!
!history UWE MERKEL
!+        19/04/2012
!+        V6P2
!+
!
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   Cleaning, cosmetic
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|           |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE
!
      INTEGER  I, J, M, L
      DOUBLE PRECISION DEPTH
!
!-----------------------------------------------------------------------
!
      ALLOCATE(PRO_D(NPOIN,PRO_MAX_MAX,NSICLA))
      ALLOCATE(PRO_F(NPOIN,PRO_MAX_MAX,NSICLA))
      ALLOCATE(PRO_MAX(NPOIN))
!
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN
        DEPTH = 0                    ! INIT DEPTH OF THE VSP
        PRO_MAX(J) =  2* NLAYER%I(J) ! 2 SECTION POINTS PER LAYER
        L = PRO_MAX(J)
!
!-----------------------------------------------------------------------
! WATER / BOTTOM
!-----------------------------------------------------------------------
!
        DO I=1,NSICLA
          PRO_D(J,L,I) = ZF%R(J)
          PRO_F(J,L,I) = AVAIL(J,1,I)
        ENDDO
!
!-----------------------------------------------------------------------
! SECTIONS
!-----------------------------------------------------------------------
!
        DO M=1,NLAYER%I(J)-1   !FOR THE UPPER 8 LAYERS
          DEPTH = DEPTH + ES(J,M)
          L = L - 1
          DO I=1,NSICLA
            PRO_D(J,L,I) = ZF%R(J) - DEPTH
            PRO_F(J,L,I) = AVAIL(J,M,I)
          ENDDO
          L = L - 1
          DO I=1,NSICLA
            PRO_D(J,L,I) = ZF%R(J) - DEPTH
            PRO_F(J,L,I) = AVAIL(J,M+1,I)
          ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
! BOTTOM / RIGID BED
!-----------------------------------------------------------------------
!
        L = L - 1
        DO I=1,NSICLA
          PRO_D(J,L,I) = ZR%R(J)
          PRO_F(J,L,I) = AVAIL(J,NLAYER%I(J),I)
        ENDDO
!
        CALL CVSP_COMPRESS_DP(J,1.D-5)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CVSP_INIT_FROM_LAYERS
