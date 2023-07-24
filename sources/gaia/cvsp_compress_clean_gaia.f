!                   ***********************************
                    SUBROUTINE CVSP_COMPRESS_CLEAN_GAIA
!                   ***********************************
!
     &(J)
!
!***********************************************************************
! GAIA   V8P1                                   14/03/2013
!***********************************************************************
!
!>@brief    CLEAN A VERTICAL SORTING PROFILE IN POINT J AFTER REMOVING FRACTIONS
!!        ELEMINATES EMPTY LAYERS
!
!
!>@history UWE MERKEL
!!        02/02/2012
!!        V6P2
!
!>@history  P. A. TASSI (EDF R&D, LNHE)
!!        12/03/2013
!!        V6P3
!!   Cleaning, cosmetic
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] J Index of a point in mesh
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA_BEDLOAD, EX => CVSP_COMPRESS_CLEAN_GAIA
      USE DECLARATIONS_GAIA
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,           INTENT(IN) :: J
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  I,K, MARKERMAX, MARKERCNT,  TTT, JG
      INTEGER MARKER(PRO_MAX_MAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! STORES THE FRACTION ERRORS THAT WILL OCCURE IF THE POINT IS ELEMINATED FROM CURRENT PROFILE
!
      DOUBLE PRECISION SUMFERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! STORES THE MAXIMUM DISTANCE OF ANY NODE IN THE CURRENT LOOP
!
      DOUBLE PRECISION  DIST, SUMF
!
!-----------------------------------------------------------------------
! LOCAL -> GLOBAL / PARALLEL STUFF
!-----------------------------------------------------------------------
!
      JG = J
      IF (NCSIZE.GT.1) JG = MESH%KNOLG%I(J)
!
!-----------------------------------------------------------------------
! INITIAL DEBUGGING OUTPUT ...
!-----------------------------------------------------------------------
!
      IF (CVSP_DB_GAIA(JG,0).EQV..TRUE.) CALL CVSP_P_GAIA('./','V_W',JG)
!
!--------------------------------------------------------------------------
! INIT
!-----------------------------------------------------------------------
!
      IF(PRO_MAX(J) <= 2) RETURN
!                                FIRST AND LAST POINT WILL ALWAYS BE KEPT
      MARKERMAX = 2             !MAXIMUM USED INDEX IN MARKER ARRAY
      MARKER(1) = 1             !FIRST WILL ALWAYS BE KEPT
      MARKERCNT = 1             !MAXIMUM USED INDEX IN MARKERTEMP ARRAY
!
!--------------------------------------------------------------------------
! TOP TO BOTTOM
!-----------------------------------------------------------------------
!
      DO TTT = 2,PRO_MAX(J)
        SUMF = 0.D0
        SUMFERR = 0.D0
        DO I = 1, NSICLA
          SUMF = PRO_F(J,TTT, I) + SUMF
          SUMFERR = ABS((PRO_F(J,TTT,I)-PRO_F(J,MARKER(MARKERCNT),I)))
     &         + SUMFERR
        ENDDO
!
        IF (TTT > 1) THEN
          DIST = ABS((PRO_D(J,TTT,1)-PRO_D(J,MARKER(MARKERCNT),1)))
          IF ((DIST.GT.ZERO).OR.(SUMFERR.GT.0.D0)) THEN
            MARKERCNT = MARKERCNT + 1
            MARKER(MARKERCNT) = TTT
          ENDIF
        ENDIF
      ENDDO                     !TTT
!
      MARKERMAX = MARKERCNT
!
!--------------------------------------------------------------------------
! RECREATE THE SORTING PROFILE WITH LESS NUMBER OF NODES=LAYERS
!-----------------------------------------------------------------------
!
      DO K = 1, NSICLA
        DO I = 1, MARKERMAX
          PRO_F(J,I,K) = PRO_F(J,MARKER(I),K)
          PRO_D(J,I,K) = PRO_D(J,MARKER(I),K)
        ENDDO                  !I
      ENDDO                     !K
      PRO_MAX(J) = MARKERMAX
!
!--------------------------------------------------------------------------
! BRUTFORCE COMPRESSION IN CASE OF EXCEPTIONAL FRAGMENTATION
!-----------------------------------------------------------------------
!
      IF (PRO_MAX(J) > PRO_MAX_MAX-8*NSICLA) THEN
        CALL CVSP_COMPRESS_DP_GAIA(J, 1.D-5)
      ENDIF
      IF (PRO_MAX(J) < 4) THEN
        CALL CVSP_COMPRESS_BRUT_GAIA(J)
      ENDIF
!
!--------------------------------------------------------------------------
! FINAL DEBUGGING OUTPUT ...
!-----------------------------------------------------------------------
!
      IF(CVSP_DB_GAIA(JG,0)) CALL CVSP_P_GAIA('./','V_V',JG)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CVSP_COMPRESS_CLEAN_GAIA
