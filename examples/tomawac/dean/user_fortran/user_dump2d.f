!                   **********************
                    SUBROUTINE USER_DUMP2D
!                   **********************
!
     &(XF1, NP1)
!
!***********************************************************************
! TOMAWAC   V6P3                                   15/06/2011
!***********************************************************************
!
!brief    USER WRITES OUT WAVE, WIND, CURRENT, BATHYMETRY, ...
!+                VARIABLES AT EACH NODE OF THE MESH.
!+                VARIES SPATIALLY IN 2D (BINARY SELAFIN FORMAT).
!
!
!history  F. MARCOS
!+        01/02/95
!+        V1P0
!+   CREATED
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NP1            |-->| NPOIN2.NDIRE.NF
!| XF1            |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC
      USE INTERFACE_TOMAWAC, EX_USER_DUMP2D => USER_DUMP2D
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NP1
      DOUBLE PRECISION, INTENT(IN) :: XF1(NP1)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION AUX1(NPOIN2),AUX2(NPOIN2)
      DOUBLE PRECISION AUX3(NPOIN2)
!
!-----------------------------------------------------------------------
!
!     ------------------------------- KMOYEN AND QMOUT1
!
      IF(SORLEO(17)) THEN
        CALL KMOYEN(SPRIVE%R,SXK%R,SF%R,NF,NDIRE,
     &              NPOIN2,AUX1,AUX2,AUX3)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
