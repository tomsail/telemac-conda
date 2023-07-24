!                   *****************
                    SUBROUTINE ECRETE
!                   *****************
!
     &( F     , DEPTH , NPOIN2, NDIRE , NF    , PROMIN)
!
!***********************************************************************
! TOMAWAC   V6P1                                   15/06/2011
!***********************************************************************
!
!brief    INITIALISES THE VARIANCE SPECTRUM (SETS IT TO 0) AT
!+                ALL THE NODES WHERE THE DEPTH OF WATER IS LESS
!+                THAN PROMIN.
!
!history  M. BENOIT (EDF LNHE)
!+        19/01/2004
!+        V5P4
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  G.MATTAROLO (EDF - LNHE)
!+        15/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEPTH          |-->| WATER DEPTH
!| F              |<->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| PROMIN         |-->| MINIMUM VALUE OF WATER DEPTH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_ECRETE => ECRETE
      IMPLICIT NONE
!
!
      INTEGER, INTENT(IN)             :: NPOIN2 , NDIRE, NF
      DOUBLE PRECISION, INTENT(IN)    :: DEPTH(NPOIN2),PROMIN
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NDIRE,NF)
      INTEGER  IP    , JP    , JF
!
      DO IP=1,NPOIN2
        IF (DEPTH(IP).LT.PROMIN) THEN
          DO JF=1,NF
            DO JP=1,NDIRE
              F(IP,JP,JF)=0.0D0
            ENDDO
          ENDDO
        ENDIF
      ENDDO
!
      RETURN
      END
