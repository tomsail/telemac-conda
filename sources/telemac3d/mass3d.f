!                   *****************
                    SUBROUTINE MASS3D
!                   *****************
!
     &(INFO)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES MASSES.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  J-M HERVOUET (LNHE)
!+        09/04/08
!+        V5P9
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
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| INFO           |-->| LOGICAL TO INDICATE IF PRINTINGS MUST BE DONE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN) :: INFO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC,I
!
!***********************************************************************
!
!   WATER MASS
!   ==========
!
      CALL VECTOR
     &  (T2_01,'=','MASVEC          ',IELM2H,1.D0,H,SVIDE,SVIDE,
     &  SVIDE, SVIDE, SVIDE, MESH2D, MSK, MASKEL)
      MASSE_WATER = BIEF_SUM(T2_01)
!
      IF(NCSIZE.GT.1) MASSE_WATER = P_SUM(MASSE_WATER)
!
      IF(INFO.AND.LT.EQ.0) THEN
        WRITE(LU,*) 'INITIAL VOLUME OF WATER IN THE DOMAIN :',
     &              MASSE_WATER
      ENDIF
!
!   TRACERS MASS
!   ============
!
      IF(NTRAC.GT.0) THEN
!
        DO ITRAC=1,NTRAC
!
!         UP TO RELEASE 5.4
!
!         CALL VECTOR
!    &    (T3_01, '=', 'MASVEC          ', IELM3, 1.D0,
!    &     TA%ADR(ITRAC)%P,
!    &     SVIDE, SVIDE, SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKEL)
!         MASSE%R(5+ITRAC) = SUM(T3_01)
!
!         FROM RELEASE 5.5 ON
!
!         THE 2 VERSIONS ARE NOT EQUIVALENT WHEN VOLU IS COMPUTED
!         WITH FORMULA MASBAS2 WHICH GIVES A COMPATIBILITY WITH 2D
!         WHEN THERE IS A MASS-LUMPING
!
!         TRACERS IN MASSE COME AFTER U,V,W,K AND EPSILON (HENCE THE 5)
!
          MASSE%R(5+ITRAC) = 0.D0
          DO I=1,NPOIN3
            MASSE%R(5+ITRAC)=MASSE%R(5+ITRAC)+TA%ADR(ITRAC)%P%R(I)*
     &                                        VOLU%R(I)
          ENDDO
!
!         END OF MODIFICATION BETWEEN 5.4 AND 5.5
!
          IF(NCSIZE.GT.1) MASSE%R(5+ITRAC) = P_SUM(MASSE%R(5+ITRAC))
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!101   FORMAT(' MASSE D''EAU INITIALE DANS LE DOMAINE : ',3X, G16.7)
!102   FORMAT(' INITIAL MASS OF WATER IN THE DOMAIN :',3X,G16.7)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
