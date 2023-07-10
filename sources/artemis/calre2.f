!                   *****************
                    SUBROUTINE CALRE2
!                   *****************
!
!
!***********************************************************************
! ARTEMIS   V7P3                                     Aug 2017
!***********************************************************************
!
!brief    COMPUTES MEAN PARAMETERS OF THE WAVE SPECTRUM
!+               (RANDOM SEAS) :
!+                    K : MEAN WAVE NUMBER;
!+                    C : MEAN PHASE CELERITY;
!+                    CG : MEAN GROUP CELERITY.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        04/06/1999
!+        V5P1
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
!history  N.DURAND (HRW)
!+        August 2017
!+        V7P3
!+   DEUPI now defined in DECLARATIONS_ARTEMIS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER I
!
      DOUBLE PRECISION DHTEST
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
!   COMPUTES THE WAVE NUMBER: K
!   USING AN EXPLICIT FORMULATION (SEE EDF'S EXCELLENT REPORT BY
!   F. DHELLEMMES 'PRECIS SUR LES VAGUES' )
!
!-----------------------------------------------------------------------
!
!
!=======================================================================
! COMPUTES MEAN OMEGA
!=======================================================================
!
! MEAN OMEGA STORED IN T1
!
      CALL OS('X=1/Y   ',X=T1, Y=T01)
      CALL OS('X=CX    ',X=T1, C=DEUPI)
      CALL OS('X=Y     ',X=OMEGAM, Y=T1)
!
!=======================================================================
! COMPUTES MEAN K
!=======================================================================
!
! OMEGA**2 * H / GRAV
!
      CALL OS('X=YZ    ', X=T2 , Y=T1 , Z=T1 )
      CALL OS('X=CXY   ', X=T2 , Y=H  , C=1.D0/GRAV )
!
!     INITIALISES DHTEST
!
      DHTEST = 1.D6
!
      DO I=1,NPOIN
        T1%R(I) = 1.D0 + T2%R(I) *( 0.6522D0 +
     &                    T2%R(I) *( 0.4622D0 +
     &                    T2%R(I) *
     &                    T2%R(I) *( 0.0864D0 +
     &                    T2%R(I) *( 0.0675D0 ) )))
        T1%R(I) = SQRT( T2%R(I)*(T2%R(I) + 1.D0/T1%R(I)) )
        K%R(I)  = T1%R(I)/H%R(I)
        DHTEST  = MIN( DHTEST , H%R(I) )
      ENDDO
!
!
!=======================================================================
! COMPUTES MEAN C
!=======================================================================
!
      CALL OS('X=1/Y   ', X=T1, Y=T01)
      CALL OS('X=CX    ', X=T1, C=DEUPI)
      CALL OS('X=Y/Z   ', X=C, Y=T1, Z=K)
!
!
!=======================================================================
! COMPUTES MEAN CG
!=======================================================================
!
      DO I=1,NPOIN
        CG%R(I) = C%R(I)/2.D0 *
     &             (1.D0 + 2.D0*K%R(I)*H%R(I)/SINH(2.D0*K%R(I)*H%R(I)))
      ENDDO
!
      RETURN
      END
