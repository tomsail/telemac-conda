!                   ****************
                    SUBROUTINE CALCG
!                   ****************
!
     & (TRAV2,TRAV3,UETCAR,NPOIN2,NPLAN)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CALLED WHEN MODELLING THE INFLUENCE OF
!+                TURBULENCE ON THE SETTLING VELOCITY.
!
!history  C LE NORMANT
!+        01/08/97
!+        V5P4
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!history  J-M HERVOUET (LNHE)
!+        23/05/2011
!+        V6P1
!+   Coefficient UQUA removed in formula (after discussion with Thomas
!+   Benson (HRW) who pointed out a problem).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NOMBER OF POINTS IN THE 2D MESH
!| TRAV2          |-->| WORK ARRAYS
!| TRAV3          |<->| WORK ARRAYS
!| UETCAR         |-->| USTAR**2
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN2,NPLAN
      DOUBLE PRECISION, INTENT(IN)    :: UETCAR(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TRAV2(NPOIN2*NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: TRAV3(NPOIN2*NPLAN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN2,IPLAN
!
!=======================================================================
!
!     COMPUTES AUBORF IN TRAV1
!
      DO IPOIN2 = 1,NPOIN2
!
!        ACCORDING TO RICHARD SOULSBY (HRW):
!        G=SQRT(EPSILON/NU) AND EPSILON=(TAU/RHO)*(DU/DZ)
!        THEN G COULD BE ALSO SQRT(NUZ/NU)*DU/DZ
!
!
!                              USTAR**2 * DU/DZ
!        COMPUTES  G  =  SQRT( --------------------- ) : TRAV3
!                                     NU
!
        DO IPLAN = 1,NPLAN
          TRAV3(IPOIN2+(IPLAN-1)*NPOIN2) =
     &        SQRT(UETCAR(IPOIN2)*1.D06*TRAV2(IPOIN2+(IPLAN-1)*NPOIN2))
!
        ENDDO
!
      ENDDO
!
!=======================================================================
!
      RETURN
      END
