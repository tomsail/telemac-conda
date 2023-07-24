!                       *****************
                        SUBROUTINE INVERT
!                       *****************
!
     &( RN    , N     , NP    )
!
!***********************************************************************
! TOMAWAC   V6P2                                   25/06/2012
!***********************************************************************
!
!brief    CALCULATION OF INVERSE MATRIX NXN
!+
!+            TAKEN FROM NUMERICAL RECEIPES IN FORTRAN 77
!+            USED IN THE FREE-MESH METHOD (DIFFRACTION)
!
!history  E. KRIEZI (LNH)
!+        04/12/2006
!+        V5P5
!+
!
!
!history  G.MATTAROLO (EDF - LNHE)
!+        23/06/2012
!+        V6P2
!+        Modification for V6P2
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| N              |-->| MATRIX DIMENSION
!| NP             |-->| MATRIX PHYSICAL DIMENSION
!| RN             |<->| MATRIX TO INVERT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_INVERT => INVERT
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN) :: N, NP
      DOUBLE PRECISION, INTENT(INOUT) :: RN(NP,NP)
!
!.....LOCAL VARIABLES
!     """""""""""""""
      INTEGER INDX(NP),I,J
      DOUBLE PRECISION Y(NP,NP), A(NP,NP)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      A=RN
      DO I=1,N
        DO J=1,N
          Y(I,J)=0.
        ENDDO
        Y(I,I)=1.
      ENDDO
      CALL LUDCMP(A,N,NP,INDX)
      DO J=1,N
        CALL LUBKSB(A,N,NP,INDX,Y(1,J))
      ENDDO
      RN=Y
!
      RETURN
      END

