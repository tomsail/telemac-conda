!                   ******************
                    SUBROUTINE IFABTOM
!                   ******************
!
     &(IFABOR,NELEM2,NETAGE)
!
!***********************************************************************
! TELEMAC3D   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    FILLS 'IFABOR' OF MESH3D FOR TOMAWAC
!
!
!history  J-M HERVOUET (LNHE)
!+        28/09/2012
!+        V6P3
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IFABOR         |<->| CORRESPONDENCE BOUNDARY FACE - 2D ELEMENT 2D
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NETAGE         |-->| NUMBER OF PLANES - 1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_IFABTOM => IFABTOM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NELEM2,NETAGE
      INTEGER, INTENT(INOUT)       :: IFABOR(NELEM2,5,NETAGE)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM2,IETAGE,LOC
!
!=======================================================================
!  IFABOR ARRAYS
!=======================================================================
!
!     SEE SCHAR41 FOR THE MEANING OF LOC
!     1 IS : DO NOT RECOMPUTE VELOCITIES WHEN CROSSING A MESH PLANE
!     2 IS : RECOMPUTE VELOCITIES WHEN CROSSING A MESH PLANE
!
      LOC = 1
!
!  IFABOR ON THE VERTICAL FACES OF THE ELEMENTS OF THE FIRST LAYER
!
      DO IELEM2 = 1,NELEM2
!       ALREADY DONE BY 2D
!       IFABOR(IELEM2,1,IETAGE) = IFABOR(IELEM2,1,1)
!       IFABOR(IELEM2,2,IETAGE) = IFABOR(IELEM2,2,1)
!       IFABOR(IELEM2,3,IETAGE) = IFABOR(IELEM2,3,1)
        IFABOR(IELEM2,4,1) = LOC
        IFABOR(IELEM2,5,1) = LOC
      ENDDO
      IF(NETAGE.GE.2) THEN
        DO IETAGE = 2,NETAGE
          DO IELEM2 = 1,NELEM2
            IFABOR(IELEM2,1,IETAGE) = IFABOR(IELEM2,1,1)
            IFABOR(IELEM2,2,IETAGE) = IFABOR(IELEM2,2,1)
            IFABOR(IELEM2,3,IETAGE) = IFABOR(IELEM2,3,1)
            IFABOR(IELEM2,4,IETAGE) = LOC
            IFABOR(IELEM2,5,IETAGE) = LOC
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     IFABOR ON THE BOTTOM AND SURFACE
!
      DO IELEM2 = 1,NELEM2
!       -1 : SOLID
!        0 : LIQUID
!        1 : PERIODICITY (TREATED AS INTERNAL BOUNDARY)
        IFABOR(IELEM2,4,1)      = LOC
        IFABOR(IELEM2,5,NETAGE) = LOC
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
