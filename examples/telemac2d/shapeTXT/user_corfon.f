!                   **********************
                    SUBROUTINE USER_CORFON
!                   **********************
!
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!warning  USER SUBROUTINE
!
!history  Y AUDOUIN (LNHE)
!+        20/09/2018
!+        V8P0
!+
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NPOLY,ID,I,J,NMAX
      INTEGER, ALLOCATABLE :: NSOM(:)
      DOUBLE PRECISION, ALLOCATABLE :: XSOM(:,:),YSOM(:,:)
!-----------------------------------------------------------------------
!
      ID = T2D_FILES(T2DFO1)%LU

      REWIND ID

      READ(ID, *) NPOLY
      WRITE(LU,*) 'NPOLY',NPOLY
      ALLOCATE(NSOM(NPOLY))
      READ(ID,*) NSOM
      NMAX=0
      DO I=1,NPOLY
        IF (NSOM(I).GT.NMAX) NMAX=NSOM(I)
      ENDDO
      ALLOCATE(XSOM(NMAX,NPOLY))
      ALLOCATE(YSOM(NMAX,NPOLY))
      DO I=1,NPOLY
        DO J=1,NSOM(I)
          READ(ID,*) XSOM(J,I),YSOM(J,I)
          WRITE(LU,*) XSOM(J,I),YSOM(J,I)
        ENDDO
      ENDDO

      DO I=1,NPOIN
        IF (INPOLY( X(I), Y(I), XSOM(:,1), YSOM(:,1), NSOM(1)))
     &   ZF%R(I) = 3.9
        IF (INPOLY( X(I), Y(I), XSOM(:,2), YSOM(:,2), NSOM(2)))
     &   ZF%R(I) = 2.
      ENDDO
      RETURN
      END
