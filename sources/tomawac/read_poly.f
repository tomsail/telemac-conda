!                   **********************
                    SUBROUTINE READ_POLY(WACZON,NPOLY,NPOIN,XPOIN,YPOIN)
!                   **********************
!***********************************************************************
! TOMAWAC   V8P4
!***********************************************************************
!     READ A LIST OF POLYGONES IN AN ASCII FILE (I2S FORMAT) to define
!     zones
!     The area is the whole domain if no file is given.
!***********************************************************************
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| WACZON         |-->| BIEF_FIEL CONTAINING THE POLYGON
!| NPOLY          |<--| NUMBER OF POLYGONS TO READ        
!| NPOIN          |<--| NUMBER OF POINTS IN THE POLYGONS
!| XPOIN          |<--| ABSCISSAE OF POINTS OF THE POLYGONS
!| YPOIN          |<--| ORDINATE OF POINTS OF THE POLYGONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE BIEF_DEF, ONLY : BIEF_FILE
      USE DECLARATIONS_TOMAWAC, ONLY: NPOIN2, X, Y
      IMPLICIT NONE
      TYPE(BIEF_FILE), TARGET,INTENT(IN) :: WACZON
      INTEGER, INTENT(INOUT):: NPOLY
      INTEGER, ALLOCATABLE, INTENT(OUT),DIMENSION(:) :: NPOIN
      DOUBLE PRECISION, ALLOCATABLE, INTENT(INOUT):: XPOIN(:,:),
     & YPOIN(:,:)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      INTEGER I,J,NMAX,FILEID
      DOUBLE PRECISION XMIN,YMIN,XMAX,YMAX
      DOUBLE PRECISION, PARAMETER :: EPSIL=1.D-8
      
      IF (WACZON%NAME.NE.' ') THEN
        FILEID=WACZON%LU
        REWIND FILEID
        READ(FILEID, *) NPOLY
        ALLOCATE(NPOIN(NPOLY))
        READ(FILEID,*) NPOIN
        NMAX=0
        DO I=1,NPOLY
          IF (NPOIN(I).GT.NMAX) NMAX=NPOIN(I)
        ENDDO
        ALLOCATE(XPOIN(NMAX,NPOLY))
        ALLOCATE(YPOIN(NMAX,NPOLY))
        DO I=1,NPOLY
          DO J=1,NPOIN(I)
            READ(FILEID,*) XPOIN(J,I),YPOIN(J,I)
          ENDDO
        ENDDO
      ELSE
        NPOLY=1
        ALLOCATE(NPOIN(NPOLY))
        NPOIN(1)=4
        ALLOCATE(XPOIN(4,NPOLY))
        ALLOCATE(YPOIN(4,NPOLY))
        XMIN=1.D308
        YMIN=1.D308
        XMAX=-1.D308
        YMAX=-1.D308
        DO I=1,NPOIN2
          IF (X(I).LT.XMIN) XMIN=X(I)
          IF (X(I).GT.XMAX) XMAX=X(I)
          IF (Y(I).LT.YMIN) YMIN=Y(I)
          IF (Y(I).GT.YMAX) YMAX=Y(I)
        ENDDO
        XPOIN(1,1)=XMIN-EPSIL
        XPOIN(2,1)=XMIN-EPSIL
        XPOIN(3,1)=XMAX+EPSIL
        XPOIN(4,1)=XMAX+EPSIL
        YPOIN(1,1)=YMIN-EPSIL
        YPOIN(2,1)=YMAX+EPSIL
        YPOIN(3,1)=YMAX+EPSIL
        YPOIN(4,1)=YMIN-EPSIL
      ENDIF  
      RETURN
      END
