!                       *******************
                        SUBROUTINE RPI_INVR
!                       *******************
!
     &( X     , Y     , NEIGB , NB_CLOSE, RK_D , RX_D , RY_D  , RXX_D ,
     &  RYY_D , NPOIN2, I     , QUO   , AC    , MAXNSP, MINDIST )
!
!***********************************************************************
! TOMAWAC   V6P2                                   25/06/2012
!***********************************************************************
!
!brief    DIFFRACTION
!+         CALCULATION OF THE RADIAL FUNCTION FOR THE
!+         FREE-MESH METHOD
!
!history  E. KRIEZI (LNH)
!+        04/12/2006
!+        V5P5
!+
!
!history  G.MATTAROLO (EDF - LNHE)
!+        23/06/2012
!+        V6P2
!+   Modification for V6P2
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |-->| CONSTANT FOR RADIAL FUNCTION COMPUT.
!| I              |-->| POINT INDEX
!| MAXNSP         |-->| CONSTANT FOR MESHFREE TECHNIQUE
!| MINDIST        |-->| CONSTANT FOR RADIAL FUNCTION COMPUT.
!| NB_CLOSE       |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| NEIGB          |-->| NEIGHBOUR POINTS FOR MESHFREE METHOD
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| QUO            |-->| CONSTANT FOR RADIAL FUNCTION COMPUT.
!| RK_D           |<->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| RX_D           |<->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| RXX_D          |<->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| RY_D           |<->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| RYY_D          |<->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEJA_RPI,RK_I,RN,RX_I,RY_I,
     &                                 RXX_I,RYY_I,RAD1
      USE INTERFACE_TOMAWAC, EX_RPI_INVR => RPI_INVR
      IMPLICIT NONE
!
!     VARIABLES IN ARGUMENT
!
      INTEGER, INTENT(IN)    :: NPOIN2, MAXNSP, I
      INTEGER, INTENT(IN)    :: NEIGB(NPOIN2,MAXNSP), NB_CLOSE(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: QUO, AC
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2), Y(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: MINDIST(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: RK_D(MAXNSP)
      DOUBLE PRECISION, INTENT(INOUT) :: RX_D(MAXNSP), RY_D(MAXNSP)
      DOUBLE PRECISION, INTENT(INOUT) :: RXX_D(MAXNSP), RYY_D(MAXNSP)
!
!     LOCAL VARIABLES
!
      INTEGER IP, IPOIN, IP1, IPOIN1, NP

!
      DOUBLE PRECISION DC, WZ, WZX1, WZY1, WZX2, WZY2
!      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
! 
      IF(.NOT.DEJA_RPI)THEN
        ALLOCATE(RK_I(MAXNSP,MAXNSP))
        ALLOCATE(RN(MAXNSP,MAXNSP))
        ALLOCATE(RX_I(MAXNSP,MAXNSP))
        ALLOCATE(RY_I(MAXNSP,MAXNSP))
        ALLOCATE(RXX_I(MAXNSP,MAXNSP))
        ALLOCATE(RYY_I(MAXNSP,MAXNSP))
        ALLOCATE(RAD1(MAXNSP,MAXNSP))
        DEJA_RPI=.TRUE.
      ENDIF
!
      DO IP1 =1,NB_CLOSE(I)
        IP=NEIGB(I,IP1)
        DO IPOIN1 =1,NB_CLOSE(I)
          IPOIN=NEIGB(I,IPOIN1)
          RAD1(IP1,IPOIN1)=(X(IP)-X(IPOIN))**2+(Y(IP)-Y(IPOIN))**2
        ENDDO
      ENDDO
!
      DO IP1 =1,NB_CLOSE(I)
        IP=NEIGB(I,IP1)
        DC=MINDIST(I)
        DO IPOIN1 =1,NB_CLOSE(I)
          IPOIN=NEIGB(I,IPOIN1)
          RK_I(IP1,IPOIN1)=(RAD1(IP1,IPOIN1)+(AC*DC)**2)**QUO
!
!         First derivative
!
          RY_I(IP1,IPOIN1)=2.D0*QUO*(RAD1(IP1,IPOIN1)+
     &     (AC*DC)**2)**(QUO-1.D0)*(Y(IP)-Y(IPOIN))

          RX_I(IP1,IPOIN1)=2.*QUO*(RAD1(IP1,IPOIN1)+
     &     (AC*DC)**2)**(QUO-1.D0)*(X(IP)-X(IPOIN))
!
!         Second derivative
!
          RYY_I(IP1,IPOIN1) =2.D0*QUO*(RAD1(IP1,IPOIN1)+
     &(AC*DC)**2)**(QUO-1.D0)+4.D0*QUO*(QUO-1.D0)*
     &(RAD1(IP1,IPOIN1)+(AC*DC)**2)**(QUO-2.D0)*(Y(IP)-Y(IPOIN))**2
!
          RXX_I(IP1,IPOIN1)=2.D0*QUO*(RAD1(IP1,IPOIN1)+
     &(AC*DC)**2)**(QUO-1.D0)+4.D0*QUO*(QUO-1.D0)*
     &(RAD1(IP1,IPOIN1)+(AC*DC)**2)**(QUO-2.D0)*(X(IP)-X(IPOIN))**2
        ENDDO
      ENDDO
!
      RN=RK_I
      NP=NB_CLOSE(I)
!
      CALL INVERT(RN,NP,MAXNSP)
!
      DO IP1 =1,NB_CLOSE(I)
        WZ=0.D0
        WZX1=0.D0
        WZY1=0.D0
        WZX2=0.D0
        WZY2=0.D0
        DO IPOIN1 =1,NB_CLOSE(I)
          WZ=WZ+RK_I(1,IPOIN1)*RN(IPOIN1,IP1)
          WZX1=WZX1+RX_I(1,IPOIN1)*RN(IPOIN1,IP1)
          WZY1=WZY1+RY_I(1,IPOIN1)*RN(IPOIN1,IP1)
          WZX2=WZX2+RXX_I(1,IPOIN1)*RN(IPOIN1,IP1)
          WZY2=WZY2+RYY_I(1,IPOIN1)*RN(IPOIN1,IP1)
        ENDDO
!       write RK etc for the right form for each domain in one row
        RK_D(IP1)  = WZ
        RX_D(IP1)  = WZX1
        RY_D(IP1)  = WZY1
        RXX_D(IP1) = WZX2
        RYY_D(IP1) = WZY2
      ENDDO
!
      RETURN
      END
