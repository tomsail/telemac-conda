!                       *******************
                        SUBROUTINE RPI_INTR
!                       *******************
!
     &(NEIGB  , NB_CLOSE, RX  , RY      , RXX     , RYY ,
     & NPOIN2 , I       , MAXNSP  , FFD , FIRDIV1 , FIRDIV2 ,
     & SECDIV1, SECDIV2 , SECDIV3, FRSTDIV , SCNDDIV)
!
!***********************************************************************
! TOMAWAC   V6P3                                   25/06/2012
!***********************************************************************
!
!brief    FREE-MESH METHOD FOR DIFFRACTION COMPUTATION
!+
!+            CALCULATES FIRST AND SECOND DERIVATIVE OF
!+            VARIABLE FFD
!
!history  E. KRIEZI (LNH)
!+        04/12/2006
!+        V5P5
!+
!
!history  G.MATTAROLO (EDF - LNHE)
!+        23/10/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  G.MATTAROLO (EDF - LNHE)
!+        23/06/2012
!+        V6P2
!+   Modification for V6P2
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        19/03/2013
!+        V6P3
!+   Arguments slightly changed to avoid copy before calling.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFD            |-->| INPUT FIELD FUNCTION
!| FIRDIV         |<--| FIRST DERIVATIVE OF FFD
!| FRSTDIV        |-->| IF TRUE COMPUTES 1ST DERIVATIVE
!| I              |-->| POINT INDEX
!| MAXNSP         |-->| CONSTANT FOR MESHFREE TECHNIQUE
!| NB_CLOSE       |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| NEIGB          |-->| NEIGHBOUR POINTS FOR MESHFREE METHOD
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| RX             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| RXX            |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| RY             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| RYY            |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| SECDIV         |<--| SECOND DERIVATIVE OF FFD
!| SCNDDIV        |-->| IF TRUE COMPUTES 2ND DERIVATIVE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEJA_RPI_INTR, WU_OM_RPI
      USE INTERFACE_TOMAWAC, EX_RPI_INTR => RPI_INTR
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN2, MAXNSP,I
      INTEGER, INTENT(IN) :: NEIGB(NPOIN2,MAXNSP),NB_CLOSE(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: RX(MAXNSP),RY(MAXNSP)
      DOUBLE PRECISION, INTENT(IN)    :: RXX(MAXNSP),RYY(MAXNSP)
      DOUBLE PRECISION, INTENT(INOUT) :: SECDIV1,SECDIV2,SECDIV3
      DOUBLE PRECISION, INTENT(INOUT) :: FIRDIV1,FIRDIV2
      DOUBLE PRECISION, INTENT(IN)    :: FFD(NPOIN2)
!
      LOGICAL, INTENT(IN)             :: FRSTDIV,SCNDDIV
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN,IP1

      DOUBLE PRECISION WZX1,WZY1,WZX2,WZY2
!
!-----------------------------------------------------------------------
!
      IF(.NOT.DEJA_RPI_INTR) THEN
        ALLOCATE(WU_OM_RPI(MAXNSP))
        DEJA_RPI_INTR=.TRUE.
      ENDIF
!
!     FFD the field function where data are coming from.
!
      DO IP1 =1,NB_CLOSE(I)
        IPOIN=NEIGB(I,IP1)
        WU_OM_RPI(IP1)=FFD(IPOIN)
      ENDDO
!
!     Calculate derivatives in IPOIN
!     TODO: never used still usefull ?
!
      IF(FRSTDIV) THEN
        WZX1=0.D0
        WZY1=0.D0
        DO IP1 =1,NB_CLOSE(I)
          WZX1=WZX1+RX(IP1)*WU_OM_RPI(IP1)
          WZY1=WZY1+RY(IP1)*WU_OM_RPI(IP1)
        ENDDO
        FIRDIV1=WZX1
        FIRDIV2=WZY1
      ENDIF
!
!     SECOND DERIVATIVES
!
      IF(SCNDDIV) THEN
        WZX2=0.D0
        WZY2=0.D0
        DO IP1 =1,NB_CLOSE(I)
          WZX2=WZX2+RXX(IP1)*WU_OM_RPI(IP1)
          WZY2=WZY2+RYY(IP1)*WU_OM_RPI(IP1)
        ENDDO
        SECDIV1=WZX2
        SECDIV2=WZY2
!       NOTE JMH : The only value really used
        SECDIV3= WZX2+WZY2
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
