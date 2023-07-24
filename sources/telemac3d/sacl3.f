!                   *****************
                    SUBROUTINE SACL3
!                   *****************
!
     &(NUBORF,LINUBOF,
     & NUBORL,LINUBOL,LIUBOL,
     & H,Z,NBOR,NPOIN2,NPLAN,NPTFR,
     & KARMAN,UETCAR,
     & NUMIN,KENT,KENTU,KSORT,KADH,KLOG,FICTIF)

!***********************************************************************
! TELEMAC3D   V8P0                                   21/08/2018
!***********************************************************************
!
!brief    COMPUTES KBOR, EBOR AND AUBOR WHEN THE TURBULENCE
!+                MODEL IS SPALART OR DES.
!
!
!history  A.Bourgoin
!+        27/02/2018
!+        V8P0
!+   Creation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AK             |-->| TURBULENT ENERGY
!| CMU            |-->| CONSTANT FOR MODELE K-EPSILON MODEL
!| DISBOR         |-->| DISTANCE TO BOUNDARY OF POINTS CLOSE TO BOUNDARY
!| EBORF          |<->| EPSILON ON BOTTOM
!| EBORL          |<->| EPSILON ON LATERAL SOLID BOUNDARIES
!| EBORS          |<->| EPSILON AT SURFACE
!| EMAX           |-->| MAXIMUM VALUE FOR EPSILON WHEN CLIPPING
!| EMIN           |-->| MINIMUM VALUE FOR EPSILON WHEN CLIPPING
!| H              |-->| WATER DEPTH AT TIME N
!| KADH           |-->| CONVENTION FOR NO SLIP BOUNDARY CONDITION
!| KARMAN         |-->| KARMAN CONSTANT
!| KBORF          |<->| K ON BOTTOM
!| KBORL          |<->| K ON LATERAL SOLID BOUNDARIES
!| KBORS          |<->| K AT SURFACE
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| KLOG           |-->| CONVENTION FOR LOGARITHMIC WALL
!| KMAX           |-->| MAXIMUM VALUE FOR K WHEN CLIPPING
!| KMIN           |-->| MINIMUM VALUE FOR K WHEN CLIPPING
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| LIEBOF         |<->| TYPE OF BOUNDARY CONDITIONS ON EPSILON AT THE BOTTOM
!| LIEBOL         |<->| TYPE OF BOUNDARY CONDITIONS ON EPSILON ON THE LATERAL WALLS
!| LIEBOS         |<->| TYPE OF BOUNDARY CONDITIONS ON EPSILON AT THE SURFACE
!| LIKBOF         |<->| TYPE OF BOUNDARY CONDITIONS ON K AT THE BOTTOM
!| LIKBOL         |<->| TYPE OF BOUNDARY CONDITIONS ON K ON THE LATERAL WALLS
!| LIKBOS         |<->| TYPE OF BOUNDARY CONDITIONS ON K AT THE SURFACE
!| LISRUF         |-->| TURBULENCE MODEL FOR BOTTOM
!|                |   | 1: SMOOTH  2: ROUGH  3: ROUGH (CHEZY)
!| LISRUL         |-->| TURBULENCE MODEL FOR SOLID BOUNDARIES
!|                |   | 1: SMOOTH  2: ROUGH  3: ROUGH (CHEZY)
!| LIUBOF         |<->| TYPE OF BOUNDARY CONDITIONS ON U AT THE BOTTOM
!| LIUBOL         |<->| TYPE OF BOUNDARY CONDITIONS ON U ON THE LATERAL WALLS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS IN 2D
!| RUGOL          |-->| NOT USED
!| UETCAL         |-->| (UETUTA*UTANG(IPTFR))**2: IN COMMENT
!| UETCAR         |-->| USTAR**2
!| VIRT           |-->| VIRTUAL ORIGIN FOR EPSILON (TELEMAC 3D): COMMENT
!| Z              |-->| ELEVATION OF REAL 3D MESH POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC3D, EX_SACL3 => SACL3
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC3D, ONLY: IPBOT
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPTFR, NPLAN, NPOIN2,KENTU
      INTEGER, INTENT(IN) :: KENT,KSORT,KADH,KLOG
!
      INTEGER, INTENT(INOUT) :: LINUBOF(NPOIN2)
      INTEGER, INTENT(INOUT) :: LINUBOL(NPTFR,NPLAN)
      INTEGER, INTENT(INOUT) :: LIUBOL(NPTFR,NPLAN)
!
      INTEGER, INTENT(IN) :: NBOR(NPTFR)
!
      DOUBLE PRECISION, INTENT(IN) :: Z(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: H(NPOIN2) ,UETCAR(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: NUBORF(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: NUBORL(NPTFR,NPLAN)

!
      DOUBLE PRECISION, INTENT(IN) :: NUMIN, KARMAN, FICTIF

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!-----------------------------------------------------------------------
!
      INTEGER IPTFR,IPLAN,IPOIN2,IP,IBOT
!
      DOUBLE PRECISION HAUT,DISTFOND
!     DOUBLE PRECISION DIST
!

!
      INTRINSIC SQRT,MAX
!
!-----------------------------------------------------------------------
!

!=======================================================================
!     BOTTOM
!=======================================================================
!
!
      DO IPOIN2=1,NPOIN2
!
        IF(IPBOT%I(IPOIN2).EQ.0) THEN
!         NORMAL CASE
          !DIST =(Z(IPOIN2,2)-Z(IPOIN2,1))/FICTIF
          IF(LINUBOF(IPOIN2).EQ.KENT) THEN
            NUBORF(IPOIN2)=NUMIN!MAX(KARMAN*DIST*SQRT(UETCAR(IPOIN2)),NUMIN)
          ENDIF
        ELSE
!         RISK OF SMASHED PLANES OR TIDAL FLATS
          IPLAN=IPBOT%I(IPOIN2)+1
          IF(IPLAN.EQ.NPLAN) THEN
!           CASE OF TIDAL FLATS
            IF(LINUBOF(IPOIN2).EQ.KENT) THEN
              NUBORF(IPOIN2)=NUMIN
            ENDIF
!           IN THIS CASE KBORF COMPUTED ABOVE MAY YIELD
!           ABNORMAL VALUES OF VISCOSITY
          ELSE
            !DIST =(Z(IPOIN2,IPLAN+1)-Z(IPOIN2,IPLAN))/FICTIF
!           CASE OF SMASHED PLANES : DIST COMPUTED ON FIRST FREE LAYER
            IF(LINUBOF(IPOIN2).EQ.KENT) THEN
              NUBORF(IPOIN2)=NUMIN!MAX(KARMAN*DIST*SQRT(UETCAR(IPOIN2)),NUMIN)
            ENDIF
          ENDIF
        ENDIF
!
      ENDDO
!
!=======================================================================
!     FREE SURFACE
!=======================================================================

!=======================================================================
!     LATERAL BOUNDARIES
!=======================================================================
!
      DO IPTFR=1,NPTFR
!
        IPOIN2 = NBOR(IPTFR)
        HAUT   = MAX(H(IPOIN2),1.D-7)

!
        DO IPLAN=1,NPLAN
!
          IP=MAX(IPLAN,2)
          IBOT=MIN(IPBOT%I(IPOIN2)+1,NPLAN-1)
!         DISTANCE TO BOTTOM (WILL BE 0 WITH TIDAL FLATS)
          DISTFOND = (Z(IPOIN2,IP)-Z(IPOIN2,IBOT))
          IF(LINUBOL(IPTFR,IPLAN).EQ.KENT) THEN

            IF(LIUBOL(IPTFR,IPLAN).EQ.KENT.OR.
     &         LIUBOL(IPTFR,IPLAN).EQ.KENTU.OR.
     &         LIUBOL(IPTFR,IPLAN).EQ.KSORT     ) THEN

              NUBORL(IPTFR,IPLAN)=MAX(KARMAN*(1.D0-DISTFOND/HAUT)**2*
     &                            SQRT(UETCAR(IPOIN2))*DISTFOND,NUMIN)

            ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KLOG .OR.
     &             LIUBOL(IPTFR,IPLAN).EQ.KADH) THEN
              NUBORL(IPTFR,IPLAN)=NUMIN
!
            ELSE
              WRITE(LU,112) IPTFR,LIUBOL(IPTFR,IPLAN)
              CALL PLANTE(1)
              STOP
!
!           *****
            ENDIF
!           *****
!
          ENDIF
!         -----

        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
112   FORMAT(' SACL3 : BOUNDARY NODE',I6,
     &       ' - UNEXPECTED CONDITION FOR KBOR : LIUBOR =',I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
