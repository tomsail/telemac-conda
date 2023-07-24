!                   *****************
                    SUBROUTINE KOMCL3
!                   *****************
!
     & (KBORF,EBORF,LIKBOF,LIEBOF,LIUBOF,
     &  KBORL,EBORL,LIKBOL,LIEBOL,LIUBOL,
     &  EBORS,LIEBOS,
     &  DISBOR,AK,U,V,H,Z,NBOR,NPOIN2,NPLAN,NPTFR,
     &  KARMAN, BETAS,OMSTAR,
     &  KMIN,EMIN,KENTU,KENT,KADH,KLOG,
     &  UETCAR,UETCAL)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES KBOR, EBOR AND AUBOR WHEN THE TURBULENCE
!+                MODEL IS K-OMEGA.
!
!history  HOLGER WEILBEER   ISEB/UHA
!+        **/02/2001
!+
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
!history  J.-M. HERVOUET (LNHE)
!+        20/05/2011
!+        V6P1
!+   Case of distance to bottom=0 treated with array IPBOT
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AK             |-->| TURBULENT ENERGY
!| BETAS          |-->| K-OMEGA CONSTANT
!| DISBOR         |-->| DISTANCE TO BOUNDARY OF POINTS CLOSE TO BOUNDARY
!| EBORF          |<->| EPSILON ON BOTTOM
!| EBORL          |<->| EPSILON ON LATERAL SOLID BOUNDARIES
!| EBORS          |<->| EPSILON AT SURFACE
!| EMIN           |-->| MINIMUM VALUE FOR EPSILON WHEN CLIPPING
!| H              |-->| WATER DEPTH AT TIME N
!| KADH           |-->| CONVENTION FOR NO SLIP BOUNDARY CONDITION
!| KARMAN         |-->| KARMAN CONSTANT
!| KBORF          |<->| K ON BOTTOM
!| KBORL          |<->| K ON LATERAL SOLID BOUNDARIES
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| KLOG           |-->| CONVENTION FOR LOGARITHMIC SOLID BOUNDARY
!| KMIN           |-->| MINIMUM VALUE FOR K WHEN CLIPPING
!| LIEBOF         |<->| TYPE OF BOUNDARY CONDITIONS ON EPSILON AT THE BOTTOM
!| LIEBOL         |<->| TYPE OF BOUNDARY CONDITIONS ON EPSILON ON THE LATERAL WALLS
!| LIEBOS         |<->| TYPE OF BOUNDARY CONDITIONS ON EPSILON AT THE SURFACE
!| LIKBOF         |<->| TYPE OF BOUNDARY CONDITIONS ON K AT THE BOTTOM
!| LIKBOL         |<->| TYPE OF BOUNDARY CONDITIONS ON K ON THE LATERAL WALLS
!|                |   | 1: SMOOTH  2: ROUGH  3: ROUGH (CHEZY)
!| LIUBOF         |<->| TYPE OF BOUNDARY CONDITIONS ON U AT THE BOTTOM
!| LIUBOL         |<->| TYPE OF BOUNDARY CONDITIONS ON U ON THE LATERAL WALLS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS IN 2D
!| OMSTAR         |-->| K-OMEGA CONSTANT
!| U              |-->| X COMPONENT OF VELOCITY AT TIME N
!| UETCAL         |-->| (UETUTA*UTANG(IPTFR))**2
!| UETCAR         |-->| USTAR**2
!| V              |-->| Y COMPONENT OF VELOCITY AT TIME N
!| Z              |-->| ELEVATION OF REAL 3D MESH POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC3D, ONLY : IPBOT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPTFR, NPLAN, NPOIN2
      INTEGER, INTENT(IN) :: KENTU,KENT,KADH,KLOG
!
      INTEGER, INTENT(INOUT) :: LIKBOF(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIKBOL(NPTFR,NPLAN)
      INTEGER, INTENT(INOUT) :: LIEBOF(NPOIN2), LIEBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIEBOL(NPTFR,NPLAN)
      INTEGER, INTENT(INOUT) :: LIUBOF(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIUBOL(NPTFR,NPLAN)
!
      INTEGER, INTENT(IN) :: NBOR(NPTFR)
!
      DOUBLE PRECISION, INTENT(IN) :: U(NPOIN2,NPLAN), V(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: Z(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: AK(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: H(NPOIN2),UETCAR(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: KBORF(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: KBORL(NPTFR,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: EBORF(NPOIN2), EBORS(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: EBORL(NPTFR,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: UETCAL(NPTFR,NPLAN)
!
      DOUBLE PRECISION, INTENT(IN)    :: DISBOR(NPTFR)
!
      DOUBLE PRECISION, INTENT(IN) :: KARMAN
      DOUBLE PRECISION, INTENT(IN) :: BETAS,OMSTAR
      DOUBLE PRECISION, INTENT(IN) :: KMIN, EMIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION DISTFOND
      DOUBLE  PRECISION, PARAMETER :: FICTIFUET = 3.7D0
      DOUBLE  PRECISION, PARAMETER :: FICTIFOM  = 2.D0
      DOUBLE  PRECISION, PARAMETER :: NIVTURB = 0.02D0
      DOUBLE  PRECISION, PARAMETER :: TESTREICH = 1.D-4
      INTEGER, PARAMETER :: MAXITEREICH = 30
!
!-----------------------------------------------------------------------
!
      INTEGER IPTFR, IPLAN, IPOIN2
!
      DOUBLE PRECISION HAUT
      DOUBLE PRECISION SBETAS, DIST
!     DOUBLE PRECISION UTANG
!
      INTRINSIC SQRT,MAX
!
!-----------------------------------------------------------------------
!
      SBETAS = 1.D0/SQRT(BETAS)
!
!=======================================================================
!
!                        /* LOOP ON THE BOTTOM */
!
!=======================================================================
!
      DO IPOIN2=1,NPOIN2
!
!       DIRICHLET ON K
!       ---------------
!
        IF(LIKBOF(IPOIN2).EQ.KENT) THEN
!       -----------------------------
!
          IF(LIUBOF(IPOIN2).EQ.KLOG) THEN
            KBORF(IPOIN2) = MAX(SBETAS*UETCAR(IPOIN2),KMIN)
          ELSE
            KBORF(IPOIN2) = KMIN
          ENDIF
!
        ENDIF
!
!
!       DIRICHLET ON EPSILON
!       ---------------------
!
        IF(LIEBOF(IPOIN2).EQ.KENT) THEN
!       -------------------------------
!
          IF(LIUBOF(IPOIN2).EQ.KLOG) THEN
            IF(IPBOT%I(IPOIN2).EQ.0) THEN
!             NORMAL CASE
              DIST  = (Z(IPOIN2,2)-Z(IPOIN2,1)) / FICTIFUET
              EBORF(IPOIN2) = MAX(SBETAS*SQRT(UETCAR(IPOIN2))
     &                                 /(KARMAN*DIST),EMIN)
            ELSE
!             RISK OF SMASHED PLANES OR TIDAL FLATS
              IPLAN=IPBOT%I(IPOIN2)+1
              IF(IPLAN.EQ.NPLAN) THEN
!               CASE OF TIDAL FLATS
                EBORF(IPOIN2)=EMIN
              ELSE
!               CASE OF SMASHED PLANES : DIST COMPUTED ON FIRST FREE LAYER
                DIST =(Z(IPOIN2,IPLAN+1)-Z(IPOIN2,IPLAN)) / FICTIFUET
                EBORF(IPOIN2) = MAX(SBETAS*SQRT(UETCAR(IPOIN2))
     &                                             /(KARMAN*DIST),EMIN)
              ENDIF
            ENDIF
          ENDIF
!
        ENDIF
!
      ENDDO
!
!=======================================================================
!
!                        /* LOOP ON THE SURFACE */
!
!=======================================================================
!
      DO IPOIN2=1,NPOIN2
!
        HAUT = MAX(H(IPOIN2),1.D-7)
!
!       DIRICHLET ON EPSILON
!       ---------------------
!
        IF(LIEBOS(IPOIN2).EQ.KENT) THEN
!       -------------------------------
!
          EBORS(IPOIN2) = OMSTAR*SQRT(AK(IPOIN2,NPLAN))/HAUT
!
        ENDIF
!
      ENDDO
!
!=======================================================================
!
!                        /* LOOP ON THE LATERAL BOUNDARIES */
!
!
!  COMPUTES KBOR,EBOR, AND AUBOR
!
!=======================================================================
!
      DO IPTFR=1,NPTFR
!
        IPOIN2 = NBOR(IPTFR)
        DIST   = DISBOR(IPTFR) / FICTIFUET
        HAUT   = MAX( H(IPOIN2), 1.D-7 )
!
        DO IPLAN=1,NPLAN
!
!BOY COMPUTES THE DISTANCE TO THE BOTTOM
          IF(IPLAN .EQ. 1) THEN
            DISTFOND = (Z(IPOIN2,2)-Z(IPOIN2,1)) / FICTIFUET
          ELSE
            DISTFOND = (Z(IPOIN2,IPLAN)-Z(IPOIN2,1))
          ENDIF
! COMPUTES THE TANGENTIAL SPEED
!         UTANG = SQRT(U(IPOIN2,IPLAN)**2+V(IPOIN2,IPLAN)**2)
!
!         DIRICHLET ON K
!         ---------------
!
          IF(LIKBOL(IPTFR,IPLAN).EQ.KENT) THEN
!         ------------------------------------
!         ************************************
            IF(LIUBOL(IPTFR,IPLAN).EQ.KENT.OR.
     &         LIUBOL(IPTFR,IPLAN).EQ.KENTU) THEN
!           ************************************
!
!             COMING INTO THE DOMAIN
!
              KBORL(IPTFR,IPLAN) = MAX(NIVTURB*U(IPOIN2,IPLAN),KMIN)
!
!           ****************************************
            ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KLOG) THEN
!           ****************************************
!
!             WALL
!
              KBORL(IPTFR,IPLAN)=MAX(SBETAS*UETCAL(IPTFR,IPLAN),KMIN)
!
!           ****************************************
            ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KADH) THEN
!           ****************************************
!
              KBORL(IPTFR,IPLAN) = KMIN
!
!           ****
            ELSE
!           ****
!
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
!
!         DIRICHLET ON EPSILON
!         ---------------------
!
          IF(LIEBOL(IPTFR,IPLAN).EQ.KENT) THEN
!         ------------------------------------
!
!           ************************************
            IF(LIUBOL(IPTFR,IPLAN).EQ.KENT.OR.
     &         LIUBOL(IPTFR,IPLAN).EQ.KENTU) THEN
!            ************************************
!
!             COMING INTO THE DOMAIN : TURBULENCE DUE TO BOTTOM
!
! BOUNDARY CONDITIONS COMING RFOM THE K-EPSILON MODEL:
!                  EBORL(IPTFR,IPLAN) = CMU**0.75*KBORL(IPTFR,1)**1.5
!     &                      /KARMAN/DISTFOND
! AUXILIARY RELATION EPSILON=BETAS*K*OMEGA LEADS TO:
!
              EBORL(IPTFR,IPLAN)=BETAS**(-0.25)*SQRT(KBORL(IPTFR,1))
     &                          /KARMAN/MAX(DISTFOND,1.D-4)
              EBORL(IPTFR,IPLAN)= MAX(EBORL(IPTFR,IPLAN),EMIN)
!
!           ****************************************
            ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KLOG) THEN
!           ****************************************
!
!             WALL
!
              EBORL(IPTFR,IPLAN) =
     &        MAX(SBETAS*SQRT(UETCAL(IPTFR,IPLAN))/(KARMAN*DIST),EMIN)
!
!           ****************************************
            ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KADH) THEN
!           ****************************************
!
!             OMEGA = USTAR**2*SR/NUE ...
!
              EBORL(IPTFR,IPLAN) = 2500.D0
!
!           ****
            ELSE
!           ****
!
!             OTHER
!
              WRITE(LU,122) IPTFR,LIUBOL(IPTFR,IPLAN)
              CALL PLANTE(1)
              STOP
!
!           *****
            ENDIF
!           *****
!
          ENDIF
!         -----
!
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
112   FORMAT(' KOMCL3 : BOUNDARY NODE',I6,
     &       ' - UNEXPECTED CONDITION FOR KBOR : LIUBOR =',I6)
122   FORMAT(' KOMCL3 : BOUNDARY NODE',I6,
     &       ' - UNEXPECTED CONDITION FOR EBOR : LIUBOR =',I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
