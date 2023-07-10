!                   *****************
                    SUBROUTINE KEPCL3
!                   *****************
!
     &(KBORF,EBORF,LIKBOF,LIEBOF,
     & KBORL,EBORL,LIKBOL,LIEBOL,LIUBOL,
     & KBORS,EBORS,LIKBOS,LIEBOS,
     & DISBOR,AK,H,Z,NBOR,NPOIN2,NPLAN,NPTFR,
     & KARMAN,CMU,
     & KMIN,EMIN,KENT,KENTU,KSORT,KADH,KLOG,
     & UETCAR,FICTIF)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    COMPUTES KBOR, EBOR AND AUBOR WHEN THE TURBULENCE
!+                MODEL IS K-EPSILON.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
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
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!history  C.T. PHAM (LNHE)
!+        02/12/2015
!+        V7P1
!+   Adding an option for the boundary conditions of k and epsilon
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AK             |-->| TURBULENT ENERGY
!| CMU            |-->| CONSTANT FOR MODELE K-EPSILON MODEL
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
!|                |   | 1: SMOOTH  2: ROUGH  3: ROUGH (CHEZY)
!| LIUBOF         |<->| TYPE OF BOUNDARY CONDITIONS ON U AT THE BOTTOM
!| LIUBOL         |<->| TYPE OF BOUNDARY CONDITIONS ON U ON THE LATERAL WALLS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS IN 2D
!| UETCAR         |-->| USTAR**2
!| Z              |-->| ELEVATION OF REAL 3D MESH POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC3D, ONLY: IPBOT, RUGOF, U
!
      USE INTERFACE_TELEMAC3D, EX_KEPCL3 => KEPCL3
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPTFR, NPLAN, NPOIN2,KENTU
      INTEGER, INTENT(IN) :: KENT,KSORT,KADH,KLOG
!
      INTEGER, INTENT(INOUT) :: LIKBOF(NPOIN2), LIKBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIKBOL(NPTFR,NPLAN)
      INTEGER, INTENT(INOUT) :: LIEBOF(NPOIN2), LIEBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIEBOL(NPTFR,NPLAN)
      INTEGER, INTENT(INOUT) :: LIUBOL(NPTFR,NPLAN)
!
      INTEGER, INTENT(IN) :: NBOR(NPTFR)
!
      DOUBLE PRECISION, INTENT(IN) :: Z(NPOIN2,NPLAN), AK(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: H(NPOIN2)      , UETCAR(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: KBORF(NPOIN2), KBORS(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: KBORL(NPTFR,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: EBORF(NPOIN2), EBORS(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: EBORL(NPTFR,NPLAN)
!
      DOUBLE PRECISION, INTENT(IN)    :: DISBOR(NPTFR)
!
      DOUBLE PRECISION, INTENT(IN) :: KARMAN
      DOUBLE PRECISION, INTENT(IN) :: CMU
      DOUBLE PRECISION, INTENT(IN) :: KMIN, EMIN
      DOUBLE PRECISION, INTENT(IN) :: FICTIF
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!-----------------------------------------------------------------------
!
      INTEGER IPTFR,IPLAN,IPOIN2
!     INTEGER IBOT, IP
!
      DOUBLE PRECISION ESURF,HAUT,SSQCMU,DIST,DISTFOND
!     DOUBLE PRECISION Z0
!
      DOUBLE PRECISION, PARAMETER :: NIVTURB = 0.005D0
      DOUBLE PRECISION, PARAMETER :: TESTREICH = 1.D-4
      INTEGER, PARAMETER :: MAXITEREICH = 30
! BEGIN OF PART SPECIFIC TO THIS CASE
      DOUBLE PRECISION, PARAMETER :: FICTIFUET = 2.D0
      DOUBLE PRECISION D_SURF
! END OF PART SPECIFIC TO THIS CASE
!
      INTRINSIC SQRT,MAX
!
!-----------------------------------------------------------------------
!
      SSQCMU = 1.D0 /SQRT(CMU)
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
          DIST =(Z(IPOIN2,2)-Z(IPOIN2,1))/FICTIF
          IF(LIEBOF(IPOIN2).EQ.KENT) THEN
            EBORF(IPOIN2)=MAX(UETCAR(IPOIN2)*SQRT(UETCAR(IPOIN2))
     &                                             /(KARMAN*DIST),EMIN)
          ENDIF
          IF(LIKBOF(IPOIN2).EQ.KENT) THEN
            KBORF(IPOIN2) = MAX(SSQCMU*UETCAR(IPOIN2),KMIN)
          ENDIF
        ELSE
!         RISK OF SMASHED PLANES OR TIDAL FLATS
          IPLAN=IPBOT%I(IPOIN2)+1
          IF(IPLAN.EQ.NPLAN) THEN
!           CASE OF TIDAL FLATS
            IF(LIEBOF(IPOIN2).EQ.KENT) THEN
              EBORF(IPOIN2)=EMIN
            ENDIF
!           IN THIS CASE KBORF COMPUTED ABOVE MAY YIELD
!           ABNORMAL VALUES OF VISCOSITY
            IF(LIKBOF(IPOIN2).EQ.KENT) KBORF(IPOIN2)=KMIN
          ELSE
!           CASE OF SMASHED PLANES : DIST COMPUTED ON FIRST FREE LAYER
            DIST =(Z(IPOIN2,IPLAN+1)-Z(IPOIN2,IPLAN))/FICTIF
            IF(LIEBOF(IPOIN2).EQ.KENT) THEN
              EBORF(IPOIN2)=MAX(UETCAR(IPOIN2)*
     &                         SQRT(UETCAR(IPOIN2))/(KARMAN*DIST),EMIN)
            ENDIF
            IF(LIKBOF(IPOIN2).EQ.KENT) THEN
              KBORF(IPOIN2) = MAX(SSQCMU*UETCAR(IPOIN2),KMIN)
            ENDIF
          ENDIF
        ENDIF
!
      ENDDO
!
!=======================================================================
!     FREE SURFACE
!=======================================================================
!
      DO IPOIN2=1,NPOIN2
!
!       DIRICHLET ON EPSILON
!       ---------------------
!
        IF(LIEBOS(IPOIN2).EQ.KENT) THEN
!
!         NEZU & NAKAGAWA: TURBULENCE IN OPEN CHANNEL FLOWS
!
!         SEE KEPICL: THIS VALUE IS NOT USED IF CONDITION
!                     IS NOT SET TO KENT ON EPSILON
          HAUT = MAX(H(IPOIN2),1.D-7)
          ESURF = SQRT(AK(IPOIN2,NPLAN)**3) / (0.18D0*HAUT)
!
!         5.9
!         ESURF = CMU**0.75D0 / KARMAN
!    &          * AK(IPOIN2,NPLAN)**1.5D0 / (VIRT*HAUT)
          EBORS(IPOIN2) = MAX(ESURF,EMIN)
!
        ENDIF
!
!       DIRICHLET ON K
!       --------------
!       NOTE: THIS IS NOT THE DEFAULT BC, HOMOGENEOUS DIRICHLET
!       FOR K AT FREE SURFACE IS PROBABLY WRONG
!
        IF(LIKBOS(IPOIN2).EQ.KENT) THEN
          KBORS(IPOIN2) = MAX(0.D0,KMIN)
        ENDIF
!
      ENDDO
!
!=======================================================================
!     LATERAL BOUNDARIES
!=======================================================================
!
      DO IPTFR=1,NPTFR
!
        IPOIN2 = NBOR(IPTFR)
        DIST   = DISBOR(IPTFR) / FICTIF
        HAUT   = MAX(H(IPOIN2),1.D-7)
!
!       Z0=RUGOF%R(IPOIN2)/30.D0
!
        DO IPLAN=1,NPLAN
!
! BEGIN OF PART SPECIFIC TO THIS CASE
!         IP=MAX(IPLAN,2)
!         IBOT=MIN(IPBOT%I(IPOIN2)+1,NPLAN-1)
!         DISTANCE TO BOTTOM (WILL BE 0 WITH TIDAL FLATS)
!         DISTFOND = (Z(IPOIN2,IP)-Z(IPOIN2,IBOT))
!
          IF(IPLAN.EQ.1.OR.IPLAN.EQ.(NPLAN+1)/2) THEN
            DISTFOND =  (Z(IPOIN2,2)-Z(IPOIN2,1)) / FICTIFUET
          ELSEIF(IPLAN.GT.((NPLAN+1)/2)) THEN
!EGR DISTANCE BETWEEN 2 LAYERS
            DISTFOND =  Z(IPOIN2,IPLAN)-Z(IPOIN2,(NPLAN+1)/2)
!EGR POURQUOI EST-CE QU ON DIVISE PAR LA HAUTEUR DE LA COUCHE DU HAUT ?
            D_SURF = (Z(IPOIN2,NPLAN)-Z(IPOIN2,IPLAN))/
     &               (Z(IPOIN2,NPLAN)-Z(IPOIN2,(NPLAN+1)/2))
          ELSE
!EGR MINIMUM DISTANCE BETWEEN BOTTOM AND UPPER LAYER
            DISTFOND = MIN(Z(IPOIN2,(NPLAN+1)/2)-Z(IPOIN2,IPLAN),
     &                     Z(IPOIN2,IPLAN)-Z(IPOIN2,1))
          ENDIF
! END OF PART SPECIFIC TO THIS CASE
!
!         DIRICHLET ON K
!         ---------------
!
          IF(LIKBOL(IPTFR,IPLAN).EQ.KENT) THEN
!         ------------------------------------
!
!           ************************************
            IF(LIUBOL(IPTFR,IPLAN).EQ.KENT.OR.
     &         LIUBOL(IPTFR,IPLAN).EQ.KENTU.OR.
     &         LIUBOL(IPTFR,IPLAN).EQ.KSORT     ) THEN
!           ************************************
!
!          THEORY BY VINCENT BOYER (SEE ALSO KEPINI)
!
!          KBORL(IPTFR,IPLAN) = MAX(NIVTURB*U(IPOIN2,IPLAN)**2,KMIN)
!
! BEGIN OF PART SPECIFIC TO THIS CASE
              IF(IPLAN.GT.(NPLAN+1)/2) THEN
                KBORL(IPTFR,IPLAN) = MAX(NIVTURB*
     &                 U%R(IPOIN2+((NPLAN+1)/2-1)*NPOIN2)**2*D_SURF,
     &                 KMIN)
              ELSE
                KBORL(IPTFR,IPLAN) = MAX(NIVTURB*U%R(IPOIN2)**2,KMIN)
              ENDIF
!
!          NO TURBULENCE
!
!           KBORL(IPTFR,IPLAN) = KMIN
!           CV  HANS AND BURCHARD CL FOR K
!            KBORL(IPTFR,IPLAN)=UETCAR(IPOIN2)
!     &                        *(1.D0-DISTFOND/HAUT)/SQRT(CMU)
! END OF PART SPECIFIC TO THIS CASE
!
!            ****************************************
            ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KLOG .OR.
     &             LIUBOL(IPTFR,IPLAN).EQ.KADH) THEN
!           ****************************************
!
!             WALL
!
              KBORL(IPTFR,IPLAN)=KMIN
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
     &         LIUBOL(IPTFR,IPLAN).EQ.KENTU.OR.
     &         LIUBOL(IPTFR,IPLAN).EQ.KSORT     ) THEN
!           ************************************
!
!              COMING IN THE DOMAIN: TURBULENCE DUE TO THE
!              BOTTOM AS IN KEPINI; COMPUTES EBORL ACCORDING
!              TO KBORL AT THE BOTTOM
! BEGIN OF PART SPECIFIC TO THIS CASE
              EBORL(IPTFR,IPLAN)=CMU**0.75*SQRT(KBORL(IPTFR,1)**3)
     &                          /KARMAN/MAX(DISTFOND,1.D-6)
              EBORL(IPTFR,IPLAN)= MAX(EBORL(IPTFR,IPLAN),EMIN)
!               EBORL(IPTFR,IPLAN)=EMIN
!             Hans et Burchard
!             CV ...
!              EBORL(IPTFR,IPLAN)=SQRT(UETCAR(IPOIN2))**3
!     &                          *(1.D0-DISTFOND/HAUT)
!     &                          /KARMAN/MAX(DISTFOND,Z0)
! END OF PART SPECIFIC TO THIS CASE
!
!           ****************************************
            ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KLOG .OR.
     &             LIUBOL(IPTFR,IPLAN).EQ.KADH) THEN
!           ****************************************
!
!             WALL
!
!             EBORL(IPTFR,IPLAN) =
!    &        MAX(UETCAL(IPTFR,IPLAN)*SQRT(UETCAL(IPTFR,IPLAN))/
!    &        (KARMAN*DIST*FICTIFUET/FICTIFEPS),EMIN)
              EBORL(IPTFR,IPLAN)=EMIN
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
!          -----
!
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
112   FORMAT(' KEPCL3 : BOUNDARY NODE',I6,
     &       ' - UNEXPECTED CONDITION FOR KBOR : LIUBOR =',I6)
122   FORMAT(' KEPCL3: BOUNDARY NODE',I6,
     &       ' - UNEXPECTED CONDITION FOR EBOR : LIUBOR =',I6)
132   FORMAT(' OPTION ',I1, 'UNEXPECTED FOR THE COMPUTATION OF THE',/,
     &       1X,'BOUNDARY CONDITIONS OF K AND EPSILON')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
