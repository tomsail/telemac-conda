!                   *****************
                    SUBROUTINE CONDIM
!                   *****************
!
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    INITIALISES VELOCITY, DEPTH AND TRACERS.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  J-M HERVOUET(LNH)
!+        11/12/2000
!+        V5P1
!+   TELEMAC 3D VERSION 5.1
!
!history
!+        20/04/2007
!+
!+   ADDED INITIALISATION OF DPWAVE
!
!history
!+        23/01/2009
!+
!+   ADDED CHECK OF ZSTAR
!
!history
!+        16/03/2010
!+
!+   NEW OPTIONS FOR BUILDING THE MESH IN CONDIM, SEE BELOW
!
!history  J-M HERVOUET(LNHE)
!+        05/05/2010
!+        V6P0
!+   SUPPRESSED INITIALISATION OF DPWAVE
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
!history  M.S.TURNBULL (HRW), N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        C.-T. PHAM (LNHE)
!+        19/07/2012
!+        V6P2
!+   Addition of the TPXO tidal model by calling CONDI_TPXO
!+   (the TPXO model being coded in module TPXO)
!
!history  C.-T. PHAM (LNHE), M.S.TURNBULL (HRW)
!+        02/11/2012
!+        V6P3
!+   Correction of bugs when initialising velocity with TPXO
!+   or when sea levels are referenced with respect to Chart Datum (CD)
!
!history  C.-T. PHAM (LNHE)
!+        03/09/2015
!+        V7P1
!+   Change in the number of arguments when calling CONDI_TPXO
!
!history  C.-T. PHAM (LNHE)
!+        24/03/2017
!+        V7P3
!+   Split of CONDIM: calls of subroutines to define initial conditions
!    for different variables, one subroutine pro variable
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC3D, EX_CONDIM => CONDIM
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      USE BIEF
      USE TPXO
!
!     USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      INTEGER IPLAN,I,J
!
!-----------------------------------------------------------------------
!
!     ORIGIN OF TIME
!
      IF(.NOT.SUIT2) AT = 0.D0
!
!     INITIALISES H, THE WATER DEPTH
!
      IF(.NOT.SUIT2) THEN
!
        IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     &     CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
          CALL OS('X=C     ', X=H, C=0.D0)
          CALL OV('X=X-Y   ', X=H%R, Y=Z, DIM1=NPOIN2)
        ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     &         CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
          CALL OS('X=C     ', X=H, C=COTINI)
          CALL OV('X=X-Y   ', X=H%R, Y=Z, DIM1=NPOIN2)
        ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     &         CDTINI(1:10).EQ.'ZERO DEPTH') THEN
          CALL OS('X=C     ', X=H, C=0.D0)
        ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     &         CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
          CALL OS('X=C     ', X=H, C=HAUTIN)
        ELSEIF(CDTINI(1:25).EQ.'ALTIMETRIE SATELLITE TPXO'.OR.
     &         CDTINI(1:24).EQ.'TPXO SATELLITE ALTIMETRY') THEN
          CALL OS('X=-Y    ', X=H, Y=ZF)
          CALL CONDI_TPXO(NPOIN2,MESH2D%NPTFR,MESH2D%NBOR%I,
     &                    X2%R,Y2%R,H%R,U2D%R,V2D%R,
     &                    LIHBOR%I,LIUBOL%I,KENT,KENTU,
     &                    GEOSYST,NUMZONE,T3DL93,LATIT,LONGIT,
     &                    T3D_FILES,T3DBB1,T3DBB2,
     &                    MARDAT,MARTIM,INTMICON,MSL,
     &                    TIDALTYPE,BOUNDARY_COLOUR,ICALHWG,
     &                    I_ORIG,J_ORIG,HMIN_VIT_IC,VITINI_TPXO)
        ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     &         CDTINI(1:10).EQ.'PARTICULAR'.OR.
     &         CDTINI(1:07).EQ.'SPECIAL') THEN
          ! USER FUNCTION
          CALL USER_CONDI3D_H
        ELSE
          WRITE(LU,*) 'CONDIM: INITIAL CONDITION UNKNOWN: ',CDTINI
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        WRITE(LU,*) 'DEPTH IS READ IN THE BINARY FILE 1'
      ENDIF
!
!     CLIPS H
!
      DO I=1,NPOIN2
        H%R(I)=MAX(H%R(I),0.D0)
      ENDDO
!
      CALL OS('X=Y     ', X=HN, Y=H)
!
!-----------------------------------------------------------------------
!
!     DATA TO BUILD VERTICAL COORDINATES IN CALCOT
!
!     STANDARD BELOW IS: EVENLY SPACED PLANES, NO OTHER DATA REQUIRED
!
      DO IPLAN = 1,NPLAN
        TRANSF_PLANE%I(IPLAN)=1
      ENDDO
!     OTHERWISE: USER_MESH_TRANSF
      ! USER FUNCTION
      CALL USER_MESH_TRANSF
!
!***********************************************************************
!
!     COMPUTES ELEVATIONS
!     IF IT IS A CONTINUATION, WILL BE DONE AFTER CALLING 'SUITE'
!
      IF(DEBU) CALL CALCOT(Z,H%R)
!
!***********************************************************************
!
!     INITIALISES VELOCITIES
!
      IF(SUIT2) THEN
        DO I=1,NPLAN
          DO J=1,NPOIN2
            U%R((I-1)*NPOIN2+J)=U2D%R(J)
            V%R((I-1)*NPOIN2+J)=V2D%R(J)
          ENDDO
        ENDDO
      ELSEIF(CDTINI(1:25).EQ.'ALTIMETRIE SATELLITE TPXO'.OR.
     &       CDTINI(1:24).EQ.'TPXO SATELLITE ALTIMETRY') THEN
!
!     U2D, V2D PREVIOUSLY COMPUTED WITH CONDI_TPXO
!
        DO I=1,NPLAN
          DO J=1,NPOIN2
            U%R((I-1)*NPOIN2+J)=U2D%R(J)
            V%R((I-1)*NPOIN2+J)=V2D%R(J)
          ENDDO
        ENDDO
      ELSE
        CALL OS('X=0     ' , X=U)
        CALL OS('X=0     ' , X=V)
      ENDIF
!
      CALL OS('X=0     ' , X=W)
      ! USER FUNCTION
      CALL USER_CONDI3D_UVW
!
!-----------------------------------------------------------------------
!
!     INITIALISES TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          CALL OS( 'X=C     ', X=TA%ADR(I)%P, C=TRAC0(I))
        ENDDO
      ENDIF
      ! USER FUNCTION
      CALL USER_CONDI3D_TRAC
!
!-----------------------------------------------------------------------
!     INITIALISES THE K-EPSILON MODEL (OPTIONAL)
!     WHEN DONE: AKEP = .FALSE.
!
      AKEP=.TRUE.
      ! USER FUNCTION
      CALL USER_CONDI3D_KEP
!
!-----------------------------------------------------------------------
!
!     INITIALISES THE PRESSURE FIELDS TO 0. IF NON-HYDROSTATIC VERSION
!
      IF(NONHYD) THEN
        CALL OS('X=C     ',X=DP,C=0.D0)
        WRITE (LU,*) 'CONDIM: DYNAMIC PRESSURE INITIALISED TO ZERO'
        CALL OS('X=C     ',X=PH,C=0.D0)
        WRITE (LU,*) '        HYDROSTATIC PRESSURE INITIALISED TO ZERO.'
      ENDIF
      ! USER FUNCTION
      CALL USER_CONDI3D_P
!
!-----------------------------------------------------------------------
!
      RETURN
      END
