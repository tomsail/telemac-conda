!                   *****************
                    SUBROUTINE SOUKOM
!                   *****************
!
     &(CV1,CV2,S1K,S1E,U,V,W,DELTAR,
     & DUDX,DUDY,DUDZ,DVDX,DVDY,DVDZ,DWDX,DWDY,DWDZ,DTADZ,
     & DKDX,DKDY,DKDZ,DODX,DODY,DODZ,ROTAT,
     & AK,EP,ALPHA,BETA,BETAS,GRAV,TR,NPOIN3,MSK,MASKEL,MESH3D,IELM3,S,
     & PRANDTL)
!
!***********************************************************************
! TELEMAC3D   V8P4
!***********************************************************************
!
!brief    PREPARES THE SOURCES TERMS IN THE DIFFUSION EQUATION OF
!+                K AND OMEGA.
!
!history  HOLGER WEILBEER   ISEB/UHA
!+        01/02/01
!+
!+
!
!history  J-M HERVOUET (LNHE)
!+        24/08/07
!+        V6P0
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AK             |-->| TURBULENT ENERGY K
!| ALPHA          |-->| K-OMEGA CONSTANT
!| BETA           |-->| K-OMEGA CONSTANT
!| BETAS          |-->| K-OMEGA CONSTANT
!| CV1            |<->| SOURCE TERM FOR K AND OMEGA
!| CV2            |<->| SOURCE TERM FOR K AND OMEGA
!| DELTAR         |-->| DELTA(RHO)/RHO
!| DKDX           |<->| DK/DX
!| DKDY           |<->| DK/DY
!| DKDZ           |<->| DK/DZ
!| DODX           |<->| DOMEGA/DX
!| DODY           |<->| DOMEGA/DY
!| DODZ           |<->| DOMEGA/DZ
!| DT             |-->| TIME STEP
!| DTADZ          |<->| DERIVATIVE OF TRACEUR N0 1 WITH RESPECT TO Z
!| DUDX           |<->| DU/DX
!| DUDY           |<->| DU/DY
!| DUDZ           |<->| DU/DZ
!| DVDX           |<->| DV/DX
!| DVDY           |<->| DV/DY
!| DVDZ           |<->| DV/DZ
!| DWDX           |<->| DW/DX
!| DWDY           |<->| DW/DY
!| DWDZ           |<->| DW/DZ
!| EP             |-->| OMEGA
!| GRAV           |-->| GRAVITY ACCELERATION
!| IELM3          |---| TYPE OF ELEMENT
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH3D         |<->| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| PRANDTL        |-->| PRANDTL NUMBER
!| ROTAT          |<->| KIND OF L1 NORM OF VORTICITY
!| S              |-->| BIEF_OBJ
!| S1E            |<->| BETA*OMEGA
!| S1K            |<->| BETAS*OMEGA
!| TR             |<->| TABLEAU DE TRAVAIL PAR POINTS
!| U              |-->| VELOCITY COMPONENT
!| V              |-->| VELOCITY COMPONENT
!| W              |-->| VELOCITY COMPONENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_SOUKOM => SOUKOM
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN3,IELM3
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CV1,CV2,S1K,S1E
      TYPE(BIEF_OBJ), INTENT(IN)    :: AK,EP
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: ROTAT
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: DUDX,DUDY,DUDZ
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: DVDX,DVDY,DVDZ
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: DWDX,DWDY,DWDZ
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: DKDX,DKDY,DKDZ
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: DODX,DODY,DODZ
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: DTADZ,TR
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,U,V,W,DELTAR,S
!
      DOUBLE PRECISION, INTENT(IN)    :: GRAV,ALPHA,BETA,BETAS,PRANDTL
!
      LOGICAL, INTENT(IN)             :: MSK
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3D
!
      INTRINSIC ABS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION VISC,PROD,C3,G,GRDKOM,SIGMAD
      INTEGER N
!
!-----------------------------------------------------------------------
!
      CALL VECTOR(DKDX,'=','GRADF          X',IELM3,1.D0,AK,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DKDY,'=','GRADF          Y',IELM3,1.D0,AK,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DKDZ,'=','GRADF          Z',IELM3,1.D0,AK,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DODX,'=','GRADF          X',IELM3,1.D0,EP,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DODY,'=','GRADF          Y',IELM3,1.D0,EP,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DODZ,'=','GRADF          Z',IELM3,1.D0,EP,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DUDX,'=','GRADF          X',IELM3,1.D0,U,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DUDY,'=','GRADF          Y',IELM3,1.D0,U,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DUDZ,'=','GRADF          Z',IELM3,1.D0,U,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DVDX,'=','GRADF          X',IELM3,1.D0,V,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DVDY,'=','GRADF          Y',IELM3,1.D0,V,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DVDZ,'=','GRADF          Z',IELM3,1.D0,V,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DWDX,'=','GRADF          X',IELM3,1.D0,W,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DWDY,'=','GRADF          Y',IELM3,1.D0,W,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DWDZ,'=','GRADF          Z',IELM3,1.D0,W,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
!
      CALL VECTOR(DTADZ,'=','GRADF          Z',IELM3,1.D0,DELTAR,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
!
      CALL VECTOR(TR,'=','MASBAS          ',IELM3,1.D0,
     &            S,S,S,S,S,S,MESH3D,MSK,MASKEL)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(DKDX ,2,MESH3D)
        CALL PARCOM(DKDY ,2,MESH3D)
        CALL PARCOM(DKDZ ,2,MESH3D)
        CALL PARCOM(DODX ,2,MESH3D)
        CALL PARCOM(DODY ,2,MESH3D)
        CALL PARCOM(DODZ ,2,MESH3D)
        CALL PARCOM(DUDX ,2,MESH3D)
        CALL PARCOM(DUDY ,2,MESH3D)
        CALL PARCOM(DUDZ ,2,MESH3D)
        CALL PARCOM(DVDX ,2,MESH3D)
        CALL PARCOM(DVDY ,2,MESH3D)
        CALL PARCOM(DVDZ ,2,MESH3D)
        CALL PARCOM(DWDX ,2,MESH3D)
        CALL PARCOM(DWDY ,2,MESH3D)
        CALL PARCOM(DWDZ ,2,MESH3D)
        CALL PARCOM(DTADZ,2,MESH3D)
        CALL PARCOM(TR,2,MESH3D)
      ENDIF
!
      CALL OS('X=1/Y   ',X=TR,Y=TR,IOPT=2,INFINI=1.D0,ZERO=1.D-10)
      CALL OS('X=XY    ',X=DKDX,Y=TR)
      CALL OS('X=XY    ',X=DKDY,Y=TR)
      CALL OS('X=XY    ',X=DKDZ,Y=TR)
      CALL OS('X=XY    ',X=DODX,Y=TR)
      CALL OS('X=XY    ',X=DODY,Y=TR)
      CALL OS('X=XY    ',X=DODZ,Y=TR)
      CALL OS('X=XY    ',X=DUDX,Y=TR)
      CALL OS('X=XY    ',X=DUDY,Y=TR)
      CALL OS('X=XY    ',X=DUDZ,Y=TR)
      CALL OS('X=XY    ',X=DVDX,Y=TR)
      CALL OS('X=XY    ',X=DVDY,Y=TR)
      CALL OS('X=XY    ',X=DVDZ,Y=TR)
      CALL OS('X=XY    ',X=DWDX,Y=TR)
      CALL OS('X=XY    ',X=DWDY,Y=TR)
      CALL OS('X=XY    ',X=DWDZ,Y=TR)
      CALL OS('X=XY    ',X=DTADZ,Y=TR)
!
!-----------------------------------------------------------------------
!
      DO N=1,NPOIN3
!
        ROTAT%R(N) = ABS(DWDY%R(N)-DVDZ%R(N))
     &             + ABS(DUDZ%R(N)-DWDX%R(N))
     &             + ABS(DVDX%R(N)-DUDY%R(N))
        GRDKOM   = DKDX%R(N)*DODX%R(N)
     &            +DKDY%R(N)*DODY%R(N)+DKDZ%R(N)*DODZ%R(N)
!
        IF(GRDKOM.GE.0.D0) THEN
          SIGMAD = 0.5D0
        ELSE
          SIGMAD   = 0.D0
        ENDIF
!
        VISC = 0.3D0*AK%R(N)/MAX(0.3D0*EP%R(N),ROTAT%R(N))
!
        PROD = (  2.D0 * (DUDX%R(N)**2+DVDY%R(N)**2+DWDZ%R(N)**2)
     &                 + ( DUDY%R(N)+DVDX%R(N) )**2
     &                 + ( DUDZ%R(N)+DWDX%R(N) )**2
     &                 + ( DVDZ%R(N)+DWDY%R(N) )**2  ) * VISC
!
!       COMPUTES G AND C3
!
        G = VISC*GRAV*DTADZ%R(N)/PRANDTL
!
        IF(G.GT.0.D0) THEN
          C3=0.D0
        ELSE
          C3=1.D0
        ENDIF
!
!       EXPLICIT K
!
        CV1%R(N) = PROD + G
!
!       EXPLICIT OMEGA
!
        CV2%R(N)=ALPHA*(PROD+(1-C3)*G)*EP%R(N)/AK%R(N)
     &           +SIGMAD/EP%R(N)*GRDKOM
!
      ENDDO
!
!***********************************************************************
!                                                                      *
!     IMPLICIT SOURCE TERMS FOR K AND OMEGA                            *
!                                                                      *
!     IMPLICIT TERM ON K :              + BETAS* EP(N) * K (N+1)       *
!     IMPLICIT TERM ON OMEGA:           + BETA * EP(N) * EP(N+1)       *
!                                                                      *
!***********************************************************************
!
      CALL OS( 'X=CY    ' , X=S1K , Y=EP  , C=BETAS )
      CALL OS( 'X=CY    ' , X=S1E , Y=EP  , C=BETA  )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
