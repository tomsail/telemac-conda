!                   *****************
                    SUBROUTINE SOUSA
!                   *****************
!
     &(S0NU,S1NU,U,V,W,ROTAN,STRAIN,TRNU,
     & NU,NPOIN3,MSK,MASKEL,MESH3D,IELM3,S,WDIST,
     & NPOIN2,ITURB)
!
!***********************************************************************
! TELEMAC3D   V8P0                                   21/08/2018
!***********************************************************************
!
!brief    PREPARES THE SOURCE TERMS IN THE DIFFUSION EQUATION OF
!+                K AND EPSILON.
!
!
!history  A. Bourgoin (EDF R&D, LNHE)
!+        30/09/2018
!+        V8P0
!+ Creation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AK             |<->| TURBULENT ENERGY K
!| C1             |-->| CONSTANT FOR K-EPSILON MODEL
!| C2             |-->| CONSTANT FOR K-EPSILON MODEL
!| CMU            |-->| CONSTANT FOR K-EPSILON MODEL
!| CV1            |<->| SOURCE TERM FOR K AND EPSILON
!| CV2            |<->| SOURCE TERM FOR K AND EPSILON
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
!| EBORS          |<->| EPSILON AT SURFACE
!| EMIN           |-->| MINIMUM VALUE FOR EPSILON WHEN CLIPPING
!| EP             |<->| TURBULENT DISSIPATION EPSILON
!| H              |-->| WATER DEPTH
!| IELM3          |---| TYPE OF ELEMENT
!| KMIN           |-->| MINIMUM VALUE FOR K WHEN CLIPPING
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH3D         |<->| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| PRANDTL        |-->| PRANDTL NUMBER
!| S              |-->| BIEF_OBJ
!| S1E            |<->| C2*EPSILON/K
!| S1K            |<->| EPSILON/K
!| TR             |<->| TABLEAU DE TRAVAIL PAR POINTS
!| U              |-->| VELOCITY COMPONENT
!| V              |-->| VELOCITY COMPONENT
!| W              |-->| VELOCITY COMPONENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC3D, ONLY : ZPROP,UNSV3D
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN3,IELM3,NPOIN2,ITURB
!
      DOUBLE PRECISION, INTENT(INOUT) :: S0NU(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: S1NU(NPOIN3)
!
      TYPE (BIEF_OBJ), INTENT(INOUT) :: NU,ROTAN,STRAIN,TRNU
      TYPE (BIEF_OBJ), INTENT(IN)    :: MASKEL,U,V,W,S
      TYPE (BIEF_OBJ), INTENT(IN)    :: WDIST
!
      LOGICAL, INTENT(IN)             :: MSK
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION S2(NPOIN3),UNSURSIX
      DOUBLE PRECISION CB1,CB2,SIGNU,KAP,CW1,CW2,CW3,CV1
      DOUBLE PRECISION FV1,FV2,DIST,S2M,G,R,FW
      DOUBLE PRECISION CHI,CHI3,CT3,CT4,FT2,PROPNU2
      DOUBLE PRECISION FWSTAR,CDES,NUM,DEN
      INTEGER N,IPOIN2
!
      DOUBLE PRECISION, PARAMETER  :: EPSS=1.D-12
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
!
!     INTEGER I
!
      INTRINSIC EXP
!     DUDX AND DUDY WILL NOT BE USED AFTER BUILDING S2
!     THEIR MEMORY IS REUSED
!      PROD=>DUDY
      SAVEZ     =>MESH3D%Z%R
      MESH3D%Z%R=>ZPROP%R
!
!-----------------------------------------------------------------------
!
      SIGNU=2.D0/3.D0
      KAP=0.41D0
      CB1=0.1355D0
      CB2=0.622D0
      CV1=7.1D0
      CW1=CB1/KAP**2+(1+CB2)/SIGNU
      CW2=0.3D0
      CW3=2.0D0
      CT3=1.1D0
      CT4=2.D0
      PROPNU2=1.D-6
      FWSTAR=0.424D0
      CDES=0.65D0
      UNSURSIX=1.D0/6.D0
!
!-----------------------------------------------------------------------
!
      CALL VECTOR(ROTAN,'=','PRSAF           ',IELM3,1.D0,S,
     &            S,S,U,V,W,MESH3D,MSK,MASKEL)
      CALL VECTOR(STRAIN,'=','STRAIN          ',IELM3,1.D0,S,
     &            S,S,U,V,W,MESH3D,MSK,MASKEL)
      CALL VECTOR(TRNU,'=','TRSAF           ',IELM3,1.D0,NU,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(ROTAN ,2,MESH3D)
        CALL PARCOM(STRAIN ,2,MESH3D)
        CALL PARCOM(TRNU  ,2,MESH3D)
      ENDIF
      CALL OS('X=XY    ',X=ROTAN,Y=UNSV3D)
      CALL OS('X=XY    ',X=STRAIN,Y=UNSV3D)
      CALL OS('X=XY    ',X=TRNU,Y=UNSV3D)

!-----------------------------------------------------------------------
!                     TURBULENCE PRODUCTION --- EXPLICIT
!-----------------------------------------------------------------------

      DO N=1,NPOIN3
!
        CHI=NU%R(N)/PROPNU2
        CHI3=CHI**3.D0
        FV1=CHI3/(CHI3+CV1**3.D0)
        FV2=1.D0-CHI/(1.D0+CHI*FV1)
        FT2=CT3*EXP(-CT4*CHI**2.D0)
        FT2=0.D0
!       COMPUTE THE REAL DISTANCE TO WALLS (AND BED)
        IPOIN2=MOD(N-1,NPOIN2)+1
        DIST=MAX(MIN(WDIST%R(IPOIN2),
     &           MESH3D%Z%R(N)-MESH3D%Z%R(IPOIN2)),
     &           1.D-4)
!       DIST WILL BE USED DIRECTLY BY ITURB=5
!       FOR ITURB=9, IT IS A BIT MORE COMPLICATED
        IF(ITURB.EQ.9) THEN
          NUM=1.D0-CB1/(CW1*KAP**2*FWSTAR)*(FT2+(1.D0-FT2)*FV2)
          DEN=FV1*MAX(1.D-10,1.D0-FT2)
          DIST=MIN(CDES*(1.D0/UNSV3D%R(N))**(1.D0/3.D0),DIST)
        ENDIF
!
        S2M=FV2*NU%R(N)/(KAP*DIST)**2
        S2(N)=MAX(SQRT(ROTAN%R(N))+S2M, 0.3D0*SQRT(ROTAN%R(N)))
        S0NU(N) = CB1*S2(N)*NU%R(N)*(1.D0-FT2)
!
!-----------------------------------------------------------------------
!                      TURBULENCE DESTRUCTION     -- IMPLICIT
!-----------------------------------------------------------------------
!
        R=MIN(NU%R(N)/MAX((S2(N)*(KAP*DIST)**2.D0),EPSS),10.D0)
        G=R+CW2*(R**6.D0-R)
        FW=G*((1+CW3**6)/(G**6+CW3**6))**UNSURSIX
        S1NU(N)=MAX((CW1*FW-CB1*FT2/KAP**2)*NU%R(N)/DIST**2,0.D0)
!
!-----------------------------------------------------------------------
!                     TURBULENCE DIFFUSION2 --- EXPLICIT
!-----------------------------------------------------------------------
!
        S0NU(N)=S0NU(N)+CB2*TRNU%R(N)/SIGNU
      ENDDO
      MESH3D%Z%R=>SAVEZ

!-----------------------------------------------------------------------
!
      RETURN
      END
