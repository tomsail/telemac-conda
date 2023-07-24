!                     ******************************
                      MODULE DECLARATIONS_POSTEL3D
!                     ******************************
!
!***********************************************************************
!  POSTEL3D VERSION 6.0
!***********************************************************************
!=======================================================================
! Telemac-3D best version number
! fortran95 version         march 1999        Jacek A. Jankowski pinxit
!=======================================================================
!
!  declaration of the global data structure in Telemac-3D
!
      USE BIEF_DEF
!
!       note: this module is organised in 10 parts
!
!       (1) vectors (will be declared as bief_obj structures)
!       (2) matrices (will be declared as bief_obj structures)
!       (3) blocks (will be declared as bief_obj structures)
!       (4) integers
!       (5) logical values
!       (6) reals
!       (7) strings
!       (8) slvcfg structures
!       (9) mesh structure
!      (10) aliases
!
!-----------------------------------------------------------------------
! (1) vectors (real and integer)
!-----------------------------------------------------------------------
!
! 3D velocity components
!
      TYPE(BIEF_OBJ), TARGET :: U, V, W
!
!-----------------------------------------------------------------------
! (2) matrices
!-----------------------------------------------------------------------
! none
!-----------------------------------------------------------------------
! (3) blocks
!-----------------------------------------------------------------------
!
!
      TYPE(BIEF_OBJ), TARGET :: TAB
!
!
! 2D output compatibility - output variables organised in blocks
!th pour bientot, avec le nouveau format
!th        type(bief_obj), target :: varsor, varcl
!
!-----------------------------------------------------------------------
! (4) integers
!-----------------------------------------------------------------------
! key words and parameters
!
!       maximum de variables de sortie
      INTEGER, PARAMETER :: MAXVAR = 100
!
! previous common mitint: integer steering parameters
!
      INTEGER NPLAN  , NTRAC  , NTRPA , NVAR(2), NVA3
      INTEGER NR3D , NENRE
!
      INTEGER IELM3, IELM2H, IELM2V
      INTEGER IELM0, IELMH, IELMU, IELM1, IELMX
      INTEGER SORG3D
      INTEGER IM,JM,NPLINT
      INTEGER NUPRSO,PESOGR,NC2DH,NC2DV
      INTEGER NPLREF(9),NSEG(9)
!
!      nombre max de coupes
      INTEGER, PARAMETER :: MAXCOU = 9
!      nombre max de points pour les coupes verticales
      INTEGER, PARAMETER :: MAXPTS = 50

!
!-----------------------------------------------------------------------
! (5) logical values
!-----------------------------------------------------------------------
!
      LOGICAL SIGMAG
      LOGICAL SPHERI
      LOGICAL VARSUB
!
!-----------------------------------------------------------------------
! (6) reals
!-----------------------------------------------------------------------
!
! previous common mitrea, real steering parameters plus new ones
!
      DOUBLE PRECISION HMIN,  COTINT
!
!th  a voir si on met le parametre
!th  en dur pour l'instant
!      double precision href(maxcou),distor(maxcou)
!      double precision x2dv(maxpts,maxcou),y2dv(maxpts,maxcou)
      DOUBLE PRECISION HREF(9),DISTOR(9)
!th      double precision zstar(5)
      DOUBLE PRECISION X2DV(50,9),Y2DV(50,9)
!
!-----------------------------------------------------------------------
! (7) strings
!-----------------------------------------------------------------------
!
!
!
      CHARACTER(LEN=72) TITCAS, SORT3D, SORT2D, VARIMP
!
      CHARACTER(LEN=20) EQUA
      CHARACTER(LEN=32) VARCLA(10), TEXTE(MAXVAR), TEXTPR(MAXVAR)
      CHARACTER(LEN=32) TEXTLU(100)
!
!
!-----------------------------------------------------------------------
! (8) slvcfg structures
!-----------------------------------------------------------------------
! none
!-----------------------------------------------------------------------
! (9) mesh structure(s)
!-----------------------------------------------------------------------
! two separate meshes, 2D as usual and 3D with sigma-mesh specific
! features, see almesh.f
!
      TYPE(BIEF_MESH) :: MESH2D, MESH3D
!
!-----------------------------------------------------------------------
! (10) aliases
!-----------------------------------------------------------------------
! declaration of pointers for aliases
! targets are allocated and pointed to in POINT_POSTEL3D.
!
! useful pointers for often used components in 2d and 3D mesh structures
!
! x,y,z node coordinates: base mesh and 3D sigma mesh
!
      TYPE(BIEF_OBJ), POINTER :: X2, Y2, Z2, X3, Y3, Z3
!
!th surement plein de choses a virer
!th
      TYPE(BIEF_OBJ), POINTER :: XNEBOR2, YNEBOR2
      TYPE(BIEF_OBJ), POINTER :: XNEBOR3, YNEBOR3, ZNEBOR3
!
! 2D and 3D lateral boundary normal vectors defined
! per boundary segment (2D) or boundary element (3D)
!
      TYPE(BIEF_OBJ), POINTER :: XSGBOR2, YSGBOR2
      TYPE(BIEF_OBJ), POINTER :: XSGBOR3, YSGBOR3, ZSGBOR3
!
! connectivity tables 2D and 3D
! (element number and local node number) --> global node number
!
      TYPE(BIEF_OBJ), POINTER :: IKLE2, IKLE3
!
! tables connecting (node boundary number) --> global node number
!
      TYPE(BIEF_OBJ), POINTER :: NBOR2, NBOR3
!
! real field pointers for node coordinates
!
      DOUBLE PRECISION, DIMENSION(:), POINTER :: X,Y,Z
      ! COORDINATES OFFSET
      INTEGER X_ORIG,Y_ORIG
!
! a number of extremely useful integers describing the mesh structure
! see almesh.f and point_telemac3d.f
!
      INTEGER, POINTER :: NELEM2, NELEM3
!
      INTEGER, POINTER :: NELMAX2
      INTEGER, POINTER :: NELMAX3 ! PREVIOUSLY NELMA3
!
      INTEGER, POINTER :: NPTFR2  ! PREVIOUSLY SIMPLY NPTFR
      INTEGER, POINTER :: NPTFR3
      INTEGER, POINTER :: NELEB, NELEBX
!
      INTEGER, POINTER :: NPTFRX2, NPTFRX3
      INTEGER, POINTER :: DIM2, DIM3
      INTEGER, POINTER :: TYPELM2, TYPELM3
      INTEGER, POINTER :: NPOIN2, NPOIN3
      INTEGER, POINTER :: NPMAX2, NPMAX3
      INTEGER, POINTER :: MXPTVS2, MXPTVS3
      INTEGER, POINTER :: MXELVS2, MXELVS3
!
!     NEW FILE FORMATS
!
      TYPE(BIEF_FILE) :: POS_FILES(100)
      INTEGER POSPRE,POSHOR,POSVER,POSGEO
!
!-----------------------------------------------------------------------
!
      SAVE
!
      END MODULE DECLARATIONS_POSTEL3D
