!                   ***************************
                    MODULE DECLARATIONS_ARTEMIS
!                   ***************************
!
!
!***********************************************************************
! ARTEMIS   V8P2
!***********************************************************************
!
!brief    DECLARATION OF PRINICIPAL ARTEMIS VARIABLES
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
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
!history  C.PEYRARD(EDF)
!+        2011
!+        V6P1
!+        VARIABLES FOR RAPIDLY VARYING TOPOGRAPHY ADDED
!
!history  C.PEYRARD(EDF)
!+        2012
!+        V6P2
!+        VARIABLES FOR INCIDENT POTENTIAL ADDED
!
!history  C.PEYRARD(EDF)
!+        2013
!+        V6P3
!+        VARIABLES FOR CURRENT ADDED
!
!history  C.PEYRARD(EDF)
!+        2014
!+        V7P0
!+        VARIABLES FOR AUTOMATIC TETAP CALCULATION ADDED
!+        VARIABLES FOR AUTOMATIC PHASE CALCULATION ADDED
!
!history  N.DURAND (HRW)
!+        November 2016
!+        V7P2
!+   New variables introduced for use in LECLIM call : ALFAPS and HBS
!+   These and TETAPS store parameters read from the cli file (RP also
!+   read in but does not require a temporary storage variable for it)
!
!history  N.DURAND (HRW)
!+        November 2016
!+        V7P2
!+   Addition of new keywords in the steering file re: animation of the
!+   free surface. ANIMFS and ARTAMP declared
!
!history  N.DURAND (HRW)
!+        August 2017
!+        V7P3
!+   1. Consolidation with CHAINTWC, meaning that it is now a choice of integers
!+   and no longer a logical
!+   2. NDTWC (now NDIR) and NFTWC (now NF) are no longer read in the
!+   steering file but directly extracted from the spectrum file
!+   3. Declaration of new structure type: SPECTRUM
!+   4. PI (and related variables) now defined here and in ARTEMIS_CONSTANTS
!+   5. Addition of new keywords in the steering file for nesting option 2:
!+   X_SFREF, Y_SFREF, ART_FILES(WACRES)%NAME, and related variables
!+   6. Introduction of BDALE, block holding spatially varying DALEs for
!+   incident boundary nodes and for nesting option 2
!+   7. Addition of IPTFR_REF, the node number for the reference spectrum
!
!history  N.DURAND (HRW)
!+        December 2018
!+        V8P0
!+   Addition of new keywords in the steering file for nesting option 2:
!+   ART_FILES(WACLQD)%NAME
!
!history  N.DURAND (HRW)
!+        January 2019
!+        V8P0
!+   TYPE SPECTRUM MIGRATED TO BIEF_DEF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
!
!       NOTE: THIS MODULE IS ORGANISED IN 10 PARTS
!
!       1) VECTORS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
!       2) MATRICES (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
!       3) BLOCKS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
!       4) INTEGERS
!       5) LOGICAL VALUES
!       6) REALS
!       7) STRINGS
!       8) SLVCFG STRUCTURES
!       9) MESH STRUCTURE
!      10) ALIASES
!
!-----------------------------------------------------------------------
!
!       1) VECTORS
!
!-----------------------------------------------------------------------
!
!brief REAL PART OF WAVE POTENTIAL
! partie reelle du potentiel
      TYPE(BIEF_OBJ), TARGET :: PHIR
!brief IMAGINARY PART OF WAVE POTENTIAL
! partie imaginaire du potentiel
      TYPE(BIEF_OBJ), TARGET :: PHII
!brief WATER DEPTH AT REST
! hauteur d'eau au repos
      TYPE(BIEF_OBJ), TARGET :: H
!brief (MEAN) WAVE NUMBER
! nombre d'onde
      TYPE(BIEF_OBJ), TARGET :: K
!brief (MEAN) PHASE CELERITY
! vitesse de phase
      TYPE(BIEF_OBJ), TARGET :: C
!brief (MEAN) GROUP CELERITY
! vitesse de groupe
      TYPE(BIEF_OBJ), TARGET :: CG
!brief SIGNIFICANT WAVE HEIGHT (REGULAR MODE)
! hauteur de la houle
      TYPE(BIEF_OBJ), TARGET :: HHO
!brief WAVE PHASE (REGULAR MODE)
! phase de la houle
      TYPE(BIEF_OBJ), TARGET :: PHAS
!brief SURFACE WAVE VELOCITY COMPONENT
! vitesse en surface (a t=0)
      TYPE(BIEF_OBJ), TARGET :: U0
!brief SURFACE WAVE VELOCITY COMPONENT
! vitesse en surface (a t=0)
      TYPE(BIEF_OBJ), TARGET :: V0
!brief MEAN COSINE OF WAVE DIRECTION
! moyennes des cosinus de la direction de houle
      TYPE(BIEF_OBJ), TARGET :: MCOS
!brief MEAN SINE OF WAVE DIRECTION
! moyennes des sinus de la direction de houle
      TYPE(BIEF_OBJ), TARGET :: MSIN
!brief WAVE INCIDENCE (OR DIRECTION)
! incidence de la houle
      TYPE(BIEF_OBJ), TARGET :: INCI
!brief FREE SURFACE ELEVATION
! cote de la surface libre
      TYPE(BIEF_OBJ), TARGET :: S
!brief BOTTOM ELEVATION
! cote du fond
      TYPE(BIEF_OBJ), TARGET :: ZF
!brief FRICTION FACTOR
! coefficient de frottement (variable en espace)
      TYPE(BIEF_OBJ), TARGET :: FW
!brief WAVE HEIGHT (RANDOM WAVE)
! hauteur de la houle aleatoire
      TYPE(BIEF_OBJ), TARGET :: HALE
!brief WAVE PERIODS ARRAY (RANDOM MODE)
! tableau des periodes de discretisation du spectre pour un calcul en houle aleatoire multidirectionnelle
      TYPE(BIEF_OBJ), TARGET :: PALE
!brief REFLEXION COEFFICIENT
! coefficient de reflexion des parois
      TYPE(BIEF_OBJ), TARGET :: RP
!brief ANGLE OF WAVE ATTACK (FROM NORMAL AXIS)
! angle d'attaque de la houle sur les limites - pas seulement les parois (compte par rapport a a la normale , inclut dans [0;90])
      TYPE(BIEF_OBJ), TARGET :: TETAP
!brief DEPHASING CAUSED BY THE WALLS
! dephasage induit par la paroi entre l'onde reflechie et l'onde incidente (si alfap est positif, l'onde reflechie est en retard)
      TYPE(BIEF_OBJ), TARGET :: ALFAP
!brief INCIDENT WAVE HEIGHT AT THE BOUNDARY
! hauteur de la houle aux frontieres ouvertes
      TYPE(BIEF_OBJ), TARGET :: HB
!brief INCIDENT WAVE DIRECTION AT THE BOUNDARY (FROM X AXIS)
! angle d'attaque de la houle aux frontieres ouvertes (compte par rapport a l'axe des x dans le sens direct)
      TYPE(BIEF_OBJ), TARGET :: TETAB
!brief REAL PART OF INCIDENT WAVE AT THE BOUNDARY
! partie reelle du potentiel impose au bord (dirichlet)
      TYPE(BIEF_OBJ), TARGET :: PHIRB
!brief IMAGINARY PART OF INCIDENT WAVE AT THE BOUNDARY
! partie imaginaire du potentiel impose au bord (dirichlet)
      TYPE(BIEF_OBJ), TARGET :: PHIIB

!brief REAL PART OF INCIDENT POTENTIAL AT THE BOUNDARY
! partie reelle du potentiel incident au bord
      TYPE(BIEF_OBJ), TARGET :: PRB
!brief IMMAGINARY PART OF INCIDENT POTENTIAL AT THE BOUNDARY
! partie imaginaire du potentiel incident au bord
      TYPE(BIEF_OBJ), TARGET :: PIB
!brief REAL PART OF GRADIENT X COMPONENT OF INCIDENT POTENTIAL AT THE BOUNDARY
! partie reele du gradient en X du potentiel incident au bord
      TYPE(BIEF_OBJ), TARGET :: DDXPRB
!brief REAL PART OF GRADIENT Y COMPONENT OF INCIDENT POTENTIAL AT THE BOUNDARY
! partie reele du gradient en Y du potentiel incident au bord
      TYPE(BIEF_OBJ), TARGET :: DDYPRB
!brief IMMAGINARY PART OF GRADIENT X COMPONENT OF INCIDENT POTENTIAL AT THE BOUNDARY
! partie imaginaire du gradient en X du potentiel incident au bord
      TYPE(BIEF_OBJ), TARGET :: DDXPIB
!brief IMMAGINARY PART OF GRADIENT Y COMPONENT OF INCIDENT POTENTIAL AT THE BOUNDARY
! partie imaginaire du gradient en Y du potentiel incident au bord
      TYPE(BIEF_OBJ), TARGET :: DDYPIB
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: APHI1B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: BPHI1B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: CPHI1B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: DPHI1B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: CGRX1B
      TYPE(BIEF_OBJ), TARGET :: CGRY1B
      TYPE(BIEF_OBJ), TARGET :: DGRX1B
      TYPE(BIEF_OBJ), TARGET :: DGRY1B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: APHI2B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: BPHI2B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: CPHI2B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: DPHI2B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: APHI3B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: BPHI3B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: CPHI3B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: DPHI3B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: APHI4B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: BPHI4B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: CPHI4B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: DPHI4B
!brief STORAGE OF ANGLE OF WAVE ATTACK (FROM NORMAL AXIS) FOR LECLIM
! angle d'attaque de la houle sur les limites - pas seulement les parois (compte par rapport a a la normale , inclut dans [0;90])
      TYPE(BIEF_OBJ), TARGET :: TETAPS
!brief STORAGE OF DEPHASING CAUSED BY THE WALLS FOR LECLIM
! dephasage induit par la paroi entre l'onde reflechie et l'onde incidente (si alfap est positif, l'onde reflechie est en retard)
      TYPE(BIEF_OBJ), TARGET :: ALFAPS
!brief STORAGE OF INCIDENT WAVE HEIGHT AT THE BOUNDARY FOR LECLIM
! hauteur de la houle aux frontieres ouvertes
      TYPE(BIEF_OBJ), TARGET :: HBS
!brief ANGLE OF WAVE ATTACK - IN LOOP STORAGE (FROM NORMAL AXIS)
! angle d'attaque de la houle sur les limites - pas seulement les parois (compte par rapport a a la normale , inclut dans [0;90])
      TYPE(BIEF_OBJ), TARGET :: TETAPM
!
!brief WORKING ARRAY
! tableau de travail
      TYPE(BIEF_OBJ), TARGET :: W1
!brief INTEGER WORKING ARRAY
!
      TYPE(BIEF_OBJ), TARGET :: IT1
!brief INTEGER WORKING ARRAY
!
      TYPE(BIEF_OBJ), TARGET :: IT2
!brief INTEGER WORKING ARRAY
!
      TYPE(BIEF_OBJ), TARGET :: IT3
!brief VOID STRUCTURE
!
      TYPE(BIEF_OBJ), TARGET :: SBID
!brief RIGHT MEMBER OF SYSTEM TO BE SOLVED
!
      TYPE(BIEF_OBJ), TARGET :: CV1
!brief RIGHT MEMBER OF SYSTEM TO BE SOLVED
!
      TYPE(BIEF_OBJ), TARGET :: CV2
!brief WAVE DISSIPATION QUANTITY
! tableau pour la dissipation
      TYPE(BIEF_OBJ), TARGET :: MU
!brief WAVE DISSIPATION QUANTITY
! tableau pour la dissipation
      TYPE(BIEF_OBJ), TARGET :: MU2
!brief WAVE DISSIPATION QUANTITY
! tableau pour la dissipation
      TYPE(BIEF_OBJ), TARGET :: QB
!brief WAVE DISSIPATION QUANTITY
! tableau pour la dissipation
      TYPE(BIEF_OBJ), TARGET :: HMU
!brief WAVE DISSIPATION QUANTITY
! tableau pour la dissipation
      TYPE(BIEF_OBJ), TARGET :: HMUANC
!brief RADIATION STRESSES QUANTITY
! tableau pour les contraintes de radiation
      TYPE(BIEF_OBJ), TARGET :: SXX
!brief RADIATION STRESSES QUANTITY
! tableau pour les contraintes de radiation
      TYPE(BIEF_OBJ), TARGET :: SXY
!brief RADIATION STRESSES QUANTITY
! tableau pour les contraintes de radiation
      TYPE(BIEF_OBJ), TARGET :: SYY
!brief RADIATION STRESSES QUANTITY
! tableau pour les contraintes de radiation
      TYPE(BIEF_OBJ), TARGET :: FX
!brief RADIATION STRESSES QUANTITY
! tableau pour les contraintes de radiation
      TYPE(BIEF_OBJ), TARGET :: FY
!brief MEAN WAVE PERIOD
! periode moyenne issue du moment d'ordre 1
      TYPE(BIEF_OBJ), TARGET :: T01
!brief MEAN WAVE PERIOD
! periode moyenne issue du moment d'ordre 2
      TYPE(BIEF_OBJ), TARGET :: T02
!brief MEAN WAVE PERIOD
! periode moyenne issue du moment d'ordre 1
      TYPE(BIEF_OBJ), TARGET :: TM
!brief WAVE DIRECTIONS AT THE BOUNDARY (RANDOM MODE)
!
      TYPE(BIEF_OBJ), TARGET :: DALE
!brief WAVE DIRECTIONS AT THE BOUNDARY (RANDOM MODE)
! these vary spatially along the boundary
      TYPE(BIEF_OBJ), TARGET :: BDALE
!brief PERIODS ASSOCIATED TO WAVE DIRECTIONS AT THE BOUNDARY (RANDOM MODE)
!
      TYPE(BIEF_OBJ), TARGET :: PDALE
!brief BOUNDARY CONDITION TYPE
! type de conditions aux limites sur u
      TYPE(BIEF_OBJ), TARGET :: LIUBOR
!brief BOUNDARY CONDITION TYPE
! type de conditions aux limites sur v
      TYPE(BIEF_OBJ), TARGET :: LIVBOR
!brief BOUNDARY CONDITION TYPE
! type de conditions aux limites sur h
      TYPE(BIEF_OBJ), TARGET :: LIHBOR
!brief
!

! 'COLOUR' OF BOUNDARY NODES (TAKEN IN BOUNDARY CONDITIONS FILE)
      TYPE(BIEF_OBJ), TARGET :: BOUNDARY_COLOUR
!brief
! 'BIDON INTEGER'
      TYPE(BIEF_OBJ), TARGET :: ITB1
!brief
! 'BIDON REEL
      TYPE(BIEF_OBJ), TARGET :: TB1
!brief


      TYPE(BIEF_OBJ), TARGET :: NUMLIQ
!brief
!
      TYPE(BIEF_OBJ), TARGET :: LIDIR
!brief
!
      TYPE(BIEF_OBJ), TARGET :: MASKEL
!brief MASKS FOR BOUNDARY NODES, CORRESPONDS TO INCIDENT WAVES (KINC)
! masque pour les points de bord
      TYPE(BIEF_OBJ), TARGET :: MASK1
!brief MASKS FOR BOUNDARY NODES, CORRESPONDS TO FREE EXIT (KSORT)
! masque pour les points de bord
      TYPE(BIEF_OBJ), TARGET :: MASK2
!brief MASKS FOR BOUNDARY NODES, CORRESPONDS TO SOLID BOUNDARY (KLOG)
! masque pour les points de bord
      TYPE(BIEF_OBJ), TARGET :: MASK3
!brief MASKS FOR BOUNDARY NODES, CORRESPONDS TO IMPOSED WAVES (KENT)
! masque pour les points de bord
      TYPE(BIEF_OBJ), TARGET :: MASK4
!brief MASKS FOR BOUNDARY NODES, CORRESPONDS TO INCIDENT POTENTIAL (KPOT)
! masque pour les points de bord
      TYPE(BIEF_OBJ), TARGET :: MASK5


!brief FLOW
! CURRENT VELOCITY IN X IDRECTION
      TYPE(BIEF_OBJ), TARGET :: UC
!brief FLOW
! CURRENT VELOCITY IN Y IDRECTION
      TYPE(BIEF_OBJ), TARGET :: VC
!brief RELATIVE ANGULAR FREQUENCY
! RELATIVE PULSATION
      TYPE(BIEF_OBJ), TARGET :: WR
!brief table for wave-current interaction
! WAVE VECTOR COMPONENT X
      TYPE(BIEF_OBJ), TARGET :: KANCX
!brief table for wave-current interaction
! WAVE VECTOR COMPONENT Y
      TYPE(BIEF_OBJ), TARGET :: KANCY
!brief table for mean omega on the domain (from T01)
! MEAN OMEGA = 2 PI / T01
      TYPE(BIEF_OBJ), TARGET :: OMEGAM
!brief table for bottom velocity in random waves
! BOTTOM VELOCITY (RANDOM SEAS)
      TYPE(BIEF_OBJ), TARGET :: UEB
!
!-----------------------------------------------------------------------
!
!       2) MATRICES
!
!-----------------------------------------------------------------------
!
!brief MATRICE FOR SYSTEM SOLVING
!
      TYPE(BIEF_OBJ), TARGET :: AM1
!brief MATRICE FOR SYSTEM SOLVING
!
      TYPE(BIEF_OBJ), TARGET :: AM2
!brief MATRICE FOR SYSTEM SOLVING
!
      TYPE(BIEF_OBJ), TARGET :: AM3
!brief MATRICE FOR SYSTEM SOLVING
!
      TYPE(BIEF_OBJ), TARGET :: BM1
!brief MATRICE FOR SYSTEM SOLVING
!
      TYPE(BIEF_OBJ), TARGET :: BM2
!brief MATRICE FOR SYSTEM SOLVING
!
      TYPE(BIEF_OBJ), TARGET :: MBOR
!
!-----------------------------------------------------------------------
!
!       3) BLOCKS
!
!-----------------------------------------------------------------------
!
!
!brief BLOCK OF POTENTIAL VECTORS
!
      TYPE(BIEF_OBJ), TARGET :: PHIB
!brief BLOCK OF WORKING ARRAYS
!
      TYPE(BIEF_OBJ), TARGET :: TB
!brief BLOCK OF WORKING ARRAYS
!
      TYPE(BIEF_OBJ), TARGET :: TBBD
!brief BLOCK OF PRIVATE VECTORS
! tableaux reserves a l'utilisateur
      TYPE(BIEF_OBJ), TARGET :: PRIVE
!brief BLOCK OF MATRICES
!
      TYPE(BIEF_OBJ), TARGET :: MAT
!brief BLOCK OF UNKNOWN VECTORS
!
      TYPE(BIEF_OBJ), TARGET :: UNK
!brief BLOCK OF RIGHT HAND SIDE VECTORS IN SOLVING SYSTEM
!
      TYPE(BIEF_OBJ), TARGET :: RHS
!brief BLOCK OF VARIABLES FOR OUTPUT
!
      TYPE(BIEF_OBJ), TARGET :: VARSOR
!brief Number of variables in varsor
      INTEGER :: NVAR_ART
!brief BLOCK OF VARIABLES FOR ANIMATION OUTPUT
!
      TYPE(BIEF_OBJ), TARGET :: VARNIM
!
!-----------------------------------------------------------------------
!
!       4) INTEGERS
!
!-----------------------------------------------------------------------
!
!brief
! maximum de variables de sortie
      INTEGER, PARAMETER :: MAXVAR = 100
!brief
! maximum de frontieres liquides
      INTEGER, PARAMETER :: MAXFRO = 3000
!brief OPEN BOUNDARY WITH INCIDENT POTENTIAL
!
      INTEGER, PARAMETER :: KPOT  =  7
!brief DEBUGGER
!
      INTEGER, TARGET :: DEBUG
!brief ORIGIN COORDINATE
! coordonnee de l'origine
      INTEGER I_ORIG
!brief ORIGIN COORDINATE
! coordonnee de l'origine
      INTEGER J_ORIG
!brief GRAPHIC PRINTOUT PERIOD
! periode de sortie graphique
      INTEGER LEOPRD
!brief LISTING PRINTOUT PERIOD
! periode de sortie listing
      INTEGER LISPRD
!brief MAXIMUM NUMBER OF ITERATIONS FOR SOLVER
! maximum d'iterations pour le solveur
      INTEGER NITMAX
!brief GEOMETRY FILE STANDARD
! standard du fichier de geometrie
!     INTEGER STDGEO
!brief RESULTS FILE STANDARD
! standard du fichier des resultats
!     INTEGER STDRES
!brief SOLVER OPTION
! option du solveur
      INTEGER ISOLVE(2)
!brief BOTTOM TOPOGRAPHY SMOOTHINGS
! nombre de lissages du fond
      INTEGER LISFON
!brief DISCRETISATION IN SPACE
! discretisation en espace
!     INTEGER DISESP
!brief NUMBER OF DISCRETISED PERIODS
! nombre de periodes de discretisation du spectre de houle
      INTEGER NPALE
!brief NUMBER OF DISCRETISED DIRECTIONS
! nombre de directions de discretisation du spectre de houle
      INTEGER NDALE
!brief MATRIX STORAGE
! stockage des matrices
      INTEGER OPTASS
!brief BREAKING LAW
! formulation du deferlement
      INTEGER IBREAK
!brief MAXIMUM OF SUB-ITERATIONS
! maximum de sous-iterations
      INTEGER NITDIS
!brief VECTOR LENGTH
! longueur du vecteur
      INTEGER LVMAC
!brief LAW OF BOTTOM FRICTION
! loi de frottement sur le fond
!     INTEGER KFROT
!brief BOTTOM FRICTION LAW
! formulation du frottement de fond
      INTEGER FORMFR
!brief HYDRAULIC REGIME TYPE
! type du regime hydraulique
      INTEGER REGIDO
!brief MATRIX-VECTOR PRODUCT
! produit matrice-vecteur
      INTEGER PRODUC
!
!brief NUMBER OF PRIVATE ARRAYS, NUMBER OF PRIVATE ARRAYS WITH GIVEN NAME
! nombre de tableaux prives et tableaux prives avec noms
      INTEGER NPRIV,N_NAMES_PRIV
!brief
!
      INTEGER PTINIG
!brief
!
      INTEGER PTINIL
!brief
! type d'element
      INTEGER IELM
!brief
!
      INTEGER IELM0
!brief
! type d'element de bord
      INTEGER IELMB
!brief
!
      INTEGER IELMB0
!brief ORIGINAL DATE OF TIME
! date de l'origine des temps
      INTEGER MARDAT(3)
!brief ORIGINAL HOUR OF TIME
! heure de l'origine des temps
      INTEGER MARTIM(3)
!brief
!
      INTEGER NFRLIQ
!brief
! prise en compte des effets de pente/courbure
      INTEGER IPENTCO
!brief
      INTEGER LPER
!brief
      INTEGER LDIR
!brief MAX NUMBER OF ITERATION ON TETAP CALCULATION
! nombre max d'iteration pour calcul auto du TETAP
      INTEGER NITTP
!brief NESTING WITHIN TOMAWAC OUTER MODEL
! si different de 0, ARTEMIS est integre dans un modele TOMAWAC global
      INTEGER CHAINTWC
!brief
! nombre de spectres
      INTEGER NSPEC
!brief
! nombre de directions definissant le(s) spectre(s)
      INTEGER NDIR
!brief
! nombre de frequences definissant le(s) spectre(s)
      INTEGER NF
!brief
! numero de reference TOMAWAC pour le spectre en freq
      INTEGER N_SFREF
!brief
! noeud frontiere de reference pour le spectre en freq
      INTEGER IPTFR_REF
!
!-----------------------------------------------------------------------
!
!       5) LOGICAL VALUES
!
!-----------------------------------------------------------------------
!
!brief LISTING PRINTOUT
! si oui, sortie listing
      LOGICAL LISTIN
!brief
!
      LOGICAL INFOGR
!brief PERIOD SCANNING
! si oui, balayage en periodes
      LOGICAL BALAYE
!brief MONODIRECTIONAL RANDOM WAVE
! si oui, houle aleatoire monodirectionnelle
      LOGICAL ALEMON
!brief MULTIDIRECTIONAL RANDOM WAVE
! si oui, houle aleatoire multidirectionnelle
      LOGICAL ALEMUL
!brief
!
      LOGICAL MSK
!brief
!
      LOGICAL SPHERI
!brief BREAKING
! si oui, deferlement
      LOGICAL DEFERL
!brief FRICTION
! si oui, frottement
      LOGICAL FROTTE
!brief FRICTION FACTOR IMPOSED
! si oui, facteur de frottement impose
      LOGICAL ENTFW
!brief HYDRAULIC REGIME IMPOSED
! si oui, regime hydraulique impose
      LOGICAL ENTREG
!brief SKIN ROUGHNESS ONLY
! si oui, rugosite de peau seule
      LOGICAL ENTRUG
!brief WAVE HEIGHTS SMOOTHING
! si oui, lissage des hauteurs de houle
      LOGICAL LISHOU
!brief
!
      LOGICAL SORLEO(MAXVAR)
!brief
!
      LOGICAL SORIMP(MAXVAR)
!brief
! a logical array holding variable output for phase and amplitude file
      LOGICAL SORNIM(MAXVAR)
!brief VALIDATION
! si oui, validation
      LOGICAL VALID
!brief COURANT
! Yes = current taken into account (defined in condih.f)
      LOGICAL COURANT
!!brief AUTOMATIC ANGLES
! si oui,calcul automatique des angles de sortie TETAP
      LOGICAL LANGAUTO
!!brief AUTOMATIC PHASES ON INCIDENT WAVE BOUNDARY
! si oui, calcul automatique des phases sur la frontière de type onde incidente (KINC)
      LOGICAL LPHASEAUTO
!brief ANIMATION
! si oui, generates the phase and amplitude file
      LOGICAL ANIMFS
!
!-----------------------------------------------------------------------
!
!       6) REALS
!
!-----------------------------------------------------------------------
!
!brief GRAVITY ACCELERATION
! acceleration de la pesanteur
      DOUBLE PRECISION GRAV
!brief MINIMUM VALUE FOR H
! valeur minimum de h
!     DOUBLE PRECISION HMIN
!brief WAVE PERIOD
! periode de la houle en cours de calcul
      DOUBLE PRECISION PER
!brief ANGULAR FREQUENCY
! pulsation de la houle
      DOUBLE PRECISION OMEGA
!brief DIRECTION OF WAVE PROPAGATION
! direction principale de propagation de la houle
      DOUBLE PRECISION TETAH
!brief INITIAL WATER LEVEL
! cote initiale
      DOUBLE PRECISION COTINI
!brief INITIAL DEPTH
! hauteur initiale
      DOUBLE PRECISION HAUTIN
!brief BEGINNING PERIOD FOR PERIOD SCANNING
! periode de debut pour le balayage en periode
      DOUBLE PRECISION PERDEB
!brief ENDING PERIOD FOR PERIOD SCANNING
! periode de fin pour le balayage en periode
      DOUBLE PRECISION PERFIN
!brief STEP FOR PERIOD SCANNING
! pas pour le balayage en periode
      DOUBLE PRECISION PERPAS
!brief PEAK PERIOD
! periode de pic
      DOUBLE PRECISION PERPIC
!brief GAMMA
! gamma
      DOUBLE PRECISION GAMMA
!brief MINIMUM ANGLE OF PROPAGATION
! valeur minimum de l'angle de propagation
      DOUBLE PRECISION TETMIN
!brief MAXIMUM ANGLE OF PROPAGATION
! valeur maximum de l'angle de propagation
      DOUBLE PRECISION TETMAX
!brief S EXPONENT
! exposant s dans la formule du spectre
      DOUBLE PRECISION EXPOS
!brief
!
!     DOUBLE PRECISION RELAX
!brief FRICTION COEFFICIENT
! coefficient de frottement
      DOUBLE PRECISION FFON
!brief SUB-ITERATIONS ACCURACY
! precision sur les sous-iterations
      DOUBLE PRECISION EPSDIS
!brief DISSIPATION RELAXATION
! relaxation sur la dissipation
      DOUBLE PRECISION RELDIS
!brief ALPHA
! alpha
      DOUBLE PRECISION ALFABJ
!brief GAMMAS
! gammas
      DOUBLE PRECISION GAMMAS
!brief
!
      DOUBLE PRECISION KDALLY
!brief
!
      DOUBLE PRECISION GDALLY
!brief FLUID KINEMATIC VISCOSITY
! viscosite cinematique du fluide
      DOUBLE PRECISION VISCO
!brief DIAMETER90
! diametre90
      DOUBLE PRECISION DIAM90
!brief DIAMETER50
! diametre50
      DOUBLE PRECISION DIAM50
!brief SEDIMENT SPECIFIC WEIGHT
! masse volumique du sediment
      DOUBLE PRECISION MVSED
!brief FLUID SPECIFIC MASS
! masse volumique du fluide
      DOUBLE PRECISION MVEAU
!brief FRICTION FACTOR
! coefficient de frottement constant impose
      DOUBLE PRECISION FWCOEF
!brief RIPPLES COEFFICIENT
! coefficient de rides
      DOUBLE PRECISION RICOEF
!brief MINIMUM SPECTRAL PERIOD
! periode minimum du spectre
      DOUBLE PRECISION PMIN
!brief MAXIMUM SPECTRAL PERIOD
! periode maximum du spectre
      DOUBLE PRECISION PMAX
!brief REFERENCE WATER DEPTH FOR AUTOMATIC PHASE
! Profondeur d'eau de référence pour le calcul automatique des phases
      DOUBLE PRECISION DEPREF
!brief REFERENCE WAVE NUMBER FOR AUTOMATIC PHASE
! Nombre d'onde de référence pour le calcul automatique des phases
      DOUBLE PRECISION KPHREF
!brief X COORDINATE OF REFERENCE POINT FOR PHASE DEFINITION
! coordonnee en x du point de reference pour les phases
      DOUBLE PRECISION X_PHREF
!brief Y COORDINATE OF REFERENCE POINT FOR PHASE DEFINITION
! coordonnee en y du point de reference pour les phases
      DOUBLE PRECISION Y_PHREF
!brief SUB-ITERATIONS ACCURACY FOR CURRENT(WAVE VECTOR DIRECTION)
! precision sur les sous-iterations pour le courant (direction vecteur d'onde)
      DOUBLE PRECISION EPSDIR
!brief SUB-ITERATIONS ACCURACY FOR TETAP
! precision sur les sous-iterations sur le TETAP
      DOUBLE PRECISION EPSTP
!brief RELAXATION COEFFICIENT FOR TETAP
! coefficient de relaxation pour calcul automatic de TETAP
      DOUBLE PRECISION RELTP
!brief TIME AT WHICH TOMAWAC SPECTRUM IS TAKEN
!
      DOUBLE PRECISION TPSTWC
!brief Significant wave height corresponding to global
! energy contained in TOMAWAC spectrum
      DOUBLE PRECISION HSCAL
!brief ABSCISSAE FOR THE REFERENCE F SPECTRUM
! x du point de reference pour le spectre en freq
      DOUBLE PRECISION X_SFREF
!brief ORDINATES FOR THE REFERENCE F SPECTRUM
! y du point de reference pour le spectre en freq
      DOUBLE PRECISION Y_SFREF
!
      DOUBLE PRECISION FP,GAM,DELTA
      DOUBLE PRECISION EXPO
!
!brief ARTEMIS CONSTANTS (INITIALISED INTO ARTEMIS_CONSTANTS)
!
      DOUBLE PRECISION :: PI,DEUPI,PISUR2,USDPI,RADDEG,DEGRAD
!
!-----------------------------------------------------------------------
!
!       7) STRINGS
!
!-----------------------------------------------------------------------
!
!brief TITLE
! titre de l'etude
      CHARACTER(LEN=72) TITCAS
!brief
!
      CHARACTER(LEN=72) VARDES
!brief VARIABLES TO BE PRINTED
! variables a imprimer
      CHARACTER(LEN=72) VARIMP
!brief INITIAL CONDITIONS
! conditions initiales
      CHARACTER(LEN=72) CDTINI
!brief GEOMETRY FILE BINARY
! binaire du fichier de geometrie
!     CHARACTER(LEN=3) BINGEO
!brief RESULTS FILE BINARY
! binaire du fichier des resultats
!     CHARACTER(LEN=3) BINRES
!brief
!
      CHARACTER(LEN=20) EQUA
!brief
!
      CHARACTER(LEN=32) VARCLA(10)
!brief
!
      CHARACTER(LEN=32) TEXTE(MAXVAR)
!brief
!
      CHARACTER(LEN=32) TEXTPR(MAXVAR)
!brief
! name of variables in phase and amplitude file
      CHARACTER(LEN=32) TEXTANIM(MAXVAR)
!
!     NAMES OF PRIVATE ARRAYS (GIVEN BY USER)
!
      CHARACTER(LEN=32) NAMES_PRIVE(4)
!
!-----------------------------------------------------------------------
!
!       8) SLVCFG STRUCTURES
!
!-----------------------------------------------------------------------
!
!brief SLVCFG STRUCTURE
!
      TYPE(SLVCFG) :: SLVART
!
!-----------------------------------------------------------------------
!
!       9) MESH STRUCTURE
!
!-----------------------------------------------------------------------
!
!brief MESH STRUCTURE
!
      TYPE(BIEF_MESH),TARGET :: MESH
!
!-----------------------------------------------------------------------
!
!      10) ALIASES
!
!-----------------------------------------------------------------------
!
!       DECLARATION OF POINTERS FOR ALIASES.
!       TARGETS ARE DEFINED IN POINT_ARTEMIS
!
!       ALIASES FOR WORKING VECTORS IN TB AND TBBD
!
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T1
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T2
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T3
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T4
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T5
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T6
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T7
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T8
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T9
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T10
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T11
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T12
!brief WORKING VECTOR IN TBBD
!
      TYPE(BIEF_OBJ),POINTER :: T13
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T14
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T15
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T16
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: TBD1
!brief WORKING VECTOR IN TBBD
!
      TYPE(BIEF_OBJ),POINTER :: TBD2
!brief WORKING VECTOR IN TBBD
!
      TYPE(BIEF_OBJ),POINTER :: TBD3
!brief WORKING VECTOR IN TBBD
!
      TYPE(BIEF_OBJ),POINTER :: TBD4
!
!       USEFUL COMPONENTS IN STRUCTURE MESH
!
!brief
! table de connectivite
      TYPE(BIEF_OBJ), POINTER :: IKLE
!brief
! coordonnees des points du maillage
      DOUBLE PRECISION, DIMENSION(:), POINTER :: X
!brief
! coordonnees des points du maillage
      DOUBLE PRECISION, DIMENSION(:), POINTER :: Y
!brief
! nombre d'elements du maillage
      INTEGER, POINTER        :: NELEM
!brief
!
      INTEGER, POINTER        :: NELMAX
!brief
! nombre de points frontiere
      INTEGER, POINTER        :: NPTFR
!brief
!
      INTEGER, POINTER        :: NPTFRX
!brief
!
      INTEGER, POINTER        :: DIM1
!brief
!
      INTEGER, POINTER        :: TYPELM
!brief
! nombre de points du maillage
      INTEGER, POINTER        :: NPOIN
!brief
!
      INTEGER, POINTER        :: NPMAX
!brief
!
      INTEGER, POINTER        :: MXPTVS
!brief
!
      INTEGER, POINTER        :: MXELVS
!brief
!
      INTEGER, POINTER        :: LV
!
!-----------------------------------------------------------------------
!
!      11) ART_FILES AND ASSOCIATED
!
!-----------------------------------------------------------------------
!
!brief
!
      INTEGER, PARAMETER :: MAXLU_ART = 46
!brief NAME OF THE GEOMETRY FILE
! nom du fichier de geometrie
      INTEGER, TARGET :: ARTGEO
!brief NAME OF THE STEERING FILE
! nom du fichier des parametres
      INTEGER :: ARTCAS
!brief NAME OF THE BOUNDARY CONDITIONS FILE
! nom du fichier des conditions aux limites
      INTEGER, TARGET :: ARTCLI
!brief NAME OF THE BOTTOM TOPOGRAPHY FILE
! nom du fichier des fonds
      INTEGER :: ARTFON
!brief NAME OF THE RESULTS FILE
! nom du fichier des resultats
      INTEGER, TARGET :: ARTRES
!brief NAME OF THE BINARY RESULTS FILE
! nom du fichier des resultats binaire
      INTEGER :: ARTRBI
!brief NAME OF THE FORMATTED RESULTS FILE
! nom du fichier des resultats formate
      INTEGER :: ARTRFO
!brief NAME OF THE AMPLITUDE AND PHASE FILE
! nom du fichier contenant phases et amplitudes
      INTEGER :: ARTAMP
!brief NAME OF THE REFERENCE FILE
! nom du fichier de reference
      INTEGER :: ARTREF
!brief NAME OF THE BINARY DATA FILE 1
! nom du fichier de donnees binaire 1
      INTEGER :: ARTBI1
!brief NAME OF THE BINARY DATA FILE 2
! nom du fichier de donnees binaire 2
      INTEGER :: ARTBI2
!brief NAME OF THE FORMATTED DATA FILE 1
! nom du fichier de donnees formate 1
      INTEGER :: ARTFO1
!brief NAME OF THE FORMATTED DATA FILE 2
! nom du fichier de donnees formate 2
      INTEGER :: ARTFO2
!brief NAME OF THE TOMAWAC OUTER SPECTRAL FILE
! nom du fichier de spectre global
      INTEGER :: WACSPE
!brief NAME OF THE TOMAWAC OUTER RESULT FILE
! nom du fichier de resultats global
      INTEGER :: WACRES
!brief NAME OF THE TOMAWAC LIQUID BOUNDARY FILE
! nom du fichier de frontieres liquides global
      INTEGER :: WACLQD
!brief
!
      TYPE(BIEF_FILE), TARGET :: ART_FILES(MAXLU_ART)
!
      SAVE
!
      END MODULE DECLARATIONS_ARTEMIS
