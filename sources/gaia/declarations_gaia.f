!                   ************************
                    MODULE DECLARATIONS_GAIA
!                   ************************
!
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief   Declaration of the principal GAIA variables
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
!
!       NOTE: this declaration file is organised in 10 parts:
!
!       1) Vectors (will be declared as BIEF_OBJ structures)
!       2) Matrices (will be declared as BIEF_OBJ structures)
!       3) Blocks (will be declared as BIEF_OBJ structures)
!       4) Integers
!       5) Logical values
!       6) Reals
!       7) Strings
!       8) SLVCFG structures
!       9) Mesh structure
!      10) Aliases
!
!-----------------------------------------------------------------------
!
!       1) Vectors
!
!-----------------------------------------------------------------------
!
!>    Evolution of the bed mass at each point for each time step [kg/m2]
!
      TYPE(BIEF_OBJ), TARGET :: E
!
!>    Evolution saved for constant flow discharge
!
      TYPE(BIEF_OBJ), TARGET :: ECPL
!
!>    Free surface elevation
!
      TYPE(BIEF_OBJ), TARGET :: Z
!
!>    Total mass evolution due to bedload [kg/m2]
!
      TYPE(BIEF_OBJ), TARGET :: EVOL_MB
!
!>    Total mass evolution due to suspension [kg/m2]
!
      TYPE(BIEF_OBJ), TARGET :: EVOL_MS
!
!>    Total mass evolution due to consolidation [kg/m2]
!
      TYPE(BIEF_OBJ), TARGET :: EVOL_MC
!
!>    Total mass evolution due to sliding (maxslope) [kg/m2]
!
      TYPE(BIEF_OBJ), TARGET :: EVOL_MM
!
!>    Cumulated bed evolution over time [kg/m2]
!
      TYPE(BIEF_OBJ), TARGET :: ESOMT
!
!>    Cumulated bed evolution over time [m]
!
      TYPE(BIEF_OBJ), TARGET :: CUMBE
!
!>    Maximum evolution to be defined by the user for automatic
!     differentiation with Nag
!
      TYPE(BIEF_OBJ), TARGET :: EMAX
!
!>    X component of the flow rate
!
      TYPE(BIEF_OBJ), TARGET :: QU
!
!>    Y component of the flow rate
!
      TYPE(BIEF_OBJ), TARGET :: QV
!
!>    Flow rate
!
      TYPE(BIEF_OBJ), TARGET :: Q
!
!>    Total solid discharge (bedload+suspension)
!
      TYPE(BIEF_OBJ), TARGET :: QS
!
!>    Solid discharge, along x and y
!
      TYPE(BIEF_OBJ), TARGET :: QSX,QSY
!
!>    Solid discharge (bedload)
!
      TYPE(BIEF_OBJ), TARGET :: QS_C
!
!>    Solid discharge (bedload), along x and y
!
      TYPE(BIEF_OBJ), TARGET :: QSXC,QSYC
!
!>    Water depth: HN is H in Telemac-2D or 3d,
!!    it is modified in case of HN<HMIN
!!    (see gaia_prepare_step);
!!    HN_GAI is a copy of HN, sent to Telemac-2D
!
      TYPE(BIEF_OBJ), TARGET :: HN,HN_GAI
!
!>    Components of depth-averaged velocity
!
      TYPE(BIEF_OBJ), TARGET :: U2D,V2D
!
!>    Norm of the mean flow velocity
!
      TYPE(BIEF_OBJ), TARGET :: UNORM
!
!>    Current direction (deg trigo)
!
      TYPE(BIEF_OBJ), TARGET :: THETAC
!
!>    Water depth saved for constant flow discharge
!
      TYPE(BIEF_OBJ), TARGET :: HCPL
!
!>    Imposed bed evolution at the boundary [m]
!
      TYPE(BIEF_OBJ), TARGET :: EBOR
!
!>    Imposed solid transport at the boundary
!!    In m3/s, for every class
!
      TYPE(BIEF_OBJ), TARGET :: QBOR
!
!>    Imposed solid transport at the boundary
!!    In m2/s, total, read in the boundary conditions file
!
      TYPE(BIEF_OBJ), TARGET :: Q2BOR
!
!>    Zf values on boundaries
!
      TYPE(BIEF_OBJ), TARGET :: FLBOR
!
!>    Bottom elevation
!
      TYPE(BIEF_OBJ), TARGET :: ZF
!
!>    Non erodable (rigid) bottom elevation
!
      TYPE(BIEF_OBJ), TARGET :: ZR
!
!>    Reference level for Nestor
!
      TYPE(BIEF_OBJ), TARGET :: ZRL          ! Nestor
!
!>    Reference elevation
!
      TYPE(BIEF_OBJ), TARGET :: ZREF
!
!>    Integral of bases
!
      TYPE(BIEF_OBJ), TARGET :: VOLU2D
!
!>    Integral of bases in parallel
!
      TYPE(BIEF_OBJ), TARGET :: V2DPAR
!
!>    Inverse of integral of bases
!
      TYPE(BIEF_OBJ), TARGET :: UNSV2D
!
!>    Cosinus of the angle between mean flow q and transport qs
!
      TYPE(BIEF_OBJ), TARGET :: CALFA_CL
!
!>    Sinus of the angle between mean flow q and transport qs
!
      TYPE(BIEF_OBJ), TARGET :: SALFA_CL
!
!>    Curve radius for secondary currents
!
      TYPE(BIEF_OBJ), TARGET :: RADSEC
!
!>    Void structure
!
      TYPE(BIEF_OBJ), TARGET :: S
!
!>    Mask on points
!
      TYPE(BIEF_OBJ), TARGET :: MASKPT
!
!>    Mask
!
      TYPE(BIEF_OBJ), TARGET :: MASKTR
!
!>    Mask
!
      TYPE(BIEF_OBJ), TARGET :: MASKB
!
!>    Mask
!
      TYPE(BIEF_OBJ), TARGET :: MASKEL
!
!>    Mask
!
      TYPE(BIEF_OBJ), TARGET :: MSKTMP
!
!>    Working arrays
!
      TYPE(BIEF_OBJ), TARGET :: W1
!
! WAVE DATA
! --------
!
!>    Wave direction (deg trigo: wrt ox axis)
!
      TYPE(BIEF_OBJ), TARGET :: THETAW
!
!>    Quadratic friction coefficient (waves)
!
      TYPE(BIEF_OBJ), TARGET :: FW
!
!>    Orbital wave velocity
!
      TYPE(BIEF_OBJ), TARGET :: UW
!
!>    Significant wave height
!
      TYPE(BIEF_OBJ), TARGET :: HW
!
!>    Mean wave period
!
      TYPE(BIEF_OBJ), TARGET :: TW
!
!>    Like IFABOR but ignoring masked elements
!
      TYPE(BIEF_OBJ), TARGET :: IFAMAS
!
!>    Integer working arrays
!
      TYPE(BIEF_OBJ), TARGET :: IT1,IT2,IT3,IT4
!
!>    Type of boundary conditions on bed evolution
!
      TYPE(BIEF_OBJ), TARGET :: LIEBOR
!
!>    Type of boundary conditions on sand transport rate
!
      TYPE(BIEF_OBJ), TARGET :: LIQBOR
!
!>    Type of boundary conditions
!
      TYPE(BIEF_OBJ), TARGET :: LIMTEC
!
!>    Correction of transport for sloping bed effect
!
      TYPE(BIEF_OBJ), TARGET :: COEFPN
!
!>    Correction of critical Shields for sloping bed effect
!
      TYPE(BIEF_OBJ), TARGET :: COEFCR
!      
!>    Liquid boundary numbering
!
      TYPE(BIEF_OBJ), TARGET :: NUMLIQ
!
!>    Bed shear stress [n/m2]
!
      TYPE(BIEF_OBJ), TARGET :: TOB
!
!>    Shear stress modified by skin friction
!
      TYPE(BIEF_OBJ), TARGET :: TAUP
!
!>    Quadratic friction coefficient
!
      TYPE(BIEF_OBJ), TARGET :: CF
!
!>    Wave induced shear stress
!
      TYPE(BIEF_OBJ), TARGET :: TOBW
!
!>    Mean of total current + wave shear stress
!
      TYPE(BIEF_OBJ), TARGET :: TOBCW_MEAN
!
!>    Maximum of total current + wave shear stress
!
      TYPE(BIEF_OBJ), TARGET :: TOBCW_MAX
!
!>    Skin friction correction factor for bed roughness:
!!    Ratio between shear stress due skin friction and total bed shear stress
!
      TYPE(BIEF_OBJ), TARGET :: MU
!
!>    Total bed roughness
!
      TYPE(BIEF_OBJ), TARGET :: KS
!
!>    Bed skin roughness
!
      TYPE(BIEF_OBJ), TARGET :: KSP
!
!>    Ripple bed roughness
!
      TYPE(BIEF_OBJ), TARGET :: KSR
!
!>    Mean diameter of active-layer
!
      TYPE(BIEF_OBJ), TARGET :: ACLADM
!
!>    Mean diameter of under-layer
!
      TYPE(BIEF_OBJ), TARGET :: UNLADM
!
!>    Number of layers for each point
!
      TYPE(BIEF_OBJ), TARGET :: NLAYER
!
!>    Hiding factor correction
!
      TYPE(BIEF_OBJ), TARGET :: HIDING
!
!>    Active layer thickness
!
      TYPE(BIEF_OBJ), TARGET :: ELAY
!
!>    Active stratum thickness
!
      TYPE(BIEF_OBJ), TARGET :: ESTRAT
!
!>    Deposition flux
!
      TYPE(BIEF_OBJ), TARGET :: FLUDP
!
!>    Deposition flux for implicitation
!
      TYPE(BIEF_OBJ), TARGET :: FLUDPT,FLUDPT_ADV
!
!>    Erosion flux
!
      TYPE(BIEF_OBJ), TARGET :: FLUER,FLUERDH,FLUER_ADV
!
!>    Erosion flux for implicitation
!
      TYPE(BIEF_OBJ), TARGET :: FLUERT
!
!>    Sediment equilibrium concentration
!
      TYPE(BIEF_OBJ), TARGET :: CSTAEQ
!
!>    Ratio between bottom concentration and average concentration
!
      TYPE(BIEF_OBJ), TARGET :: CSRATIO
!
!>    Components of velocity vectors
!
      TYPE(BIEF_OBJ), TARGET ::  UCONV_GAI,VCONV_GAI
      LOGICAL :: CONV_GAI_POINTER
!
!>    Propagation height
!
      TYPE(BIEF_OBJ), TARGET :: HPROP
!
!>    Flux condition nu df/dn=afbor * f + bfbor
!
      TYPE(BIEF_OBJ), TARGET :: AFBOR,BFBOR
!
!>    Flux at the boundaries
!
      TYPE(BIEF_OBJ), TARGET :: FLBOR_GAI
!
!>    Flux at the boundaries for tracer
!
      TYPE(BIEF_OBJ), TARGET :: FLBORTRA
!
!     TYPES OF BOUNDARY CONDITIONS FOR H                   : LIHBOR
!     TYPES OF BOUNDARY CONDITIONS FOR PROPAGATION         : LIMPRO
!                    POINTS   :    .1:H  .2:U  .3:V
!                    SEGMENTS :    .4:H  .5:U  .6:V
!
!>    Type of boundary conditions for h
!
      TYPE(BIEF_OBJ), TARGET :: LIHBOR
!
!>    Type of boundary conditions for propagation
!
      TYPE(BIEF_OBJ), TARGET :: LIMPRO
!
!>    Type of boundary conditions for diffusion
!
      TYPE(BIEF_OBJ), TARGET :: LIMDIF
!
!>    Last column of the boundary condition file
!
      TYPE(BIEF_OBJ), TARGET :: BOUNDARY_COLOUR
!
!>    Boundary conditions for tracer, u and v (modified litbor, liubor,livbor)
!
      TYPE(BIEF_OBJ), TARGET :: CLT,CLU,CLV
!
!>    Work arrays for elements
!
      TYPE(BIEF_OBJ), TARGET :: TE1,TE2,TE3
!
!>    Coefficients of the dispersion tensor (dim. npoin)
!
      TYPE(BIEF_OBJ), TARGET :: KX,KY,KZ
!
!>    Array saying whether the non-erodable bottom has been reached (vf)
!
      TYPE(BIEF_OBJ), TARGET :: BREACH
!
!>    For mixed sediments
!
      TYPE(BIEF_OBJ), TARGET :: MS_SABLE,MS_VASE
!
!>    Surface mass of mud (kg/m2), for imud,ilayer,ipoin
!
      DOUBLE PRECISION,DIMENSION(:,:,:),TARGET,ALLOCATABLE :: MASS_MUD
!
!>    Surface mass of sand (kg/m2), for isand,ilayer,ipoin
!
      DOUBLE PRECISION,DIMENSION(:,:,:),TARGET,ALLOCATABLE :: MASS_SAND
!
!>    Surface total mass of mud (kg/m2), for ilayer,ipoin
!
      DOUBLE PRECISION,DIMENSION(:,:), ALLOCATABLE :: MASS_MUD_TOT
!
!>    Surface total mass of sand (kg/m2), for ilayer,ipoin
!
      DOUBLE PRECISION,DIMENSION(:,:), ALLOCATABLE :: MASS_SAND_TOT
!
!>    Surface total mass of sediments (kg/m2), for ilayer,ipoin
!
      DOUBLE PRECISION,DIMENSION(:,:), ALLOCATABLE :: MASS_MIX_TOT
!
!>    Ratio of sand to all sands, for isand,ilayer,ipoin
!
      DOUBLE PRECISION,DIMENSION(:,:,:),TARGET,ALLOCATABLE :: RATIO_SAND
!
!>    Ratio of mud to all muds, for imud,ilayer,ipoin
!
      DOUBLE PRECISION,DIMENSION(:,:,:),TARGET,ALLOCATABLE :: RATIO_MUD
!
!>    Ratio of mud to sand, for ilayer,ipoin
!
      DOUBLE PRECISION,DIMENSION(:,:), ALLOCATABLE :: RATIO_MUD_SAND
!
!>    Ratio of sand in the prescribed solid discharge
!
      DOUBLE PRECISION,DIMENSION(:), ALLOCATABLE :: RATIO_DEBIMP
!
! Variables allocated only for SUSPENSION_ERODE
      DOUBLE PRECISION, ALLOCATABLE :: QER_MUD(:),QER_SAND(:),TIME(:)
      DOUBLE PRECISION, ALLOCATABLE :: QE_MOY(:)
      DOUBLE PRECISION,DIMENSION(:,:,:), ALLOCATABLE :: TOCE_MIX
      DOUBLE PRECISION,DIMENSION(:,:,:), ALLOCATABLE :: CAE_ILAY
!
      DOUBLE PRECISION,DIMENSION(:,:), ALLOCATABLE :: FLUER_PUR_MUD
      DOUBLE PRECISION,DIMENSION(:,:), ALLOCATABLE :: FLUER_PUR_SAND

      DOUBLE PRECISION,DIMENSION(:,:), ALLOCATABLE :: FLUER_MIX
!
!>    Bedload boundary flux for every class (kg/s): variable for mass balance
!
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: BEDLOAD_B_FLUX
!
!>    Cumulated bedload on boundary for every class (kg): variable for mass balance
!
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: SUMBEDLOAD_B
!
!>    Sum over classes of bedload boundary flux or cumulated bedload:
!     variable for mass balance
!
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: SUMBEDLOAD_B_FLUX
!
!>    cumulated mass through bedload boundary per class per time step (kg/s)
!
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: MCUMUCLA
!
!
!-----------------------------------------------------------------------
!
!       2) MATRICES
!
!-----------------------------------------------------------------------
!
!>    Boundary matrix
!
      TYPE(BIEF_OBJ), TARGET :: MBOR
!
!-----------------------------------------------------------------------
!
!       3) BLOCKS
!
!-----------------------------------------------------------------------
!
!>    Block of masks
!
      TYPE(BIEF_OBJ), TARGET :: MASK
!
!>    Blocks of working arrays
!
      TYPE(BIEF_OBJ), TARGET :: TB
!
!>    Block of private vectors
!
      TYPE(BIEF_OBJ), TARGET :: PRIVE
!
!>    Block of differentiated vectors
!
      TYPE(BIEF_OBJ), TARGET :: ADVAR
!
!>    Block of clandestine variables
!
      TYPE(BIEF_OBJ), TARGET :: VARCL
!
!>    Block of variables for input
!
      TYPE(BIEF_OBJ), TARGET :: VARHYD
!
!>    Block of variables for output
!
      TYPE(BIEF_OBJ), TARGET :: VARSOR
!
!>    Vertical sorting profile: fraction for each layer, class, point
!
      DOUBLE PRECISION,DIMENSION(:,:,:),TARGET,ALLOCATABLE::PRO_F
!
!>    Vertical sorting profile: depth for each layer, class, point
!
      DOUBLE PRECISION,DIMENSION(:,:,:),TARGET,ALLOCATABLE::PRO_D
!
!>    Sediment fraction for each layer, class, point
!
      DOUBLE PRECISION,DIMENSION(:,:,:),TARGET,ALLOCATABLE::AVAIL
!
!>    Layer thicknesses as double precision
!
      DOUBLE PRECISION,DIMENSION(:,:),TARGET,ALLOCATABLE :: ES
!
!>    conversion mass to thickness
!
      DOUBLE PRECISION,DIMENSION(:,:),TARGET,ALLOCATABLE::M2T
!
!>    conversion mass per area to thickness
!
      DOUBLE PRECISION,DIMENSION(:),TARGET,ALLOCATABLE :: MPA2T
!
!>    Sediment composition : sand (for output variables)
!
      TYPE(BIEF_OBJ), TARGET :: RATIOS
!
!>    Sediment composition : mud (for output variables)
!
      TYPE(BIEF_OBJ), TARGET :: RATIOM
!
!>    Layer thicknesses (for output variables)
!
      TYPE(BIEF_OBJ), TARGET :: LAYTHI
!
!>    Layer concentration (for output variables)
!
      TYPE(BIEF_OBJ), TARGET :: LAYCONC
!
!>    Layer porosity (for output variables)
!
      TYPE(BIEF_OBJ), TARGET :: LAYPOR
!
!>    Sand mass (for output variables)
!
      TYPE(BIEF_OBJ), TARGET :: MASS_S
!
!>    Mud mass (for output variables)
!
      TYPE(BIEF_OBJ), TARGET :: MASS_M
!
!>    Mass transfer for consolidation between layers
!!    (for output variables)
!
      TYPE(BIEF_OBJ), TARGET :: MTRANSFER
!
!>    Critical erosion shear stress of the mud
!!    (for output variables)
!
      TYPE(BIEF_OBJ), TARGET :: TOCEMUD
!
!>    Partheniades erosion coefficient
!!    (for output variables)
!
      TYPE(BIEF_OBJ), TARGET :: PARTHE
!
!>    Total transport rate for a sediment class : bedload+suspended load
!
      TYPE(BIEF_OBJ), TARGET :: QSCL
!
!>    Bedload transport rate for a sediment class [kg*(m-1*s-1)]
!
      TYPE(BIEF_OBJ), TARGET :: QSCL_C
!
!>    Bedload transport rate in the x and y direction for a sediment class [kg*(m-1*s-1)]
!
      TYPE(BIEF_OBJ), TARGET :: QSCLXC, QSCLYC
!
!>    Mass of mud in bedload added in suspension
!
      TYPE(BIEF_OBJ), TARGET :: MUDB
!>    Flux of mud in bedload added in suspension
!
      TYPE(BIEF_OBJ), TARGET :: F_MUDB
!
!>    Suspended load transport rate for a sediment class
!
      TYPE(BIEF_OBJ), TARGET :: QSCL_S
!
!>    Bed evolution per class (due to bedload)
!
      TYPE(BIEF_OBJ), TARGET :: ZFCL_C
!
!>    Mass evolution for class (due to bedload)
!
      TYPE(BIEF_OBJ), TARGET :: EVCL_MB
!
!>    Mass evolution for class (due to suspension)
!
      TYPE(BIEF_OBJ), TARGET :: EVCL_MS
!
!>    Evolution for each class due to sloping bed effects
!
      TYPE(BIEF_OBJ), TARGET :: ZFCL_MS
!
!>    Meyer Peter Mueller factor
!
      TYPE(BIEF_OBJ), TARGET :: MPM_ARAY
!
!>    Flux limitation per segment
!
      TYPE(BIEF_OBJ), TARGET :: FLULIM_GAI
!
!>    Fluxes at boundary for every class
!
      TYPE(BIEF_OBJ), TARGET :: FLBCLA
!
!>    Void index of bed layers
!
      DOUBLE PRECISION,DIMENSION(:,:),TARGET,ALLOCATABLE::IVIDE
!
!-----------------------------------------------------------------------
!
!       4) INTEGERS
!
!-----------------------------------------------------------------------
!
!      KEYWORDS AND PARAMETERS
!
!>    Maximum layer number in a vertical sorting profile for each point
!
      INTEGER, ALLOCATABLE :: PRO_MAX(:)
!
!>     Maximum number of output variables
!
      INTEGER, PARAMETER :: MAXVAR = 500
!
!>    Maximum number of (liquid boundaries, solid boundaries)
!
      INTEGER MAXFRO
!
!>    Number of liquid boundaries
!
      INTEGER NFRLIQ
!
!>    Supg option
!
      INTEGER OPTSUP
!
!>    Bed-load transport formula
!
      INTEGER ICF
!
!>    Number of clandestine variable
!
      INTEGER NVARCL
!
!>    Missing comment
!
      INTEGER IELMT,IELMH_GAI,IELMU_GAI,IELMX
!
!>    Option for the treatment of tidal flats
!
      INTEGER OPTBAN
!
!>    Vector length (for vectorisation) from steering file
!
      INTEGER LVMAC
!
!>    Matrix storage
!
      INTEGER OPTASS
!
!>    Number of sub-iterations
!
      INTEGER NSOUS
!
!>    Orginal date of time
!
      INTEGER MARDAT(3)
!
!>    Original hour of time
!
      INTEGER MARTIM(3)
!
!>    Matrix-vector product
!
      INTEGER PRODUC
!
!>    First time from which to write the graphical outputs
!
      INTEGER PTINIG
!
!>    First time from which to write the listing outputs
!
      INTEGER PTINIL
!
!>    Number of private arrays, number of private arrays with given name
!
      INTEGER NPRIV,N_NAMES_PRIV
!
!>    Number of differentiating arrays, and those with a given name
!
      INTEGER NADVAR,N_NAMES_ADVAR
!
!>    Number of directions for differentiating in vector modes
!
      INTEGER AD_NUMOFDIR
!
!>    Numero du pas de temps
!
      INTEGER, TARGET :: LT
!
!>    Formula for deviation
!
      INTEGER DEVIA
!
!>    Formula for slope effect
!
      INTEGER SLOPEFF
!
! NON-EQUILIBRIUM BEDLOAD AND NON-UNIFORM BED MATERIA (BMD AND MGDL)
! --------
!
!>    Maximum number of sediment classes
!
      INTEGER, PARAMETER :: NSICLM = 10
!
!>    Number of sediment classes of bed material (less than NISCLM)
!
      INTEGER, TARGET :: NSICLA
!
!>    Number of suspension sediment classes for TELEMAC3D or TELEMAC2D
!!    (less than NISCLM)
!
      INTEGER :: NSUSP_TEL
!
!>    Number of prescribed suspended sediments for TELEMAC3D or TELEMAC2D
!
      INTEGER :: NPRESED
!
!>    Number of sediment vertical profiles
!
      INTEGER :: NVERPROSED
!
!>    Number of suspended sediments at the sources
!
      INTEGER :: NSEDSCE
!
!>    Maximum number of layers on the mesh
!
      INTEGER, PARAMETER :: NLAYMAX = 20
!
!>    Number of layers of initial stratification
!
      INTEGER :: NUMSTRAT
!
!>    Number of bed load model layers = NUMSTRAT+1
!!    to take the active layer into account
!
      INTEGER, TARGET :: NOMBLAY
!
!>    Hiding factor formulas
!
      INTEGER :: HIDFAC
!
!>    Debugger
!
      INTEGER :: DEBUG
!
!>    Reference concentration formula
!
      INTEGER :: ICQ
!
!>    Number of control sections points
!
      INTEGER :: NCP
!
!>    Array containing the global number of the points in the control sections
!
      INTEGER, ALLOCATABLE :: CTRLSC(:)
!
!>    Coordinates of the origin
!
      INTEGER :: I_ORIG,J_ORIG
!
!>    Number of layers for consolidation
!
      INTEGER :: NCOUCH_TASS
!
!>    Skin friction correction
!
      INTEGER :: ICR
!
!>    Bed roughness predictor option
!
      INTEGER :: IKS
!
!>    Number of given solid discharges given by user
!
      INTEGER NSOLDIS
!
!>    Number of class proportion for imposed discharge given by user
!
      INTEGER NPROP
!
!>    Maximum number of iterations used for ensuring positive
!!    layer thickness. Beware that with larger time steps
!!    this parameter should be increased.
!
      INTEGER MAXADV
!
!>    Index in varsor for output variables
!
      INTEGER NVAR
!
!>    Index in varsor for output variables
!
      INTEGER NVAR_RATIOS,NVAR_QSCL,NVAR_QS_C,NVAR_QSXC,
     &   NVAR_QSYC,NVAR_QSCL_C,
     &   NVAR_LAYTHI,NVAR_LAYCONC,NVAR_PRIV,NVAR_RATIOM,
     &   NVAR_VARCL,NVAR_ADVAR,NVAR_MASS_S,NVAR_MASS_M,
     &   NVAR_MTRANS, NVAR_TOCEMUD, NVAR_PARTHE
!
!>    Hindered settling method linked to floculation effects,
!!    1:WHITEHOUSE ET AL. (2000), 2:WINTERWERP (1999),
!!    at the moment hard-set to 1 in gaia.F
!
      INTEGER HIND_TYPE
!
!>    Floculation method: 1: apply reduction due to turbulent breakup of
!!    flocs; 2: Soulsby floculation model
!
      INTEGER FLOC_TYPE
!
!>    Type of waves (regular or irregular)
!
      INTEGER TYPE_HOULE
!
!>    Choose the advection field in cvdftr
!
      INTEGER SOLSYS_GAI
!
!>    Option for finite volumes (see cvtrvf)
!
      INTEGER OPTVF_GAI
!
!>    Scheme for advection of suspended sediments
!
      INTEGER, ALLOCATABLE :: SCHADVSED(:)
!
!>    Advection scheme options for suspended sediments
!
      INTEGER, ALLOCATABLE :: OPTADV_SED(:)
!
!>    Scheme for diffusion of suspended sediments in 3D
!
      INTEGER SCHDSED
!
!>    Finite volume scheme for diffusion of suspended sediments in 2D
!
      INTEGER MVIST_SED
!
!     Sediment vertical profiles
!
      INTEGER, ALLOCATABLE :: VERPROSED(:)
!
!> For the Continous Vertical Sorting MODEL
!
!>    Type of the Vertical Grain Sorting: Hirano Layers or Continous-VSM
!
      INTEGER VSMTYPE
!
!>    Maximum Number of Profile SECTIONS
!
      INTEGER PRO_MAX_MAX
!
!>    Printout Period for Full Vertical Sorting Model: PRO_D & PRO_F
!
      INTEGER CVSMPPERIOD
!
!>    CHOOSE POINTS or FULL MODEL AS PRINTOUT
!
      INTEGER CVSMOUTPUT(100)    !Limited to 100 for no specific reason
!
!>    CHOOSE A MODEL FOR ESTIMATION OF A DYNAMIC ACTIVE LAYER THICKNESS
!
      INTEGER ALT_MODEL
!
!>    COUPLING PERIOD USED IN CVSM TO CALCULATE THE TIME, SHOULD COME FROM TELEMAC ONE DAY
      INTEGER PERCOU
!
!>    GRAPHIC PRINTOUT PERIOD
      INTEGER LEOPR
!
!>    LISTING PRINTOUT PERIOD
      INTEGER LISPR
!
!-----------------------------------------------------------------------
!
!       5) LOGICAL VALUES
!
!-----------------------------------------------------------------------
!
!>    Used in function cgl_gaia
!
      LOGICAL, ALLOCATABLE :: OKCGL(:)
!
!>    Used in function qgl_gaia!
!
      LOGICAL, ALLOCATABLE :: OKQGL(:)
!
!>    Graphical output
!
      LOGICAL :: SORLEO(MAXVAR)
!
!>    Listing output
!
      LOGICAL :: SORIMP(MAXVAR)
!
!>    Include masking
!
      LOGICAL :: MSK
!
!>    Writes out (or not)
!
      LOGICAL :: ENTET
!
!>    Resolution for suspension is implicit (or not)
!
      LOGICAL :: YASMI
!
!>    Work in spherical coordinates (hard-coded)
!
      LOGICAL :: SPHERI
!
!>    Include tidal flats in the simulation
!
      LOGICAL :: BANDEC
!
!>    Include wave effects
!
      LOGICAL :: HOULE
!
!>    Include bedload in the simulation
!
      LOGICAL, TARGET :: CHARR
!
!>    Loading law used or not
!
      LOGICAL :: NOEQUBED
!
!>    Use a Finite Volumes formulation
!
      LOGICAL :: VF
!
!>    Constant active layer thickness
!
      LOGICAL :: CONST_ALAYER
!
!>    Suspension : yes if there is at least one suspended sediment
!!    this is the case if there is mud or if
!!    suspension is activated for the sand
!
      LOGICAL, TARGET :: SUSP
!
!>    Suspension for all sands (mud is assumed to be suspended)
!
      LOGICAL, TARGET :: SUSP_SAND
!
!>    Mass balance
!
      LOGICAL :: BILMA
!
!>    Validation
!
      LOGICAL :: VALID
!
!>    Imposed concentration in inflow
!
      LOGICAL :: IMP_INFLOW_C
!
!>    Secondary currents
!
      LOGICAL :: SECCURRENT
!
!>    Secondary currents radii file
!
      LOGICAL :: HAVESECFILE
!
!>    Correction on convection velocity
!
      LOGICAL :: CORR_CONV
!
!>    Computation continued
!
      LOGICAL :: DEBU
!
!>    Sediment slide
!
      LOGICAL :: SLIDE
!
!>    Cohesive sediments (for each class)
!
      LOGICAL :: SEDCO(NSICLM)
!
!>    Coupling with NESTOR
!
      LOGICAL :: NESTOR
!
!>    Bed roughness prediction
!
      LOGICAL :: KSCALC
!
!>    Settling lag: determines choice between Rouse
!!    and Miles concentration profile
!!    SET_LAG = TRUE : Miles
!!            = FALSE: Rouse
!
      LOGICAL :: SET_LAG
!
!>    Fluxline
!
      LOGICAL :: DOFLUX
!
!>    Option used in computation continued: if true, mass is contained in
!!    the previous sedimentological file
!
      LOGICAL :: DEBU_MASS
!
!>    Hindered settling switch
!
      LOGICAL :: HINDER
!
!>    Include floculation effects
!
      LOGICAL :: FLOC
!
!>    Logical used for coupling with T2D/T3D when suspension activated
!
      LOGICAL :: SECOND_SUSP_STEP
!
!>    Logicals used to know whether the mud arrays were found in
!!    the restart file. If they are not found, they are taken
!!    from the steering file and constant in space in the active
!!    layer
!
      LOGICAL ::  CONC_MUD_FOUND,TOCE_MUD_FOUND,PARTHENIADES_FOUND,
     &            MTRANS_FOUND
!
!>    Logical for modification of boundary fluxes
!
      LOGICAL :: YAFLULIM_GAI
!
!>     C-VSM WRITES OUT (OR NOT) IN THIS TIMESTEP
!
      LOGICAL :: CVSM_OUT

!>     C-VSM_FULL WRITES OUT (OR NOT) EVER
!
      LOGICAL :: CVSM_OUT_FULL
!
!>    Logical for trigonometrical convention in wave file
!
      LOGICAL :: CONV_WAVES
!
!-----------------------------------------------------------------------
!
!       6) REALS
!
!-----------------------------------------------------------------------
!
!>    Water density (from steering file of T2D or T3D)
!
      DOUBLE PRECISION XMVE
!
!>    Sand density
!
      DOUBLE PRECISION XMVS0(NSICLM)
!
!>    Initial porosity by layers
!
      DOUBLE PRECISION, TARGET :: XKV0(NLAYMAX)
!
!>    Gravity acceleration
!
      DOUBLE PRECISION GRAV
!
!>    Water viscosity: it is defined here because
!!    the viscosity set in TELEMAC2D or TELEMAC3D may not b
!!    the physical one
!
      DOUBLE PRECISION VCE
!
!>    Minimal value of the water height:
!!    below this value, the sediment flow rate
!!    is set to 0
!
      DOUBLE PRECISION HMIN
!
!>    Beta coefficient for Koch and Flokstra slope effect formulation
!
      DOUBLE PRECISION BETA
!
!>    Time step
!!    It may be different from the one in TELEMAC because
!!    of the morphological factor
!
      DOUBLE PRECISION, TARGET :: DT
!
!>    Initial settling velocities
!
      DOUBLE PRECISION XWC0(NSICLM)
!
!>    Settling velocities
!
      DOUBLE PRECISION, TARGET :: XWC(NSICLM)
!
!>    Critical shields parameter
!
      DOUBLE PRECISION, TARGET ::  AC(NSICLM)
!
!>    Friction angle of the sediment
!
      DOUBLE PRECISION, TARGET :: PHISED
!
!>    Parameter for deviation
!
      DOUBLE PRECISION, TARGET :: BETA2
!
!>   Sediment diameter D90, for sand when only
!    one sand class is present (otherwise, D90 is
!    calculated by GAIA).
!
      DOUBLE PRECISION, TARGET :: D90
!
!>    Hiding factor for each sediment class
!!    Used only if HIDFAC is set to 0.
!!    By default it is set to 1, which means there
!!    no hiding.
!
      DOUBLE PRECISION HIDI(NSICLM)
!
!>    Sediment diameter for each class
!!    It is only relevant for non-cohesive sediments.
!!    For the bedload, it can be the median diameter d50,
!!    or d65, or another, depending on the sediment
!!    flowrate formula used. Beware of this when you define
!!    the diameters in the steering file.
!!    For the suspension, it is used to assess the drag force
!!    on the sediments.
!!    The diameters of cohesive sediments have to be defined
!!    in the steering file anyway because it may be used
!!    for floculation.
!
      DOUBLE PRECISION, TARGET ::  DCLA(NSICLM)
!
!>    Initial fraction of each sediment class, the sum
!!    of AVA0 over all classes has to be equal to 1.
!
      DOUBLE PRECISION AVA0(NSICLM)
!
!>    Wanted active layer thickness; ELAYO is a target value for ELAY,
!!    but ELAY may be lower than ELAY0 if there is not enough sediment.
!
      DOUBLE PRECISION ELAY0
!
!>    Total volume of sediment of each class
!
      DOUBLE PRECISION VOLTOT(NSICLM)
!
!>    Initial volume of sediment of each class for CVSM
!
      DOUBLE PRECISION VOLINI(NSICLM)

!
!>    Total mass of sediment of each class
!
      DOUBLE PRECISION MASSTOT(NSICLM)
!
!>    Initial mass in active layer of sediment of each class
!
      DOUBLE PRECISION MASS0ACT(NSICLM)
!
!>    Initial total mass of sediment of each class
!
      DOUBLE PRECISION MASS0TOT(NSICLM)
!
!>    Cumulated evolution mass per class
!
      DOUBLE PRECISION SUMRMASCL(NSICLM)
!
!>    Cumulated erosion / deposition mass per class
!
      DOUBLE PRECISION SUMMCUMUCL(NSICLM)
!
!>    Sediment mass from Nestor per class per time step
!
      DOUBLE PRECISION MASSNESTOR(NSICLM)
!
!>    Cumulated sediment mass from Nestor per class
!
      DOUBLE PRECISION SUMMASSNESTOR(NSICLM)
!
!>    Cumulated sediment mass erosion
!
      DOUBLE PRECISION SUM_EROSION(NSICLM)
!
!>    Cumulated sediment mass deposition
!
      DOUBLE PRECISION SUM_DEPOSITION(NSICLM)
!
!>    Ratio between skin friction and mean diameter
!
      DOUBLE PRECISION, TARGET :: KSPRATIO
!
!>    Karim, Holly & Yang constant for hiding (with
!!    multiclass beds)
!
      DOUBLE PRECISION :: KARIM_HOLLY_YANG
!
!>    Karman constant
!
      DOUBLE PRECISION :: KARMAN
!
!>    Maximum concentration used in the equilibrium
!!    concentration calculation (for suspension)
!
      DOUBLE PRECISION :: CMAX
!
!>    Pi
!
      DOUBLE PRECISION :: PI
!
!>    Meyer Peter Mueller-Coefficient
!
      DOUBLE PRECISION, TARGET :: MPM
!
!>    Secondary Current Alpha Coefficient
!
      DOUBLE PRECISION, TARGET :: ALPHA
!
!>    Morphological factor on the hydrodynamics: distorts
!!    the evolution of the hydrodynamics with respect to
!!    the morphodynamics: the end time of the simulation is
!!    going to be MOFAC times the end time asked for by the
!!    user. One hydrodynamics step is done every MOFAC
!!    morphodynamic step. Its use is recommended in fluvial
!!    cases to decrease computational times. Beware that it
!!    requires to distort the hydrodynamics forcing
!!    (for example the hydrograph at the entrance of
!!    the domain) with the same factor - this is up to the user.
!!    Beware with the use of suspension because erosion/
!!    deposition is distorted but not the bedload...
!
      DOUBLE PRECISION :: MOFAC
!
!>    Morphological factor on the bed: distorts the
!!    evolution of the morphodynamics with respect to the
!!    hydrodynamics: the morphodynamics time step size is
!!    MOFAC times the hydrodynamics time step size.
!!    Its use is recommended in maritime cases to decrease
!!    computational times. It is compatible with the use of
!!    suspension
      DOUBLE PRECISION :: MOFAC_BED
!
!>    Parameter used for clipping variables or testing
!!    values against zero
!
      DOUBLE PRECISION :: ZERO
!
!>    B value for the Bijker formula
!
      DOUBLE PRECISION :: BIJK
!
!>    Prescribed solid discharges
!
      DOUBLE PRECISION, ALLOCATABLE :: SOLDIS(:)
!
!>    For mass balance of cohesive sediment
!
      DOUBLE PRECISION :: MASBED0,MASBED
!
!>    Minimum depth for bedload
!
      DOUBLE PRECISION :: HMIN_BEDLOAD
!
!>    Upwinding for Exner FV
!
      DOUBLE PRECISION :: DVF
!
!>    Flocculation coefficient
!
      DOUBLE PRECISION :: TURBA
!
!>    Coefficient for floc destruction
!
      DOUBLE PRECISION :: TURBB
!
!>    Weak soil concentration for mud
!
      DOUBLE PRECISION :: CGEL
!
!>    Threshold concentration for hindered settling
!
      DOUBLE PRECISION :: CINI
!
!>    sand fraction for Wilcock & Crowe transport formula
!
      DOUBLE PRECISION, ALLOCATABLE :: SANFRA(:)
!
!>    Initial values of suspended sediments
!
      DOUBLE PRECISION, ALLOCATABLE :: SED0(:)
!
!>    Prescribed values of suspended sediments
!
      DOUBLE PRECISION, ALLOCATABLE :: PRESED(:)
!
!>    Values of suspended sediments at the sources
!
      DOUBLE PRECISION, ALLOCATABLE :: SEDSCE(:,:)
!
!>    Coefficient for diffusion of suspended sediments in 2D
!
      DOUBLE PRECISION :: DIFSEDNU
!
!>    Coefficient for horizontal diffusion of suspended sediments in 3D
!
      DOUBLE PRECISION, ALLOCATABLE :: DNUSEDH(:)
!
!>    Coefficient for vertical diffusion of suspended sediments in 3D
!
      DOUBLE PRECISION, ALLOCATABLE :: DNUSEDV(:)
!
!-----------------------------------------------------------------------
!
!       7) STRINGS
!
!-----------------------------------------------------------------------
!>    Title of the case
!
      CHARACTER(LEN=72) TITCA
!
!>    List of the variable to ouput in the result file
!
      CHARACTER(LEN=72) SORTIS
!
!>    List of the variable to print to the listing
!
      CHARACTER(LEN=72) VARIM
!
!>    For clandestine variables
!
      CHARACTER(LEN=32) VARCLA(NSICLM)
!
!>    Name of output variable
!
      CHARACTER(LEN=32) TEXTE(MAXVAR)
!
!>    Name of variable in previous computation file
!
      CHARACTER(LEN=32) TEXTPR(MAXVAR)
!
!>    Names of private arrays (given by user)
!
      CHARACTER(LEN=32) NAMES_PRIVE(4)
!
!>    Names of differenting arrays (given by user)
!
      CHARACTER(LEN=32) NAMES_ADVAR(MAXVAR)
!
!>    Equation solved
!
      CHARACTER(LEN=20) EQUA
!
!>    Mnemo of variables for graphic printouts (b for bottom, etc.)
!
      CHARACTER(LEN=8) MNEMO(MAXVAR)
!
!-----------------------------------------------------------------------
!
!       8) SLVCFG STRUCTURES
!
!-----------------------------------------------------------------------
!
!>    Solver for the diffusion of sediment
!
      TYPE(SLVCFG), ALLOCATABLE :: SLVSED(:)
!
!-----------------------------------------------------------------------
!
!       9) MESH STRUCTURE
!
!-----------------------------------------------------------------------
!
!>    Mesh structure
!
      TYPE(BIEF_MESH),TARGET :: MESH
!
!-----------------------------------------------------------------------
!
!      10) ALIASES
!
!-----------------------------------------------------------------------
!
!     DECLARATION OF POINTERS FOR ALIASES
!     TARGETS ARE DEFINED IN POINT_TELEMAC2D
!
!>    Aliases for work vectors in tb
!
      TYPE(BIEF_OBJ),POINTER :: T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12
      TYPE(BIEF_OBJ),POINTER :: T13,T14
!
!
!     USEFUL COMPONENTS IN STRUCTURE MESH
!
!
!>    Connectivity table
!
      TYPE(BIEF_OBJ),   POINTER :: IKLE
!
!>    2d coordinates of the mesh
!
      DOUBLE PRECISION, DIMENSION(:), POINTER :: X,Y
!
!>    Number of elements in the mesh
!
      INTEGER, POINTER:: NELEM
!
!>    Maximum number of elements in the mesh
!
      INTEGER, POINTER:: NELMAX
!
!>    Number of boundary points
!
      INTEGER, POINTER:: NPTFR
!
!>    Maximum number number of boundary points
!
      INTEGER, POINTER:: NPTFRX
!
!>    Type of element
!
      INTEGER, POINTER:: TYPELM
!
!>    Number of 2d points in the mesh
!
      INTEGER, POINTER:: NPOIN
!
!>    Maximum number of 2d points in the mesh
!
      INTEGER, POINTER:: NPMAX
!
!>    Maximum number of neighbouring points
!
      INTEGER, POINTER:: MXPTVS
!
!>    Maximum number of neighbouring elemnts
!
      INTEGER, POINTER:: MXELVS
!
!>    Vector length (for vectorisation) from mesh object
!
      INTEGER, POINTER:: LV
!
!>    Save the velocity fields in the suspension computation
!
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVE_UCONV,SAVE_VCONV
!
!>    Save the water depth in the suspension computation
!
      TYPE(BIEF_OBJ), POINTER :: HOLD
!
!-----------------------------------------------------------------------
!
!      11) GAIA FILES + INTEGER DECLARATION FOR MED APPROACH
!
!-----------------------------------------------------------------------
!
!>    Maximum rank of logical units as declared in submit strings in the dictionary
!
      INTEGER, PARAMETER :: MAXLU_GAI = 46
!
!>    For storing information on files
!
      TYPE(BIEF_FILE), TARGET :: GAI_FILES(MAXLU_GAI)
!
!>    Various files ranks, which are also logical units if no coupling
!
      INTEGER, TARGET :: GAIRES,GAIREF,GAIPRE
      INTEGER, TARGET :: GAICOU,GAIGEO,GAICLI,GAICAS
      INTEGER GAIFON,GAISEC,GAISEO,GAILIQ,GAIFLX
      INTEGER SINACT, SINPOL, SINREF, SINRST    ! nestor
      INTEGER VSPRES

!-----------------------------------------------------------------------
!
!      12) SECTIONS
!
!-----------------------------------------------------------------------
!
      TYPE (CHAIN_TYPE), ALLOCATABLE :: CHAIN(:)
!
!     SAVED VARIABLE
!
      ! fluxpr_gaia
      DOUBLE PRECISION, ALLOCATABLE :: WORK(:),WORKB(:)
      LOGICAL :: INIT_FLUXPR=.TRUE.
      ! READ_FIC_CONC_GAIA
      INTEGER, PARAMETER :: MAXVAL_RFC=50
      INTEGER NVALUE_RFC,NLIG_RFC,IL1_RFC,IL2_RFC
      DOUBLE PRECISION TL1_RFC,TL2_RFC,LASTAT_RFC
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: INFIC_RFC
      DOUBLE PRECISION, DIMENSION(:)  , ALLOCATABLE :: TIME_RFC
      CHARACTER(LEN=8) CHOIX_RFC(MAXVAL_RFC),LASTWHAT_RFC
      LOGICAL :: DEJA_RFC = .FALSE.
      !LEO new variable for node_Depth of gaia
      DOUBLE PRECISION, ALLOCATABLE, TARGET :: TDS_GAI_NODE_DEPTH(:)
      DOUBLE PRECISION, ALLOCATABLE,
     &         TARGET :: TDS_NODE_SEDIMENT_FRACTION(:,:)
      DOUBLE PRECISION, ALLOCATABLE, TARGET :: TDS_HN(:)
      ! gaia
      LOGICAL :: PASS
      INTEGER :: NIT, VALNIT !
!     CURRENT TIME
!
      DOUBLE PRECISION :: AT0
! flusec_gaia
      LOGICAL :: DEJA_FLUSEC = .FALSE.
      INTEGER, ALLOCATABLE :: NSEG(:),LISTE(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: VOLNEGS(:),VOLPOSS(:)
      DOUBLE PRECISION, ALLOCATABLE :: VOLNEGC(:),VOLPOSC(:)
      DOUBLE PRECISION, ALLOCATABLE :: FLX(:),VOLNEG(:),VOLPOS(:)
      DOUBLE PRECISION, ALLOCATABLE :: FLXS(:),FLXC(:)
      LOGICAL :: OLD_METHOD_FLUSEC=.FALSE.
      ! flusec_gai
      TYPE FLUXLINE
        INTEGER, ALLOCATABLE :: SECTIONIDS(:)
        INTEGER, ALLOCATABLE :: DIRECTION(:)
        INTEGER              :: NOFSECTIONS
      END TYPE FLUXLINE
!
      LOGICAL :: DEJA_FLUSEC2 = .FALSE.
      TYPE(FLUXLINE), ALLOCATABLE :: FLUXLINEDATA_FLUSEC2(:)
      DOUBLE PRECISION, ALLOCATABLE :: FLUX_FLUSEC2(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: VOLFLUX_FLUSEC2(:,:)
      INTEGER :: NUMBEROFLINES_FLUSEC2
      DOUBLE PRECISION TIME_FLUSEC2
!
!-----------------------------------------------------------------------
!
!      13)HERE WE DECLARE HERE EXTRA DATASETUP NEEDED FOR ARTELIA
!      BED MODEL: TO TIDY UP LATER
!
!-----------------------------------------------------------------------
!
!
!>    Bed model (3 choices, cf. dico)
!
      INTEGER :: BED_MODEL
!
!>    Hirano model used
!
      LOGICAL :: HIRANO
!
!>    Total number of sand
!
      INTEGER :: NSAND
!
!>    Total number of muds
!
      INTEGER :: NMUD
!
!>    Type of sediment (co or nco)
!
      CHARACTER(LEN=3), ALLOCATABLE :: TYPE_SED(:)
!
!>    Tables to switch from mud number to class number and
!!    from sand number to class number
!
      INTEGER, ALLOCATABLE :: NUM_IMUD_ICLA(:),NUM_ISAND_ICLA(:)
!
!>    Tables to switch from class number to mud number and
!!    from class number to sand number
!
      INTEGER, ALLOCATABLE :: NUM_ICLA_IMUD(:),NUM_ICLA_ISAND(:)
!
!>    Tables to switch from suspended sediment number to class number
!
      INTEGER, ALLOCATABLE :: NUM_ISUSP_ICLA(:)
!
!>    Minimum value to detect sediment mass
!
      DOUBLE PRECISION, PARAMETER :: MIN_SED_MASS_COMP = 1.D-9
!
!>    Thickness of each bed layer (constant)
!
      DOUBLE PRECISION SED_THICK(NLAYMAX)
!
!>    Mud concentration for each bed layer (constant)
!
      DOUBLE PRECISION CONC_MUD0(NLAYMAX)
!
!>    Mud concentration for each bed layer, at each point.
!!    It is variable in space for the active layer (layer 1)
!!    only. It is a choice related to the way the bed model
!!    is done: the active layer is treated along with the
!!    others, there are no specialised arrays for the active
!!    layer.
!
      DOUBLE PRECISION,TARGET,ALLOCATABLE::CONC_MUD(:,:)
!
!>    Critical erosion shear stress of the mud per layer
!
      DOUBLE PRECISION TOCE_MUD0(NLAYMAX)
!
!>    Critical erosion shear stress of the mud, for each bed layer,
!!    for each point.
!!    It is variable in space for the active layer (layer 1)
!!    only. It is a choice related to the way the bed model
!!    is done: the active layer is treated along with the
!!    others, there are no specialised arrays for the active
!!    layer.
!
      DOUBLE PRECISION,TARGET,ALLOCATABLE :: TOCE_MUD(:,:)
!
!>    Critical shear stress for mud deposition
!
      DOUBLE PRECISION TOCD_MUD0(NSICLM)
!
!>    Critical shear stress for mud deposition, for each bed layer,
!!    for each point
!
      DOUBLE PRECISION, ALLOCATABLE :: TOCD_MUD(:,:)
!
!>    Partheniades erosion coefficient: depends on the type of erosion
!!    so it actually varies on the sediment column
!
      DOUBLE PRECISION PARTHENIADES0(NLAYMAX)
!
!>    Partheniades erosion coefficient, for each bed layer, for each point.
!!    It is variable in space for the active layer (layer 1)
!!    only. It is a choice related to the way the bed model
!!    is done: the active layer is treated along with the
!!    others, there are no specialised arrays for the active
!!    layer.
!
      DOUBLE PRECISION,TARGET,ALLOCATABLE :: PARTHENIADES(:,:)
!
!>    Mass transfer for consolidation between layers
!
      DOUBLE PRECISION TRANS_MASS0(NLAYMAX)
!
!>    Mass transfer for consolidation between layers, for each bed
!!    layer, for each point.
!!    It is variable in space for the active layer (layer 1)
!!    only. It is a choice related to the way the bed model
!!    is done: the active layer is treated along with the
!!    others, there are no specialised arrays for the active
!!    layer.
!
      DOUBLE PRECISION,TARGET,ALLOCATABLE :: TRANS_MASS(:,:)
!
!>    Critical erosion shear stress of the sand
!
      DOUBLE PRECISION TOCE_SAND0(NSICLM)
!
!>    Critical erosion shear stress of the sand, for each sand,
!!    for each point
!
      DOUBLE PRECISION, ALLOCATABLE :: TOCE_SAND(:,:)
!
!>    Ratio between critical shear stress of pure sediment and mixed
!!    sediment in the same layer
!
      TYPE(BIEF_OBJ), TARGET :: RATIO_TOCE
!
!>    Concentation of mud in active layer
!!    (array defined for temporary work in some subroutines)
!
      DOUBLE PRECISION, ALLOCATABLE ::  CONC_MUD_ACTIV_TEMPO(:)
!
!>    Mass of mud in active layer (array defined for temporary work in some subroutines)
!
      DOUBLE PRECISION, ALLOCATABLE ::  MASS_MUD_ACTIV_TEMPO(:)
!
!>    Mass of sand in active layer (array defined for temporary work in some subroutines)
!
      DOUBLE PRECISION, ALLOCATABLE ::  MASS_SAND_ACTIVE_LAYER(:)
!
!>    Contains "hidden" sand mass if more than 30% of mud (because no
!!    bedload for sand in that case)
!
      DOUBLE PRECISION, ALLOCATABLE ::  MASS_SAND_MASKED(:)
!
!>    Evolution of total (sum from each class) mass of sand for each point
!
      DOUBLE PRECISION, ALLOCATABLE ::  EVCL_M_TOT_SAND(:)
!
!>    Ratio between EVCL_M_TOT_SAND and the mass in the active layer
!
      DOUBLE PRECISION, ALLOCATABLE ::  RATIO_EVOL_TOT_SAND(:)
!
!>    Flux of mud transfert inbetween layers
!
      DOUBLE PRECISION, ALLOCATABLE ::  FLUX_MASS_MUD(:,:,:)
!
!>    Same as FLUX_MASS_MUD but summed on the mud layer
!
      DOUBLE PRECISION,ALLOCATABLE  ::  FLUX_MASS_MUD_TOT(:,:)
!
!>    Flux of sand transfert inbetween layers
!
      DOUBLE PRECISION, ALLOCATABLE ::  FLUX_MASS_SAND(:,:,:)
!
!>    Layer of mud of active layer into which mud will be added
!
      INTEGER, ALLOCATABLE ::  NUM_TRANSF(:)
!
!>    Negative mud transfer to apply in case we have an active layer
!
      DOUBLE PRECISION, ALLOCATABLE :: FLUX_NEG_MUD_ACTIV_LAYER(:,:)
!
!>    Negative sand transfer to apply in case we have an active layer
!
      DOUBLE PRECISION, ALLOCATABLE :: FLUX_NEG_SAND_ACTIV_LAYER(:,:)
!
!>    Positive mud transfer to apply in case we have an active layer
!
      DOUBLE PRECISION, ALLOCATABLE :: FLUX_POS_MUD_ACTIV_LAYER(:,:,:)
!
!>    Positive sand transfer to apply in case we have an active layer
!
      DOUBLE PRECISION, ALLOCATABLE :: FLUX_POS_SAND_ACTIV_LAYER(:,:,:)
!
!>    Options for the vertical advection-diffusion scheme with settling
!!    velocity (only relevant in 3D). This could be declared in
!!    TELEMAC3D instead, to be discussed (maybe this concerns other
!!    variables).
!
      INTEGER :: SETDEP
!
!>    Variables to read if computation is continued
!!    --------------------------------
!!    0 : DISCARD
!!    1 : READ  (SEE SUBROUTINE NOMVAR)
!!
!!    BED EVOLUTION
      INTEGER :: ALIRE(MAXVAR) =
     &         (/ (0,KKK=1,9),(0,KKK=10,MAXVAR) /)
!
!     WAVES ONLY
      INTEGER :: ALIRH(MAXVAR) =
     &         (/ (0,KKK=1,10),(1,KKK=11,13),(0,KKK=14,MAXVAR) /)
!
!>    For validation, each variable in the file is compared
!
      INTEGER :: ALIRV(MAXVAR) = (/ (1,KKK=1,MAXVAR) /)

      SAVE
      END MODULE DECLARATIONS_GAIA
