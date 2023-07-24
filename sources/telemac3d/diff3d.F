!                   *****************
                    SUBROUTINE DIFF3D
!                   *****************
!
     &(FD,FC,FN,VISCF,SIGMAF,S0F,YAS0F,S1F,YAS1F,
     & FBORL,FBORF,FBORS,AFBORL,AFBORF,AFBORS,
     & BFBORL,BFBORF,BFBORS,LIFBOF,LIFBOL,LIFBOS,
     & FMIN,CLIMIN,FMAX,CLIMAX,SCHCF,SCHDF,SLVDIF,TRBAF,INFO,
     & NEWDIF,DT,T2_01,T2_03,T3_01,T3_02,T3_03,T3_04,
     & NPOIN2,NPOIN3,INCHYD,SEM3D,YASEM3D,IT1,NPTFR3,NBOR3,MASKPT,
     & TRAV3,MESH2D,MESH3D,MTRA1,MTRA2,IELM3,MSUPG,IELM2H,IELM2V,
     & MDIFF,MASKBR,SVIDE,MSK,MASKEL,
     & NPLAN,OPTBAN,TETADI,YAWCHU,WCHU,S3D_WCHU,AGGLOD,VOLU,
     & YASCE,NSCE,FSCE,SOURCES,TETASUPG,RAIN,PLUIE,TRAIN,
     & SIGMAG,IPBOT,S3D_SETDEP,OPTSOU,SETDEP)
!
!***********************************************************************
! TELEMAC3D   V8P4
!***********************************************************************
!
!brief    SOLVES THE DIFFUSION AND SUPG ADVECTION STEPS
!+               (IF REQUIRED).
!
!warning  Two WARNING in text below propose alternative implementation
!+        in the case of crushed planes. Not enough test cases yet to
!+        decide.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  JMH
!+        14/12/2009
!+
!+   DIRICHLET POINTS ON THE BOTTOM
!
!history  J.M. HERVOUET (LNHE)
!+        18/03/2010
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
!history  J.M. HERVOUET (LNHE)
!+        09/04/2012
!+        V6P2
!+   Void volumes tested up to free point (which may be also crushed
!+   in case of tidal flats).
!
!history  J-M HERVOUET (LNHE)
!+        23/04/2012
!+        V6P2
!+   Values of tracers in rain taken into account.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        24/12/2013
!+        V7P0
!+   With SUPG, upwinding of time derivative now done.
!
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        29/04/2014
!+        V7P0
!+   Argument S3D_SETDEPadded. New option S3D_SETDEP= 2 corresponds to a
!+   vertical advection of sediment done by SED_FALL (based on the
!+   forward method of characteristics in weak form).
!
!history  J.M. HERVOUET (EDF LAB, LNHE)
!+        04/06/2014
!+        V7P0
!+   Implicit upwind treatment of settling velocity modified. Now a
!+   single matrix is called (MATWC) and upwind is not called.
!+   Positivity is ensured without any condition, and stability
!+   if the solver can resist to non dominant diagonals...
!+   Call to t3d_stress changed (for treatment of tidal flats).
!
!history  J.M. HERVOUET (EDF LAB, LNHE)
!+        16/06/2015
!+        V7P1
!+   Changing %FAC%R into %IFAC%I.
!
!history  A. JOLY (EDF LAB, LNHE)
!+        27/08/2015
!+        V7P1
!+   Imposed flowrates on the bed.
!
!history  A. LEROY (EDF LAB, LNHE)
!+        28/08/2015
!+        V7P1
!+   Add the option OPTSOU to treat sources as a dirac (OPTSOU=2) or
!+   not (OPTSOU=1).
!
!history  J.M. HERVOUET (EDF LAB, LNHE)
!+        15/09/2016
!+        V7P2
!+   In case RAIN is treated here (case of advection and SUPG), clipping
!+   of the matrix diagonal to avoid negative values.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AFBORF         |-->| LOGARITHMIC LAW FOR COMPONENT ON THE BOTTOM:
!|                |   |  NU*DF/DN = AFBORF*U + BFBORF
!| AFBORL         |-->| LOGARITHMIC LAW FOR COMPONENT ON THE
!|                |   | LATERAL BOUNDARIES:
!|                |   | NU*DF/DN = AFBORL*U + BFBORL
!| AFBORS         |-->| LOGARITHMIC LAW FOR COMPONENT AT THE SURFACE:
!|                |   | NU*DF/DN = AFBORS*U + BFBORS
!| AGGLOD         |-->| MASS-LUMPING IN DIFFUSION
!| BFBORF         |-->| LOGARITHMIC LAW FOR COMPONENT ON THE BOTTOM:
!|                |   |  NU*DF/DN = AFBORF*U + BFBORF
!| BFBORL         |-->| LOGARITHMIC LAW FOR COMPONENT ON THE
!|                |   | LATERAL BOUNDARIES:
!|                |   | NU*DF/DN = AFBORL*U + BFBORL
!| BFBORS         |-->| LOGARITHMIC LAW FOR COMPONENT AT THE SURFACE:
!|                |   | NU*DF/DN = AFBORS*U + BFBORS
!| CLIMAX         |-->| LOGICAL FOR CLIPPING (MAX VALUE)
!| DT             |-->| TIME STEP
!| FBORF          |<->| DIRICHLET CONDITIONS ON F AT THE BOTTOM
!| FBORL          |<->| DIRICHLET CONDITIONS ON F ON LATERAL BOUNDARIES
!| FBORS          |<->| DIRICHLET CONDITIONS ON F AT THE SURFACE
!| FC             |<->| VARIABLE AFTER CONVECTION
!| FD             |<->| VARIABLE AFTER DIFFUSION
!| FMAX           |-->| MAX CLIPPING VALUE
!| FMIN           |-->| MIN CLIPPING VALUE
!| FN             |<->| VARIABLE F AT TIME N
!| FSCE           |-->| SOURCE TERM OF F
!| IELM2H         |-->| DISCRETISATION TYPE FOR 2D HORIZONTAL MESH
!| IELM2V         |-->| DISCRETISATION TYPE FOR 2D VERTICAL MESH
!| IELM3          |-->| DISCRETISATION TYPE FOR 3D
!| INCHYD         |-->| IF YES, HYDROSTATIC INCONSISTENCY FILTER
!| INFO           |-->| INFORMATIONS FOR SOLVERS
!| IPBOT          |-->| PLANE NUMBER OF LAST CRUSHED PLANE (0 IF NONE)
!| IT1            |<->| BIEF_OBJ STRUCTURES FOR INTEGER ARRAYS
!| LIFBOF         |<->| TYPE OF BOUNDARY CONDITIONS AT THE BOTTOM
!| LIFBOL         |<->| TYPE OF BOUNDARY CONDITIONS ON LATERAL BOUNDARIES
!| LIFBOS         |<->| TYPE OF BOUNDARY CONDITIONS AT THE SURFACE
!| MASKBR         |-->| 3D MASK ON LATERAL BOUNDARIES
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| MDIFF          |<->| DIFFUSION MATRIX
!| MESH2D         |<->| 2D MESH
!| MESH3D         |<->| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| MSUPG          |<->| NON SYMMETRIC SUPG MATRIX
!| MTRA1          |<->| 3D WORK MATRIX
!| MTRA2          |<->| 3D WORK MATRIX
!| NBOR3          |-->| GLOBAL NUMBER OF 3D BOUNDARY POINTS
!| NEWDIF         |-->| RECALCULATE OR NOT DIFFUSION MATRIX
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| NPTFR3         |-->| NUMBER OF LATERAL BOUNDARY POINTS IN 3D
!| NSCE           |-->| NUMBER OF GIVEN POINTS FOR SOURCES
!| OPTBAN         |-->| OPTION FOR TIDAL FLATS, IF 1, FREE SURFACE
!|                |   | MODIFIED AND PIECE-WISE LINEAR
!| PLUIE          |-->| RAIN IN M/S MULTIPLIED BY VOLU2D
!| RAIN           |-->| IF YES, THERE IS RAIN OR EVAPORATION
!| S0F            |<->| EXPLICIT SOURCE TERM (DIM=F/T)
!| S1F            |<->| IMPLICIT SOURCE TERM (DIM=1/T)
!| S3D_SETDEP     |-->| ADVECTION-DIFFUSION SCHEME WITH SETTLING VELOCITY
!|                |   | DECLARED IN T3D
!|                |   | 0 : IMPLICIT TREATED IN DIFFUSION
!|                |   | 1 : EXPLICIT TREATED IN ADVECTION
!| S3D_WCHU       |-->| VELOCITY (POSITIVE IN 6.3 IF SEDIMENT SETTLING
!|                |   | VELOCITY AND SEDI3D)
!| SCHCF          |-->| ADVECTION SCHEME OF F
!| SCHDF          |-->| DIFFUSION SCHEME OF F
!| SEM3D          |<->| SECOND MEMBERS (RIGHT HAND SIDE)
!|                |   | FOR THE LINEAR EQUATIONS 3D
!| SETDEP         |-->| ADVECTION-DIFFUSION SCHEME WITH SETTLING VELOCITY
!|                |   | DECLARED IN GAIA
!|                |   | 0 : IMPLICIT TREATED IN DIFFUSION
!|                |   | 1 : EXPLICIT TREATED IN ADVECTION
!| SIGMAF         |-->| COEFFICIENT OF VISCOSITY REDUCTION
!|                |   | ONLY USED FOR K AND EPSILON
!| SIGMAG         |-->| LOGICAL FOR GENERALISED SIGMA TRANSFORMATION
!| SLVDIF         |-->| SOLVER FOR DIFFUSION OF VELOCITIES
!| SOURCES        |-->| RIGHT HAND SIDE OF CONTINUITY EQUATION WHEN SOURCES
!| SVIDE          |-->| VOID STRUCTURE
!| T2_01          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T2_03          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T3_02          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T3_03          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T3_04          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| TETADI         |<->| IMPLICITATION COEFFICIENT OF DIFFUSION
!| TETASUPG       |-->| IMPLICITATION COEFFICIENT FOR SUPG
!| TRAIN          |-->| VALUE OF TRACER IN RAIN
!| TRAV3          |<->| 3D WORK ARRAYS
!| TRBAF          |-->| TREATMENT ON TIDAL FLATS FOR F
!| VISCF          |<->| VISCOSITY COEFFICIENTS
!|                |   | VISCF(*,1 OU 2) HORIZONTAL VISCOSITY
!|                |   | VISCF(*,3)      VERTICAL VISCOSITY
!| VOLU           |-->| VOLUME AROUND POINTS AT TIME N+1
!| WCHU           |-->| VELOCITY (POSITIVE IN 6.3 IF SEDIMENT SETTLING
!|                |   | VELOCITY AND GAIA)
!| YAS0F          |-->| LOGICAL TO TAKE INTO ACCOUNT S0F TERM IN DIFF3D
!| YAS1F          |-->| LOGICAL TO TAKE INTO ACCOUNT S1F TERM IN DIFF3D
!| YASCE          |-->| IF TRUE, THERE IS SOURCE
!| YASEM3D        |-->| IF TRUE, RIGHT HAND SIDE HAS BEEN PARTLY
!|                |   | COMPUTED BEFORE CALLING DIFF3D
!| YAWCHU         |-->| LOGICAL TO TAKE INTO ACCOUNT WCHU FOR SEDIMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE INTERFACE_TELEMAC3D, EX_DIFF3D => DIFF3D
!
!     PROVISOIRE
!
      USE DECLARATIONS_TELEMAC3D, ONLY:UCONV,VCONV,WSCONV,DM1,ZCONV
      USE DECLARATIONS_TELEMAC3D, ONLY:BEDBOU,BEDFLU,ISCE,KSCE
!
      USE DECLARATIONS_WAQTEL, ONLY : VITCHU_WAQ,WAQPROCESS
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN)             :: SCHCF,SCHDF,TRBAF,NSCE
      INTEGER, INTENT(IN)             :: NPOIN2,S3D_SETDEP,OPTSOU
      INTEGER, INTENT(IN)             :: SETDEP
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FD,FC,FN,S0F,S1F,VISCF
      TYPE(BIEF_OBJ), INTENT(IN)      :: LIFBOL,LIFBOF,LIFBOS
      TYPE(BIEF_OBJ), INTENT(IN)      :: FBORL,FBORF,FBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: AFBORL,AFBORF,AFBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: BFBORL,BFBORF,BFBORS,WCHU
      TYPE(BIEF_OBJ), INTENT(IN)      :: S3D_WCHU
      DOUBLE PRECISION, INTENT(IN)    :: SIGMAF,FMIN,FMAX,DT,AGGLOD
      DOUBLE PRECISION, INTENT(IN)    :: FSCE(NSCE),TETASUPG
      DOUBLE PRECISION, INTENT(INOUT) :: TETADI
      LOGICAL, INTENT(IN)             :: CLIMIN,CLIMAX,YASCE,YAS0F
      LOGICAL, INTENT(IN)             :: INFO,NEWDIF,YASEM3D,YAS1F
      LOGICAL, INTENT(IN)             :: SIGMAG
      TYPE(SLVCFG)                    :: SLVDIF
!
      LOGICAL, INTENT(IN)             :: INCHYD,MSK,YAWCHU,RAIN
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKPT,MASKBR,MASKEL,VOLU
      TYPE(BIEF_OBJ), INTENT(IN)      :: NBOR3,SVIDE,SOURCES
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T3_01,T3_02,T3_03,T3_04
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T2_01,T2_03
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3D,MESH2D
      INTEGER, INTENT(IN)             :: NPOIN3,IELM3,IELM2H,IELM2V
      INTEGER, INTENT(IN)             :: NPTFR3,NPLAN,OPTBAN
      INTEGER, INTENT(IN)             :: IPBOT(NPOIN2)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SEM3D,IT1,TRAV3,MTRA1,MTRA2
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: MSUPG,MDIFF
      DOUBLE PRECISION, INTENT(IN)    :: PLUIE(*),TRAIN
!
!-----------------------------------------------------------------------
!
      INTEGER IPOIN3,IPOIN2,IPTFR3,IS,IIS,I,I2D,NP
      DOUBLE PRECISION C,AGGLO,QTOT,VTOT,FTOT
      CHARACTER(LEN=16) FORMUL
      DOUBLE PRECISION, POINTER :: VOLUME(:)
!
!-----------------------------------------------------------------------
!
!=======================================================================
!   MASS MATRIX
!=======================================================================
!
!     COMPUTES MTRA2 : MASS MATRIX DIVIDED BY DT
!
      FORMUL='MATMAS          '
      CALL MATRIX(MTRA2, 'M=N     ', FORMUL, IELM3, IELM3, 1.D0/DT,
     &         SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE, MESH3D, MSK, MASKEL)
!
! MASS-LUMPING OF THE MASS MATRIX (FOR THE MOMENT ONLY IF
! EXPLICIT DIFFUSION OR 2 X IMPLICIT DIAGONAL)
! OF IF COMPATIBILITY WITH PSI SCHEME NEEDED (USE OF VOLU WHICH
! IS DIFFERENT FROM INTEGRAL OF TEST FUNCTIONS IF MASS-LUMPING
! IS DONE IN TELEMAC-2D)
!
      AGGLO=AGGLOD
      IF(TETADI.LT.0.001D0.OR.SCHCF.EQ.ADV_NSC.OR.SCHCF.EQ.ADV_PSI
     &                    .OR.SCHCF.EQ.ADV_LPO.OR.SCHCF.EQ.ADV_LPO_TF
     &                    .OR.SCHCF.EQ.ADV_NSC_TF) AGGLO=1.D0
      IF(AGGLO.GT.0.001D0) THEN
!       VOLU/DT REPLACES MTRA2 LUMPED
        CALL OS('X=CY    ',X=T3_01,Y=VOLU,C=AGGLO/DT)
!       IF(MSK) THERE IS A RISK THAT VOLU=0 SOMEWHERE
!               THIS MAY CAUSE PROBLEMS IN LINEAR SYSTEMS
        IF(MSK) CALL OS('X=+(Y,C)',X=T3_01,Y=T3_01,C=1.D-3)
!       CALL LUMP(T3_01,MTRA2,MESH3D,AGGLO)
        CALL OM('M=CM    ', M=MTRA2, C=1.D0-AGGLO, MESH=MESH3D)
        CALL OM('M=M+D   ', M=MTRA2, D=T3_01, MESH=MESH3D)
      ENDIF
!
!=======================================================================
!   CASES OF ADVECTION OF SETTLING VELOCITY WITH EXPLICIT SCHEME
!   FC IS AT THE BEGINNING THE RESULT OF ADVECTION WITHOUT SETTLING
!   VELOCITY, THEN THE EFFECT OF SETTLING VELOCITY IS ADDED
!=======================================================================
!
      IF(SETDEP.EQ.2) THEN
        CALL OS('X=Y     ',X=T3_02,Y=FC)
        CALL SED_FALL(FC,T3_02,WCHU,MESH3D,DT,VOLU,
     &                NPOIN2,NPOIN3,NPLAN,T3_01)
      ELSE IF(S3D_SETDEP.EQ.2) THEN
        CALL OS('X=Y     ',X=T3_02,Y=FC)
        CALL SED_FALL(FC,T3_02,S3D_WCHU,MESH3D,DT,VOLU,
     &                NPOIN2,NPOIN3,NPLAN,T3_01)
      ENDIF
!
!=======================================================================
!   SECOND MEMBER OF ADVECTION SCHEME + EXPLICIT SOURCE TERM
!=======================================================================
!
!   COMPUTES SEM3D = DT * S0F + FC
!
      IF(S0F%TYPR.NE.'0'.AND.YAS0F) THEN
        CALL OS('X=Y+CZ  ' , X=T3_04 , Y=FC , Z=S0F , C=DT )
      ELSE
        CALL OS('X=Y     ' , X=T3_04 , Y=FC )
      ENDIF
!
      IF(YASEM3D) THEN
!       HAS STARTED COMPUTING SEM3D IN TRISOU OR SOURCE
        CALL MATVEC ('X=X+CAY  ',SEM3D, MTRA2,T3_04,DT, MESH3D)
!                                                   DT BECAUSE MTRA2
!                                                   HAS A FACTOR 1/DT
      ELSE
        CALL MATVEC ('X=AY    ',SEM3D, MTRA2,T3_04, C, MESH3D)
      ENDIF
!
!=======================================================================
!
!     SUPG SCHEME: TEST OF UPWINDING OF THE TIME DERIVATIVE
!
      IF(SCHCF.EQ.ADV_SUP) THEN
!       HERE OPTSUP=2 ONLY
        CALL MATRIX(MTRA1,'M=TN    ','MATVGR 2        ',
     &              IELM3,IELM3,0.5D0,DM1,ZCONV,SVIDE,
     &              UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
!       COEFFICIENT 1/DT HAS ALREADY BEEN PUT IN MTRA1 (0.5=0.5*DT/DT)
        CALL OM('M=X(M)  ', M=MTRA2, MESH=MESH3D)
        CALL OM('M=M+N   ', M=MTRA2, N=MTRA1, MESH=MESH3D)
!       RIGHT HAND SIDE PART
!                                            =FN IN THIS OPTION
        CALL MATVEC ('X=X+CAY  ',SEM3D,MTRA1,FC,1.D0,MESH3D)
      ENDIF
!
!=======================================================================
!
!     SOURCES INSIDE THE DOMAIN
!
      IF(YASCE.AND.NSCE.GT.0) THEN
        DO IS=1,NSCE
          IF(OPTSOU.EQ.1) THEN
!          SOURCE NOT CONSIDERED AS A DIRAC
!          IF INTAKE FSCE=F, SO NO EXTRA TERM
            IIS=IS
!           IN PARALLEL MODE SOURCES WITHOUT PARCOM
            IF(NCSIZE.GT.1) IIS=IIS+NSCE
            DO I=1,NPOIN3
!             EXPLICIT SOURCE TERM
              SEM3D%R(I) = SEM3D%R(I)
     &                   + MAX(SOURCES%ADR(IIS)%P%R(I),0.D0)*
     &              (FSCE(IS)-(1.D0-TETASUPG)*FN%R(I))
!            IMPLICIT SOURCE TERM : SEE BELOW
            ENDDO
          ELSE IF(OPTSOU.EQ.2) THEN
!           SOURCE CONSIDERED AS A DIRAC
            IIS=1
!           IN PARALLEL MODE SOURCES WITHOUT PARCOM
            IF(NCSIZE.GT.1) IIS=2
            IF(ISCE(IS).GT.0) THEN
              I=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
!             EXPLICIT SOURCE TERM
              SEM3D%R(I) = SEM3D%R(I)
     &                   + MAX(SOURCES%ADR(IIS)%P%R(I),0.D0)*
     &              (FSCE(IS)-(1.D0-TETASUPG)*FN%R(I))
!            IMPLICIT SOURCE TERM : SEE BELOW
            ENDIF
          ENDIF
        ENDDO
!       BEDFLUXES
        IF(BEDBOU)THEN
          DO I=1,NPOIN2
!           EXPLICIT SOURCE TERM
            SEM3D%R(I) = SEM3D%R(I)
     &                 - MAX(BEDFLU%R(I),0.D0)*
     &                   (1.D0-TETASUPG)*FN%R(I)
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!
!     EXPLICIT ADVECTION TERM:
!
      IF(SCHCF.EQ.ADV_SUP) THEN
        CALL MATVEC('X=X+CAY ',SEM3D,MSUPG,FN,TETASUPG-1.D0,MESH3D)
      ENDIF
!
!=======================================================================
!   IMPLICIT SOURCE TERM (MASS-LUMPED AND ADDED TO THE DIAGONAL)
!=======================================================================
!
!     TODO: PROPOSITION (LUMPED IMPLICIT SOURCE TERM)
!                  BEWARE THE + SIGN: THE TERM S1F HAS A + SIGN WHEN
!                  TO THE LEFT OF THE = SIGN
!                 (CAREFUL: OPPOSITE IN CVDFTR)
!                  VOLU USED FOR MASS-CONSERVATION
!                 (COMPATIBILITY WITH CONTINUITY EQUATION)
      IF(S1F%TYPR.NE.'0'.AND.YAS1F) THEN
        CALL OS('X=YZ    ', X=T3_01, Y=S1F, Z=VOLU)
        CALL OM('M=M+D   ', M=MTRA2, D=T3_01, MESH=MESH3D)
      ENDIF
!
!=======================================================================
!
!     SOURCES INSIDE THE DOMAIN
!
      IF(YASCE.AND.NSCE.GT.0) THEN
        IF(OPTSOU.EQ.1) THEN
!         SOURCE NOT CONSIDERED AS A DIRAC
          DO IS=1,NSCE
!           IF INTAKE FSCE=T, SO NO EXTRA TERM
            IIS=IS
!           IN PARALLEL MODE SOURCES WITHOUT PARCOM
            IF(NCSIZE.GT.1) IIS=IIS+NSCE
            DO I=1,NPOIN3
!             IMPLICIT SOURCE TERM
              MTRA2%D%R(I)=MTRA2%D%R(I)+
     &                     MAX(SOURCES%ADR(IIS)%P%R(I),0.D0)*TETASUPG
            ENDDO
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
!         SOURCE CONSIDERED AS A DIRAC
          IIS=1
!         IN PARALLEL MODE SOURCES WITHOUT PARCOM
          IF(NCSIZE.GT.1) IIS=2
          DO I=1,NPOIN3
!           IMPLICIT SOURCE TERM
            MTRA2%D%R(I)=MTRA2%D%R(I)+
     &                   MAX(SOURCES%ADR(IIS)%P%R(I),0.D0)*TETASUPG
          ENDDO
        ENDIF
!
        IF(BEDBOU)THEN
          DO I=1,NPOIN2
!           IMPLICIT BEDFLUX TERM
            MTRA2%D%R(I)=MTRA2%D%R(I)+
     &                   MAX(BEDFLU%R(I),0.D0)*TETASUPG
          ENDDO
        ENDIF
      ENDIF
!
!     RAIN (ALL TRACERS) - IMPLICIT AND EXPLICIT PARTS
!
      IF(RAIN) THEN
        DO I=1,NPOIN2
          IPOIN3=NPOIN3-NPOIN2+I
!         WITH CLIPPING TO AVOID TOO SMALL DIAGONAL<0 WITH EVAPORATION
          IF(MTRA2%D%R(IPOIN3)+PLUIE(I)*TETASUPG.GT.1.D-6) THEN
            MTRA2%D%R(IPOIN3)=MTRA2%D%R(IPOIN3)+PLUIE(I)*TETASUPG
            SEM3D%R(IPOIN3)=SEM3D%R(IPOIN3)
     &                     -PLUIE(I)*(1.D0-TETASUPG)*FN%R(IPOIN3)
          ENDIF
!         RAIN (ALL TRACERS) - EXPLICIT PART
!         VALUE OF TRACER IN RAIN TAKEN INTO ACCOUNT ONLY IF RAIN POSITIVE
!         NOT IN CASE OF EVAPORATION, HENCE THE MAX(PLUIE,0)
!         THERE IS NO COEFFICIENT (1-TETASUPG) ON THIS TERM BECAUSE IT HAS
!         NO IMPLICIT PART
          SEM3D%R(IPOIN3)=SEM3D%R(IPOIN3)+MAX(PLUIE(I),0.D0)*TRAIN
        ENDDO
      ENDIF
!
!=======================================================================
!
!
!=======================================================================
!   DIFFUSION MATRIX + BOUNDARY TERMS
!=======================================================================
!
      IF(SCHDF.NE.0) THEN
!
        IF(INFO) THEN
          IF(SCHCF.EQ.ADV_SUP) THEN
            WRITE(LU,*) 'DIFFUSION OF ',FN%NAME,
     &                               ' WITH SUPG ADVECTION'
          ELSE
            WRITE(LU,*) 'DIFFUSION OF ',FN%NAME
          ENDIF
        ENDIF
!
        IF(NEWDIF) THEN
!         RELEASE 5.7
!         FORMUL='MATDIF          '
!         RELEASE 5.8 : MONOTONICITY ENSURED
          FORMUL='MATDIF       MON'
          IF(INCHYD) FORMUL(7:7)='2'
          CALL MATRIX(MDIFF,'M=N     ',FORMUL,IELM3,IELM3,1.D0,
     &                VISCF%ADR(1)%P,VISCF%ADR(2)%P,VISCF%ADR(3)%P,
     &                SVIDE,SVIDE,SVIDE,MESH3D,MSK,MASKEL)
        ENDIF
!
!=======================================================================
!
!       SEMI-IMPLICITATION OF THE DIFFUSION
!
        IF(TETADI.LT.0.999D0) THEN
!         IF(TETADI.GT.0.001) THEN
          CALL MATVEC ('X=X+CAY  ',SEM3D,MDIFF,FN,
     &                 (TETADI-1.D0)/SIGMAF,MESH3D)
        ENDIF
!       IF TETADI=0, NO NEED TO ADD MDIFF
        IF(TETADI.GT.0.001D0) THEN
          CALL OM('M=M+CN  ', M=MTRA2, N=MDIFF, C=TETADI/SIGMAF,
     &            MESH=MESH3D)
        ENDIF
!
! TAKES THE IMPLICIT BOUNDARY TERMS INTO ACCOUNT (FRICTION FOR EXAMPLE)
! ---------------------------------------------
!       BE CAREFUL! BFBOR* HAVE TO BE ADDED IN THE RHS (E.G. T3D_STRESS.F)
!       AS THE SAME WAY THAN AFBOR* IN THE MATRIX
!       USE OF MASVEC INSTEAD OF MASBAS COULD BE TESTED
!
!   LATERAL FACES : (MASKBR SET ACCORDING TO MASKEL IN MASK3D)
!  (MASS-LUMPED FORM)
!
        IF(AFBORL%TYPR.NE.'0') THEN
          CALL VECTOR(T3_02,'=','MASBAS          ',IELM2V,-1.D0,SVIDE,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,MSK,MASKEL)
          CALL OS('X=XY    ',X=T3_02,Y=AFBORL)
          CALL OSDB( 'X=X+Y   ' , MTRA2%D , T3_02 , T3_02, C , MESH3D )
        ENDIF
!
!   BOTTOM (MASS-LUMPED FORM AS IN 2D):
!
        IF(AFBORF%TYPR.NE.'0') THEN
          CALL VECTOR(T2_03, '=','MASBAS          ',IELM2H,-1.D0,SVIDE,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH2D,MSK,MASKEL)
          CALL OV('X=XY    ', X=T2_03%R, Y=AFBORF%R, DIM1=NPOIN2)
!         DRY ZONES OR CRUSHED ELEMENTS
!         SEE EQUIVALENT TREATMENT IN WAVE_EQUATION
          IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
            DO IPOIN2 = 1,NPOIN2
!             FLUXES ON THE BOTTOM MUST NOT BE TAKEN INTO ACCOUNT ON TIDAL FLATS
!             E.G. NO SETTLING VELOCITY IF NO WATER!
!             IF(IPBOT(IPOIN2).NE.NPLAN-1) THEN
!               IF NOT A TIDAL FLAT... WE CAN HOWEVER HAVE CRUSHED POINTS
!               SAME TREATMENT FROM PLANE 1 UP TO ACTUAL BOTTOM PLANE
!               THE CRUSHED POINTS ARE TREATED AS THE POINT ON ACTUAL BOTTOM PLANE ABOVE THEM
!               TODO: WARNING 1
!               THIS MAY NOT CORRECT FOR SEDIMENT, AS DEPOSITION WOULD BE TAKEN INTO ACCOUNT
!               SEVERAL TIMES. TO BE INVESTIGATED ON MORE TESTS.
!               THIS IS RATHER MEANT FOR VELOCITIES, TO STOP THEM ALL.
                DO NP=0,IPBOT(IPOIN2)
                  I=NP*NPOIN2+IPOIN2
                  MTRA2%D%R(I)=MTRA2%D%R(I)+T2_03%R(IPOIN2)
                ENDDO
!             ENDIF
!             PROPOSED ALTERNATIVE IMPLEMENTATION, ONLY FIRST FREE POINT TREATED
!             AND NOTHING DONE ON TIDAL FLATS.
!             IF(IPBOT(IPOIN2).NE.NPLAN-1) THEN
!               I=IPBOT(IPOIN2)*NPOIN2+IPOIN2
!               MTRA2%D%R(I)=MTRA2%D%R(I)+T2_03%R(IPOIN2)
!             ENDIF
            ENDDO
          ELSE
            DO IPOIN2 = 1,NPOIN2
              MTRA2%D%R(IPOIN2)=MTRA2%D%R(IPOIN2)+T2_03%R(IPOIN2)
            ENDDO
          ENDIF
        ENDIF
!
!       SURFACE (MASS-LUMPED FORM):
!
        IF(AFBORS%TYPR.NE.'0') THEN
          CALL VECTOR(T2_03, '=','MASBAS          ',IELM2H,-1.D0,SVIDE,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH2D,MSK,MASKEL)
          CALL OV('X=XY    ', X=T2_03%R, Y=AFBORS%R, DIM1=NPOIN2)
          DO I=1,NPOIN2
            MTRA2%D%R(NPOIN3-NPOIN2+I)=
     &      MTRA2%D%R(NPOIN3-NPOIN2+I)+T2_03%R(I)
          ENDDO
!         CALL OV('X=X+Y   ',X=MTRA2%D%R(NPOIN3-NPOIN2+1:NPOIN3),
!    &                       Y=T2_03%R, DIM1=NPOIN2)
        ENDIF
!
!       TAKES THE EXPLICIT BOUNDARY TERMS INTO ACCOUNT
!       ---------------------------------------------
!
        CALL T3D_STRESS(SEM3D,'X=X+Y   ',T2_01,T3_02,
     &                  BFBORL,BFBORF,BFBORS,NPOIN2,NPOIN3,MESH2D,
     &                  MESH3D,IELM2H,IELM2V,SVIDE,
     &                  MSK,MASKBR,MASKEL,IPBOT,SIGMAG,OPTBAN,NPLAN)
!
!=======================================================================
!   SEDIMENT-SPECIFIC ++++ This is for WC > 0 downwards
!                                D
!   THE MATRIX + PSI1(J) * WCHU * --( PSI2(I) ) IS ADDED
!                                DZ
!                                                     D          N+1
!   IT IS AN INTEGRATION BY PART OF TERM :  PSI2(I) * --( WCHU * C    )
!                                                     DZ
!
!   CORRESPONDING BOUNDARY TERMS ARE THE DEPOSITION
!   THIS TERM IS INCLUDED IN ATABOF !!!
!
!   NOTE: IT IS DONE IF AND ONLY IF SED. DIFFUSION IS REQUIRED !
!=======================================================================
!
        IF(YAWCHU.AND.SETDEP.EQ.0.AND.
     &    7*INT(WAQPROCESS/7).EQ.WAQPROCESS)THEN
          DO I=1,NPOIN3
            WCHU%R(I)=VITCHU_WAQ
          ENDDO
!         SEDIMENT DEPOSITION FOR MICROPOL CHANGE TO VITCHU_WAQ
          CALL MATRIX(MTRA1,'M=N     ','MATWC           ',
     &                IELM3,IELM3,1.D0,WCHU,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,MSK,MASKEL)
          CALL OM('M=X(M)  ', M=MTRA2, MESH=MESH3D)
          CALL OM('M=M+N   ', M=MTRA2, N=MTRA1, MESH=MESH3D)
!
        ELSEIF(YAWCHU.AND.SETDEP.EQ.0) THEN
!         FOR BOUNDARY TERMS, SEE SUBROUTINE FLUSED
!         NOTE: MATWC IS NOT PROGRAMMED WITH TETRAHEDRA...
          CALL MATRIX(MTRA1,'M=N     ','MATWC           ',
     &                IELM3,IELM3,1.D0,WCHU,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,MSK,MASKEL)
          CALL OM('M=X(M)  ', M=MTRA2, MESH=MESH3D)
          CALL OM('M=M+N   ', M=MTRA2, N=MTRA1, MESH=MESH3D)
        ELSE IF(YAWCHU.AND.S3D_SETDEP.EQ.0) THEN
!         FOR BOUNDARY TERMS, SEE SUBROUTINE FLUSED
!         NOTE: MATWC IS NOT PROGRAMMED WITH TETRAHEDRA...
          CALL MATRIX(MTRA1,'M=N     ','MATWC           ',
     &                IELM3,IELM3,1.D0,S3D_WCHU,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,MSK,MASKEL)
          CALL OM('M=X(M)  ', M=MTRA2, MESH=MESH3D)
          CALL OM('M=M+N   ', M=MTRA2, N=MTRA1, MESH=MESH3D)
        ENDIF
!
!=======================================================================
!   END OF DIFFUSION MATRIX + BOUNDARY TERMS
!=======================================================================
!
      ENDIF
!
!=======================================================================
!
!   ADDS SUPG MATRIX IF SCHCF=ADV_SUP
!
!=======================================================================
!
      IF(SCHCF.EQ.ADV_SUP) THEN
        IF(MTRA2%TYPEXT.EQ.'S') THEN
          CALL OM('M=X(M)  ',M=MTRA2, MESH=MESH3D)
        ENDIF
        CALL OM ('M=M+CN  ', M=MTRA2, N=MSUPG, C=TETASUPG, MESH=MESH3D)
      ENDIF
!
!=======================================================================
!
!   BOUNDARY CONDITIONS + PRECONDITIONING + SOLVER
!
!=======================================================================
!
!   BOUNDARY CONDITIONS FOR BOUNDARY POINTS (POINTS OF TYPE DIRICHLET)
!
      CALL CPSTVC(MTRA2%D,T3_03)
!
      DO IPOIN3 = 1,NPOIN3
        IT1%I(IPOIN3) = KDDL
        T3_03%R(IPOIN3) = 0.D0
      ENDDO
!
!   LATERAL BOUNDARY CONDITIONS
!
      DO IPTFR3 = 1,NPTFR3
        IF(LIFBOL%I(IPTFR3).EQ.KENT.OR.
     &    LIFBOL%I(IPTFR3).EQ.KENTU.OR.LIFBOL%I(IPTFR3).EQ.KADH) THEN
          IT1%I(NBOR3%I(IPTFR3)) = KDIR
          T3_03%R(NBOR3%I(IPTFR3)) = FBORL%R(IPTFR3)
        ENDIF
      ENDDO
!
!   BOTTOM AND SURFACE BOUNDARY CONDITIONS
!
      DO IPOIN2 = 1,NPOIN2
        IF(LIFBOF%I(IPOIN2).EQ.KENT.OR.LIFBOF%I(IPOIN2).EQ.KADH) THEN
          IT1%I(IPOIN2) = KDIR
          T3_03%R(IPOIN2) = FBORF%R(IPOIN2)
        ENDIF
        IF(LIFBOS%I(IPOIN2).EQ.KENT.OR.LIFBOS%I(IPOIN2).EQ.KADH) THEN
          IT1%I(NPOIN3-NPOIN2+IPOIN2) = KDIR
          T3_03%R(NPOIN3-NPOIN2+IPOIN2) = FBORS%R(IPOIN2)
        ENDIF
      ENDDO
!
!   CRUSHED POINTS AND TIDAL FLATS: FIRST TREATED AS DIRICHLET
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
        IF(NCSIZE.GT.1) THEN
!         ONLY DIFFERENCE : VALUE OF MTRA2 EQUAL TO FAC INSTEAD OF 1
          CALL OS('X=Y     ',X=T3_02,Y=VOLU)
!         T3_02 WILL BE THE ASSEMBLED VOLUME
          CALL PARCOM(T3_02,2,MESH3D)
          DO I2D=1,NPOIN2
            IF(IPBOT(I2D).GT.0) THEN
              IF(LIFBOF%I(I2D).EQ.KENT.OR.LIFBOF%I(I2D).EQ.KADH) THEN
!               DIRICHLET POINT ON THE BOTTOM: ALL POINTS UP TO THE
!               FIRST FREE ARE TREATED AS DIRICHLET WITH FBORF VALUE
                DO I=0,IPBOT(I2D)
                  IPOIN3 = I2D+I*NPOIN2
                  MTRA2%D%R(IPOIN3)=MESH3D%IFAC%I(IPOIN3)
                  IT1%I(IPOIN3)   = KDIR
                  T3_03%R(IPOIN3) = FBORF%R(I2D)
                ENDDO
              ELSE
!               POINTS WITH NO VOLUME PROVISIONALLY SET TO FN
!               TESTED UP TO FIRST FREE POINT (IPBOT+1),
!               WHICH SHOULD NOT HAVE VOLUME=0 EXCEPT ON TIDAL
!               FLATS BECAUSE IN THIS CASE IPBOT=NPLAN-1
!               DO I=0,IPBOT(I2D)-1
                DO I=0,IPBOT(I2D)
                  IPOIN3 = I2D+I*NPOIN2
                  IF(T3_02%R(IPOIN3).LT.1.D-10) THEN
                    MTRA2%D%R(IPOIN3)=MESH3D%IFAC%I(IPOIN3)
                    IT1%I(IPOIN3)   = KDIR
                    T3_03%R(IPOIN3) = FN%R(IPOIN3)
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
          ENDDO
        ELSE
          DO I2D=1,NPOIN2
            IF(IPBOT(I2D).GT.0) THEN
              IF(LIFBOF%I(I2D).EQ.KENT.OR.LIFBOF%I(I2D).EQ.KADH) THEN
!               DIRICHLET POINT ON THE BOTTOM: ALL POINTS UP TO THE
!               FIRST FREE ARE TREATED AS DIRICHLET WITH FBORF VALUE
                DO I=0,IPBOT(I2D)
                  IPOIN3 = I2D+I*NPOIN2
                  MTRA2%D%R(IPOIN3)=1.D0
                  IT1%I(IPOIN3) = KDIR
                  T3_03%R(IPOIN3) = FBORF%R(I2D)
                ENDDO
              ELSE
!               POINTS WITH NO VOLUME PROVISIONALLY SET TO FN
!               TESTED UP TO FIRST FREE POINT (IPBOT+1),
!               WHICH SHOULD NOT HAVE VOLUME=0 EXCEPT ON TIDAL
!               FLATS BECAUSE IN THIS CASE IPBOT=NPLAN-1
!               DO I=0,IPBOT(I2D)-1
                DO I=0,IPBOT(I2D)
                  IPOIN3 = I2D+I*NPOIN2
                  IF(VOLU%R(IPOIN3).LT.1.D-10) THEN
                    MTRA2%D%R(IPOIN3)=1.D0
                    IT1%I(IPOIN3) = KDIR
                    T3_03%R(IPOIN3) = FN%R(IPOIN3)
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!---> IMPLICIT DIFFUSION
!
      CALL DIRI01(FD,MTRA2,SEM3D,T3_03,IT1%I,T3_01,T3_02,MESH3D,
     &            KDIR,MSK,MASKPT)
!
!   SOLVES THE LINEAR SYSTEM:
!
      IF(TETADI.GT.0.001D0.OR.SCHCF.EQ.ADV_SUP) THEN
#if defined COMPAD
        CALL AD_SOLVE (FD, MTRA2, SEM3D, TRAV3, SLVDIF,
     &              INFO, MESH3D, MTRA1)
#else
        CALL SOLVE (FD, MTRA2, SEM3D, TRAV3, SLVDIF,
     &              INFO, MESH3D, MTRA1)
#endif
      ELSE
        IF(NCSIZE.GT.1) CALL PARCOM(MTRA2%D,2,MESH3D)
        CALL OS('X=Y/Z   ',X=FD,Y=SEM3D,Z=MTRA2%D,C=0.D0,
     &                     IOPT=2,INFINI=0.D0,ZERO=1.D-10)
      ENDIF
!
!=======================================================================
!
!   TREATS THE POINTS WHICH ARE COMPLETELY DRY
!    (BY DEFAULT FORCED TO 0)
!
      IF(MSK.AND.TRBAF.EQ.1) THEN
        DO IPOIN3 = 1,NPOIN3
          IF(MASKPT%R(IPOIN3).LT.0.5D0) FD%R(IPOIN3) = FN%R(IPOIN3)
        ENDDO
      ENDIF
!
!=======================================================================
!
!   CRUSHED POINTS AND THEIR FREE POINT ABOVE MUST HAVE THE SAME VALUE
!   IT IS DONE HERE WITH MASS CONSERVATION: THE VOLUMIC AVERAGE IS
!   TAKEN. TO AVOID SPURIOUS GRADIENTS...
!   HEAVY BUT SAVES TIME IN COMPLEX APPLICATIONS!
!
!=======================================================================
!
!     TODO: WARNING 2
!     THIS SECTION MAY CHANGE THE MASS-BALANCE, AS THE POINT TAKEN INTO
!     ACCOUNT FOR ENTERING AFBORF WILL HAVE ITS VALUE CHANGED.
!     TO SOLVE THE PROBLEM, EITHER WE REMOVE THIS OR WE HAVE A MORE
!     SOPHISTICATED COMPUTATION OF THE MASS-BALANCE, WITH TERMS DEPENDING
!     ON AFBORF TREATED BEFORE THIS SECTION.
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
        IF(NCSIZE.GT.1) THEN
          CALL OS('X=Y     ',X=T3_02,Y=VOLU)
!         T3_02 IS THE ASSEMBLED VOLUME
          CALL PARCOM(T3_02,2,MESH3D)
          VOLUME => T3_02%R
        ELSE
          VOLUME => VOLU%R
        ENDIF
        DO I2D=1,NPOIN2
          IF(IPBOT(I2D).GT.0) THEN
            VTOT=0.D0
            QTOT=0.D0
            DO I=0,IPBOT(I2D)
              VTOT=VTOT+VOLUME(I2D+I*NPOIN2)
              QTOT=QTOT+VOLUME(I2D+I*NPOIN2)*FD%R(I2D+I*NPOIN2)
            ENDDO
            IF(VTOT.GT.1.D-10) THEN
              FTOT=QTOT/VTOT
            ELSE
              FTOT=FD%R(I2D+IPBOT(I2D)*NPOIN2)
            ENDIF
            DO I=0,IPBOT(I2D)
              FD%R(I2D+I*NPOIN2)=FTOT
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
!=======================================================================
!
!   CLIPS F (IF REQUESTED WITH CLIMIN AND CLIMAX)
!
!=======================================================================
!
      CALL CLIP(FD,FMIN,CLIMIN,FMAX,CLIMAX,0)
!
!=======================================================================
!
      RETURN
      END

