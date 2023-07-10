!                   *****************
                    SUBROUTINE ERODNC
!                   *****************
!
     &(S3D_CFDEP, S3D_WCS, S3D_HDEP , S3D_FLUER, TOB   , DT    ,
     & NPOIN2 , NPOIN3 , S3D_AC , S3D_RHOS, RHO0  , HN ,
     & GRAV   , S3D_DMOY, S3D_CREF , ZREF   , S3D_ICQ,RUGOF,
     & Z      , UETCAR , S3D_SETDEP, S3D_EPAINCO, S3D_MIXTE)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    MODELS EROSION FOR NON-COHESIVE SEDIMENTS.
!
!history  CAMILLE LEQUETTE
!+        06/06/2003
!+        V5P3
!+   First version.
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        05/05/2014
!+        V7P0
!+   New extrapolation of Rouse profile removed. It spoils the test case
!+   depot, which is so far the only official test case of Telemac-3D
!+   with erosion and deposition. This must be clarified.
!
!history  G. ANTOINE & M. JODEAU & J.M. HERVOUET (EDF - LNHE)
!+        13/10/2014
!+        V7P0
!+   New developments in sediment for mixed sediment transport
!+   WC changed into S3D_WCS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |-->| QUADRATIC FRICTION COEFFICIENT (NOT USED)
!| DT             |-->| TIME STEP
!| GRAV           |-->| GRAVITY ACCELERATION
!| HN             |-->| WATER DEPTH AT TIME N
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| RHO0           |-->| WATER DENSITY AT REFERENCE CONCENTRATION
!| RUGOF          |<->| FRICTION COEFFICIENT ON THE BOTTOM
!| S3D_AC         |-->| CRITICAL SHIELDS PARAMETER
!| S3D_CFDEP      |-->| CONCENTRATION OF FRESH DEPOSIT DES DEPOTS FRAIS
!| S3D_CREF       |<->| EQUILIBRIUM CONCENTRATION
!| S3D_DMOY       |-->| MEAN DIAMETER OF GRAINS
!| S3D_EPAINCO    |-->| THICKNESS OF NON-COHESIVE SUB-LAYER
!| S3D_FLUER      |<->| EROSION  FLUX
!| S3D_HDEP       |<->| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| S3D_ICQ        |-->| FLAG FOR REFERENCE CONCENTRATION FORMULA
!| S3D_KSPRATIO   |-->| NOT USED
!| S3D_MIXTE      |-->| LOGICAL, MIXED SEDIMENTS OR NOT
!| S3D_RHOS       |-->| SEDIMENT DENSITY
!| S3D_SETDEP     |-->| OPTION FOR THE TREATMENT OF SETTLING VELOCITY
!| S3D_WCS        |-->| SETTLING VELOCITY FOR SAND
!| TOB            |-->| BOTTOM FRICTION
!| UETCAR         |-->| SQUARE OF THE FRICTION VELOCITY
!| Z              |-->| NODE COORDINATES
!| ZREF           |<->| REFERENCE ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY: KARMAN,PRANDTL,FICT
!
      USE INTERFACE_TELEMAC3D, EX_ERODNC => ERODNC
!     TRIGGERS A PGI COMPILER ERROR
      USE INTERFACE_SISYPHE,ONLY:SUSPENSION_FREDSOE,SUSPENSION_VANRIJN
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3,S3D_ICQ
      INTEGER, INTENT(IN)             :: S3D_SETDEP
!
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_HDEP(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: S3D_EPAINCO(*)
!
      DOUBLE PRECISION, INTENT(IN)    :: DT,S3D_CFDEP,GRAV,S3D_RHOS,RHO0
      DOUBLE PRECISION, INTENT(IN)    :: S3D_AC
!
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: S3D_DMOY,TOB,HN
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: S3D_CREF,ZREF,RUGOF
!
      DOUBLE PRECISION, INTENT(IN)    :: S3D_WCS(NPOIN3)

      LOGICAL, INTENT(IN)             :: S3D_MIXTE

      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN3), UETCAR(NPOIN2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN,I
      DOUBLE PRECISION USTAR, ROUSE, ROUSE_Z, DELTAZ, QS
!
      INTRINSIC MIN,MAX
!
!-----------------------------------------------------------------------
!
!  ---------------------------------------------------------------------
!  ------- COMPUTES THE REFERENCE CONCENTRATION S3D_CREF(IN G/L) ----------
!  ---------------------------------------------------------------------
!
!     CV depth to extrapolate the near bed concentrations (see also KEPLC)
!     For FV scheme use delta= Dz1/4
!     FE scheme          = Dz1/2
!
!     ZYSERMAN & FREDSOE (1994) (BY DEFAULT)
!
!     SO FAR S3D_DMOYIS A CONSTANT
!
      IF(S3D_ICQ.EQ.1) THEN
        CALL OS('X=CY    ', X=ZREF, Y=S3D_DMOY, C=2.D0)
        CALL SUSPENSION_FREDSOE(S3D_DMOY%R(1),TOB, NPOIN2,
     &                        GRAV,RHO0,S3D_RHOS,S3D_AC,S3D_CREF)
        CALL OS('X=CY    ', X=ZREF, Y=S3D_DMOY, C=2.D0)
      ELSEIF(S3D_ICQ.EQ.3) THEN
        CALL OS('X=CY    ', X=ZREF, Y=RUGOF, C=0.5D0)
        CALL SUSPENSION_VANRIJN(S3D_DMOY%R(1),TOB,NPOIN2,
     &                          GRAV,RHO0,S3D_RHOS,1.D-06,1.D-06,
     &                          S3D_AC,S3D_CREF,ZREF)
      ENDIF
!
!     UNITS FOR S3D_CREFG/L, NOT LIKE IN SISYPHE
!
      CALL OS('X=CX    ',X=S3D_CREF,C=S3D_RHOS)
!
!     JMH ON 05/05/2014
!     Following lines put under condition of S3D_SETDEP=1, they change a
!     lot the test case depot (76 cm of deposition at the
!     entrance, while the bed should not evolve too much, as
!     we simulate a Rouse profile. Lines below are probably compatible
!     with S3D_FLUDPTdone when S3D_SETDEP=1.
!
!     CV: Extrapolation of Rouse profile from ZREF to 1/2 or 1/4 of first grid mesh
!
      IF(S3D_SETDEP.EQ.1) THEN
!
        DO IPOIN =1,NPOIN2
          USTAR=MAX(SQRT(UETCAR(IPOIN)),1.D-6)
          ROUSE=PRANDTL*S3D_WCS(IPOIN)/KARMAN/USTAR
!         rouse profile extrapolation up to 1/4 of the first layer
          DELTAZ=(Z(IPOIN +NPOIN2)-Z(IPOIN))/FICT
          ROUSE_Z=ZREF%R(IPOIN)/(HN%R(IPOIN)-ZREF%R(IPOIN))
     &           *(HN%R(IPOIN)-DELTAZ)/DELTAZ
          S3D_CREF%R(IPOIN)=S3D_CREF%R(IPOIN)*ROUSE_Z**ROUSE
        ENDDO
!
      ENDIF
!
!  ------------------------------------------------------------
!  -----------------     EROSION STEP    ----------------------
!  ------------------------------------------------------------
!
      IF(S3D_MIXTE) THEN

        DO I=1,NPOIN2
!
        S3D_FLUER(I)= S3D_WCS(I)*S3D_CREF%R(I)
        QS=S3D_CFDEP*S3D_EPAINCO(I)
        S3D_FLUER(I)=MIN(S3D_FLUER(I),QS/DT)
!
        ENDDO

      ELSE

        DO I=1,NPOIN2
!
!       COMPUTES THE EROSION FLUX
!
        S3D_FLUER(I)= S3D_WCS(I)*S3D_CREF%R(I)
!
!       QUANTITY OF SOLID IN THE LAYER BEFORE EROSION
!
!       S3D_CFDEPIN KG/M3 ( ~ 0.65 S3D_RHOS)
        QS=S3D_CFDEP*S3D_HDEP(I)
!
!       LAYER THICKNESS AFTER EROSION
!
!CV     S3D_HDEP(I)=MAX(0.D0,S3D_HDEP(I)-(S3D_FLUER(I)*DT/S3D_CFDEP))
!
!       LIMITS THE EROSION FLUX
!
!       BEGINNING OF SPECIFIC TO THIS CASE
!       S3D_FLUER(I)=MIN(S3D_FLUER(I),QS/DT)
        S3D_FLUER(I)=0.D0
!       END OF SPECIFIC TO THIS CASE
!
      ENDDO
!
      ENDIF
!-----------------------------------------------------------------------
!
      RETURN
      END
