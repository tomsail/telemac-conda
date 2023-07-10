!                   *****************
                    SUBROUTINE FLUX3D
!                   *****************
!
     &(FLUINT,FLUEXT,FLUEXTPAR,UCONV,VCONV,TRA02,
     & NPLAN,IELM3,IELM2V,SVIDE,MESH3,
     & MSK,MASKEL,MASK_3D,LIHBOR,KENT,NPTFR,DT,VOLU,VOLUN,
     & MESH2,SIGMAG,NPOIN2,NPOIN3,DM1,GRAZCO,
     & FLBOR,PLUIE,RAIN,FLODEL,OPT_HNEG,FLULIM,YACVVF,BYPASS,
     & N_ADV,WEL)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES RELATIVE WATER AND TRACER MASS BALANCES
!+                DURING A TIMESTEP, AS WELL AS ABSOLUTE CUMULATIVE
!+                BALANCES.
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
!history  J-M HERVOUET(LNHE)
!+        26/08/2011
!+        V6P2
!+   Call to FLUX_EF_VF_3D changed, argument MASK suppressed, MASKBR
!+   replaced by MASK_3D, which avoids the call to EXTMSK (see LICHEK)
!
!history  J-M HERVOUET(LNHE)
!+        24/01/2012
!+        V6P2
!+   Combination of tests with YACCVF and OPTHNEG=2 corrected (this goes
!+   with a correction in precon.f).
!
!history  J-M HERVOUET(LNHE)
!+        08/07/2012
!+        V6P2
!+   Rain taken into account on prescribed depths to compute FLUEXT.
!
!history  J-M HERVOUET(LNHE)
!+        23/08/2012
!+        V6P2
!+   Call to na_flux3d_lim added to limit the non assembled fluxes
!+   stored into WEL and subsequent re-assembly of FLUINT.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BYPASS         |---| IF YES, BYPASS VOID VOLUMES
!| DM1            |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS. GRAD(ZCONV) IS
!|                |   | HERE GRAZCO.
!| DT             |-->| TIME STEP
!| FLBOR          |<->| FLUXES AT BOUNDARIES
!| FLODEL         |<->| FLUXES BY SEGMENT
!| FLUEXT         |<--| OUTPUT FLUX BY NODE
!| FLUEXTPAR      |<--| OUTPUT FLUX BY NODE, IN PARALLEL
!| FLUINT         |<--| INPUT FLUX BY NODE
!| FLULIM         |<->| LIMITATION OF FLUXES
!| GRAZCO         |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), GRAZCO=GRAD(ZCONV)
!| IELM2V         |-->| TYPE DE DISCRETISATION 2DV
!| IELM3          |-->| TYPE DE DISCRETISATION 3D
!| KENT           |-->| CONVENTION FOR PRESCRIBED DEPTH
!| LIHBOR         |-->| BOUNDARY CONDITIONS ON DEPTH
!| MASK_3D        |-->| 3D MASKS ON LATERAL BOUNDARIES
!| MASKEL         |<->| MASKING OF 3D ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH2          |<->| 2D MESH
!| MESH3          |<->| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| OPT_HNEG       |---| OPTION FOR NEGATIVE DEPTHS
!| PLUIE          |-->| RAIN IN M/S MULTIPLIED BY VOLU2D
!| RAIN           |-->| IF YES, THERE IS RAIN OR EVAPORATION
!| SIGMAG         |-->| LOGICAL FOR GENERALISED SIGMA TRANSFORMATION
!| SVIDE          |<->| VOID STRUCTURE
!| TRAV2          |<->| WORK ARRAY
!| VCONV          |-->| ADVECTION VELOCITY FIELD
!| VOLU           |-->| VOLUME AROUND POINTS AT TIME N+1
!| VOLUN          |-->| VOLUME AROUND POINTS AT TIME N
!| WEL            |<->| BIEF WORK ARRAY
!| YACVVF         |-->| THERE IS AN ADVECTION WITH FINITE VOLUMES
!|                |   | (HENCE COMPUTATION OF FLUXES REQUIRED)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC,   ONLY : ADV_NSC,ADV_PSI,ADV_NSC_TF
      USE DECLARATIONS_TELEMAC3D, ONLY : LV
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPLAN,NPOIN2,NPOIN3
      INTEGER, INTENT(IN) :: IELM3,IELM2V,OPT_HNEG
      INTEGER, INTENT(IN) :: KENT,NPTFR
      INTEGER, INTENT(IN) :: LIHBOR(NPTFR),N_ADV(0:15)
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FLUINT,FLUEXT,FLUEXTPAR
      TYPE(BIEF_OBJ), INTENT(IN)    :: VOLU,VOLUN,DM1,GRAZCO
      TYPE(BIEF_OBJ), INTENT(IN)    :: UCONV,VCONV,PLUIE,MASK_3D
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASKEL,WEL
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FLULIM
      TYPE(BIEF_OBJ), INTENT(INOUT), TARGET :: FLODEL
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FLBOR
      TYPE(BIEF_OBJ), INTENT(INOUT) :: SVIDE,TRA02
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH3, MESH2
!
      DOUBLE PRECISION, INTENT(IN)  :: DT
      LOGICAL, INTENT(IN)           :: MSK,SIGMAG,RAIN,YACVVF,BYPASS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPLAN,IPTFR,I,IOPT
      CHARACTER(LEN=16) FORMUL
!     DOUBLE PRECISION SUM_FLUEXT
!
!***********************************************************************
!
      CALL OS('X=0     ',X=FLUEXT)
!
!=======================================================================
!
!   INTERNAL ADVECTION FLUXES
!
!=======================================================================
!
!        /            D(PSII*)           D(PSII*)
!       /     H * U * -------- + H * V * --------   D(OMEGA*)
!      /OMEGA*           DX                 DY
!
!
      FORMUL = 'VGRADP 2     HOR'
      CALL VECTOR(FLUINT,'=',FORMUL,IELM3,1.D0,DM1,GRAZCO,GRAZCO,
     &            UCONV,VCONV,SVIDE,MESH3,MSK,MASKEL)
!
!     STORING NON-ASSEMBLED FLUINT IN WEL
!     WHICH IS NOT TO BE USED BEFORE BUILDING MATRICES MMURD OR
!     MURD_TF (SEE PRECON AND MT14PP)
!
      IF(N_ADV(ADV_NSC).GT.0.OR.N_ADV(ADV_PSI   ).GT.0
     &                      .OR.N_ADV(ADV_NSC_TF).GT.0) THEN
        IF(IELM3.EQ.41) THEN
          DO I=1,6*MESH3%NELEM
            WEL%R(I)=MESH3%W%R(I)
          ENDDO
          IF(OPT_HNEG.EQ.2) THEN
!           LIMITATION OF NON-ASSEMBLED FLUXES STORED IN WEL
            CALL NA_FLUX3D_LIM(WEL%R,FLULIM%R,
     &                         MESH2%NSEG,
     &                         MESH3%NELEM,MESH3%NELMAX,
     &                         MESH2%NELEM,MESH2%NELMAX,
     &                         MESH2%ELTSEG%I)
!           REASSEMBLING FLUINT
            CALL ASSVEC(FLUINT%R,MESH3%IKLE%I,NPOIN3,MESH3%NELEM,
     &                MESH3%NELMAX,WEL%R,.TRUE.,
     &                LV,MSK,MASKEL%R,6)
          ENDIF
        ELSEIF(IELM3.EQ.51) THEN
          DO I=1,4*MESH3%NELEM
            WEL%R(I)=MESH3%W%R(I)
          ENDDO
        ELSE
          WRITE(LU,*) 'FLUX3D: ELEMENT ',IELM3,' NOT IMPLEMENTED'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!     COMPUTING POINT TO POINT FLUXES:
!     FOR ADVECTION SCHEMES ADV_LEO OR ADV_LEO_TF, TO HAVE FLODEL
!     OR IN CASE THERE IS A LIMITATION WITH FLULIM (OPT_HNEG=2) BECAUSE
!     IN THAT CASE A NEW FLUINT CONSISTENT WITH THE NEW CONTINUITY
!     EQUATION MUST BE BUILT
!
!     HERE THE CONVENTION FOR SEGMENTS, DUE TO THE CHOICE OF FLUINT, IS
!     THAT A SEGMENT WITH POSITIVE FLUX IS MEANT WITH A FLOW FROM POINT 2
!     TO POINT 1.
!
!     NOT YET DONE FOR TETRAHEDRA
      IF(IELM3.EQ.41) THEN
!
      IF(YACVVF) THEN
!
        IOPT=2
!       BEWARE, WITH IELM3=51 RETURNS FLODEL IN 2D
!       BUT ADV_LEO AND ADV_LEO_PT NEVER DONE WITH IELM3=51
        CALL FLUX_EF_VF_3D(FLODEL%R,MESH2%W%R,MESH3%W%R,
     &                     MESH2%NSEG,MESH2%NELEM,
     &                     MESH2%NELMAX,
     &                     MESH2,.TRUE.,IOPT,1,
     &                     IELM3,NPLAN,MESH3%IKLE%I,MESH3%NELMAX,
     &                     MESH3%KNOLG%I)
!
        IF(OPT_HNEG.EQ.2) THEN
!         LIMITATION OF 3D FLUXES WITH 2D LIMITATIONS
          CALL FLUX3DLIM(FLODEL%R,FLULIM%R,NPLAN,MESH2%NSEG,NPOIN2,1)
!         NEW ASSEMBLY OF FLUINT (IN THIS CASE ASSEMBLING FLUINT
!                                 IN VECTOR ABOVE IS USELESS)
          CALL ASSEG_3D(FLODEL%R,FLUINT%R,NPOIN3,NPLAN,MESH2%NSEG,
     &                  MESH3%GLOSEG%I,MESH3%GLOSEG%DIM1,.TRUE.)
        ENDIF
!
      ENDIF
!
      ENDIF
!
!=======================================================================
!
!   COMPUTES THE ADVECTIVE FLUXES ON THE LATERAL LIQUID BOUNDARIES
!
!=======================================================================
!
!     /        ->  ->
!    /     H * U . N  PSII*  D(OMEGA*)
!   /
!  /LIQUID BOUNDARIES*
!
      FORMUL = 'FLUBOR          '
!
!     MASK_3D%ADR(8)%P: MASK ON LIQUID LATERAL BOUNDARIES
!
      CALL VECTOR
     & (TRA02, '=', FORMUL, IELM2V, 1.D0, SVIDE, SVIDE, SVIDE,
     &  UCONV, VCONV, SVIDE, MESH3,.TRUE.,MASK_3D%ADR(8)%P)
!
      CALL OSDB( 'X=Y     ' , FLUEXT , TRA02 , TRA02 , 0.D0 , MESH3 )
!
!
!     IF(RAIN) THEN
!       DO I=1,NPOIN2
!         FLUEXT%R((NPLAN-1)*NPOIN2+I)=
!    &    FLUEXT%R((NPLAN-1)*NPOIN2+I)+PLUIE%R(I)
!       ENDDO
!     ENDIF
!
!-----------------------------------------------------------------------
!
!     COMPUTATION OF FLUEXT ON POINTS WITH PRESCRIBED DEPTH
!     SO THAT CONTINUITY IS ENSURED.
!
!     EXCEPT AT THE FIRST CALL (BY THE FIRST CALL TO PREADV) FLBOR
!     HAS ALREADY BEEN COMPUTED (WAVE_EQUATION AND POSSIBLY
!     POSITIVE_DEPTHS). IT SHOULD GIVE HERE THE SAME VALUE
!
      IF(NPTFR.GT.0) THEN
        DO IPTFR = 1,NPTFR
          IF(LIHBOR(IPTFR).EQ.KENT) THEN
            FLBOR%R(IPTFR)=0.D0
            DO IPLAN = 1,NPLAN-1
              I=(IPLAN-1)*NPOIN2+MESH2%NBOR%I(IPTFR)
!             FLUEXT COMPUTED TO SOLVE CONTINUITY IN 3D
!             WITH ASSUMPTION THAT W* IS ZERO.
              FLUEXT%R(I)=FLUINT%R(I)+(VOLUN%R(I)-VOLU%R(I))/DT
              FLBOR%R(IPTFR)=FLBOR%R(IPTFR)+FLUEXT%R(I)
            ENDDO
!           LAST PLANE, RAIN MUST BE TAKEN INTO ACCOUNT
            I=(NPLAN-1)*NPOIN2+MESH2%NBOR%I(IPTFR)
            IF(RAIN) THEN
              FLUEXT%R(I)=FLUINT%R(I)+(VOLUN%R(I)-VOLU%R(I))/DT
     &                   +PLUIE%R(MESH2%NBOR%I(IPTFR))
            ELSE
              FLUEXT%R(I)=FLUINT%R(I)+(VOLUN%R(I)-VOLU%R(I))/DT
            ENDIF
            FLBOR%R(IPTFR)=FLBOR%R(IPTFR)+FLUEXT%R(I)
!
!           CHECKING THAT SUM OF FLUEXT IS STILL EQUAL TO FLBOR
!           IN THIS CASE DO NOT COMPUTE FLBOR ABOVE
!
!           SUM_FLUEXT=0.D0
!           DO IPLAN = 1,NPLAN
!             I=(IPLAN-1)*NPOIN2+MESH2%NBOR%I(IPTFR)
!             SUM_FLUEXT=SUM_FLUEXT+FLUEXT%R(I)
!           ENDDO
!           IF(ABS(SUM_FLUEXT-FLBOR%R(IPTFR)).GT.1.D-10) THEN
!             WRITE(LU,*)'PROBLEM AT POINT ',IPTFR
!             WRITE(LU,*)'FLBOR= ',FLBOR%R(IPTFR),' SUM_FLUEXT=',SUM_FLUEXT
!             DO IPLAN = 1,NPLAN
!               I=(IPLAN-1)*NPOIN2+MESH2%NBOR%I(IPTFR)
!               FLUEXT%R(I)=FLUEXT%R(I)*(FLBOR%R(IPTFR)/SUM_FLUEXT)
!             ENDDO
!             CALL PLANTE(1)
!             STOP
!           ENDIF
          ENDIF
        ENDDO
!
!       SPECIFIC TREATMENT OF POINTS THAT REMAIN WITHOUT VOLUME
!       THIS DOES NOT CHANGE FLBOR
!
        IF((OPT_HNEG.EQ.2.OR.SIGMAG).AND.BYPASS) THEN
          DO IPTFR = 1,NPTFR
            I=MESH2%NBOR%I(IPTFR)
            DO IPLAN = 1,NPLAN-1
              IF(VOLUN%R(I).LT.1.D-14.AND.VOLU%R(I).LT.1.D-14) THEN
!               FLUEXT GIVEN TO UPPER LAYER
                FLUEXT%R(I+NPOIN2)=FLUEXT%R(I+NPOIN2)+FLUEXT%R(I)
                FLUEXT%R(I)=0.D0
              ENDIF
              I=I+NPOIN2
            ENDDO
          ENDDO
        ENDIF
!
      ENDIF
!
!     ASSEMBLED VERSION OF FLUEXT
!
      IF(NCSIZE.GT.1) THEN
        CALL OS('X=Y     ',X=FLUEXTPAR,Y=FLUEXT)
        CALL PARCOM(FLUEXTPAR,2,MESH3)
!     ELSE
!       FLUEXTPAR%R=>FLUEXT%R   ! DONE ONCE FOR ALL IN POINT_TELEMAC3D
      ENDIF
!
!=======================================================================
!
!   COMPUTES THE ADVECTIVE FLUXES THROUGH THE BOTTOM AND FREE SURFACE
!
!=======================================================================
!
!  DIRICHLET TERMS AT THE BOTTOM AND FREE SURFACE
!
!     /
!    /     H * W*  PSII*  D(OMEGA*)
!   /FREE SURFACE AND BOTTOM (IN THE TRANSFPORMED MESH)
!
!   BOTTOM :
!
!     CALL VECTOR
!    &(TRAV2, '=', 'FLUBOR          ', IELM2H, -1.D0, SVIDE, SVIDE,
!    & SVIDE, SVIDE, SVIDE, WSBORF, MESH2,MSK,MASKEL)
!
!     CALL OV('X=X+Y   ', X=FLUEXT%R(1:NPOIN2),
!    &                    Y=TRAV2%R, DIM1=NPOIN2)
!
!   SURFACE :
!
!     CALL VECTOR
!    &(TRAV2, '=', 'FLUBOR          ', IELM2H, 1.D0, SVIDE, SVIDE,
!    & SVIDE, SVIDE, SVIDE, WSBORS, MESH2,MSK,MASKEL)
!
!     CALL OV('X=X+Y   ', X=FLUEXT%R((NPOIN3-NPOIN2+1):NPOIN3),
!    &                    Y=TRAV2%R, DIM1=NPOIN2)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
