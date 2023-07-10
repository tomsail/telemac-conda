!                   ***************************
                    SUBROUTINE BEDLOAD_SOLVS_FE_GAIA
!                   ***************************
!
     &(MESH,S,EBOR,MASKEL,MASK,QSX,QSY,IELMT,NPOIN,NPTFR,KENT,KDIR,KDDL,
     & LIMTEC,DT,MSK,ENTET,T1,T2,T3,T4,T8,HZ,HZN,GLOSEG,DIMGLO,
     & FLODEL,FLULIM,NSEG,UNSV2D,ICLA,FLBCLA,RATIO_SAND,
     & LIQBOR,QBOR,MAXADV,EVCL_MB,XMVS)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@todo: Rewrite equation in latex
!>@brief    Solves:
!!
!!               D(HZ)
!!    RHO_SAND*[ ---- + DIV(QS)] = 0
!!                DT
!!
!!    the main output of this subroutine is EVCL_MB=RHO_S*(HZ-HZN)
!>@warning
!!     LIMTEC is used here instead of LIEBOR. The difference is that
!!     LIMTEC is LIEBOR corrected in view of sign of u.n at boundaries
!!     then KENT, KSORT apply to LIEBOR, while KDIR and KDDL apply on
!!     LIMTEC, see BEDLOAD_DIFFIN_GAIA.f
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in,out] MESH    Mesh structure
!>@param[in]     S       Void structure
!>@param[in,out] EBOR    Boundary condition for bed evolution (dirichlet)
!>@param[in]     MASKEL  Masking of elements
!>@param[in]     MASK    Block of masks, every one for a type of boundary
!!                       See diffin.f in library bief.
!>@param[in]     QSX     Solid discharge x
!>@param[in]     QSY     Solid discharge y
!>@param[in]     IELMT   Type of element
!>@param[in]     NPOIN   Number of points
!>@param[in]     NPTFR   Number of boundary points
!>@param[in]     KENT    Convention for liquid input with prescribed value
!>@param[in]     KDIR    Convention for dirichlet point
!>@param[in]     KDDL    Convention for degree of freedom
!>@param[in]     LIMTEC  Type of boundary condition
!>@param[in]     DT      Time step
!>@param[in]     MSK     If yes, there is masked elements
!>@param[in]     ENTET   Logical, if yes information is given on mass conservation
!>@param[in,out] T1      Work bief_obj structure
!>@param[in,out] T2      Work bief_obj structure
!>@param[in,out] T3      Work bief_obj structure
!>@param[in,out] T4      Work bief_obj structure
!>@param[in,out] T8      Work bief_obj structure
!>@param[in,out] HZ      New available mass of sediment
!>@param[in]     HZN     Old available mass of sediment
!>@param[in]     GLOSEG  Connectivity table for segments
!>@param[in]     DIMGLO  First dimension of gloseg
!>@param[in,out] FLODEL  Fluxes between points (per segment)
!>@param[in,out] FLULIM  Limitation of fluxes
!>@param[in]     NSEG    Number of segments per control section
!>@param[in]     UNSV2D  Inverse of integrals of test functions
!>@param[in]     ICLA    Class number
!>@param[in,out] FLBCLA  Fluxes at boundary for the class
!>@param[in]     RATIO_SAND Ratio of sand to all sands, for isand,ilayer,ipoin
!>@param[in]     LIQBOR  Type of boundary condition on bedload discharge
!>@param[in]     QBOR    Prescribed bedload discharges
!>@param[in]     MAXADV  Maximum number of iterations (in positive_depth)
!>@param[in,out] EVCL_MB Evcl_mb=rho_s*(hz-hzn)
!>@param[in]     XMVS    Sand density
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_GAIA, EX_BEDLOAD_SOLVS_FE => BEDLOAD_SOLVS_FE_GAIA
      USE DECLARATIONS_GAIA, ONLY : DOFLUX,HN,HMIN_BEDLOAD
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: S,MASKEL,MASK,QSX,QSY
      INTEGER,          INTENT(IN)    :: IELMT,NPOIN,NPTFR,KENT,KDIR
      INTEGER,          INTENT(IN)    :: DIMGLO,NSEG,ICLA,KDDL,MAXADV
      INTEGER,          INTENT(IN)    :: GLOSEG(DIMGLO,2)
      DOUBLE PRECISION, INTENT(IN)    :: DT,RATIO_SAND(NPOIN),XMVS
      DOUBLE PRECISION, INTENT(INOUT) :: FLULIM(NSEG)
      LOGICAL,          INTENT(IN)    :: MSK,ENTET
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FLODEL,T1,T2,T3,T4,T8
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HZ,EBOR,LIMTEC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FLBCLA,EVCL_MB
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HZN,UNSV2D,LIQBOR,QBOR
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,N,I1,I2,ISEG
!
!-----------------------------------------------------------------------
!
!     BOUNDARY FLUXES
!
      CALL VECTOR(FLBCLA,'=','FLUBOR          ',IELBOR(IELMT,1),1.D0,
     &            S,S,S,QSX,QSY,S,MESH,.TRUE.,MASK)
!
!     FLBCLA built from qsx and qsx, so already in kg/s or kg/(m*s)
!
!     BOUNDARY CONDITIONS: EITHER EBOR OR QBOR PRESCRIBED (NOT THE 2)
!
      DO K=1,NPTFR
        IF(LIQBOR%I(K).EQ.KENT) THEN
!         QBOR IS GIVEN BY USER, AND POSITIVE IF ENTERING
!         HERE WE PUT THE INTERNAL USAGE <0 = ENTERING
          FLBCLA%R(K)=-QBOR%R(K)
!         EVEN IF USER HAS SPECIFIED LIEBOR=KSORT, LIMTEC MAY HAVE BEEN
!         SET TO KDIR BY CHECKING IF VELOCITY IS ENTERING, THIS IS
!         UNWANTED HERE AS QBOR ONLY IS TAKEN INTO ACCOUNT,
!         SO DDL IS PUT TO AVOID A DIRICHLET TREATMENT IN POSITIVE_DEPTHS.
          LIMTEC%I(K)=KDDL
        ELSEIF(LIMTEC%I(K).EQ.KDIR) THEN
!         HERE THE VARIABLE WILL BE THE LAYER DEPTH OF THE SEDIMENT CLASS,
!         PUT IN T8, NOT THE EVOLUTION
          N=MESH%NBOR%I(K)
! here we should replace : t8=masse imposee au bord+ masse precedente
          T8%R(K)=RATIO_SAND(N)*EBOR%R(K)*XMVS+HZN%R(N)
        ENDIF
      ENDDO
!
!     HERE T1 MAY NOT BE ASSEMBLED, WE WORK DIRECTLY ON MESH%W%R AFTER,
!     FOR CALLING FLUX_EF_VF (IT IS T1 IN NON ASSEMBLED FORM)
!
      CALL VECTOR(T1,'=','VGRADP          ',QSX%ELM,-1.D0,
     &            S,S,S,QSX,QSY,S,MESH,MSK,MASKEL,LEGO=.FALSE.)
!
!     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     NEW SECTION JMH 10/06/2016: FLODEL COMPUTED HERE, NOT IN POSITIVE_DEPTHS
!
      CALL FLUX_EF_VF(FLODEL%R,MESH%W%R,MESH%NSEG,MESH%NELEM,
     &                MESH%NELMAX,MESH%ELTSEG%I,MESH%ORISEG%I,
     &                MESH%IKLE%I,.TRUE.,2)
!
!     CANCELLING THE FLUXES TO AND FROM DRY POINTS
!
      DO ISEG=1,NSEG
!
        I1=GLOSEG(ISEG,1)
        I2=GLOSEG(ISEG,2)
        IF(HN%R(I1).LT.HMIN_BEDLOAD.OR.
     &     HN%R(I2).LT.HMIN_BEDLOAD) FLODEL%R(ISEG)=0.D0
      ENDDO
!
!     END OF NEW SECTION (ONLY TRUE CHANGED INTO FALSE AFTER FLODEL IN THE NEXT CALL)
!
!     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      CALL POSITIVE_DEPTHS(T1,T2,T3,T4,HZ,HZN,MESH,
!                                  !!!!!!
!    &                     FLODEL, .TRUE.,FLBCLA,DT,UNSV2D,NPOIN,
     &                     FLODEL,.FALSE.,FLBCLA,DT,UNSV2D,NPOIN,
     &                     GLOSEG(1:DIMGLO,1),GLOSEG(1:DIMGLO,2),
     &                     MESH%NBOR%I,NPTFR,T8,.FALSE.,T8,.FALSE.,
!                                            VOID (SMH) VOID (PLUIE)
     &                     1,FLULIM,
     &                     LIMTEC%I,T8%R  ,KDIR,ENTET,MESH%W%R,
!                                   EBOR%R
     &                     'GAIA                    ',2,MAXADV)
!                                                     2 : HARDCODED
!                             OPTION FOR POSITIVE DEPTHS ALGORITHMS
!                             HERE CHOICE OF OPTION WITH EDGE-BASED APPROACH
!
      CALL OS('X=Y-Z   ' ,X=EVCL_MB,Y=HZ,Z=HZN)
!
!-----------------------------------------------------------------------
!
!     NEW FLUXES ACROSS CROSS-SECTIONS
!
      IF(DOFLUX) THEN
        CALL FLUSEC_GAI(GLOSEG,DIMGLO,NSEG,NPOIN,DT,MESH,UNSV2D,
     &                  FLODEL,FLULIM,HZ,ICLA,ENTET)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

