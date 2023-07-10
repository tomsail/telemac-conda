!                   ****************************
                    SUBROUTINE BEDLOAD_EVOL_GAIA
!                   ****************************
!
     &(S,COEFPN,CALFA,SALFA,LIMTEC,EBOR,MASKEL,MASK,V2DPAR,
     & UNSV2D,DEBUG,NPOIN,NPTFR,IELMT,KENT,KDIR,KDDL,
     & DT,XMVS,VF,ENTETS,MSK,MESH,QS,
     & T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,BREACH,QSX,QSY,
     & SLOPEFF,ICLA,FLBCLA,LIQBOR,QBOR,MAXADV,MASS_SAND,
     & RATIO_SAND,EVCL_MB)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Computes the mass evolution for the bedload transport.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     S          TODO
!>@param[in,out] COEFPN     Correction of transport for sloping bed effect
!>@param[in,out] CALFA      Cosinus of the angle between mean flow and transport
!>@param[in,out] SALFA      Sinus of the angle between transport rate and current
!>@param[in,out] LIMTEC     Type of boundary condition
!>@param[in,out] EBOR       Boundary condition for bed evolution (dirichlet)
!>@param[in]     MASKEL     Masking of elements
!>@param[in]     MASK       Block of masks, every one for a type of boundary
!>@param[in]     V2DPAR     Integral of test functions, assembled in parallel
!>@param[in]     UNSV2D     Inverse of integrals of test functions
!>@param[in]     DEBUG      Flag for debugging
!>@param[in]     NPOIN      Number of points
!>@param[in]     NPTFR      Number of boundary points
!>@param[in]     IELMT      Type of element
!>@param[in]     KENT       Convention for liquid input with prescribed value
!>@param[in]     KDIR       Convention for dirichlet point
!>@param[in]     KDDL       Convention for degree of freedom
!>@param[in,out] DT         Time step
!>@param[in]     XMVS       Sediment density
!>@param[in]     VF         Logical, finite volumes or not
!>@param[in]     ENTETS     Logical, if yes information is given on mass conservation
!>@param[in]     MSK        If yes, there is masked elements
!>@param[in,out] MESH       Mesh structure
!>@param[in,out] QS        Bedload transport rate
!>@param[in,out] T1         Work bief_obj structure
!>@param[in,out] T2         Work bief_obj structure
!>@param[in,out] T3         Work bief_obj structure
!>@param[in,out] T4         Work bief_obj structure
!>@param[in,out] T5         Work bief_obj structure
!>@param[in,out] T6         Work bief_obj structure
!>@param[in,out] T7         Work bief_obj structure
!>@param[in,out] T8         Work bief_obj structure
!>@param[in,out] T9         Work bief_obj structure
!>@param[in,out] T10        Work bief_obj structure
!>@param[in,out] T11        Work bief_obj structure
!>@param[in,out] T12        Work bief_obj structure
!>@param[in,out] T13        Work bief_obj structure
!>@param[in,out] BREACH     Indicator for non erodible bed (finite volumes shemes)
!>@param[in,out] QSX        Solid discharge x
!>@param[in,out] QSY        Solid discharge y
!>@param[in]     SLOPEFF    Formula for slope effect
!>@param[in]     ICLA       Class number
!>@param[in]     FLBCLA     Block of fluxes at boundary for each class
!>@param[in]     LIQBOR     Type of boundary condition for bedload discharge
!>@param[in]     QBOR       Prescribed bedload discharge
!>@param[in]     MAXADV     Maximum number of iterations (in positive_depths)
!>@param[in]     MASS_SAND  Mass of sand of the first layer
!>@param[in]     RATIO_SAND Mass fraction of sand
!>@param[in,out] EVCL_MB    Mass evolution (per class) during a time step
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA, EX_BEDLOAD_EVOL => BEDLOAD_EVOL_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: S,UNSV2D
      TYPE(BIEF_OBJ),   INTENT(IN)    :: COEFPN,CALFA,SALFA
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,MASK,V2DPAR
      INTEGER,          INTENT(IN)    :: DEBUG,SLOPEFF,NPOIN,NPTFR,ICLA
      INTEGER,          INTENT(IN)    :: IELMT,KENT,KDIR,KDDL
      INTEGER,          INTENT(IN)    :: MAXADV
      DOUBLE PRECISION, INTENT(IN)    :: DT
      DOUBLE PRECISION, INTENT(IN)    :: XMVS
      DOUBLE PRECISION, INTENT(IN)    :: RATIO_SAND(NPOIN)
      LOGICAL,          INTENT(IN)    :: VF,ENTETS,MSK
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QS,EBOR,FLBCLA
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1, T2, T3, T4, T5, T6, T7
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T8, T9, T10, T11, T12, T13
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: BREACH, QSX, QSY, LIMTEC
      TYPE(BIEF_OBJ),   INTENT(IN)    :: LIQBOR,QBOR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: EVCL_MB
      DOUBLE PRECISION, INTENT(IN)    :: MASS_SAND(NPOIN)
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: J
      DOUBLE PRECISION, POINTER :: FLULIM(:)
!
!=======================================================================
!                               PROGRAM
!=======================================================================
!
!     POINTER TO A WORK ARRAY
!
      FLULIM => MESH%MSEG%X%R(MESH%NSEG+1:2*MESH%NSEG)
!
!     SLOPE EFFECT
!
      IF(SLOPEFF.EQ.1) CALL OS('X=XY    ', X=QS , Y=COEFPN)
      CALL OS('X=YZ    ', X=QSX, Y=QS, Z=CALFA)
      CALL OS('X=YZ    ', X=QSY, Y=QS, Z=SALFA)
!
!     TREATMENT OF NON ERODABLE BOTTOM
!
      IF(VF) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BEDLOAD_NERBED_VF_GAIA'
        CALL BEDLOAD_NERBED_VF_GAIA
     &        (MESH,LIMTEC,KDDL,V2DPAR%R,QSX,QSY,
     &         NPOIN,MESH%NSEG,NPTFR,DT,QS,T1,T2,T3,BREACH,
     &         MESH%NUBO%I,MESH%VNOIN%R,MASS_SAND)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETURN FROM BEDLOAD_NERBED_VF_GAIA'
        CALL OS('X=YZ    ', X=QSX, Y=QS, Z=CALFA)
        CALL OS('X=YZ    ', X=QSY, Y=QS, Z=SALFA)
      ENDIF
!
!     SOLVES THE BED-EVOLUTION EQUATION : F.V.
!
      IF(VF) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BEDLOAD_SOLVS_VF_GAIA'
        CALL BEDLOAD_SOLVS_VF_GAIA(MESH,QSX,QSY,LIMTEC,UNSV2D,EBOR,
     &                        BREACH,MESH%NSEG,NPTFR,NPOIN,
     &                        KENT,KDIR,KDDL,DT,T11,
     &                        FLBCLA%ADR(ICLA)%P,
     &                        LIQBOR,QBOR,MESH%NUBO%I,MESH%VNOIN%R,
     &                        EVCL_MB,RATIO_SAND,XMVS)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETURN FROM BEDLOAD_SOLVS_VF_GAIA'
!
!     SOLVES THE BED-EVOLUTION EQUATION : F.E.
!
      ELSE
        DO J=1,NPOIN
!         T13 IS THE MASS OF THE ACTIVE LAYER (FOR THIS SEDIMENT CLASS)
          T13%R(J)=MASS_SAND(J)
        ENDDO
        IF(DEBUG.GT.0) WRITE(LU,*) 'BEDLOAD_SOLVS_FE_GAIA'
        CALL BEDLOAD_SOLVS_FE_GAIA(MESH,S,EBOR,MASKEL,MASK,
     &                        QSX,QSY,IELMT,NPOIN,NPTFR,KENT,KDIR,KDDL,
     &                        LIMTEC,DT,MSK,ENTETS,T1,T2,T3,T4,T8,
     &                        T12,T13,MESH%GLOSEG%I,
     &                        MESH%GLOSEG%DIM1,MESH%MSEG%X,
     &                        FLULIM,MESH%NSEG,UNSV2D,ICLA,
     &                        FLBCLA%ADR(ICLA)%P,RATIO_SAND,LIQBOR,QBOR,
     &                        MAXADV,EVCL_MB,XMVS)
        IF(DEBUG.GT.0) WRITE(LU,*) 'END_BEDLOAD_SOLVS_FE'
      ENDIF
!
!======================================================================!
!======================================================================!
!
      RETURN
      END

