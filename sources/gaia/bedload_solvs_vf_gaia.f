!                   ********************************
                    SUBROUTINE BEDLOAD_SOLVS_VF_GAIA
!                   ********************************
!
     &(MESH,QSX,QSY,LIMTEC,UNSV2D,EBOR,BREACH,NSEG,NPTFR,NPOIN,
     & KENT,KDIR,KDDL,DT,FLUX,FLBCLA,
     & LIQBOR,QBOR,NUBO,VNOIN,EVCL_MB,RATIO_SAND,XMVS)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Solves a part of exner equation (divergence term)
!        with the finite volume method.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in,out] BREACH      Indicator for non erodible bed
!>@param[in]     DT          Time step
!>@param[in,out] EBOR        Boundary condition for bed evolution (dirichlet)
!>@param[in,out] FLBCLA      Fluxes at boundary for the class
!>@param[in,out] FLUX        Sediment flux
!>@param[in]     KDIR        Convention for dirichlet value
!>@param[in]     KDDL        Convention for degree of freedom
!>@param[in]     KENT        Convention for prescribed value at entrance
!>@param[in]     LIMTEC      Technical boundary conditions for bed evolution
!>@param[in]     LIQBOR      Type of boundary conditions for bedload discharge
!>@param[in,out] EVCL_MB     Mass evolution during a time step
!>@param[in,out] MESH        Mesh structure
!>@param[in]     NPOIN       Number of points
!>@param[in]     NPTFR       Number of boundary points
!>@param[in]     NSEG        Number of segments
!>@param[in]     NUBO        Global number of edge extremities
!>@param[in]     QBOR        Prescribed bedload discharges
!>@param[in,out] QSX         Bedload transport rate x-direction
!>@param[in,out] QSY         Bedload transport rate y-direction
!>@param[in]     RATIO_SAND  Mass fraction of sand, for the class
!>@param[in]     UNSV2D      Inverse of integrals of test functions
!>@param[in]     VNOIN       Outward unit normals
!>@param[in]     XMVS        Sediment density
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA, EX_BEDLOAD_SOLVS_VF => BEDLOAD_SOLVS_VF_GAIA
      USE BIEF
      USE DECLARATIONS_GAIA, ONLY : HN,HMIN_BEDLOAD,DVF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: QSX,QSY,LIMTEC,UNSV2D,EBOR
      TYPE(BIEF_OBJ),   INTENT(IN)    :: BREACH,LIQBOR,QBOR
      INTEGER,          INTENT(IN)    :: NSEG,NPTFR,NPOIN,KENT,KDIR,KDDL
      DOUBLE PRECISION, INTENT(IN)    :: DT
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FLBCLA,FLUX
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: EVCL_MB
      INTEGER, INTENT(IN)             :: NUBO(2,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: VNOIN(3,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: RATIO_SAND(NPOIN),XMVS
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: ISEGIN,K,N,IEL,IEL1,IEL2,IELEM,NELEM,ERR
      DOUBLE PRECISION :: QSMOY1,QSMOY2,QSP,VNOIN1,VNOIN2,RNORM,XN,YN
      DOUBLE PRECISION :: EVCL_MDIR,PROD_SCAL
      LOGICAL, ALLOCATABLE :: YESNO(:)
!
      NELEM = MESH%NELEM
      ALLOCATE(YESNO(NSEG),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'YESNO')
!     INITIALIZATION OF YESNO
      DO K=1,NSEG
        YESNO(K)=.FALSE.
      ENDDO
!
!     INITIALISES THE DIVERGENCE
!
      CALL OS('X=0     ', X=FLUX)
!
!     DETERMINES THE OUTGOING FLUX FOR EACH CELL
!     CONVENTION: POSITIVE FLUX IS OUTGOING FLUX
!
      DO IELEM=1,NELEM
        DO K=1,3
          ISEGIN = MESH%ELTSEG%I(IELEM+(K-1)*NELEM)
          IF(.NOT.YESNO(ISEGIN)) THEN
            IEL1 = NUBO(1,ISEGIN)
            IEL2 = NUBO(2,ISEGIN)
!
!           1 - SEGMENT LENGTH (RNORM)
!
            VNOIN1 = VNOIN(1,ISEGIN)
            VNOIN2 = VNOIN(2,ISEGIN)
            RNORM  = VNOIN(3,ISEGIN)
            PROD_SCAL= (MESH%X%R(IEL2)-MESH%X%R(IEL1))*VNOIN1+
     &                 (MESH%Y%R(IEL2)-MESH%Y%R(IEL1))*VNOIN2
            IF(PROD_SCAL.LT.0.D0)THEN
              IEL1 = NUBO(2,ISEGIN)
              IEL2 = NUBO(1,ISEGIN)
            ENDIF
!
!           2 - QS FOR THE SEGMENT, BROKEN UP ACCORDING TO X AND Y
!           CENTRED FLUX: ONLY USED TO FIND THE SOLID TRANSPORT DIRECTION
!
            QSMOY1 = 0.5D0*(QSX%R(IEL1) + QSX%R(IEL2))
            QSMOY2 = 0.5D0*(QSY%R(IEL1) + QSY%R(IEL2))
!
!           3 - PROJECTS QS FOR THE SEGMENT ONTO THE SEGMENT NORMAL
!
            QSP = VNOIN1*QSMOY1 + VNOIN2*QSMOY2
!
!           4 - UPWIND SCHEME OR CENTRED SCHEME
!
            IF(QSP.GT.0.D0) THEN
!             NODE WITH A "PROBLEM": UPWIND
              IF(BREACH%I(IEL1).EQ.1) THEN
                QSMOY1 = QSX%R(IEL1)
                QSMOY2 = QSY%R(IEL1)
              ELSE
!             UPWIND OR CENTRED ACCORDING TO DVF
                QSMOY1 = DVF*QSX%R(IEL1) + (1.D0-DVF)*QSX%R(IEL2)
                QSMOY2 = DVF*QSY%R(IEL1) + (1.D0-DVF)*QSY%R(IEL2)
              ENDIF
            ELSE !QSP.LE.0.D0
!             NODES WITH A "PROBLEM": UPWIND
              IF(BREACH%I(IEL2).EQ.1) THEN
                QSMOY1 = QSX%R(IEL2)
                QSMOY2 = QSY%R(IEL2)
              ELSE
!               UPWIND OR CENTRED ACCORDING TO DVF
                QSMOY1 = (1.D0-DVF)*QSX%R(IEL1) + DVF*QSX%R(IEL2)
                QSMOY2 = (1.D0-DVF)*QSY%R(IEL1) + DVF*QSY%R(IEL2)
              ENDIF
            ENDIF
            QSP = VNOIN1*QSMOY1 + VNOIN2*QSMOY2
!
!           5 - INTEGRATES BY THE SEGMENT LENGTH:
!           ONLY IF NODE IS WET
!
            IF(HN%R(IEL1).GE.HMIN_BEDLOAD.AND.
     &         HN%R(IEL2).GE.HMIN_BEDLOAD) THEN
              IF(MESH%IFABOR%I(IELEM+(K-1)*NELEM).NE.-2) THEN
                FLUX%R(IEL1) = FLUX%R(IEL1) + RNORM*QSP
                FLUX%R(IEL2) = FLUX%R(IEL2) - RNORM*QSP
              ELSE
!             IF IT IS AN INTERFACE EDGE IN PARALLEL, ONLY HALF
                FLUX%R(IEL1) = FLUX%R(IEL1) + 0.5D0*RNORM*QSP
                FLUX%R(IEL2) = FLUX%R(IEL2) - 0.5D0*RNORM*QSP
              ENDIF
            ENDIF
            YESNO(ISEGIN)=.TRUE.
          ENDIF
        ENDDO
      ENDDO
!
!     BOUNDARIES
!
      IF(NCSIZE.LE.1) THEN
        DO K = 1 , NPTFR
          IEL = MESH%NBOR%I(K)
!         VECTOR NORMAL TO A BOUNDARY NODE
!         VERSION WHICH IS NOT NORMED
          XN = MESH%XNEBOR%R(K+NPTFR)
          YN = MESH%YNEBOR%R(K+NPTFR)
!
!         ADDING BOUNDARY FLUX AT OPEN BOUNDARIES
!         QBOR HAS PRIORITY HERE, SO IN CASE OF LIQBOR=KENT
!         LIMTEC=KDIR WILL NOT BE CONSIDERED, SEE BEDLOAD_SOLVS_FE_GAIA
!         FOR MORE EXPLANATIONS
!
!         QBOR AND FLUX MUST HAVE THE SAME DIMENSION !
!
          IF(LIQBOR%I(K).EQ.KENT) THEN
            FLBCLA%R(K) = -QBOR%R(K)
            FLUX%R(IEL) = FLUX%R(IEL) + FLBCLA%R(K)
          ELSEIF(LIMTEC%I(K).EQ.KDDL.OR.LIMTEC%I(K).EQ.KDIR) THEN
!           IF KDIR WILL BE UPDATED LATER
            FLBCLA%R(K)= QSX%R(IEL)*XN + QSY%R(IEL)*YN
!           ADDS THE CONTRIBUTION OF THE FLUX ON THE BOUNDARY SEGMENT
            FLUX%R(IEL) = FLUX%R(IEL) + FLBCLA%R(K)
          ELSE
!           NO SEDIMENT FLUX ACCROSS SOLID BOUNDARIES
            FLBCLA%R(K)=0.D0
          ENDIF
        ENDDO
      ELSE
!       COMPUTED ONLY ONCE WHEN THE LIQUID BOUNDARY NODE BELONGS TO
!       2 SUBDDOMAINS
        DO K = 1 , NPTFR
          IEL = MESH%NBOR%I(K)
!         VECTOR NORMAL TO A BOUNDARY NODE
!         VERSION WHICH IS NOT NORMED
          XN = MESH%XNEBOR%R(K+NPTFR)
          YN = MESH%YNEBOR%R(K+NPTFR)
!
!         ADDING BOUNDARY FLUX AT OPEN BOUNDARIES
!         QBOR HAS PRIORITY HERE, SO IN CASE OF LIQBOR=KENT
!         LIMTEC=KDIR WILL NOT BE CONSIDERED, SEE BEDLOAD_SOLVS_FE_GAIA
!         FOR MORE EXPLANATIONS
!
!         QBOR AND FLUX MUST HAVE THE SAME DIMENSION !
!
          IF(LIQBOR%I(K).EQ.KENT) THEN
            FLBCLA%R(K) = -QBOR%R(K)
            FLUX%R(IEL) = FLUX%R(IEL) + FLBCLA%R(K)*MESH%IFAC%I(IEL)
          ELSEIF(LIMTEC%I(K).EQ.KDDL.OR.LIMTEC%I(K).EQ.KDIR) THEN
!           IF KDIR WILL BE UPDATED LATER
            FLBCLA%R(K)= QSX%R(IEL)*XN + QSY%R(IEL)*YN
!           ADDS THE CONTRIBUTION OF THE FLUX ON THE BOUNDARY SEGMENT
            FLUX%R(IEL) = FLUX%R(IEL) + FLBCLA%R(K)*MESH%IFAC%I(IEL)
          ELSE
!           NO SEDIMENT FLUX ACCROSS SOLID BOUNDARIES
            FLBCLA%R(K)=0.D0
          ENDIF
        ENDDO
      ENDIF
!
!     ASSEMBLING IN PARALLEL
!
      IF(NCSIZE.GT.1) CALL PARCOM(FLUX, 2, MESH)
!
!     SOLVING, NEGATIVE SIGN BECAUSE OUTGOING FLUX IS POSITIVE
!     NOTE JMH: FLUX MUST BE HERE DIV(QS)
!
      CALL OS('X=CYZ   ', X=EVCL_MB, Y=FLUX, Z=UNSV2D, C=-DT)
!
!     ebor*xmvs replaced by a unique value given by the user? Not done
!
      DO K=1,NPTFR
!                                  PRIORITY OF LIQBOR, SEE ABOVE
        IF(LIMTEC%I(K).EQ.KDIR.AND.LIQBOR%I(K).NE.KENT) THEN
          N = MESH%NBOR%I(K)
          EVCL_MDIR = RATIO_SAND(N)*EBOR%R(K)*XMVS
!         CORRECTION OF BOUNDARY FLUX TO GET EVCL_MDIR
          FLBCLA%R(K)=FLBCLA%R(K)-(EVCL_MDIR-EVCL_MB%R(N))/
     &                (DT*UNSV2D%R(N))
!         EVCL_MDIR FINALLY IMPOSED
          EVCL_MB%R(N) = EVCL_MDIR
        ENDIF
!
      ENDDO
!
      DEALLOCATE(YESNO)
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
