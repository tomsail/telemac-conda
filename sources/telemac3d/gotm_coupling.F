      MODULE GOTM_COUPLING

      USE DECLARATIONS_TELEMAC3D, ONLY: NPLAN,T3D_FILES,T3DGTM,KFROT,
     &    AKEP,NPOIN2,VISCVI,VISCTA,NTRAC,DNUVIV,DNUTAV,KARMAN,HLIM,
     &    NETAGE,GRAV,RHOPOT,VENT,BUBORS,BVBORS,DT,UETCAR,RUGOF
#ifdef  HAVE_GOTM
      USE DECLARATIONS_TELEMAC3D, ONLY: H_TEL => H, U_TEL => U,
     &    V_TEL => V, Z_TEL => Z,  K_TEL => AK, EP_TEL => EP
      USE TURBULENCE, ONLY: NUM,NUH,TURB_METHOD
      USE TURBULENCE, ONLY: TKE,EPS,EPS_MIN,K_MIN,L,CDE,KAPPA
      USE TURBULENCE, ONLY: INIT_TURBULENCE, DO_TURBULENCE
      USE TURBULENCE, ONLY: CLEAN_TURBULENCE
      USE MTRIDIAGONAL, ONLY: INIT_TRIDIAGONAL
#endif

      IMPLICIT NONE

      PRIVATE
      PUBLIC :: GOTM_COUPLING_INIT,GOTM_COUPLING_STEP,DEALLOC_GOTM
!
!-----------------------------------------------------------------------
!
      INTEGER NELEV
      LOGICAL USE_KPP
#ifdef HAVE_GOTM
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: TKE_DEF, EPS_DEF,
     &                                               NUH_DEF, NUM_DEF
#endif

      CONTAINS

!                   *****************************
                    SUBROUTINE GOTM_COUPLING_INIT
!                   *****************************
!
!
!***********************************************************************
! TELEMAC3D   V8P1
!***********************************************************************
!
!brief    Initialization of the coupling between GOTM and TELEMAC-3D
!
!history  WA BREUGEM (IMDC)
!+        13/08/2015
!+        V8P1
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL

      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     INPUT PARAMETERS

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

      ! LOCAL VARIABLES
      INTEGER IPLAN, IPOIN, ITRAC, ITMP

!***********************************************************************

#ifdef HAVE_GOTM
      NELEV  = NPLAN-1

      IF(T3D_FILES(T3DGTM)%NAME(1:1).EQ.'') THEN
        WRITE (LU,*)'ERROR: GOTM STEERING FILE NEEDED WHEN USING GOTM'
        CALL PLANTE(1)
      ENDIF

      USE_KPP = .FALSE.

      ! CHECK ALL MODEL OPTIONS

      IF(KFROT.NE.5) THEN
        WRITE (LU,*)
     &  'ERROR: NIKURADSE LAW OF BOTTOM FRICTION NEEDED WHEN USING GOTM'
        CALL PLANTE(1)
      ENDIF

      ! GOTM INITIALISATION

      ! INITIATE TRIDIAGONAL
      CALL INIT_TRIDIAGONAL(NELEV)
      CALL INIT_TURBULENCE(T3D_FILES(T3DGTM)%LU,
     &                     T3D_FILES(T3DGTM)%TELNAME,NELEV)

      !CHECK PARAMETERS IN GOTM STEERING FILE ARE COMPATIBLE
      IF((.NOT.USE_KPP).AND.TURB_METHOD.EQ.99) THEN
        WRITE (LU,*) 'KPP SELECTED IN GOTM STEERING FILE'
        WRITE (LU,*) 'BUT KPP NOT YET IMPLEMENTED'
        CALL PLANTE(1)
      ENDIF

      IF(TURB_METHOD.EQ.99) THEN
        WRITE (LU,*) 'KPP NOT YET IMPLEMENTED'
        CALL PLANTE(1)
      ENDIF

      ! K-EPSILON LIKE
      ALLOCATE(EPS_DEF(0:NPLAN-1))
      ALLOCATE(TKE_DEF(0:NPLAN-1))
      ALLOCATE(NUH_DEF(0:NPLAN-1))
      ALLOCATE(NUM_DEF(0:NPLAN-1))
!SAVE DEFAULT VALUES TO USE ON TIDAL FLATS
      DO IPLAN =0,NPLAN-1
        EPS_DEF(IPLAN)= EPS(IPLAN)
        TKE_DEF(IPLAN)= TKE(IPLAN)
        NUH_DEF(IPLAN)= NUH(IPLAN)
        NUM_DEF(IPLAN)= NUM(IPLAN)
      ENDDO
      IF(AKEP) THEN
        DO IPOIN=1,NPOIN2
          DO IPLAN=1,NPLAN
            ITMP  = (IPLAN-1)*NPOIN2 + IPOIN
            K_TEL%R(ITMP)  = TKE(IPLAN-1)
            EP_TEL%R(ITMP) = EPS(IPLAN-1)
            VISCVI%ADR(3)%P%R(ITMP) = NUM(IPLAN-1)
            DO ITRAC=1,NTRAC
              VISCTA%ADR(ITRAC)%P%ADR(3)%P%R(ITMP) = NUH(IPLAN-1)
            ENDDO
          ENDDO
        ENDDO
      ELSE !?
      ENDIF
! TODO; NOT CORRECT IF INITIAL CONDITIONS IS USED FOR K OR EPS
! todo: CHECK WHY THE ABOVE IS IGNORED
      CALL OS('X=C     ',X=VISCVI%ADR(3)%P,C=1.D-5 + DNUVIV)
!      VISCVI%ADR(3)%P%R = 1.D-5 + DNUVIV
      DO ITRAC=1,NTRAC
        CALL OS('X=C     ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,
     &                     C=DNUTAV(ITRAC))
!        VISCTA%ADR(ITRAC)%P%ADR(3)%P%R = DNUTAV(ITRAC)
      ENDDO
#else
      WRITE (LU,*) 'ERROR: CODE IS NOT COMPILED WITH GOTM LIBRARY'
      WRITE (LU,*) 'MAYBE FORGOTTEN TO COMPILE WITH -DHAVE_GOTM'
      CALL PLANTE(1)
#endif
      END SUBROUTINE GOTM_COUPLING_INIT
!                   *****************************
                    SUBROUTINE GOTM_COUPLING_STEP
!                   *****************************
!
!
!***********************************************************************
! TELEMAC3D   V8P1
!***********************************************************************
!
!brief    Calculation of the vertical turbulence profiles using GOTM
!
!history  WA BREUGEM (IMDC)
!+        13/08/2015
!+        V8P1
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL

      IMPLICIT NONE

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     INPUT PARAMETERS

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

      ! LOCAL VARIABLES

      !TODO: ADD PARAMETER TO CAS FILE
      DOUBLE PRECISION, PARAMETER :: A_CHARNOCK = 0.014D0

      INTEGER IPLAN, IPOIN, ITMP, ITRAC
      DOUBLE PRECISION DEPTH,DZ,UETCAS,RUGOS
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: DELTAS,NN,SS

!***********************************************************************

      ! LOOP OVER ALL POINTS
#ifdef HAVE_GOTM
      !COPY VON KARMAN COEFFCIENT TO GOTM
      KAPPA = KARMAN

      ALLOCATE(DELTAS(0:NPLAN-1))
      ALLOCATE(NN(0:NPLAN-1))
      ALLOCATE(SS(0:NPLAN-1))

      DELTAS(0) = 0.D0

      ! GOTM TWO-EQUATION MODELS
      ! WRITE (LU,*) 'GOTM 2-EQUATION MODELS'
      DO IPOIN=1,NPOIN2
        ! CHECK FOR DRYING FLOODING
        IF(H_TEL%R(IPOIN).GT.HLIM) THEN
          ! COPY DATA TO GOTM FORMAT
          DEPTH = -Z_TEL(IPOIN)
          DO IPLAN=1,NPLAN
            ITMP = (IPLAN-1)*NPOIN2 + IPOIN
            ! K AND EPSILON
            TKE(IPLAN-1) = MAX(K_TEL%R(ITMP),K_MIN)
            EPS(IPLAN-1) = MAX(EP_TEL%R(ITMP),EPS_MIN)
            ! VISCOSITY (FIRST TRACER IS USED)
            NUM(IPLAN-1) = VISCVI%ADR(3)%P%R(ITMP)
            L(IPLAN-1)   = CDE*SQRT(TKE(IPLAN-1)**3)/EPS(IPLAN-1)
          ENDDO
          IF(NTRAC.GT.0) THEN
            DO IPLAN=1,NPLAN
              NUH(IPLAN-1) = VISCTA%ADR(1)%P%ADR(3)%P%R(ITMP)
            ENDDO
          ENDIF
          DO IPLAN=1,NETAGE
            ITMP = (IPLAN-1)*NPOIN2 + IPOIN
            DZ   = MAX(Z_TEL(ITMP+NPOIN2)-Z_TEL(ITMP),1.D-16)
            DELTAS(IPLAN) = DZ
          ENDDO
          DO IPLAN=2,NETAGE
            ITMP = (IPLAN-1)*NPOIN2 + IPOIN
            DZ = MAX(Z_TEL(ITMP+NPOIN2)-Z_TEL(ITMP-NPOIN2),1.D-16)
            ! BUOYANCY FREQUENCY AT W POINTS
            NN(IPLAN-1) =
     &          -GRAV*(RHOPOT%R(ITMP+NPOIN2)-RHOPOT%R(ITMP-NPOIN2))
     &               /(DZ*RHOPOT%R(ITMP))
            ! SHEAR FREQUENCY AT W POINTS
            SS(IPLAN-1) =
     &          ((U_TEL%R(ITMP+NPOIN2)-U_TEL%R(ITMP-NPOIN2))**2  +
     &           (V_TEL%R(ITMP+NPOIN2)-V_TEL%R(ITMP-NPOIN2))**2)
     &          /DZ**2
          ENDDO
          ! COPY BOUNDARY CONDITIONS
          NN(0)      = 0.D0
          NN(NETAGE) = 0.D0
          SS(0)      = SS(1)
          SS(NETAGE) = SS(NETAGE-1)

          !SURFACE STRESSES AND ROUGHNESS USING CHARNOCK'S RELATION
          IF(VENT) THEN
            UETCAS = (BUBORS%R(IPOIN)**2+BVBORS%R(IPOIN)**2)**0.25D0
            RUGOS  =  MAX(A_CHARNOCK*UETCAS**2/GRAV,1.D-16)
          ELSE
            UETCAS = 0.D0
            RUGOS  = 1.D-16
          ENDIF

          ! CALL GOTM (WITHOUT SEAGRASS)
          ! CONVERSION FROM NIKURADSE TO Z0
          CALL DO_TURBULENCE(NELEV,DT,DEPTH,UETCAS,UETCAR%R(IPOIN),
     &                         RUGOS,RUGOF%R(IPOIN)/30.0D0,DELTAS,NN,SS)
          ! COPY DATA BACK
          DO IPLAN=1,NPLAN
            ITMP = (IPLAN-1)*NPOIN2 + IPOIN
            ! VISCOSITY/DIFFUSIVITY
            VISCVI%ADR(3)%P%R(ITMP) = NUM(IPLAN-1) + DNUVIV
            DO ITRAC=1,NTRAC
              VISCTA%ADR(ITRAC)%P%ADR(3)%P%R(ITMP) = NUH(IPLAN-1)
     &                                             + DNUTAV(ITRAC)
            ENDDO
            ! K AND EPSILON
            K_TEL%R(ITMP)  = TKE(IPLAN-1)
            EP_TEL%R(ITMP) = EPS(IPLAN-1)
          ENDDO
        ELSE
          ! DRY CELL; SET DEFAULTS
          DO IPLAN=1,NPLAN
            ITMP = (IPLAN-1)*NPOIN2 + IPOIN
            ! VISCOSITY/DIFFUSIVITY
            VISCVI%ADR(3)%P%R(ITMP) = NUM_DEF(IPLAN-1) + DNUVIV
            DO ITRAC=1,NTRAC
              VISCTA%ADR(ITRAC)%P%ADR(3)%P%R(ITMP) = NUH_DEF(IPLAN-1)
     &                                             + DNUTAV(ITRAC)
            ENDDO
            ! K AND EPSILON
            K_TEL%R(ITMP)  = TKE_DEF(IPLAN-1)
            EP_TEL%R(ITMP) = EPS_DEF(IPLAN-1)
          ENDDO
        ENDIF
      ! WRITE (LU,*) 'COPIED DATA BACK TO TELEMAC FORMAT.'
      ENDDO
!
      DEALLOCATE(DELTAS,NN,SS)
#else
      WRITE (LU,*) 'ERROR: CODE IS NOT COMPILED WITH GOTM LIBRARY'
      WRITE (LU,*) 'MAYBE FORGOTTEN TO COMPILE WITH -DHAVE_GOTM'
      CALL PLANTE(1)
#endif
      END SUBROUTINE GOTM_COUPLING_STEP
!                   ***********************
                    SUBROUTINE DEALLOC_GOTM
!                   ***********************
!
!***********************************************************************
! TELEMAC3D   V8P1
!***********************************************************************
!
!brief    Deallocation of memory used by GOTM
!
!history  WA BREUGEM (IMDC) + C.-T. PHAM (LNHE)
!+        24/07/2019
!+        V8P1
!+
!+        From original GOTM_COUPLING_END
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
#ifdef HAVE_GOTM
      CALL CLEAN_TURBULENCE()
      IF(ALLOCATED(TKE_DEF)) DEALLOCATE(TKE_DEF)
      IF(ALLOCATED(EPS_DEF)) DEALLOCATE(EPS_DEF)
      IF(ALLOCATED(NUH_DEF)) DEALLOCATE(NUH_DEF)
      IF(ALLOCATED(NUM_DEF)) DEALLOCATE(NUM_DEF)
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE DEALLOC_GOTM

      END MODULE
