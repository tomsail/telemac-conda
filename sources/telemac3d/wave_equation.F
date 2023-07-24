!                   ************************
                    SUBROUTINE WAVE_EQUATION
!                   ************************
!
     &(ISOUSI)
!
!***********************************************************************
! TELEMAC3D   V7P2
!***********************************************************************
!
!brief    DIFFUSION AND PROPAGATION STEP IN 3D USING THE WAVE
!+                EQUATION METHOD.
!
!history  JMH
!+        23/01/2009
!+
!+   SUMS FRICTION TERMS IN T3_04 ;
!
!history  JMH
!+        27/07/2009
!+
!+   MODIFIED TREATMENT OF FRICTION TERMS ON DRY ZONES ;
!
!history  JMH
!+        18/08/2009
!+
!+   COMPUTES UCONVC AND VCONVC AT THE END (SEE PRECON)
!
!history  JMH
!+        20/08/2009
!+
!+   NOW COMPUTES UNSV3D IN MESH_PROP
!
!history  J.M. HERVOUET (LNHE)
!+        05/05/2010
!+        V6P0
!+   MODIFIED CASE DPWAVEQ (SECOND COMPUTATION OF
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
!history  J-M HERVOUET (LNHE)
!+        15/09/2011
!+        V6P1
!+   Call to NUWAVE_P0 modified
!
!history  J-M HERVOUET (LNHE)
!+        02/04/2012
!+        V6P2
!+   Initialization of DH moved to telemac3d.f
!
!history  J-M HERVOUET (LNHE)
!+        19/09/2012
!+        V6P3
!+   Using S0U is now double-checked by the advection scheme to know if
!+   S0U has been treated before. S0U%TYPR is no longer cancelled in
!+   CVDF3D, otherwise S0U is forgotten if there are iterations for non
!+   linearities.
!
!history  J-M HERVOUET (LNHE)
!+        31/07/2014
!+        V7P0
!+   Correction in case of atmospheric pressure. Free surface and
!+   atmospheric pressure did not balance their gradients exactly when
!+   the free surface gradient compatibility was not 1. Now
!+   PATMOS/(RO*G) is added to the free surface also for the non
!+   compatible part.
!
!history  J-M HERVOUET (LNHE)
!+        27/07/2015
!+        V7P1
!+   In parallel SEM2D must be non assembled but it was initialised with
!+   SMH (assembled in parallel). SEM2D is now shared again after copy
!+   of SMH.
!
!history  J-M HERVOUET (LNHE)
!+        27/06/2016
!+        V7P2
!+   Changing the call to VELRES and the memory for UAUX and VAUX, now
!+   taken in the diagonals of MTRA1 and MTRA2.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ISOUSI         |-->| RANK OF CURRENT SUB-ITERATION
!| LT             |-->| CURRENT TIME STEP NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_WAVE_EQUATION => WAVE_EQUATION
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      USE METEO_TELEMAC, ONLY: PATMOS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: ISOUSI
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=16) :: FORMUL
      INTEGER           :: I,IPTFR3,IPOIN2,IPLAN,I1,I2,I3,NP
      INTEGER           :: I3D,IP
      DOUBLE PRECISION  :: C
!
!-----------------------------------------------------------------------
!
!     DEFINES POINTERS TO RENAME PARTS OF MEMORY
!     MTRA2%X%R HAS AT LEAST THE SIZE 30*NELMAX (STORAGE 1)
!                                  OR  2*NSEG3D (STORAGE 3)
!
!     WE NEED: 8*NPOIN
!           OR 8*NPOIN
!
!
      DOUBLE PRECISION, POINTER :: TRIC(:),TRID(:),TRIE(:)
      DOUBLE PRECISION, POINTER :: UAUX(:),VAUX(:)
      TRIC=>MTRA2%X%R(          1:   NPOIN3)
      TRID=>MTRA2%X%R(   NPOIN3+1: 2*NPOIN3)
      TRIE=>MTRA2%X%R( 2*NPOIN3+1: 3*NPOIN3)
!         =>MTRA2%X%R( 3*NPOIN3+1: 4*NPOIN3) USED IN TRID3D
!         =>MTRA2%X%R( 4*NPOIN3+1: 5*NPOIN3) USED IN TRID3D
      UAUX=>MTRA1%D%R(          1:   NPOIN3)
      VAUX=>MTRA2%D%R(          1:   NPOIN3)
!
      IF(7*NPOIN3.GT.30*MESH3D%NELMAX.OR.
     &   7*NPOIN3.GT.2*MESH3D%NSEG) THEN
        WRITE(LU,*) 'PROBLEME DE PLACE MEMOIRE DANS WAVE_EQUATION'
        WRITE(LU,*) 'NPOIN3=',NPOIN3
        WRITE(LU,*) 'NELMAX=',MESH3D%NELMAX
        WRITE(LU,*) 'NSEG=',MESH3D%NSEG
        CALL PLANTE(1)
        STOP
      ENDIF
!
!=======================================================================
!     COMPUTES THE DIFFUSION TERMS DIFF
!
!       AND THEN UC + DT(F - DIFF -G GRAD(Z))
!
!       STORED IN T3_01 AND T3_02
!
!=======================================================================
!
      FORMUL='MATDIF          '
!     TODO: NEXT LINE TO TEST, IT WOULD BE CONSISTENT WITH W IN CVDF3D
!     FORMUL='MATDIF       MON'
      IF(INCHYD) FORMUL(7:7)='2'
!     NOTE: COULD BE OPTIMISED IF SCHDVI=0
      CALL MATRIX(MDIFF,'M=N     ',FORMUL,IELM3,IELM3,1.D0,
     &            VISCVI%ADR(1)%P,VISCVI%ADR(2)%P,VISCVI%ADR(3)%P,
     &            SVIDE,SVIDE,SVIDE,MESH3D,MSK,MASKEL)
!
!     IMPLICITATION OF DIAGONAL TERMS
!     BUILDS A TRIDIAGONAL MATRIX IN OFF-DIAGONAL TERMS OF MTRA2
!
      CALL GETTRI(MTRA2%X%R,MDIFF,TETADI,MESH3D,NPLAN,MESH2D%NPOIN,
     &            MESH2D%NSEG,IELM3,NELEM2)
!
      DO I=1,U%DIM1
        TRIC(I)=TRIC(I)*UNSV3D%R(I)*DT
        TRID(I)=TRID(I)*UNSV3D%R(I)*DT
        TRIE(I)=TRIE(I)*UNSV3D%R(I)*DT
      ENDDO
!
!     EXPLICIT DIFFUSION TERMS
!
      CALL MATVEC ('X=AY     ',T3_01,MDIFF,UN,0.D0,MESH3D)
      CALL MATVEC ('X=AY     ',T3_02,MDIFF,VN,0.D0,MESH3D)
!
!     EXPLICIT STRESS TERMS
!
      CALL T3D_STRESS(T3_01,'X=X-Y   ',T2_02,T3_04,
     &                BUBORL,BUBORF,BUBORS,NPOIN2,NPOIN3,MESH2D,
     &                MESH3D,IELM2H,IELM2V,SVIDE,
     &                MSK,MASKBR,MASKEL,IPBOT%I,SIGMAG,OPTBAN,NPLAN)
      CALL T3D_STRESS(T3_02,'X=X-Y   ',T2_02,T3_04,
     &                BVBORL,BVBORF,BVBORS,NPOIN2,NPOIN3,MESH2D,
     &                MESH3D,IELM2H,IELM2V,SVIDE,
     &                MSK,MASKBR,MASKEL,IPBOT%I,SIGMAG,OPTBAN,NPLAN)
!
!     REQUIRES REAL VALUES IN PARALLEL MODE
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(T3_01,2,MESH3D)
        CALL PARCOM(T3_02,2,MESH3D)
      ENDIF
!
!     DIVIDES BY VOLUME OF BASES
!
      CALL OS('X=XY    ',X=T3_01,Y=UNSV3D)
      CALL OS('X=XY    ',X=T3_02,Y=UNSV3D)
!
!     COMPUTES UC + DT(F - DIFF -G GRAD(Z))
!     STARTS THE COMPUTATION OF UAUX AND VAUX
!
!     NEW IN VERSION 5.8 : SUPG SCHEME IS POSSIBLE
!                          AND RESULT OF ADVECTION IS THEN IN UD AND VD
!                          AFTER CALL TO CVDF3D (BUT DIFFUSION AND
!                          SOURCES TERMS NOT DONE IN CVDF3D, SEE
!                          SCHDVI_HOR AND YAS0U, YAS1U IN TELEMAC3D.F)
!
      IF(SCHCVI.EQ.ADV_SUP) THEN
        CALL OS('X=Y     ',X=UC,Y=UD)
        CALL OS('X=Y     ',X=VC,Y=VD)
      ENDIF
!
      IF(  (SCHCVI.NE.ADV_NSC.AND.
     &      SCHCVI.NE.ADV_PSI.AND.
     &      SCHCVI.NE.ADV_LPO.AND.
     &      SCHCVI.NE.ADV_NSC_TF.AND.
     &      SCHCVI.NE.ADV_LPO_TF).AND.S0U%TYPR.NE.'0') THEN
!       CASES WHERE S0U MUST BE TAKEN INTO ACCOUNT:
!       IT IS NOT 0 AND IT HAS NOT BEEN TREATED BY THE ADVECTION SCHEME
!       (SO FAR ACTUALLY CASES ADV_SUPG AND ADV_CAR)
        DO I=1,U%DIM1
          I2=MOD(I-1,NPOIN2)+1
          T3_01%R(I)=UC%R(I)
     &    +DT*(S0U%R(I)-T3_01%R(I)-TETAZCOMP*GRAV*GRADZN%ADR(1)%P%R(I2))
          T3_02%R(I)=VC%R(I)
     &    +DT*(S0V%R(I)-T3_02%R(I)-TETAZCOMP*GRAV*GRADZN%ADR(2)%P%R(I2))
        ENDDO
      ELSE
!       FORMULA WITHOUT S0U
        DO I=1,U%DIM1
          I2=MOD(I-1,NPOIN2)+1
          T3_01%R(I)=UC%R(I)
     &    +DT*(-T3_01%R(I)-TETAZCOMP*GRAV*GRADZN%ADR(1)%P%R(I2))
          T3_02%R(I)=VC%R(I)
     &    +DT*(-T3_02%R(I)-TETAZCOMP*GRAV*GRADZN%ADR(2)%P%R(I2))
        ENDDO
      ENDIF
!
!=======================================================================
!     COMPUTES 1/(1+DT*(FROT3D+S1U))      IN T3_04
!
!       IT IS ASSUMED THAT S1V=S1U         (IT SHOULD FOR TENSORIALITY)
!                     THAT AVBORL=AUBORL
!                     THAT AVBORF=AUBORF
!                     THAT AVBORS=AUBORS
!
!=======================================================================
!
!     COMPUTES THE FRICTION TERM FROT3D + IMPLICIT SOURCES
!
      CALL CPSTVC(S1U,T3_04)
!     ERASES ALL VALUES (ONLY BOUNDARY VALUES WILL BE CHANGED BELOW)
      CALL OS('X=0     ',X=T3_04)
!
!     BOTTOM (MASS-LUMPED FORM AS IN 2D):
!
      IF(AUBORF%TYPR.NE.'0') THEN
!       VERSION WITH 1/COS(SLOPE) (OTHERWISE USE VOLU2D INSTEAD OF T2_01)
        CALL SLOPES(TE1,ZF,MESH2D)
        CALL VECTOR(T2_01,'=','MASBAS          ',IELM2H,1.D0,SVIDE,
     &              SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH2D,.TRUE.,TE1)
!       IF(NCSIZE.GT.1) DONE ON T3_04 AT THE END
!
        IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
!         TREATS CRUSHED LAYERS AND TIDAL FLATS IN THE SAME WAY
          DO IPOIN2=1,NPOIN2
            DO NP=0,IPBOT%I(IPOIN2)
              I=NP*NPOIN2+IPOIN2
              T3_04%R(I)=-AUBORF%R(IPOIN2)*T2_01%R(IPOIN2)
            ENDDO
          ENDDO
        ELSE
          DO I=1,NPOIN2
            T3_04%R(I)=-AUBORF%R(I)*T2_01%R(I)
          ENDDO
        ENDIF
      ENDIF
!
!     LATERAL FACES (MASS-LUMPED FORM)
!
      IF(AUBORL%TYPR.NE.'0') THEN
        CALL VECTOR(T3_06, '=','MASBAS          ',IELM2V,+1.D0,SVIDE,
     &              SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,MSK, MASKEL)
!       IF(NCSIZE.GT.1) : DONE ON THE FINAL T3_04
        CALL OSDB( 'X=X-YZ  ' ,T3_04,AUBORL,T3_06,C,MESH3D)
      ENDIF
!
!     SURFACE (MASS-LUMPED FORM):
!
      IF(AUBORS%TYPR.NE.'0') THEN
        CALL OV('X=X-YZ  ', X=T3_04%R(NPOIN3-NPOIN2+1:NPOIN3),
     &                     Y=AUBORS%R, Z=VOLU2D%R, DIM1=NPOIN2)
      ENDIF
!
      IF(NCSIZE.GT.1) CALL PARCOM(T3_04,2,MESH3D)
!
!     COMPUTES THE INVERSE OF THE DENOMINATOR FOR U(N+1) AND V(N+1)
!
      IF(S1U%TYPR.NE.'0') THEN
        DO I=1,U%DIM1
          TRID(I)=TRID(I)+1.D0+DT*(S1U%R(I)+T3_04%R(I)*UNSV3D%R(I))
        ENDDO
      ELSE
        DO I=1,U%DIM1
          TRID(I)=TRID(I)+1.D0+DT*(T3_04%R(I)*UNSV3D%R(I))
        ENDDO
      ENDIF
!
!     COMPUTES THE SOLUTION OF TRI * X = UNITY VECTOR EVERYWHERE
!     PUT IN INV1
!
      CALL OS('X=C     ',X=T3_04,C=1.D0)
      CALL TRID3D(MTRA2%X%R,DM1%R,T3_04%R,NPOIN3,NPOIN2)
!
!     LATERAL BOUNDARY CONDITION: CANCELS DM1 FOR THE VELOCITY DIRICHLET
!
      DO IPTFR3 = 1,NPTFR3
        IF(LIUBOL%I(IPTFR3).EQ.KENT.OR.
     &     LIUBOL%I(IPTFR3).EQ.KENTU.OR.LIUBOL%I(IPTFR3).EQ.KADH) THEN
          DM1%R(MESH3D%NBOR%I(IPTFR3)) = 0.D0
        ENDIF
      ENDDO
!
!=======================================================================
!     COMPUTES THE NEW DEPTH WITH WAVE EQUATION
!=======================================================================
!
!     STARTS COMPUTATION OF THE SECOND MEMBER (IN SEM2D%ADR(1)%P)
!
      CALL OS('X=Y     ',X=SEM2D%ADR(1)%P,Y=SMH)
!
!     SMH IS ASSEMBLED BUT SEM2D MUST NOT, IT IS SHARED AGAIN HERE.
!     NOT VERY ELEGANT BUT EFFECTIVE...
!
      IF(NCSIZE.GT.1) THEN
        DO I=1,NPOIN2
          SEM2D%ADR(1)%P%R(I)=SEM2D%ADR(1)%P%R(I)*MESH2D%IFAC%I(I)
        ENDDO
      ENDIF
!
!     PSEUDO-VISCOSITY IN THE WAVE EQUATION (IN NUWAVE, P0 FUNCTION)
!
      CALL NUWAVE_P0(NUWAVE%R,DM1%R,Z,T3_03%R,MESH3D%IKLE%I,
     &               NPOIN2,NPLAN,MESH3D%NELEM,MESH3D%NELMAX,NELEM2,
     &               GRAV*TETAH*TETAU*DT,IELM3,MESH3D%X%R,MESH3D%Y%R,
     &               MESH2D%SURFAC%R)
!
!     CORRESPONDING DIFFUSION MATRIX
!
      CALL MATRIX(MAT2D%ADR(1)%P,'M=N     ','MATDIF          ',
     &            IELM2H,IELM2H,1.D0,SVIDE,SVIDE,SVIDE,
     &            NUWAVE,SVIDE,SVIDE,MESH2D,MSK,MASKEL)
!
!     STORES THIS MATRIX FOR THE COMPUTATION OF FLINT2
!
      CALL OM('M=N     ',M=MAT2D%ADR(2)%P,N=MAT2D%ADR(1)%P,MESH=MESH2D)
!
!     SEM2D%ADR(1)%P = SEM2D%ADR(1)%P + INTEGRAL ON OMEGA3D
!
!     3D VECTOR TO INTEGRATE (IN UCONV, VCONV)
!
!     COMPUTES UAUX BY SOLVING TRIDIAGONAL SYSTEMS
!
      CALL TRID3D(MTRA2%X%R,UAUX,T3_01%R,NPOIN3,NPOIN2)
      CALL TRID3D(MTRA2%X%R,VAUX,T3_02%R,NPOIN3,NPOIN2)
!
!     TAKES THE PRESSURE GRADIENT INTO ACCOUNT
!
      IF(NONHYD.AND.DPWAVEQ) THEN
!
!       COMPUTES AN ESTIMATE OF THE DYNAMIC PRESSURE WITH UAUX TAKEN
!       AS U(N+1). THIS ESTIMATE WILL BE THE RESULT GIVEN IN THE RESULT
!       FILE, AS DP IN THE SECOND CALL TO PREDIV IS (ONLY) INCREMENTAL.
!
        CALL CPSTVC(UN,T3_04)
        CALL CPSTVC(VN,T3_05)
        CALL CPSTVC(WD,T3_06)
        CALL OV('X=Y     ', X=T3_04%R, Y=UAUX, DIM1=NPOIN3)
        CALL OV('X=Y     ', X=T3_05%R, Y=VAUX, DIM1=NPOIN3)
        CALL OS('X=Y     ',X=T3_06,Y=WD)
!       NON COMPATIBLE PART OF FREE SURFACE GRADIENT
!      (WHICH IS NOT IN UAUX, SEE ALSO FINAL COMPUTATION OF U AND V)
        IF(ABS(1.D0-TETAZCOMP).GT.1.D-6) THEN
          C=-DT*GRAV*(1.D0-TETAZCOMP)
          DO I=1,U%DIM1
            T3_04%R(I)=T3_04%R(I)+
     &      C*GRADZN%ADR(1)%P%R(MOD(I-1,NPOIN2)+1)*DM1%R(I)
            T3_05%R(I)=T3_05%R(I)+
     &      C*GRADZN%ADR(2)%P%R(MOD(I-1,NPOIN2)+1)*DM1%R(I)
          ENDDO
        ENDIF
!
!       TAKES DH INTO ACCOUNT, IF KNOWN (I.E. FROM 2ND SUBITERATIONS ON)
!       TAKING DH FROM PREVIOUS TIMESTEP IS NOT A GOOD IDEA
!
        IF(ISOUSI.GT.1) THEN
          CALL VECTOR(T2_02,'=','GRADF          X',
     &                IELM2H,-GRAV*TETAH,DH,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &                MESH2D,MSK,MASKEL)
          CALL VECTOR(T2_03,'=','GRADF          Y',
     &                IELM2H,-GRAV*TETAH,DH,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &                MESH2D,MSK,MASKEL)
          IF(NCSIZE.GT.1) THEN
            CALL PARCOM(T2_02,2,MESH2D)
            CALL PARCOM(T2_03,2,MESH2D)
          ENDIF
          CALL OS('X=XY    ',X=T2_03,Y=UNSV2D)
          CALL OS('X=XY    ',X=T2_02,Y=UNSV2D)
          DO I=1,U%DIM1
            T3_04%R(I)=T3_04%R(I)+DT*T2_02%R(MOD(I-1,NPOIN2)+1)*DM1%R(I)
            T3_05%R(I)=T3_05%R(I)+DT*T2_03%R(MOD(I-1,NPOIN2)+1)*DM1%R(I)
          ENDDO
        ENDIF
!
!       APPLIES LATERAL BOUNDARY CONDITIONS
!       NOT VERY SIGNIFICANT
!
!       DO IPTFR3 = 1,NPTFR3
!         IF(LIUBOL%I(IPTFR3).EQ.KENT .OR.
!    *       LIUBOL%I(IPTFR3).EQ.KENTU.OR.
!    *       LIUBOL%I(IPTFR3).EQ.KADH) THEN
!            T3_04%R(MESH3D%NBOR%I(IPTFR3)) = UBORL%R(IPTFR3)
!         ENDIF
!         IF(LIVBOL%I(IPTFR3).EQ.KENT .OR.
!    *       LIVBOL%I(IPTFR3).EQ.KENTU.OR.
!    *       LIVBOL%I(IPTFR3).EQ.KADH) THEN
!            T3_05%R(MESH3D%NBOR%I(IPTFR3)) = VBORL%R(IPTFR3)
!         ENDIF
!       ENDDO
!
!       BEWARE: PREDIV WILL ERASE ALL T3_** WORK ARRAYS BECAUSE CALLS SOLVE
        CALL PREDIV(DP,T3_04,T3_05,T3_06,INFOGR,.TRUE.,1,
     &              .TRUE.,.TRUE.,.TRUE.)
!       APPLIES CORRECTION TO UAUX, VAUX AND WD
!       UAUX VAUX HAVE BEEN PUT IN MTRA1%D AND MTRA2%D, BUT THE STRUCTURES ARE SECURED HERE
        CALL CPSTVC(WD,MTRA1%D)
        CALL CPSTVC(WD,MTRA2%D)
        CALL VELRES(MTRA1%D,MTRA2%D,WD,DP,
     &              T3_08,T3_09,T3_10,MSK,MASKEL,MESH3D,
     &              SVIDE,IELM3,OPTBAN,UNSV3D,.FALSE.,
     &              NPOIN3,NPOIN2,
     &              SIGMAG,IPBOT%I,AGGLOH,KSORT,NPTFR3,LIUBOL,CONCOR)
!
      ENDIF
!
!     END OF 'TAKES THE PRESSURE GRADIENT INTO ACCOUNT'
!
!
!     LATERAL BOUNDARY CONDITION IN UAUX AND VAUX
!
      DO IPTFR3 = 1,NPTFR3
        IF(LIUBOL%I(IPTFR3).EQ.KENT.OR.
     &     LIUBOL%I(IPTFR3).EQ.KENTU.OR.LIUBOL%I(IPTFR3).EQ.KADH) THEN
          UAUX(MESH3D%NBOR%I(IPTFR3)) = UBORL%R(IPTFR3)
        ENDIF
        IF(LIVBOL%I(IPTFR3).EQ.KENT.OR.
     &     LIVBOL%I(IPTFR3).EQ.KENTU.OR.LIVBOL%I(IPTFR3).EQ.KADH) THEN
          VAUX(MESH3D%NBOR%I(IPTFR3)) = VBORL%R(IPTFR3)
        ENDIF
      ENDDO
!
!     COMPUTES TETAU * UAUX + (1-TETAU) * UN
!
      DO I=1,U%DIM1
        UCONV%R(I)=TETAU*UAUX(I)+(1.D0-TETAU)*UN%R(I)
        VCONV%R(I)=TETAU*VAUX(I)+(1.D0-TETAU)*VN%R(I)
      ENDDO
!
!     SEM2D%ADR(1)%P = SEM2D%ADR(1)%P - FLUX2D
!
!     UNONNEU=8 : 1 IF NOT A WALL
!     CALL EXTMSK(MASKBR,MASK%ADR(8)%P%R,MESH2D%NPTFR,NETAGE)
!     CALL VECTOR(T3_06,'=','FLUBOR          ',IELBOR(IELM3,2),
!    &            1.D0,SVIDE,SVIDE,SVIDE,UCONV,VCONV,SVIDE,
!    &            MESH3D,.TRUE.,MASKBR)
      CALL VECTOR(T3_06,'=','FLUBOR          ',IELBOR(IELM3,2),
     &            1.D0,SVIDE,SVIDE,SVIDE,UCONV,VCONV,SVIDE,
     &            MESH3D,.TRUE.,MASK_3D%ADR(8)%P)
!
      CALL SUMVER(FLBOR%R,T3_06%R,NPLAN,MESH2D%NPTFR)
!
      CALL OSDB( 'X=X-Y   ',SEM2D%ADR(1)%P,FLBOR,FLBOR,C,MESH2D)
!
!     MULTIPLIES BY THE GRADIENT OF THE 3D BASES
!
      FORMUL = 'VGRADP       HOR'
      CALL VECTOR(T3_01,'=',FORMUL,IELM3,1.D0,SVIDE,SVIDE,SVIDE,
     &            UCONV,VCONV,SVIDE,MESH3D,MSK,MASKEL)
!
!     SUM ON THE VERTICAL
!     FLINT2 WILL BE ADDED TO SEM2D, BUT MAY BE USED TO CHECK IN TRIDW2
!     THAT THE SUM ON THE VERTICAL OF FLUINT = FLINT2
!
      CALL OS('X=0     ',X=FLINT2)
      DO IPLAN=1,NPLAN
        DO I=1,NPOIN2
          FLINT2%R(I)=FLINT2%R(I)+T3_01%R(I+(IPLAN-1)*NPOIN2)
        ENDDO
      ENDDO
!
!=======================================================================
!     CONTRIBUTION OF NON COMPATIBLE LAPLACIAN
!     SEE ALSO MODIFICATION OF ZCONV LATER ON
!=======================================================================
!
      IF(ABS(1.D0-TETAZCOMP).GT.1.D-6) THEN
!
!       ADDS NON COMPATIBLE LAPLACIAN
!       FOR THE CONTRIBUTION OF EXPLICIT FREE-SURFACE
!       TO THE VELOCITY
!       TETAZCOMP=1 : COMPATIBLE
!       TETAZCOMP=0 : NON COMPATIBLE
!
!       ADDS THE NON COMPATIBLE EXPLICIT FREE SURFACE GRADIENT
        CALL CPSTVC(H,T2_04)
        CALL CPSTVC(H,T2_05)
        IF(OPTBAN.EQ.1) THEN
!         FREE SURFACE PIECE-WISE LINEAR IN ZFLATS
          CALL VECTOR(FLINT2,'+','VGRADP 2        ',IELM2H,
     &                -(1.D0-TETAZCOMP)/TETAH,
     &                SVIDE,SVIDE,SVIDE,NUWAVE,ZFLATS,SVIDE,
     &                MESH2D,MSK,MASKEL)
        ELSE
!         FREE SURFACE LINEAR IN T2_04
          DO I=1,NPOIN2
            T2_04%R(I)=HN%R(I)+ZF%R(I)
          ENDDO
          IF(ATMOS) THEN
!         ADDING ATMOSPHERIC PRESSURE
            C=1.D0/(RHO0*GRAV)
            DO I=1,NPOIN2
              T2_04%R(I)=T2_04%R(I)+C*PATMOS%R(I)
            ENDDO
          ENDIF
          CALL VECTOR(FLINT2,'+','VGRADP 2        ',IELM2H,
     &                -(1.D0-TETAZCOMP)/TETAH,
     &                SVIDE,SVIDE,SVIDE,NUWAVE,T2_04,SVIDE,
     &                MESH2D,MSK,MASKEL)
        ENDIF
!
      ENDIF
!
      CALL OS('X=X+Y   ',X=SEM2D%ADR(1)%P,Y=FLINT2)
!
!=======================================================================
!     ADDS THE MASS MATRIX (LUMPED OR NOT) TO THE SYSTEM MATRIX
!     SOLVES THE EQUATION
!=======================================================================
!
!     COMPUTES THE PARTIALLY LUMPED 2D MASS MATRIX
!
      FORMUL='MATMAS          '
!     NOTE: THERE IS LOCAL LUMPING IN PROPAG
!           ON THE TIDAL FLATS
      CALL MATRIX(MAT2D%ADR(3)%P,'M=N     ',FORMUL,IELM2H,IELM2H,
     &            1.D0/DT,
     &            SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH2D,MSK,MASKEL)
      CALL LUMP(T2_01,MAT2D%ADR(3)%P,MESH2D,AGGLOH)
      CALL OM('M=M+CN  ', M=MAT2D%ADR(1)%P, N=MAT2D%ADR(3)%P,
     &        C=1.D0-AGGLOH, MESH=MESH2D)
      CALL OM('M=M+D   ', M=MAT2D%ADR(1)%P, D=T2_01, MESH=MESH2D)
!
!     INITIAL GUESS FOR DH
!
      IF(IORDRH.EQ.0) THEN
        CALL OS('X=0     ',X=DH)
      ELSEIF(IORDRH.EQ.1) THEN
!       DONE IN TELEMAC3D.F
!       IF(LT.EQ.1.AND.ISOUSI.EQ.1) CALL OS('X=0     ',X=DH)
      ELSE
        WRITE(LU,31) IORDRH
31      FORMAT(1X,'WAVE_EQUATION: IORDRH=',1I6,' VALUE OUT OF RANGE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     SAVES THE ORIGINAL MATRIX, BEFORE DIRICHLETS AND PRECONDITIONING
!
      IF(OPTBAN.EQ.1.AND.OPT_HNEG.EQ.2.AND.NPTFR2.GT.0) THEN
        CALL OM('M=N     ', M=MAT2D%ADR(3)%P, N=MAT2D%ADR(1)%P,
     &          MESH=MESH2D)
      ENDIF
!
!     DIRICHLET + SOLVES THE SYSTEM
!
      CALL OSBD( 'X=X-Y   ' , HBOR , HN , HN , C , MESH2D )
      CALL DIRICH(DH,MAT2D%ADR(1)%P,SEM2D%ADR(1)%P,
     &            HBOR,LIHBOR%I,TRAV2,MESH2D,KENT,MSK,MASKPT)
#if defined COMPAD
      CALL AD_SOLVE(DH,MAT2D%ADR(1)%P,SEM2D%ADR(1)%P,
     &           TRAV2,SLVPRO,INFOGR,MESH2D,MAT2D%ADR(2)%P)
#else
      CALL SOLVE(DH,MAT2D%ADR(1)%P,SEM2D%ADR(1)%P,
     &           TRAV2,SLVPRO,INFOGR,MESH2D,MAT2D%ADR(2)%P)
#endif
      CALL OSBD( 'X=X+Y   ' , HBOR , HN , HN , C , MESH2D )
!
!     RECOVERS THE NEW DEPTH
!
      CALL OS('X=Y+Z   ',X=H,Y=HN,Z=DH)
!
!     COMPLETES THE 2D INTERNAL FLUXES
!     COMPATIBLE WITH CONTINUITY EQUATION
!
!     BOUNDARY FLUXES THAT SOLVE THE CONTINUITY EQUATION
!     WHEN DEPTHS ARE PRESCRIBED
!
      IF(OPTBAN.EQ.1.AND.OPT_HNEG.EQ.2.AND.NPTFR2.GT.0) THEN
        CALL MATVEC('X=AY    ',T2_01,MAT2D%ADR(3)%P,DH,1.D0,MESH2D)
        DO I=1,NPTFR2
          IF(LIHBOR%I(I).EQ.KENT) THEN
            FLBOR%R(I)=FLINT2%R(NBOR2%I(I))-T2_01%R(NBOR2%I(I))
          ENDIF
        ENDDO
      ENDIF
!
!     UNCOMMENT THIS LINE
!     TO CHECK THAT SUM OF FLUINT = FLINT2 (IN TRIDW2)
!
!     CALL MATVEC ('X=X+CAY  ',FLINT2,MAT2D%ADR(2)%P,DH,-1.D0,MESH2D)
!
!=======================================================================
!     PREPARING ZCONV AND GRAZCO
!=======================================================================
!
      CALL MAKE_ZCONV(ZCONV,GRAZCO,ZFLATS,DH,HN,ZF,TETAZCOMP,TETAH,
     &                NELEM2,NELMAX2,OPTBAN,MESH2D%IKLE%I,MESH2D)
!
!=======================================================================
!     COMPUTES NEW VELOCITIES
!=======================================================================
!
!     GRADIENT OF DH (IN T2_02 AND T2_03)
!
!     COMPONENT X (IN T2_02)
!
      CALL VECTOR
     & (T2_02,'=','GRADF          X',IELM2H,-GRAV*TETAH,DH,SVIDE,SVIDE,
     &  SVIDE,SVIDE,SVIDE, MESH2D, MSK, MASKEL)
      IF (NCSIZE.GT.1) CALL PARCOM(T2_02,2,MESH2D)
      CALL OS ( 'X=XY    ' ,X=T2_02,Y=UNSV2D)
!
!     COMPONENT Y (IN T2_03)
!
      CALL VECTOR
     & (T2_03,'=','GRADF          Y',IELM2H,-GRAV*TETAH,DH,SVIDE,SVIDE,
     &  SVIDE,SVIDE,SVIDE, MESH2D, MSK, MASKEL)
      IF(NCSIZE.GT.1) CALL PARCOM(T2_03,2,MESH2D)
      CALL OS ( 'X=XY    ' ,X=T2_03,Y=UNSV2D)
!
!     THE NON COMPATIBLE PART OF THE LAPLACIAN FOR THE FREE SURFACE
!     GRADIENT HAS NOT BEEN PUT IN UAUX, IT IS ADDED HERE IN A
!     COMPATIBLE WAY TO COMPUTE U AND V
!
      IF(ABS(1.D0-TETAZCOMP).GT.1.D-6) THEN
        C=-GRAV*(1.D0-TETAZCOMP)
        CALL OS('X=X+CY  ',X=T2_02,Y=GRADZN%ADR(1)%P,C=C)
        CALL OS('X=X+CY  ',X=T2_03,Y=GRADZN%ADR(2)%P,C=C)
      ENDIF
!
!     FINAL COMPUTATION OF U AND V
!
      DO I=1,U%DIM1
        U%R(I)=UAUX(I)+DT*T2_02%R(MOD(I-1,NPOIN2)+1)*DM1%R(I)
        V%R(I)=VAUX(I)+DT*T2_03%R(MOD(I-1,NPOIN2)+1)*DM1%R(I)
      ENDDO
!
!     MODIFIES DM1 FOR USE IN PRECON, FLUX3D, ETC
!
      CALL OS('X=CX    ',X=DM1,C=-DT*GRAV*TETAH*TETAU)
!
!     DIRICHLET TYPE BOUNDARY CONDITIONS
!
!     LATERAL BOUNDARY CONDITION
!
      DO IPTFR3 = 1,NPTFR3
        IF(LIUBOL%I(IPTFR3).EQ.KENT.OR.
     &     LIUBOL%I(IPTFR3).EQ.KENTU.OR.LIUBOL%I(IPTFR3).EQ.KADH) THEN
          U%R(MESH3D%NBOR%I(IPTFR3)) = UBORL%R(IPTFR3)
        ENDIF
        IF(LIVBOL%I(IPTFR3).EQ.KENT.OR.
     &     LIVBOL%I(IPTFR3).EQ.KENTU.OR.LIVBOL%I(IPTFR3).EQ.KADH) THEN
          V%R(MESH3D%NBOR%I(IPTFR3)) = VBORL%R(IPTFR3)
        ENDIF
      ENDDO
!
!     BOTTOM AND SURFACE BOUNDARY CONDITION
!
      DO IPOIN2 = 1,NPOIN2
        IF(LIUBOF%I(IPOIN2).EQ.KENT.OR.LIUBOF%I(IPOIN2).EQ.KADH) THEN
          U%R(IPOIN2) = UBORF%R(IPOIN2)
        ENDIF
        IF(LIVBOF%I(IPOIN2).EQ.KENT.OR.LIVBOF%I(IPOIN2).EQ.KADH) THEN
          V%R(IPOIN2) = VBORF%R(IPOIN2)
        ENDIF
        IF(LIUBOS%I(IPOIN2).EQ.KENT.OR.LIUBOS%I(IPOIN2).EQ.KADH) THEN
          U%R(NPOIN3-NPOIN2+IPOIN2) = UBORS%R(IPOIN2)
        ENDIF
        IF(LIVBOS%I(IPOIN2).EQ.KENT.OR.LIVBOS%I(IPOIN2).EQ.KADH) THEN
          V%R(NPOIN3-NPOIN2+IPOIN2) = VBORS%R(IPOIN2)
        ENDIF
      ENDDO
!
!     PROJECTION ON SOLID BOUNDARIES (KLOG)
!
      IF(VELPROLAT) THEN
        CALL AIRWIK3(LIHBOR%I,U%R,V%R,MESH2D%XNEBOR%R,MESH2D%YNEBOR%R,
     &               NBOR2%I,NPTFR2,NPLAN,NPOIN2,KLOG)
      ENDIF
!
!     THIS SEQUENCE WILL BE DONE AFTER IF DYNAMIC PRESSURE HAS NOT BEEN
!     COMPUTED HERE
!
      IF(NONHYD) CALL OS ('X=Y     ', X=W , Y=WD  )
      IF(NONHYD.AND.DPWAVEQ) THEN
!       BOUNDARY CONDITION ON FREE SURFACE STRONGLY ENFORCED
        IF(CLDYN) THEN
          CALL OV('X=Y     ', X=W%R(NPOIN3-NPOIN2+1:NPOIN3), Y=DSSUDT%R,
     &                       DIM1=NPOIN2)
          CALL OV('X=X+YZ  ',X=W%R(NPOIN3-NPOIN2+1:NPOIN3),
     &                       Y=GRADZS%ADR(1)%P%R,
     &                       Z=U%R(NPOIN3-NPOIN2+1:NPOIN3),
     &                       DIM1=NPOIN2)
          CALL OV('X=X+YZ  ',X=W%R(NPOIN3-NPOIN2+1:NPOIN3),
     &                       Y=GRADZS%ADR(2)%P%R,
     &                       Z=V%R(NPOIN3-NPOIN2+1:NPOIN3),
     &                       DIM1=NPOIN2)
        ENDIF
!       BOUNDARY CONDITION ON BOTTOM STRONGLY ENFORCED
        IF(VELPROBOT) THEN
          IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
            DO I=1,NPOIN2
              DO IP=0,IPBOT%I(I)
                I3D=IP*NPOIN2+I
                W%R(I3D)=GRADZF%ADR(1)%P%R(I)*U%R(I3D)
     &                  +GRADZF%ADR(2)%P%R(I)*V%R(I3D)
              ENDDO
            ENDDO
          ELSE
            DO I=1,NPOIN2
              W%R(I)=GRADZF%ADR(1)%P%R(I)*U%R(I)
     &              +GRADZF%ADR(2)%P%R(I)*V%R(I)
            ENDDO
          ENDIF
        ENDIF
      ENDIF
!
!     VELOCITIES OF FIRST FREE POINT COPIED BELOW
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
        DO I2=1,NPOIN2
          IF(IPBOT%I(I2).GT.0) THEN
            I1=I2+IPBOT%I(I2)*NPOIN2
!           VALUE OF THE FIRST FREE POINT IS COPIED BELOW
            DO NP=0,IPBOT%I(I2)-1
              I3=I2+NP*NPOIN2
              U%R(I3)=U%R(I1)
              V%R(I3)=V%R(I1)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
!     COMPUTING WCONV, BEFORE MODIFICATION OF W BY PRESSURE,
!     IN ACCORDANCE WITH UCONV AND VCONV DONE BEFORE
!
      IF(NONHYD) THEN
        DO I=1,NPOIN3
          WCONV%R(I)=TETAU*W%R(I)+(1.D0-TETAU)*WN%R(I)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      CALL VERMOY(U2D%R,V2D%R,U%R,V%R,2,Z,
     &            T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,NPLAN,OPTBAN)
!
!-----------------------------------------------------------------------
!
!     CLASSICAL ADVECTION FIELD IS USED FOR CHARACTERISTICS
!     IT IS REBUILT HERE IN UCONVC AND VCONVC FOR USE IN PRECON
!
      IF(N_ADV(ADV_CAR).GT.0) THEN
        CALL OS( 'X=CY    ' , X=UCONVC, Y=UN , C=1.D0-TETAU )
        CALL OS( 'X=X+CY  ' , X=UCONVC, Y=U  , C=     TETAU )
        CALL OS( 'X=CY    ' , X=VCONVC, Y=VN , C=1.D0-TETAU )
        CALL OS( 'X=X+CY  ' , X=VCONVC, Y=V  , C=     TETAU )
      ENDIF
!
!     MERCATOR PROJECTION, ADVECTING FIELD MODIFIED FOR CHARACTERISTICS
!
      IF(SPHERI) THEN
        DO IPLAN=1,NPLAN
          DO I=1,NPOIN2
            I3D=(IPLAN-1)*NPOIN2+I
            UCONVC%R(I3D)=UCONVC%R(I3D)/MESH3D%COSLAT%R(I)
            VCONVC%R(I3D)=VCONVC%R(I3D)/MESH3D%COSLAT%R(I)
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

