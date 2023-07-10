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
      INTEGER           :: I
!
!=======================================================================
!    1) COMPUTES THE DIFFUSION TERMS DIFF
!
!       AND THEN UC + DT(F - DIFF -G GRAD(Z))
!
!       STORED IN T3_01 AND T3_02
!
!=======================================================================
!
      CALL OS('X=Y     ',X=UC,Y=UN)
      CALL OS('X=Y     ',X=VC,Y=VN)
!
      DO I=1,U%DIM1
        UCONV%R(I)=UN%R(I)
        VCONV%R(I)=VN%R(I)
      ENDDO
!
      CALL VECTOR(T3_06,'=','FLUBOR          ',IELBOR(IELM3,2),
     &            1.D0,SVIDE,SVIDE,SVIDE,UCONV,VCONV,SVIDE,
     &            MESH3D,.TRUE.,MASK_3D%ADR(8)%P)
!
      CALL SUMVER(FLBOR%R,T3_06%R,NPLAN,MESH2D%NPTFR)
!
      CALL OS('X=0     ',X=DH)
!
      CALL MAKE_ZCONV(ZCONV,GRAZCO,ZFLATS,DH,HN,ZF,TETAZCOMP,TETAH,
     &                NELEM2,NELMAX2,OPTBAN,MESH2D%IKLE%I,MESH2D)
!
      CALL OS('X=0     ',X=DM1)
!
      IF(NONHYD) CALL OS ('X=0     ', X=WD)
      IF(NONHYD) CALL OS ('X=0     ', X=W)
!
      IF(NONHYD) THEN
        DO I=1,NPOIN3
          WCONV%R(I)=WN%R(I)
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
!-----------------------------------------------------------------------
!
      RETURN
      END

