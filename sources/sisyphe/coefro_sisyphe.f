!                   *************************
                    SUBROUTINE COEFRO_SISYPHE
!                   *************************
!
     &(CF,H,KFROT,CHESTR,GRAV,NPOIN,HMIN,KARMAN)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE QUADRATIC FRICTION COEFFICIENT CF.
!
!history  C. VILLARET (LNHE)
!+        01/10/2003
!+        V5P4
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |<->| FRICTION COEFFICIENT
!| CHESTR         |-->| FRICTION COEFFICIENTS (BED)
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HMIN           |-->| MINIMUM VALUE OF WATER DEPTH
!| HN             |-->| WATER DEPTH
!| KARMAN         |-->| VON KARMAN CONSTANT
!| KFROT          |-->| FRICTION LAW (BED)
!| NPOIN          |-->| NUMBER OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN):: NPOIN,KFROT
      DOUBLE PRECISION,INTENT(IN):: GRAV,KARMAN,HMIN
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CF
      TYPE(BIEF_OBJ),INTENT(IN) :: CHESTR,H
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N
      DOUBLE PRECISION HC, AUX, TIERS,ZERO
      INTRINSIC MAX,LOG
!
!-----------------------------------------------------------------------
!
      TIERS  = 1.D0/3.D0
      ZERO = 1.D-6
!
!  CONSTRUCTION OF THE FRICTION COEFFICIENT
!
!     FRICTION LAWS:
!
!     KFROT = 0 :  FLAT BOTTOM  (KS=3D50)
!     KFROT = 1 :  EQUILIBRIUM SAND RIPPLES (WAVES ONLY) KS=(MAX 3D50,ETA)
!     KFROT = 2 :  CHEZY
!     KFROT = 3 :  STRICKLER
!     KFROT = 4 :  MANNING
!     KFROT = 5 :  NIKURADSE
!
      DO N=1,NPOIN
        IF(CHESTR%R(N).LE.0.D0) THEN
          WRITE(LU,*) 'FROTTEMENT NON DEFINI DANS COEFRO AU POINT ',N
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!     ***********************
      IF(KFROT.EQ.5) THEN
!    ***********************
!          AUX=30.D0/EXP(1.D0) =11.036D0
        DO N=1,NPOIN
            AUX = MAX(1.001D0,H%R(N)*11.036D0/CHESTR%R(N))
            CF%R(N) = 2.D0 / (LOG( AUX)/KARMAN )**2
        ENDDO
!     ***********************
      ELSEIF(KFROT.EQ.2) THEN
!     ***********************
!
        DO N=1,NPOIN
          CF%R(N) = 2.D0 * GRAV / CHESTR%R(N)**2
        ENDDO
!
!     ***********************
      ELSEIF(KFROT.EQ.3) THEN
!     ***********************
!
        DO N=1,NPOIN
          HC = MAX(H%R(N),HMIN)
          CF%R(N) = 2.D0 * GRAV / CHESTR%R(N)**2 / HC**TIERS
        ENDDO
!
!     ***********************
      ELSEIF(KFROT.EQ.4) THEN
!     ***********************
!
        DO N=1,NPOIN
          HC = MAX(H%R(N),HMIN)
          CF%R(N) = 2.D0 * CHESTR%R(N)**2 * GRAV / HC**TIERS
        ENDDO
!
!     ****
      ELSE
!     ****
!
        WRITE(LU,301) KFROT
301     FORMAT(1X,'COEFRO: UNKNOWN LAW OF BOTTOM FRICTION: ',1I6)
        CALL PLANTE(1)
        STOP
!
!     *****
      ENDIF
!     *****
!
!-----------------------------------------------------------------------
!
      RETURN
      END
