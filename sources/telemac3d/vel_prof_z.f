!                   ************************************
                    DOUBLE PRECISION FUNCTION VEL_PROF_Z
!                   ************************************
!
     &( I , IPOIN2 , IPLAN , IOPT )
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    GIVES THE VELOCITY VERTICAL PROFILE AT ENTRANCES.
!+
!+            THIS PROFILE IS LOGARITHMIC AND DESIGNED SO THAT THE
!+                INTEGRAL ON THE VERTICAL EQUALS THE DEPTH.
!
!history  J-M HERVOUET (LNHE)
!+        01/09/06
!+        V5P7
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
!| I              |-->| NUMBER OF THE LIQUID BOUNDARY.
!| IOPT           |-->| OPTION FOR THE VELOCITY PROFILE
!| IPLAN          |-->| PLAN NUMBER
!| IPOIN2         |-->| 2D GLOBAL NUMBER OF POINT CONSIDERED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN) :: I,IPOIN2,IPLAN,IOPT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION Y0,HH,DENOM,AUX,DELTAZ,DZ
!
!-----------------------------------------------------------------------
!
      IF(IOPT.EQ.0) THEN
!
        ! USER DEFINED
        CALL USER_VEL_PROF_Z
     &( VEL_PROF_Z, I , IPOIN2 , IPLAN , IOPT )
!
      ELSEIF(IOPT.EQ.2) THEN
!
!       FROM FRICTION COEFFICENT CF TO KS (NIKURADSE LAW...)
!
        AUX=KARMAN*SQRT(2.D0/CF%R(IPOIN2))
        HH=MAX( MESH3D%Z%R(IPOIN2+(NPLAN-1)*NPOIN2)
     &         -MESH3D%Z%R(IPOIN2)                  , 1.D-4)
!
! : Y0 = KS/30
!
        Y0=HH/EXP(1.D0+AUX)
!   EXP(1)= 2.71828182845D0
        DENOM=MAX(LOG(HH/Y0/2.71828182845D0),1.D-4)
!
        IF(IPLAN.EQ.1) THEN
!
!         VELOCITY AT THE BOTTOM IS TAKEN AT A HEIGHT IN THE
!         LOGARITHMIC PROFILE THAT ENSURES THAT THE FLUX OF THE
!         FIRST LAYER WILL BE CORRECT IF COMPUTED WITH A LINEAR
!         INTERPOLATION
!         NOTE: THE CRITERION OF STRESS INSTEAD OF FLUX WOULD
!         ALSO LEAD TO A DIVISION BY E**2
!
          DZ=(MESH3D%Z%R(IPOIN2+NPOIN2)-MESH3D%Z%R(IPOIN2))
          AUX=2.D0
          DELTAZ=DZ/EXP(AUX)
        ELSE
          DELTAZ=MESH3D%Z%R(IPOIN2+(IPLAN-1)*NPOIN2)-MESH3D%Z%R(IPOIN2)
        ENDIF
        DELTAZ=MAX(DELTAZ,Y0)
        VEL_PROF_Z=LOG(DELTAZ/Y0)/DENOM
!
!     ELSEIF(IOPT.EQ.3) THEN
!
!
      ELSE
!
        WRITE(LU,*) 'VEL_PROF_Z: UNKNOWN OPTION FOR THE PROFILE'
        WRITE(LU,*) 'IOPT=',IOPT,' 0 AND 2 ONLY ARE POSSIBLE'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
