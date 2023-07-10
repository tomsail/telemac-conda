!                   ******************************************
                    SUBROUTINE SUSPENSION_DISPERSION ! (_IMP_)
!                   ******************************************
!
     &  (TOB, XMVE,HN,  OPTDIF, NPOIN, XKX, XKY,
     &   T1, T2, T3, KX, KY, KZ, DISP,U2D,V2D,VISC_TEL,CODE)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE DISPERSION PARAMETERS.
!
!history  C. MOULIN (LNH)
!+        13/12/2000
!+        V5P1
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+   Name of variables
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CODE           |-->| HYDRODYNAMIC CODE IN CASE OF COUPLING
!| DISP           |-->| VISCOSITY COEFFICIENTS ALONG X,Y AND Z
!|                |   | IF P0 : PER ELEMENT
!|                |   | IF P1 : PER POINT
!| HN             |-->| WATER DEPTH
!| KX             |<->| COEFFICIENTS OF THE DISPERSION TENSOR (DIM. NPOIN)
!| KY             |<->| COEFFICIENTS OF THE DISPERSION TENSOR (DIM. NPOIN)
!| KZ             |<->| COEFFICIENTS OF THE DISPERSION TENSOR (DIM. NPOIN)
!| NPOIN          |-->| NUMBER OF POINTS
!| OPTDIF         |-->| OPTION FOR THE DISPERSION
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| TOB            |-->| BED SHEAR STRESS (TOTAL FRICTION)
!| U2D            |-->| MEAN FLOW VELOCITY X-DIRECTION
!| V2D            |-->| MEAN FLOW VELOCITY Y-DIRECTION
!| VISC_TEL       |-->| VELOCITY DIFFUSIVITY (TELEMAC)
!| XKX            |-->| COEFFICIENT USED FOR COMPUTING THE DISPERSION
!|                |   | DEPENDS OF OPTIONS
!| XKY            |-->| COEFFICIENT USED FOR COMPUTING THE DISPERSION
!|                |   | DEPENDS OF OPTIONS
!| XMVE           |-->| FLUID DENSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,
     &    EX_SUSPENSION_DISPERSION => SUSPENSION_DISPERSION
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE (BIEF_OBJ),  INTENT(IN)    :: TOB,HN,VISC_TEL
      INTEGER,          INTENT(IN)    :: OPTDIF, NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, XKX, XKY
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T1, T2, T3
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: KX, KY, KZ, DISP
      TYPE (BIEF_OBJ),  INTENT(IN)    :: U2D,V2D
      CHARACTER(LEN=24), INTENT(IN)   :: CODE
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER                     :: K,DIMVISC
      DOUBLE PRECISION            :: UETH, COST, SINT
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      ! ****************************************************** !
      ! IA - CONSTANT DISPERSION OR DISPERSION(I) = ALPHA(I)*H !
      ! ****************************************************** !
      IF (OPTDIF == 2.OR.OPTDIF == 1) THEN
!
        CALL CPSTVC(U2D,T1)
        CALL CPSTVC(U2D,T2)
!
        IF(OPTDIF == 2) THEN
          DO K = 1, NPOIN
            UETH = SQRT(TOB%R(K)/XMVE)
            T1%R(K) = XKX * UETH * HN%R(K)
            T2%R(K) = XKY * UETH * HN%R(K)
          ENDDO
        ELSE
          CALL OS('X=C     ', X=T1, C=XKX)
          CALL OS('X=C     ', X=T2, C=XKY)
        ENDIF
!
        CALL OS('X=N(Y,Z)', X=T3, Y=U2D, Z=V2D)
!
        DO K=1,NPOIN
!
          IF(T3%R(K).GE.1.D-6) THEN
            COST = U2D%R(K)/T3%R(K)
            SINT = V2D%R(K)/T3%R(K)
          ELSE
            COST = 0.D0
            SINT = 0.D0
          ENDIF
!
          KX%R(K) = (T1%R(K) - T2%R(K))*(COST**2) + T2%R(K)
          KY%R(K) = (T2%R(K) - T1%R(K))*(COST**2) + T1%R(K)
          KZ%R(K) = (T1%R(K) - T2%R(K))*COST*SINT
!
        ENDDO
!
!
      ! *********************************** !
      ! IB - DISPERSION GIVEN BY TELEMAC-2D ! (_IMP_)
      ! *********************************** !
      ELSEIF(OPTDIF == 3) THEN
!
        IF(CODE(1:9).EQ.'TELEMAC2D') THEN
          IF(VISC_TEL%DIM2.EQ.1) THEN
            CALL OS('X=Y     ', X=KX,Y=VISC_TEL)
            CALL OS('X=Y     ', X=KY,Y=KX)
            CALL OS('X=0     ', X=KZ)
          ELSEIF(VISC_TEL%DIM2.EQ.3) THEN
            DIMVISC=VISC_TEL%MAXDIM1
            DO K=1,NPOIN
              KX%R(K)=VISC_TEL%R(K)
              KY%R(K)=VISC_TEL%R(K+  DIMVISC)
              KZ%R(K)=VISC_TEL%R(K+2*DIMVISC)
            ENDDO
          ELSE
            WRITE(LU,*) 'SUSPENSION_DISPERSION:'
            WRITE(LU,*) ' '
            WRITE(LU,*) 'UNEXPECTED DIMENSION OF VISC_TEL:',
     &                   VISC_TEL%DIM2
            CALL PLANTE(1)
            STOP
          ENDIF
        ELSE
          WRITE(LU,*) ' '
          WRITE(LU,*) 'SUSPENSION_DISPERSION:'
          WRITE(LU,*) ' '
          WRITE(LU,*) 'OPTION 3: DIFFUSIVITY GIVEN BY TELEMAC'
          WRITE(LU,*) 'NOT IMPLEMENTED OR IMPOSSIBLE WITH ',CODE
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ! ***************************************** !
      ! IC - OPTION FOR DISPERSION NOT CODED UP   ! (_IMP_)
      ! ***************************************** !
      ELSE
        WRITE(LU,31) OPTDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
      CALL OV_2('X=Y     ', DISP%R, 1, KX%R, 1, KX%R, 1, 0.D0,
     &          DISP%MAXDIM1, DISP%DIM1)
      CALL OV_2('X=Y     ', DISP%R, 2, KY%R, 1, KY%R, 1, 0.D0,
     &          DISP%MAXDIM1, DISP%DIM1)
      CALL OV_2('X=Y     ', DISP%R, 3, KZ%R, 1, KZ%R, 1, 0.D0,
     &          DISP%MAXDIM1, DISP%DIM1)
      !----------------------------------------------------------------!
31    FORMAT('DISPERSION: OPTION FOR THE DISPERSION NOT AVAILABLE:',1I6)
      !----------------------------------------------------------------!
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE SUSPENSION_DISPERSION
