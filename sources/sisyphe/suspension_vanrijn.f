!                     *****************************
                      SUBROUTINE SUSPENSION_VANRIJN
!                     *****************************
!
     &(FDM,TAUP,NPOIN,GRAV,XMVE,XMVS,VCE,ZERO,AC,CSTAEQ,ZREF)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
! NEW SUBROUTINE C. VILLARET N. HUYBRECHTS
!
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+   Name of variables
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| ACLADM         |-->| MEAN DIAMETER OF SEDIMENT
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| NPOIN          |-->| NUMBER OF POINTS
!| VCE            |-->| FLOW VISCOSITY
!| XMVE           |-->| FLUID DENSITY
!| XMVS           |-->| SEDIMENT DENSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    ::  TAUP,ZREF
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    ::  GRAV,  XMVE, XMVS,VCE
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,AC,FDM
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CSTAEQ
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER            :: I
      DOUBLE PRECISION   ::  TAUC,AUX,DSTAR,DENS
!
!======================================================================!
!======================================================================!
!                               PROGRAMME                              !
!======================================================================!
!======================================================================!
!
      ! ******************************** !
      !    I - CRITICAL SHIELD PARAMETER !
      ! ******************************** !
!
!
      DO I=1,NPOIN
!
! ****************** !
! II - SKIN FRICTION !
! ****************** !
!
        TAUC = AC * GRAV*(XMVS-XMVE)*FDM
        DENS  = (XMVS - XMVE )/ XMVE
        DSTAR = FDM*(GRAV*DENS/VCE**2)**(1.D0/3.D0)
!
! ***************** !
! IV - EROSION FLUX !
! ***************** !
! Concentration increased by AVA because it is assumed
! that it is computed only with one class of sediment
!
        IF(DSTAR.LE.ZERO) THEN
          WRITE(LU,*) 'ERROR SUSPENSION_VANRIJN'
          CALL PLANTE(1)
          STOP
        ENDIF
        AUX=(TAUP%R(I)-TAUC)/TAUC
        IF(AUX.GT.ZERO) THEN
          CSTAEQ%R(I)=0.015*FDM*SQRT(AUX**3)/(ZREF%R(I)*DSTAR**0.3D0)
        ELSE
          CSTAEQ%R(I) = 0.D0
        ENDIF
!
      ENDDO
!
!======================================================================!
!======================================================================!
!======================================================================!
!
      RETURN
      END
