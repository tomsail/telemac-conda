!                   *****************
                    SUBROUTINE VISCSA
!                   *****************
!
     &(VISCVI,VISCNU,VISCTA,NU,NTRAC,
     & DNUVIH,DNUVIV,DNUTAH,DNUTAV,AK,EP,STRAIN,PRANDTL)
!
!***********************************************************************
! TELEMAC3D   V8P4
!***********************************************************************
!
!brief    COMPUTES THE TURBULENT VISCOSITY
!+           AND TURBULENT THERMAL DIFFUSIVITY, K AND EPSILON.
!            USING THE SPALART ALLMARAS MODLE
!
!history A. Bourgoin & R. ATA
!+        21/08/2018
!+        V8P0
!+   Creation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AK             |-->| TURBULENT ENERGY
!| DNUVIH         |-->| COEFFICIENT FOR HORIZONTAL DIFFUSION OF VELOCITIES
!| DNUVIV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF VELOCITIES
!| DNUVIH         |-->| COEFFICIENT FOR HORIZONTAL DIFFUSION OF VELOCITIES
!| DNUVIV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF VELOCITIES
!| EMIN           |-->| MINIMUM VALUE OF EPSILON FOR CLIPPING
!| EP             |-->| TURBULENT DISSIPATION
!| KMIN           |-->| MINIMUM VALUE OF K FOR CLIPPING
!| NTRAC          |-->| NUMBER OF TRACERS
!| PRANDTL        |-->| PRANDTL NUMBER
!| VISCTA         |<->| TURBULENT VISCOSITY COEFFICIENTS FOR TRACERS
!| VISCVI         |<->| TURBULENT VISCOSITY COEFFICIENTS FOR VELOCITIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_VISCSA => VISCSA
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NTRAC
      DOUBLE PRECISION, INTENT(IN) :: DNUVIH, DNUVIV,PRANDTL
      DOUBLE PRECISION, INTENT(IN) :: DNUTAH(NTRAC),DNUTAV(NTRAC)
      TYPE(BIEF_OBJ), INTENT(INOUT):: VISCVI,VISCNU,VISCTA,AK,EP
      TYPE(BIEF_OBJ), INTENT(IN)   :: NU,STRAIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION :: PROPNU2,CHI,CHI3,CV13,FV1,SQCMU,TIERS,SSIG
      INTEGER I,NPOIN3,ITRAC
!
!***********************************************************************
!
      NPOIN3 = NU%DIM1
      PROPNU2 = 1.D-6
      CV13=7.1D0**3
      SQCMU=0.3D0
      SSIG=1.5D0
      TIERS=1.D0/3.D0

      DO I=1,NPOIN3
        CHI=NU%R(I)/PROPNU2
        CHI3=CHI**3
        FV1=CHI3/(CHI3+CV13)
        AK%R(I)=FV1**TIERS*NU%R(I)*SQRT(STRAIN%R(I))/SQCMU
        EP%R(I)=FV1**0.5D0*(SQCMU*AK%R(I))**2/(NU%R(I)+DNUVIH)
!
        VISCVI%ADR(1)%P%R(I)=FV1*NU%R(I)+DNUVIH
        VISCVI%ADR(2)%P%R(I)=FV1*NU%R(I)+DNUVIH
        VISCVI%ADR(3)%P%R(I)=FV1*NU%R(I)+DNUVIV
        VISCNU%ADR(1)%P%R(I)=SSIG*(NU%R(I)+DNUVIH)
        VISCNU%ADR(2)%P%R(I)=SSIG*(NU%R(I)+DNUVIH)
        VISCNU%ADR(3)%P%R(I)=SSIG*(NU%R(I)+DNUVIV)
      ENDDO
!
      IF(NTRAC.GT.0) THEN
        IF(ABS(PRANDTL-1.D0).LT.1.D-4) THEN
          DO ITRAC=1,NTRAC
!           HERE PRANDTL TURBULENT = 1.0
            DO I=1,NPOIN3
              CHI3=(NU%R(I)/PROPNU2)**3
              FV1=CHI3/(CHI3+CV13)
              VISCTA%ADR(ITRAC)%P%ADR(1)%P%R(I)=FV1*NU%R(I)
     &                                         +DNUTAH(ITRAC)
              VISCTA%ADR(ITRAC)%P%ADR(3)%P%R(I)=FV1*NU%R(I)
     &                                         +DNUTAV(ITRAC)
            ENDDO
            CALL OS('X=Y     ',X=VISCTA%ADR(ITRAC)%P%ADR(2)%P,
     &                         Y=VISCTA%ADR(ITRAC)%P%ADR(1)%P)
          ENDDO
        ELSE
          DO ITRAC=1,NTRAC
            DO I=1,NPOIN3
              CHI3=(NU%R(I)/PROPNU2)**3
              FV1=CHI3/(CHI3+CV13)
              VISCTA%ADR(ITRAC)%P%ADR(1)%P%R(I)=FV1*NU%R(I)/PRANDTL
     &                                         +DNUTAH(ITRAC)
              VISCTA%ADR(ITRAC)%P%ADR(3)%P%R(I)=FV1*NU%R(I)/PRANDTL
     &                                         +DNUTAV(ITRAC)
            ENDDO
            CALL OS('X=Y     ',X=VISCTA%ADR(ITRAC)%P%ADR(2)%P,
     &                         Y=VISCTA%ADR(ITRAC)%P%ADR(1)%P)
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

