!                 **********************************
                  DOUBLE PRECISION FUNCTION CVSP_ALT
!                 **********************************
!
     &(J, FORMULA)
!
!***********************************************************************
! SISYPHE   V7P2                                   05/12/2017
!***********************************************************************
!
!brief   CALCULATES A DYNAMIC ACTIVE LAYER THICKNESS
!+        ACCORDING TO 1 OF A COUPLE OF FORMULAS
!
!
!history  UWE MERKEL
!+        20/07/2011
!+       V6P2
!+
!
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   cleaning, cosmetic
!
!history  LEOPOLD STADLER (BAW) & J-M HERVOUET (EDF LAB, LNHE)
!+        28/07/2014
!+        V7P0
!+   Computation of D90 and DIAMAX secured to avoid divisions by 0.
!
!history  UWE MERKEL (UHM) R. KOPMANN (BAW)
!+        22/11/2016 / 2017
!+        V6P3 / V7P2
!+   Many changes
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX OF A POINT IN MESH
!| FORMULA        |<--| WHICH FORMULA TO USE TO CALCULATE THE ALT
!| VCE            |-->| WATER VISCOSITY
!| XMVE           |-->| FLUID DENSITY
!| XMVS           |-->| SEDIMENT DENSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: J
      INTEGER, INTENT(IN) :: FORMULA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  I
      DOUBLE PRECISION RHO, RHO_S, G, D50, D90, DMAX, FMAX, TAUC, TAUB
      DOUBLE PRECISION PON, SUMME
      DOUBLE PRECISION DSTAR, DENS
!
!-----------------------------------------------------------------------
!
! ATTENTION !
!
!-----------------------------------------------------------------------
!
! EXPECTS GRAIN CLASSES TO BE SORTED IN ASCENDING ORDER
! UNLIKE SISYPHE!!!
! IMPROVE IT!!!
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
! CHECK ASCENDING ORDER OF CLASSES D50
!-----------------------------------------------------------------------
      DO I=1,NSICLA-1
        IF(FDM(I).GE.FDM(I+1)) THEN
          WRITE(LU,*) 'STOPPING!!!! GRAIN CLASSES HAVE TO BE',
     &                ' IN ASCENDING ORDER!!! FOR DYNAMIC ALT'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!-----------------------------------------------------------------------
! BASICS
!-----------------------------------------------------------------------
      G = GRAV
      RHO = XMVE
      RHO_S = XMVS
      PON = XKV
      D50 = ACLADM%R(J)
!-----------------------------------------------------------------------
! DMAX - NEW APPROXIMATION: DMAX = D99 / BEFORE DMAX = MAX(FDM)
!-----------------------------------------------------------------------
      FMAX = 0.99D0
      SUMME = AVAIL(J,1,1)
      DMAX = 0.D0
      IF (SUMME.GE.FMAX) THEN
        DMAX = FDM(1)
      ELSE
        DO I=2,NSICLA
          IF(AVAIL(J,1,I).GT.0.D0) THEN
            SUMME = AVAIL(J,1,I) + SUMME
            IF(SUMME.GE.FMAX.AND.DMAX.EQ.0.D0) THEN
              DMAX = FDM(I-1) + ( (FDM(I)-FDM(I-1)) /
     &        AVAIL(J,1,I)) * (FMAX-(SUMME-AVAIL(J,1,I)))
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!DMAX = MAX(FDM(1), DMAX)
      IF (DMAX.LE.0.D0.AND.SUMME.GT.ZERO) THEN
        WRITE(LU,*)'UHM DMAXERROR'
        DO I=1,NSICLA
          WRITE(LU,*)'DMAXERROR',J,I,FDM(I),AVAIL(J,1,I),SUMME
          WRITE(LU,*)ZF%R(J)-ZR%R(J)
        ENDDO
        STOP
      ENDIF
!-----------------------------------------------------------------------
! D90 - ONLY FIRST APPROXIMATION!
!-----------------------------------------------------------------------
      FMAX = 0.90D0
      SUMME = AVAIL(J,1,1)
      D90 = 0.D0
      IF (SUMME.GE.FMAX) THEN
        D90 = FDM(1)
      ELSE
        DO I=2,NSICLA
          IF(AVAIL(J,1,I).GT.0.D0) THEN
            SUMME = AVAIL(J,1,I) + SUMME
            IF(SUMME.GE.FMAX.AND.D90.EQ.0.D0) THEN
              D90 = FDM(I-1) + ( (FDM(I)-FDM(I-1)) /
     &              AVAIL(J,1,I)) * (FMAX-(SUMME-AVAIL(J,1,I)))
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!D90 = MAX(FDM(1), D90)
      IF (D90.LE.0.D0.AND.SUMME.GT.ZERO) THEN
        WRITE(LU,*)'UHM D90ERROR', D90, FMAX, SUMME
        DO I=1,NSICLA
          WRITE(LU,*)'D90ERROR',J,I,FDM(I),AVAIL(J,1,I),SUMME
        ENDDO
        WRITE(LU,*)'D90ERROR'
      ENDIF
!-----------------------------------------------------------------------
! SHEAR PARAMETERS
!-----------------------------------------------------------------------
! HERE ARE ENOUGH POSSIBILITIES FOR IMPROVEMENT
! THE INITIATION OF MOTION STARTS WITH THE SMALLEST GRAIN
! LEEDS TO BIGGER ACTIVE LAYER THICKNESSES
      TAUC = AC(1)*((XMVS-XMVE)*G*D50)
      TAUB = TOB%R(J)
!
!-----------------------------------------------------------------------
! NEW ACTIVE LAYER THICKNESS
!-----------------------------------------------------------------------
!
      SELECT CASE (FORMULA)
!-----------------------------------------------------------------------
! HUNZIKER & GUENTHER
!-----------------------------------------------------------------------
!
      CASE (1)
        CVSP_ALT = 5.D0 * DMAX
!
!-----------------------------------------------------------------------
! FREDSOE & DEIGAARD 1992
!-----------------------------------------------------------------------
!
      CASE (2)
        CVSP_ALT = 2.D0 * TAUB / (G*(RHO_S-RHO))
     &     / TAN(PHISED/180.0D0*PI) / (1.D0-PON)
!
!-----------------------------------------------------------------------
! VAN RIJN 1993
!-----------------------------------------------------------------------
!
      CASE (3)
        CVSP_ALT = 0.D0
        IF(TAUC.GT.0.D0) THEN
          IF(TAUB.GT.TAUC) THEN
            DENS = (RHO_S - RHO) / RHO
            DSTAR = D50*(G*DENS/VCE**2)**(1.D0/3.D0)
            CVSP_ALT = 0.3D0*(DSTAR**0.7D0)*
     &         ((TAUB-TAUC)/TAUC)**0.5*D50
          ENDIF
        ENDIF
!-----------------------------------------------------------------------
! WONG 2006
!-----------------------------------------------------------------------
      CASE (4)
        IF((TAUB/(RHO_S-RHO)/G/D50).LT.0.0549D0) THEN
          CVSP_ALT = 0.D0
        ELSE
          CVSP_ALT=5.0D0*D50*((TAUB/(RHO_S-RHO)/G/D50)
     &          -0.0549D0)**0.56D0
        ENDIF
!-----------------------------------------------------------------------
! MALCHEREK 2003
!-----------------------------------------------------------------------
      CASE (5)
        CVSP_ALT = 0.D0
        IF (TAUC.GT.0) THEN
          CVSP_ALT = D90 / (1.D0-PON) * TAUB/TAUC
        ENDIF
!-----------------------------------------------------------------------
! SISYPHE
!-----------------------------------------------------------------------
      CASE (6)
        CVSP_ALT = 3.D0 * D50
!-----------------------------------------------------------------------
! CONSTANT FROM CAS FILE
!-----------------------------------------------------------------------
      CASE (0)
        CVSP_ALT = ELAY0
!-----------------------------------------------------------------------
      CASE DEFAULT
        WRITE(LU,*)'NO VALID CHOICE FOR "C-VSM DYNAMIC ALT MODEL"'
        WRITE(LU,*)'MUST BE BETWEEN 0 AND 6'
        CALL PLANTE(1)
!
      END SELECT
!-----------------------------------------------------------------------
! MINIMUM ALT VALUE = SMALLEST GRAIN SIZE
!-----------------------------------------------------------------------
      CVSP_ALT = MAX(CVSP_ALT, FDM(1))
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION CVSP_ALT
