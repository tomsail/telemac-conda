!                   **************************
                    SUBROUTINE SUSPENSION_CONV
!                   **************************
!
     &(TOB,XMVE,KSR,NPOIN,ZREF,U2D,V2D,HN,
     & UCONV,VCONV,KARMAN,ZERO,XWC,ALPHA,RESOL,GLOSEG1,GLOSEG2,NSEG,
     & FLULIM,YAFLULIM,SOLSYS_SIS,SOLSYS,UCONV_TEL,VCONV_TEL)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    CORRECTS U2D, V2D VELOCITIES.
!
!history  C. VILLARET (LNHE)
!+        01/08/2006
!+
!+
!
!history
!+        02/05/2008
!+        V6P0
!+   ADDED ALPHA IN ARGUMENT, TO KEEP A RECORD OF THIS COEFFICIENT
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
!| ALPHA          |<->| RATIO BETWEEN CONVECTION VELOCITY FOR SEDIMENT AND MEAN FLOW  VELOCITY
!| HN             |-->| WATER DEPTH
!| KARMAN         |-->| VON KARMAN CONSTANT
!| KSR            |-->| RIPPLE BED ROUGHNESS
!| NPOIN          |-->| NUMBER OF POINTS
!| TOB            |-->| BED SHEAR STRESS
!| U2D            |-->| MEAN FLOW VELOCITY X-DIRECTION
!| UCONV          |<->| X-COMPONENT ADVECTION FIELD (TELEMAC)
!| V2D            |-->| MEAN FLOW VELOCITY Y-DIRECTION
!| VCONV          |<->| Y-COMPONENT ADVECTION FIELD
!| XMVE           |-->| WATER DENSITY
!| XWC            |-->| SETTLING VELOCITIES
!| ZERO           |-->| ZERO
!| ZREF           |<->| REFERENCE ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (BIEF_OBJ),  INTENT(IN)    :: HN,U2D,V2D,ZREF,KSR
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: UCONV,VCONV,ALPHA,FLULIM
      TYPE (BIEF_OBJ),  INTENT(IN)    :: TOB,UCONV_TEL,VCONV_TEL
      INTEGER,          INTENT(IN)    :: NPOIN,RESOL,NSEG,SOLSYS
      INTEGER,          INTENT(IN)    :: GLOSEG1(NSEG),GLOSEG2(NSEG)
      INTEGER,          INTENT(INOUT) :: SOLSYS_SIS
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,XWC
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN,XMVE
      LOGICAL, INTENT(INOUT)          :: YAFLULIM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION R1,I1,I2,A,B,AUX,LAUX,LL,USTAR,ROUSE
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      LL=LOG(30.D0)
!
      DO I = 1, NPOIN
!
        IF(TOB%R(I).GT.ZERO) THEN
!
          USTAR = SQRT(TOB%R(I)/XMVE)
!
!         B --> KS/H
!
!         AUX = 1.D0 + KARMAN*SQRT(2.D0/MAX(CF%R(I),ZERO))
!         B = 30.D0*EXP(-AUX)
!
          B = KSR%R(I) /MAX(HN%R(I),1.1D0*KSR%R(I))
          A = ZREF%R(I)/MAX(HN%R(I),1.1D0*ZREF%R(I))
!
! TAKES MAX VALUE OF A = ZREF/H AND B=KSR/H
          A=MAX(A,B)
!
! SIMPLIFIED VERSION
          ROUSE=MIN(XWC/MAX(USTAR,ZERO),1.D0)/KARMAN
          R1=  1.D0-ROUSE
          LAUX=LOG(A)
!
          IF(ABS(R1).LT.1.D-8) THEN
            I1= -LAUX
            I2= -LAUX**2/2.D0
          ELSE
            AUX=A**R1
            I1=(1.D0-AUX)/R1
            I2=-(I1+LAUX*AUX)/R1
          ENDIF
!
!         AUX=LOG(A/30.D0)
          AUX=LAUX - LL
          ALPHA%R(I)=-(I2-AUX*I1)/(I1*(AUX+1.D0))
!
        ELSE
!
          ALPHA%R(I)=1.D0
!
        ENDIF
!
!       CHECKS 0
!
        ALPHA%R(I)=MIN(ALPHA%R(I),1.D0)
        ALPHA%R(I)=MAX(ALPHA%R(I),0.D0)
!
      ENDDO
!
!     DEPENDING ON ADVECTION SCHEME : LIMITATION OF VELOCITY OR FLUXES
!
      IF(RESOL.EQ.13.OR.RESOL.EQ.14) THEN
!
!       LIMITATION OF FLUXES WITH FLULIM
!
        SOLSYS_SIS=SOLSYS
        IF(SOLSYS_SIS.EQ.1) THEN
          UCONV%R=>U2D%R
          VCONV%R=>V2D%R
        ELSE
!         HERE UCONV_TEL IS PASSED ON
          UCONV%R=>UCONV_TEL%R
          VCONV%R=>VCONV_TEL%R
        ENDIF
        DO I=1,NSEG
          FLULIM%R(I)=0.5D0*(ALPHA%R(GLOSEG1(I))+ALPHA%R(GLOSEG2(I)))
        ENDDO
        YAFLULIM=.TRUE.
!
      ELSE
!
!       LIMITATION OF VELOCITY
!
        SOLSYS_SIS=1
        DO I = 1,NPOIN
          UCONV%R(I) = ALPHA%R(I)*U2D%R(I)
          VCONV%R(I) = ALPHA%R(I)*V2D%R(I)
        ENDDO
        YAFLULIM=.FALSE.
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
