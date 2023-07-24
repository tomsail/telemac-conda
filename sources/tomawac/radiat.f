!                   *****************
                    SUBROUTINE RADIAT
!                   *****************
!
     &     ( FX1    , FY1, XK1   ,FS, CG1,
     &  CGSUC1,DSXXDX, DSXYDX, DSXYDY, DSYYDY)
!
!***********************************************************************
! TOMAWAC   V7P0
!***********************************************************************
!
!brief    COMPUTES THE RADIATION STRESSES AND DRIVING FORCES
!+                FOR THE GENERATION OF WAVE-INDUCED CURRENTS.
!+
!+           (SEE NOTES FOR METHODOLOGY)
!code
!+  THE RESULT OF THIS COMPUTATION IS GIVEN AS :
!+       FI = - 1/D D( SIJ )/D( XJ )    UNIT : M/S**2
!
!note     COMPUTATION ACCORDING TO THE "THEORICAL" FORMULATION, WITH
!+          COMPUTATION OF THE TERMS IN THE TENSOR OF THE RADIATION
!+          STRESSES, AND THEN THEIR GRADIENTS IN SPACE.
!
!history  M. BENOIT (EDF/DER/LNH)
!+        13/12/95
!+        V1P0
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
!history  G.MATTAROLO (EDF - LNHE)
!+        27/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF - LNHE)
!+        08/01/2014
!+        V7P0
!+   CALL PARCOM suppressed by using new argument ASSPAR in VECTOR
!
!history  C. VILLARET (HR-WALLINGFORD)
!+        15/09/2014
!+        V7P0
!+   Cancellation of radiation stresses below a given depth hmin.
!
!history T. Fouquet (made by C Raoul)
!        01/02/2018
!        V7P3
!        Forces calculated in spherical coordinates
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CG1            |-->| DISCRETIZED GROUP VELOCITY
!| CGSUC1         |<--| WORK TABLE
!| DEPTH1         |-->| WATER DEPTH
!| DSXXDX         |<->| WORK TABLE
!| DSXYDX         |<->| WORK TABLE
!| DSXYDY         |<->| WORK TABLE
!| DSYYDY         |<->| WORK TABLE
!| FS             |-->| DIRECTIONAL SPECTRUM
!| FX             |<--| DRIVING FORCE ALONG X
!| FY             |<--| DRIVING FORCE ALONG Y
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| SXX            |<--| RADIATION STRESS ALONG XX
!| SXY            |<--| RADIATION STRESS ALONG XY
!| SYY            |<--| RADIATION STRESS ALONG YY
!| XK1            |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC
!
      USE INTERFACE_TOMAWAC, EX_RADIAT => RADIAT
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)    :: FS(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: CG1(NPOIN2,NF),XK1(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: CGSUC1(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FX1(NPOIN2),FY1(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: DSXXDX(NPOIN2),DSXYDX(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: DSXYDY(NPOIN2),DSYYDY(NPOIN2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER JP,JF,IP
      DOUBLE PRECISION COEF,COEF2,COCO,SISI,SICO,OMEGA,DTETAR
!
!     CV: MINIMUM WATER DEPTH FOR TIDAL FLATS TREATMENT
!     NEW KEYWORD IS NEEDED
!
      DOUBLE PRECISION HMIN
!
!-----------------------------------------------------------------------
!
      HMIN=0.1D0
!
      DTETAR=DEUPI/NDIRE
!
      DO IP=1,NPOIN2
        SXX(IP) = 0.D0
        SXY(IP) = 0.D0
        SYY(IP) = 0.D0
      ENDDO
!
!     COMPUTES THE WORKING ARRAY N = CG/C
!
      DO JF = 1,NF
        OMEGA=DEUPI*FREQ(JF)
        DO IP=1,NPOIN2
          CGSUC1(IP,JF)=CG1(IP,JF)*XK1(IP,JF)/OMEGA
        ENDDO
      ENDDO
!
!     COMPUTES THE RADIATION STRESSES INTEGRATED OVER THE SPECTRUM
!     SUMS UP THE DISCRETISED PART OF THE SPECTRUM
!
      DO JP=1,NDIRE
        COCO=COSTET(JP)**2
        SISI=SINTET(JP)**2
        SICO=SINTET(JP)*COSTET(JP)
        DO JF=1,NF
          COEF=GRAVIT*DFREQ(JF)*DTETAR
          DO IP=1,NPOIN2
            COEF2=COEF*FS(IP,JP,JF)
            SXX(IP)=SXX(IP)+(CGSUC1(IP,JF)*(1.D0+SISI)-0.5D0)*COEF2
            SXY(IP)=SXY(IP)+(CGSUC1(IP,JF)*SICO             )*COEF2
            SYY(IP)=SYY(IP)+(CGSUC1(IP,JF)*(1.D0+COCO)-0.5D0)*COEF2
          ENDDO
        ENDDO
      ENDDO
!
!     COMPUTES THE GRADIENTS IN SPACE OF THE RADIATION STRESSES
!
!
!     INVERSE OF INTEGRALS OF TEST FUNCTIONS
!
      CALL VECTOR(ST0,'=','MASBAS          ',IELM2,1.D0,
     &            ST1,ST1,ST1,ST1,ST1,ST1,MESH,.FALSE.,ST1,
     &            ASSPAR=.TRUE.)
      CALL OS('X=1/Y   ',X=ST0,Y=ST0)
!
!     DERIVATIVE IN X
!
      CALL OV('X=Y     ',X=T4,Y=SXX,DIM1=NPOIN2)

      CALL VECTOR
     & (ST1,'=','GRADF          X',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,ST3,ASSPAR=.TRUE.)
!
      CALL OV('X=Y     ',X=T4,Y=SXY,DIM1=NPOIN2)
      CALL VECTOR
     & (ST2,'=','GRADF          X',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,ST3,ASSPAR=.TRUE.)
!
      CALL OV('X=YZ    ',X=DSXXDX, Y=T1, Z=T0, DIM1=NPOIN2)
      CALL OV('X=YZ    ',X=DSXYDX, Y=T2, Z=T0, DIM1=NPOIN2)
!
!     DERIVATIVE IN Y
!
      CALL OV('X=Y     ',X=T4, Y=SYY, DIM1=NPOIN2)
      CALL VECTOR
     & (ST1,'=','GRADF          Y',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,ST3,ASSPAR=.TRUE.)
!
      CALL OV('X=Y     ',X=T4, Y=SXY, DIM1=NPOIN2)
      CALL VECTOR
     & (ST2,'=','GRADF          Y',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,ST3,ASSPAR=.TRUE.)
!
      CALL OV('X=YZ    ',X=DSYYDY, Y=T1, Z=T0, DIM1=NPOIN2)
      CALL OV('X=YZ    ',X=DSXYDY, Y=T2, Z=T0, DIM1=NPOIN2)
!
!     COMPUTES THE DRIVING FORCES FOR WAVE-INDUCED CURRENTS

      IF (.NOT.SPHE) THEN
!                               +-----------------------------+
!.............................. ! CARTESIAN COORDINATE SYSTEM !
!                               +-----------------------------+
        DO IP=1,NPOIN2
          IF(DEPTH(IP).GE.HMIN) THEN
            FX1(IP)= - (DSXXDX(IP)+DSXYDY(IP))/DEPTH(IP)
            FY1(IP)= - (DSXYDX(IP)+DSYYDY(IP))/DEPTH(IP)
          ELSE
            FX1(IP)=0.D0
            FY1(IP)=0.D0
          ENDIF
        ENDDO
!
      ELSE
!                               +-----------------------------+
!.............................. ! SPHERICAL COORDINATE SYSTEM !
!                               +-----------------------------+
        DO IP=1,NPOIN2
          TRA40(IP)=1.0D0/COSF(IP)
        ENDDO
        CALL OV('X=CYZ    ',X=DSXXDX, Y=DSXXDX, Z=TRA40, C=SR,
     &          DIM1=NPOIN2)
        CALL OV('X=CYZ    ',X=DSXYDX, Y=DSXYDX, Z=TRA40, C=SR,
     &          DIM1=NPOIN2)
        CALL OV('X=CY     ',X=DSYYDY, Y=DSYYDY, C=SR, DIM1=NPOIN2)
        CALL OV('X=CY     ',X=DSXYDY, Y=DSXYDY, C=SR, DIM1=NPOIN2)

        DO IP=1,NPOIN2
          IF(DEPTH(IP).GE.HMIN) THEN
            FX1(IP)= - (DSXXDX(IP)+DSXYDY(IP))*RADDEG/DEPTH(IP)
            FY1(IP)= - (DSXYDX(IP)+DSYYDY(IP))*RADDEG/DEPTH(IP)
          ELSE
            FX1(IP)=0.D0
            FY1(IP)=0.D0
          ENDIF
        ENDDO
      ENDIF


!

!
!-----------------------------------------------------------------------
!
      RETURN
      END

