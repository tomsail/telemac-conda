!                   ********************************
                    DOUBLE PRECISION FUNCTION STWC1
!                   ********************************
!
     &(F,DIR,SPEC,I)
!
!***********************************************************************
! ARTEMIS   V8P1
!***********************************************************************
!
!brief    COMPUTES THE ENERGY DENSITY BASED ON A TOMAWAC SPECTRUM.
!
!history  C. PEYRARD (LNHE)
!+        07/2014
!+        V7P0
!+   Interpolation of TOMAWAC spectrum for required F and DIR
!
!history  N. DURAND (HRW)
!+        Feb 2017
!+        V7P2
!+   bug fix last line (STWC= )
!
!history  N.DURAND (HRW)
!+        August 2017
!+        V7P3
!+   STWC updated to reflect use of new spectrum structure and for
!+   CHAINTWC.EQ.1
!
!history  N.DURAND (HRW)
!+        January 2019
!+        V8P0
!+   Added USE BIEF_DEF since TYPE SPECTRUM is now defined in BIEF_DEF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FREQUENCY FOR WHICH ENERGY DENSITY IS CALCULATED (Hz)
!| DIR            |-->| DIRECTION FOR WHICH ENERGY DENSITY IS CALCULATED (°)
!| SPEC           |-->| SPECTRUM STRUCTURE
!| I              |-->| NUMBER OF THE SPECTRAL POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF, ONLY: SPECTRUM
      USE DECLARATIONS_ARTEMIS
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(SPECTRUM)   , INTENT(IN) :: SPEC
      DOUBLE PRECISION, INTENT(IN)  :: F,DIR
      INTEGER, INTENT(IN)           :: I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION              :: F1,F2,D1,D2,EPS,TET
      DOUBLE PRECISION              :: SPF1D1,SPF2D1,SPF1D2,SPF2D2
      DOUBLE PRECISION              :: SPF1D,SPF2D
!
      INTEGER                       :: IFF,INDF,IFF1,IDD,INDD,IDD1
!
!-----------------------------------------------------------------------
!     TOMAWAC SPECTRUM IS GIVEN AT DISCRETE FREQUENCIES AND DIRECTIONS
!     THAT ARE COARSER THAN REQUIRED TO GIVE A SMOOTH ESTIMATE OF ENERGY
!     IN SPECTRUM
!     => REQUIRES INTERPOLATION (WITHIN RANGE) AS FOLLOWS
!
!     known at frequencies (f1<f2) and directions (d1<d2)
!     INTERPOLATION :
!     Sp(f,d) =  Sp(f1,d) + (f-f1)*(Sp(f2,d)-Sp(f1,d))/(f2-f1)
!     with
!     Sp(f1,d)=  Sp(f1,d1) + (d-d1)*(Sp(f1,d2)-Sp(f1,d1))/(d2-d1)
!     Sp(f2,d)=  Sp(f2,d1) + (d-d1)*(Sp(f2,d2)-Sp(f2,d1))/(d2-d1)
!
!-----------------------------------------------------------------------
!
!     FINDS CLOSEST F1 AND F2
!
      EPS=1E-5
      INDF=0
      F1=0.D0
      F2=0.D0
      DO IFF=1,NF-1
        IF((SPEC%FRE(IFF)-EPS.LE.F).AND.(SPEC%FRE(IFF+1)+EPS.GE.F))THEN
          F1=SPEC%FRE(IFF)
          F2=SPEC%FRE(IFF+1)
          IFF1=IFF
          INDF=1
        ENDIF
      ENDDO
!
!     FINDS CLOSEST D1 AND D2
!
!     LOCAL VARIABLE TET TO HAVE THE DIRECTION THAT MAY BE CHANGED
      TET = DIR
      IF (TET+EPS.LE.SPEC%DIR(1)) TET=TET+360.D0
      INDD=0
      D1=0.D0
      D2=0.D0
      DO IDD=1,NDIR
        IF ((SPEC%DIR(IDD).LE.TET).AND.(SPEC%DIR(IDD+1).GE.TET)) THEN
          D1=SPEC%DIR(IDD)
          D2=SPEC%DIR(IDD+1)
          IDD1=IDD
          INDD=1
        ENDIF
      ENDDO
!
! ----------------------------------------------------------------------
!
!     COMPUTES ENERGY DENSITY AT REQUIRED F AND DIR
!
      IF ((INDD*INDF).EQ.0) THEN
        STWC1 = 0.D0
        WRITE(LU,*) '--------------------WARNING----------------------'
        WRITE(LU,*) 'SUBROUTINE STWC1: YOU ASK FOR A PERIOD/DIRECTION '
        WRITE(LU,*) 'OUTSIDE THE RANGE OF THE TOMAWAC SPECTRUM        '
        IF(INDF.EQ.0) THEN
          WRITE(LU,*) 'F = ',F
          WRITE(LU,*) 'FMIN, FMAX =' ,SPEC%FRE(1),SPEC%FRE(NF)
        ENDIF
        IF(INDD.EQ.0) WRITE(LU,*) 'DIR = ',TET
!
        CALL PLANTE(1)
        STOP
!
      ELSE
!
!     Sp(f1,d1), Sp(f2,d1), Sp(f1,d2), Sp(f2,d2)
!
        SPF1D1=SPEC%ADR(I)%SOUTER(IFF1  ,IDD1  )
        SPF2D1=SPEC%ADR(I)%SOUTER(IFF1+1,IDD1  )
        SPF1D2=SPEC%ADR(I)%SOUTER(IFF1  ,IDD1+1)
        SPF2D2=SPEC%ADR(I)%SOUTER(IFF1+1,IDD1+1)
!
!     Sp(f1,d), Sp(f2,d)
!
        SPF1D= SPF1D1 + (TET-D1)*(SPF1D2-SPF1D1)/(D2-D1)
        SPF2D= SPF2D1 + (TET-D1)*(SPF2D2-SPF2D1)/(D2-D1)
!
!     Sp(f,d) =  Sp(f1,d) + (f-f1)*(Sp(f2,d)-Sp(f1,d))/(f2-f1)
!
        STWC1 = SPF1D  + (F-F1)*(SPF2D-SPF1D)/(F2-F1)
!
      ENDIF
!-----------------------------------------------------------------------
!
      RETURN
      END
