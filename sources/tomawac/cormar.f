!                   *****************
                    SUBROUTINE CORMAR
!                   *****************
!
     &( PART, UTEL, VTEL, HTEL, WINDXTEL, WINDYTEL)
!
!***********************************************************************
! TOMAWAC   V8P4
!***********************************************************************
!
!brief    INITIALISES ARRAYS OF PHYSICAL PARAMETERS.
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
!history  G.MATTAROLO (EDF)
!+        05/2011
!+        V6P1
!+   Modification for direct coupling with TELEMAC
!
!history  G.MATTAROLO (EDF - LNHE)
!+        14/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF - LNHE)
!+        07/12/2012
!+        V6P3
!+   Taking into account tidal flats + various optimisations.
!
!history  J-M HERVOUET (EDF - LNHE)
!+        08/01/2014
!+        V7P0
!+   CALL PARCOM suppressed by using new argument ASSPAR in VECTOR
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| HTEL           |-->| TELEMAC WATER DEPTH
!| LT             |-->| NUMBER OF THE TIME STEP CURRENTLY SOLVED
!| NVCOU          |<--| NUMBER OF VARIABLES OF THE FORMATTED CURRENT FILE
!| NVHMA          |<--| N.OF VARIABLES OF THE FORMATTED WATER LEVEL FILE
!| PART           |-->| FLAG FOR DIRECT COUPLING WITH TELEMAC
!| TC1            |<--| TIME T1 IN THE CURRENT FILE
!| TC2            |<--| TIME T2 IN THE CURRENT FILE
!| TM1            |<--| TIME T1 IN THE WATER LEVEL FILE
!| TM2            |<--| TIME T2 IN THE WATER LEVEL FILE
!| UTEL           |-->| X-AXIS TELEMAC CURRENT SPEED
!| VTEL           |-->| Y-AXIS TELEMAC CURRENT SPEED
!| WINDXTEL       |-->| TELEMAC WIND ALONG X-AXIS
!| WINDYTEL       |-->| TELEMAC WIND ALONG Y-AXIS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
!
      USE INTERFACE_TOMAWAC, EX_CORMAR => CORMAR
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)        :: PART
      TYPE(BIEF_OBJ), INTENT(IN) :: UTEL,VTEL,HTEL, WINDXTEL, WINDYTEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IP,UL
      LOGICAL TROUVE(3)
      CHARACTER(LEN=8) FMTCOU, FMTMAR
!
!-----------------------------------------------------------------------
!
!     UPDATES THE TIDAL CURRENT AND WATER LEVEL ARRAYS
!     ================================================
!
!     UPDATES THE CURRENT AT TIME 'AT'
!
      IF(NAMCOB(1:1).NE.' '.OR. NAMCOF(1:1).NE.' ' ) THEN
!
        IF(NAMCOB(1:1).NE.' ') THEN
          UL=LUCOB
          FMTCOU=FMTCOB
        ELSE
          UL=LUCOF
          FMTCOU=FMTCOF
        ENDIF
        CALL NOUDON(UC,NAMEU, 2, VC,NAMEV ,2, DEPTH,NAMEH,1, NPOIN2,
     &              UL,FMTCOU,AT,TC1,TC2, UC1, UC2, VC1, VC2, ZM1, ZM2,
     &              INDIC,'COURANT',NVCOU,TEXCOB,TROUVE,UNITCOB,PHASCOB)
        IF(TROUVE(3)) THEN
          CALL OV('X=Y-Z   ', X=DZHDT, Y=ZM2, Z=ZM1, DIM1=NPOIN2)
          CALL OV('X=CX    ', X=DZHDT, C=1.D0/(TC2-TC1), DIM1=NPOIN2)
        ENDIF
!
      ELSEIF ((PART.EQ.WAC_FULL_RUN.OR.PART.GE.WAC_API_INIT).AND.
     &    (NAMMAB(1:1).EQ.' ')) THEN
!
        CALL ANAMAR
!
      ENDIF
!
      IF(PART.EQ.WAC_CPL_RUN) THEN
        CALL OV('X=Y     ', X=UC, Y=UTEL%R, DIM1=NPOIN2)
        CALL OV('X=Y     ', X=VC, Y=VTEL%R, DIM1=NPOIN2)
        IF (VENT) THEN
          CALL OV('X=Y     ', X=UV, Y=WINDXTEL%R, DIM1=NPOIN2)
          CALL OV('X=Y     ', X=VV, Y=WINDYTEL%R, DIM1=NPOIN2)
        ENDIF
      ENDIF
!
!     UPDATES THE WATER DEPTH AT TIME 'AT' IF NOT FOUND IN CURRENT FILE
!
!      IF(.NOT.TROUVE(3)) THEN
      IF(NAMMAB(1:1).NE.' '.OR.NAMMAF(1:1).NE.' ') THEN
!
        IF(NAMMAB(1:1).NE.' ') THEN
          UL=LUMAB
          FMTMAR=FMTMAB
        ELSE
          UL=LUMAF
          FMTMAR=FMTMAF
        ENDIF
!
        CALL NOUDON(UC,NAMEU,0, VC,NAMEV,0, DEPTH,NAMEH,2, NPOIN2,
     &              UL,FMTMAR, AT, TM1, TM2, UC1, UC2, VC1, VC2,
     &              ZM1, ZM2, INDIM, 'HAUTEUR', NVHMA, TEXMAB,
     &              TROUVE,UNITMAB,PHASMAB)
        CALL OV('X=Y-Z   ', X=DZHDT, Y=ZM2, Z=ZM1, DIM1=NPOIN2)
        CALL OV('X=CX    ', X=DZHDT, C=1.D0/(TM2-TM1), DIM1=NPOIN2)
!
      ELSEIF (PART.EQ.WAC_FULL_RUN.OR.PART.GE.WAC_API_INIT) THEN
!
        IF(NAMCOF(1:1).NE.' '.OR.NAMCOB(1:1).NE.' ') THEN
          CALL ANAMAR
        ENDIF
!
      ENDIF
!      ENDIF
!
      IF(PART.EQ.WAC_CPL_RUN) THEN
!       water depth time gradient is updated
!       SDEPTH has still water depth values of the previous time step)
        DO IP=1,NPOIN2
          DZHDT(IP)=(HTEL%R(IP)-DEPTH(IP))/(NIT*DT)
        ENDDO
!       water depth is updated
        CALL OV('X=Y     ', X=DEPTH, Y=HTEL%R, DIM1=NPOIN2)
      ENDIF
!
!     UPDATES THE CURRENT AND WATER DEPTH GRADIENTS AT TIME 'AT'
!
      IF(.NOT.PROINF) THEN
        CALL VECTOR(SDZX,'=','GRADF          X',IELM2,1.D0,SDEPTH,
     &              ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST1,ASSPAR=.TRUE.)
        CALL VECTOR(SDZY,'=','GRADF          Y',IELM2,1.D0,SDEPTH,
     &              ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST1,ASSPAR=.TRUE.)
      ENDIF
!
      CALL VECTOR(SDUX,'=','GRADF          X',IELM2,1.D0,SUC,
     &            ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST1,ASSPAR=.TRUE.)
      CALL VECTOR(SDUY,'=','GRADF          Y',IELM2,1.D0,SUC,
     &            ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST1,ASSPAR=.TRUE.)
!
      CALL VECTOR(SDVX,'=','GRADF          X',IELM2,1.D0,SVC,
     &            ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST1,ASSPAR=.TRUE.)
      CALL VECTOR(SDVY,'=','GRADF          Y',IELM2,1.D0,SVC,
     &            ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST1,ASSPAR=.TRUE.)
!
!     INTEGRAL OF TEST FUNCTIONS
!
      CALL VECTOR(ST0,'=','MASBAS          ',IELM2,1.D0,ST1,
     &            ST1,ST1,ST1,ST1,ST1,MESH,.FALSE.,ST1,ASSPAR=.TRUE.)
!
      CALL OV('X=1/Y   ', X=T0 ,Y=T0, DIM1=NPOIN2)
!
!     DIVISION BY INTEGRAL OF TEST FUNCTIONS TO GET NODAL VALUES
!
      IF(.NOT.PROINF) THEN
        CALL OV('X=XY    ', X=DZX, Y=T0, DIM1=NPOIN2)
        CALL OV('X=XY    ', X=DZY, Y=T0, DIM1=NPOIN2)
      ENDIF
      CALL OV('X=XY    ', X=DUX, Y=T0, DIM1=NPOIN2)
      CALL OV('X=XY    ', X=DVX, Y=T0, DIM1=NPOIN2)
      CALL OV('X=XY    ', X=DUY, Y=T0, DIM1=NPOIN2)
      CALL OV('X=XY    ', X=DVY, Y=T0, DIM1=NPOIN2)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
