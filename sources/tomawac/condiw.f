!                   *****************
                    SUBROUTINE CONDIW
!                   *****************
!
     &( PART, UTEL, VTEL, HTEL, WINDUTEL, WINDVTEL)
!
!***********************************************************************
! TOMAWAC   V8P4
!***********************************************************************
!
!brief    INITIALISES THE ARRAYS WITH PHYSICAL PARAMETERS.
!
!history  F.MARCOS (LNH)
!+        01/02/95
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
!history  G.MATTAROLO (EDF)
!+        05/2011
!+        V6P1
!+   Modification for direct coupling with TELEMAC
!
!history  G.MATTAROLO (EDF - LNHE)
!+        08/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        21/01/2013
!+        V6P3
!+   Calls modified.
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!history T FOUQUET (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to initialise spectrum when no wind
!+       and speini =1,3,5
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |<--| COMPUTATION TIME
!| HTEL           |-->| TELEMAC WATER DEPTH
!| LT             |-->| NUMBER OF THE TIME STEP CURRENTLY SOLVED
!| NVCOU          |---| NUMBER OF VARIABLES OF THE CURRENT FILE
!| NVHMA          |<--| N.OF VARIABLES OF THE WATER LEVEL FILE
!| NVWIN          |<--| N.OF VARIABLES OF THE WIND FILE
!| PART           |-->| FLAG FOR DIRECT COUPLING WITH TELEMAC
!| TC1            |<--| TIME T1 IN THE CURRENT FILE
!| TC2            |<--| TIME T2 IN THE CURRENT FILE
!| TM1            |<--| TIME T1 IN THE WATER LEVEL FILE
!| TM2            |<--| TIME T2 IN THE WATER LEVEL FILE
!| TV1            |<--| TIME T1 IN THE WIND FILE
!| TV2            |<--| TIME T2 IN THE WIND FILE
!| UTEL           |-->| X-AXIS TELEMAC CURRENT SPEED
!| VTEL           |-->| Y-AXIS TELEMAC CURRENT SPEED
!| WINDUTEL       |-->| TELEMAC WIND ALONG X-AXIS
!| WINDVTEL       |-->| TELEMAC WIND ALONG Y-AXIS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC
      USE INTERFACE_TOMAWAC, EX_CONDIW=> CONDIW
      USE BIEF
!
      USE DECLARATIONS_TOMAWAC
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)        :: PART
      TYPE(BIEF_OBJ), INTENT(IN) :: UTEL,VTEL,HTEL,WINDUTEL,WINDVTEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER UL,J
      CHARACTER(LEN=8) ::FFORMAT
      LOGICAL TROUVE(3)
!
!-----------------------------------------------------------------------
!
      AT = 0.D0
!
!-----------------------------------------------------------------------
!
!     INITIALISES THE TIDAL CURRENT AND WATER LEVEL
!
      TROUVE(1)=.FALSE.
      TROUVE(2)=.FALSE.
      TROUVE(3)=.FALSE.
!
      IF(MAREE) THEN
!
!     IN CASE THE TIDAL FILE DOES NOT CONTAIN ANY CURRENT
        CALL OV('X=0     ',X=UC, DIM1=NPOIN2) 
        CALL OV('X=0     ',X=VC, DIM1=NPOIN2)
!
!       READS IN THE TIDAL CURRRENT AND (OPTIONAL) DEPTH
!
        IF(NAMCOB(1:1).EQ.' '.AND.NAMMAB(1:1).EQ.' '.AND.
     &      NAMCOF(1:1).EQ.' '.AND.NAMMAF(1:1).EQ.' ') THEN
          WRITE(LU,*) ' '
          WRITE(LU,*)'USE OF TIDAL CURRENT VELOCITIES'
          WRITE(LU,*)'BUT NO CURRENT FILE (NEITHER TELEMAC DATA FILE)'
          WRITE(LU,*)
     &    '==> INITIALISATION OF TIDAL CURRENT VELOCITIES IN ANAMAR'
          WRITE(LU,*) ' '
          CALL ANAMAR
        ELSEIF(NAMCOB(1:1).NE.' ') THEN
!
!         READS IN THE CURRENTS FROM BINARY FILE, AND POSSIBLY THE DEPTH
!
          CALL LECDOI(UC,NAMEU,2,VC,NAMEV,2,DEPTH,NAMEH,1,
     &                NPOIN2, LUCOB, FMTCOB, AT,TC1,TC2,
     &                UC1,UC2,VC1,VC2,ZM1,ZM2,
     &                INDIC,'COURANT',NVCOU,TEXCOB,TROUVE,
     &                UNITCOB,PHASCOB)
!         IF DEPTH READ
          IF(TROUVE(3)) THEN
            CALL OV('X=Y-Z   ', X=DZHDT, Y=ZM2, Z=ZM1, DIM1=NPOIN2)
            CALL OV('X=CX    ', X=DZHDT, C=1.D0/(TC2-TC1), DIM1=NPOIN2)
          ELSE
            CALL OV('X=0     ',X=DZHDT, DIM1=NPOIN2)
          ENDIF
!
        ELSEIF(NAMCOF(1:1).NE.' ') THEN
!
!         READS IN THE CURRENTS FROM BINARY FILE, AND POSSIBLY THE DEPTH
!
          CALL LECDOI(UC,NAMEU,2,VC,NAMEV, 2, DEPTH,NAMEH,1,
     &                NPOIN2, LUCOF, FMTCOF, AT,TC1,TC2,
     &                UC1,UC2,VC1,VC2, ZM1,ZM2,
     &                INDIC,'COURANT',NVCOU,TEXCOB,TROUVE,
     &                UNITCOB,PHASCOB)
!         IF DEPTH READ
          IF(TROUVE(3)) THEN
            CALL OV('X=Y-Z   ', X=DZHDT, Y=ZM2, Z=ZM1, DIM1=NPOIN2)
            CALL OV('X=CX    ', X=DZHDT, C=1.D0/(TC2-TC1), DIM1=NPOIN2)
          ELSE
            CALL OV('X=0     ',X=DZHDT, DIM1=NPOIN2)
          ENDIF
!
        ENDIF
!
!       READS IN THE TIDAL WATER LEVEL IF NOT FOUND IN CURRENT FILE
!
        IF(.NOT.TROUVE(3)) THEN
        IF(NAMMAF(1:1).EQ.' '.AND.NAMMAB(1:1).EQ.' ') THEN
          IF(NAMCOF.NE.' '.OR.NAMCOB.NE.' ') THEN
            CALL ANAMAR
            WRITE(LU,*)
     &      '==> INITIALISATION OF TIDAL WATER LEVEL IN ANAMAR'
          ENDIF
        ELSE
          IF(NAMMAF(1:1).NE.' ') THEN
            WRITE(LU,*) 'READING DEPTH IN FILE MAF'
            UL=LUMAF
            FFORMAT=FMTMAF
          ELSE
            WRITE(LU,*) 'READING DEPTH IN FILE MAB'
            UL=LUMAB
            FFORMAT=FMTMAB
          ENDIF
          CALL LECDOI(UC, NAMEU,0, VC, NAMEV, 0, DEPTH,NAMEH,2,
     &                NPOIN2, UL, FFORMAT, AT, TM1, TM2,
     &                UC1, UC2, VC1, VC2,ZM1, ZM2,
     &                INDIM, 'HAUTEUR', NVHMA, TEXMAB,TROUVE,
     &                UNITMAB,PHASMAB)
          CALL OV('X=Y-Z   ', X=DZHDT, Y=ZM2, Z=ZM1, DIM1=NPOIN2)
          CALL OV('X=CX    ', X=DZHDT, C=1.D0/(TM2-TM1), DIM1=NPOIN2)
        ENDIF
!
      ENDIF
      ENDIF
!
!     INITIALISES THE CONSTANT CURRENT
!
      IF(COUSTA) THEN
        IF(NAMCOF(1:1).EQ.' '.AND.NAMCOB(1:1).EQ.' ') THEN
          CALL ANACOS
          WRITE(LU,*)' '
          WRITE(LU,*)'USE OF CURRENT VELOCITIES'
          WRITE(LU,*)
     &    'BUT NO CURRENT FILE (NEITHER TELEMAC DATA FILE)'
          WRITE(LU,*)
     &    '==> INITIALISATION OF CURRENT VELOCITIES IN ANACOS'
        ELSE
          IF(NAMCOF(1:1).NE.' ') THEN
            UL=LUCOF
            FFORMAT=FMTCOF
          ELSE
            UL=LUCOB
            FFORMAT=FMTCOB
          ENDIF
!     HERE DEPTH POSSIBLY READ AS THIRD VARIABLE
          CALL LECDON(UC, NAMEU, 2, VC, NAMEV, 2, DEPTH, NAMEH, 1,
     &         NPOIN2, UL, FFORMAT, INDIC, 'COURANT', TEXCOB,
     &         TROUVE)
        ENDIF
        CALL OV('X=0     ', X=DZHDT, DIM1=NPOIN2)
      ENDIF
!
      IF(PART.EQ.WAC_CPL_INIT) THEN
        CALL OS('X=Y     ', X=SDEPTH, Y=HTEL)
        CALL OV('X=Y     ', X=UC, Y=UTEL%R, DIM1=NPOIN2)
        CALL OV('X=Y     ', X=VC, Y=VTEL%R, DIM1=NPOIN2)
        IF(VENT) THEN
          CALL OV('X=Y     ', X=UV, Y=WINDUTEL%R, DIM1=NPOIN2)
          CALL OV('X=Y     ', X=VV, Y=WINDVTEL%R, DIM1=NPOIN2)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE WIND
!
      IF(VENT) THEN
!
        DO J=1,3
          TROUVE(J)=.FALSE.
        ENDDO
        IF(NAMVEF(1:1).EQ.' '.AND.NAMVEB(1:1).EQ.' ') THEN
          IF(PART.NE.WAC_CPL_INIT) THEN
            WRITE(LU,*) ' '
            WRITE(LU,*)'USE OF WIND VELOCITIES'
            WRITE(LU,*)'BUT NO WIND FILE '
            WRITE(LU,*)'==> INITIALISATION OF WIND VELOCITIES IN ANAVEN'
            WRITE(LU,*) ' '
            CALL ANAVEN
          ENDIF
        ELSE
          IF(NAMVEF(1:1).NE.' ') THEN
            UL=LUVEF
            FFORMAT=FMTVEF
          ELSE
            UL=LUVEB
            FFORMAT=FMTVEB
          ENDIF
          IF(VENSTA) THEN
            CALL LECDON(UV, NAMEWX,2,
     &                  VV, NAMEWY,2,
     &                  VV,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX',0,
     &                  NPOIN2,UL, FFORMAT,
     &                  INDIV,'WIND   ',TEXVEB,TROUVE)
          ELSE
            CALL LECDOI(UV, NAMEWX,2,
     &                  VV, NAMEWY,2,
     &                  VV, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX',0,
     &                  NPOIN2, UL, FFORMAT,
     &                  AT,TV1,TV2, UV1, UV2, VV1, VV2,
     &                  VV1, VV2, INDIV, 'VENT   ', NVWIN, TEXVEB,
     &                  TROUVE, UNITVEB, PHASVEB)
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISES F
!
      IF((INISPE.EQ.1.OR.INISPE.EQ.3.OR.INISPE.EQ.5).AND..NOT.VENT) THEN
!       SPECTRUM is NULL
        CALL SPEINI(F, TRA01(1:NF), TRA01(NF+1:NF+NDIRE),
     &              UV, VV,
     &              FREMAX, FETCH, SIGMAA, SIGMAB, GAMMA, FPIC, HM0I,
     &              ALPHIL, TETA1, SPRED1, TETA2, SPRED2, XLAMDA,NPOIN2,
     &              NDIRE, NF, 0, DEPTH, FRABI)
      ELSE
        CALL SPEINI(F, TRA01(1:NF), TRA01(NF+1:NF+NDIRE),
     &              UV, VV,
     &              FREMAX, FETCH, SIGMAA, SIGMAB, GAMMA, FPIC, HM0I,
     &              ALPHIL,TETA1,SPRED1,TETA2,SPRED2,XLAMDA,NPOIN2,
     &              NDIRE, NF, INISPE, DEPTH, FRABI)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
