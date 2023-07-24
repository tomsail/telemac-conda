!                   *****************
                    SUBROUTINE QWIND1
!                   *****************
!
     &( TSTOT , TSDER , F     , XK    , USOLD , USNEW , TWOLD , TWNEW ,
     &  Z0OLD , Z0NEW , NF    , NDIRE , NPOIN2, TOLD  , TNEW  ,
     &  USN   , USO   , OMNEW , OMOLD , BETAN , BETAO )
!
!***********************************************************************
! TOMAWAC   V6P1                                   27/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE WAVE GENERATION
!+               (BY WIND) SOURCE TERM BASED ON JANSSEN (1989,1991).
!
!reference  JANSSEN P.A.E.M (1989) :
!+                     "WIND-INDUCED STRESS AND THE DRAG OF AIR
!+                      FLOW OVER SEA WAVES". JPO, VOL 19, PP 745-754.
!reference JANSSEN P.A.E.M (1991) :
!+                     "QUASI-LINEAR THEORY OF WIND-WAVE GENERATION
!+                      APPLIED TO WAVE FORECASTING". JPO, VOL 21, PP 1631-1642.
!
!history  P. THELLIER; M. BENOIT (EDF/DER/LNH)
!+        11/04/95
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BETAM          |-->| WIND GENERATION COEFFICIENT
!| BETAN          |<--| WORK TABLE
!| BETAO          |<--| WORK TABLE
!| CPHAS          |<--| WORK TABLE
!| F              |-->| DIRECTIONAL SPECTRUM
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| OMNEW          |<--| WORK TABLE
!| OMOLD          |<--| WORK TABLE
!| TNEW           |<--| WORK TABLE
!| TOLD           |<--| WORK TABLE
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| TWNEW          |-->| WIND DIRECTION AT TIME N+1
!| TWOLD          |-->| WIND DIRECTION AT TIME N
!| USN            |<--| WORK TABLE
!| USNEW          |-->| FRICTION VELOCITY AT TIME N+1
!| USO            |<--| WORK TABLE
!| USOLD          |-->| FRICTION VELOCITY AT TIME N
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| XKAPPA         |-->| VON KARMAN CONSTANT
!| Z0NEW          |-->| SURFACE ROUGHNESS LENGTH AT TIME N+1
!| Z0OLD          |-->| SURFACE ROUGHNESS LENGTH AT TIME N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI, GRAVIT, ROAIR, ROEAU,
     &                    TETA, FREQ, DECAL, XKAPPA,BETAM, CIMPLI

!
      USE INTERFACE_TOMAWAC, EX_QWIND1 => QWIND1
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)   ::  NF    , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN)   :: F(NPOIN2,NDIRE,NF),XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(IN)   :: TWOLD(NPOIN2), TWNEW(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: USOLD(NPOIN2), USNEW(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: Z0OLD(NPOIN2), Z0NEW(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: TNEW(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT):: TOLD(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT):: USO(NPOIN2),USN(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: OMNEW(NPOIN2),OMOLD(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: BETAN(NPOIN2), BETAO(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT):: TSDER(NPOIN2,NDIRE,NF)
!.....VARIABLES FROM MODULE TOMAWAC
!     """"""""""""""""""""
!     DECAL           SHIFT GROWING CURVE DUE TO WIND
!     XKAPPA          VON KARMAN CONSTANT
!     BETAM           WIND GENERATION COEFFICIENT
!     CIMPLI          IMPLICITATION COEFFICIENT FOR SOURCE TERMS
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION C1,DIREC,CONST,DIMPLI,XX,ZLOGMU, CPHAS
!
      C1 = DEUPI * (ROAIR/ROEAU) * (BETAM/XKAPPA**2)
      DIMPLI=1.D0-CIMPLI
!
!.....COMPUTES (1ST PASS) THE DIRECTIONAL DEPENDENCES
!     """"""""""""""""""""""""""""""""""""""""""""""
      DO JP=1,NDIRE
        DIREC=TETA(JP)
        DO IP=1,NPOIN2
          TOLD(IP,JP)=COS(DIREC-TWOLD(IP))
          TNEW(IP,JP)=COS(DIREC-TWNEW(IP))
        ENDDO
      ENDDO
!
!.....LOOP ON THE DISCRETISED FREQUENCIES
!     """"""""""""""""""""""""""""""""""""""""""""
      DO JF=1,NF
        CONST=C1*FREQ(JF)
!
!.......COMPUTES (1ST PASS) THE FREQUENCIES (OMEGA AND UETOILE/CPHASE)
!       """""""""""""""""""""""""""""""""""""""""""""""""
        DO IP=1,NPOIN2
!.......COMPUTES THE PHASE VELOCITY
          CPHAS = DEUPI * FREQ(JF) / XK(IP,JF)
          OMOLD(IP) = GRAVIT * Z0OLD(IP) / CPHAS**2
          OMNEW(IP) = GRAVIT * Z0NEW(IP) / CPHAS**2
          USO(IP) = (USOLD(IP) / CPHAS) + DECAL
          USN(IP) = (USNEW(IP) / CPHAS) + DECAL
        ENDDO
!
!.......LOOP ON THE DISCRETISED DIRECTIONS
!       """"""""""""""""""""""""""""""""""""""""""""
        DO JP=1,NDIRE
!
          DO IP=1,NPOIN2
            BETAO(IP)=0.D0
            BETAN(IP)=0.D0
          ENDDO
!
!.........COMPUTES THE SOURCE TERM
!         """"""""""""""""""""""
          DO IP=1,NPOIN2
            IF(TOLD(IP,JP).GT.0.01D0) THEN
              XX = USO(IP) * TOLD(IP,JP)
              ZLOGMU = LOG(OMOLD(IP)) + XKAPPA/XX
              IF(ZLOGMU.LT.0.D0) THEN
                BETAO(IP) = CONST*OMOLD(IP)*EXP(XKAPPA/XX)*
     &                          ZLOGMU**4*XX**2
              ENDIF
            ENDIF
          ENDDO
          DO IP=1,NPOIN2
            IF(TNEW(IP,JP).GT.0.01D0) THEN
              XX = USN(IP) * TNEW(IP,JP)
              ZLOGMU = LOG(OMNEW(IP)) + XKAPPA/XX
              IF(ZLOGMU.LT.0.D0) THEN
                BETAN(IP) = CONST*OMNEW(IP)*EXP(XKAPPA/XX)*
     &                          ZLOGMU**4*XX**2
              ENDIF
            ENDIF
          ENDDO
!
!.........TAKES THE SOURCE TERM INTO ACCOUNT
!         """"""""""""""""""""""""""""""""
          DO IP=1,NPOIN2
            TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)
     &              + (DIMPLI*BETAO(IP)+CIMPLI*BETAN(IP)) * F(IP,JP,JF)
            TSDER(IP,JP,JF) = TSDER(IP,JP,JF) + BETAN(IP)
          ENDDO
!
        ENDDO
      ENDDO
!
      RETURN
      END
