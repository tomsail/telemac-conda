!                       *****************
                        SUBROUTINE QWIND3
!                       *****************
!
     &( TSTOT , TSDER , F     , XK    , USOLD , USNEW , TWOLD , TWNEW ,
     &  NF    , NDIRE , NPOIN2, BETAN , BETAO , DIRN  , DIRO  )
!
!**********************************************************************
! TOMAWAC   V6P1                                   27/06/2011
!**********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE WAVE GENERATION
!+               (BY WIND) SOURCE TERM BASED ON THE YAN PARAMETRISATION
!+               (1987) AND USING THE COEFFICIENTS PROPOSED BY WESTHUYSEN
!+               ET AL. (2007)
!
!reference  YAN (1987) : AN IMPROVED WIND INPUT SOURCE TERM FOR
!+                THIRD GENERATION OCEAN WAVE MODELLING, REP NO 87-8,
!+                ROYAL DUTCH METEOR. INST., 20 PP.
!
!reference  WESTHUYSEN ET AL. (2007) : NONLINEAR SATURATION-BASED
!+                WHITECAPPING DISSIPATION IN SWAN FOR DEEP AND SHALLOW
!+                WATER, COASTAL ENG., VOL 54, 151-170.
!
!history  E. GAGNAIRE-RENOU (EDF/LNHE)
!+        09/2010
!+        V6P0
!+
!
!history  G.MATTAROLO (EDF - LNHE)
!+        27/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BETAN          |<--| WORK TABLE
!| BETAO          |<--| WORK TABLE
!| DIRN           |<--| WORK TABLE
!| DIRO           |<--| WORK TABLE
!| F              |-->| DIRECTIONAL SPECTRUM
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TETA           |-->| DISCRETIZED DIRECTIONS
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| TWNEW          |-->| WIND DIRECTION AT TIME N+1
!| TWOLD          |-->| WIND DIRECTION AT TIME N
!| USNEW          |-->| FRICTION VELOCITY AT TIME N+1
!| USOLD          |-->| FRICTION VELOCITY AT TIME N
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!  APPELS :    - PROGRAMME(S) APPELANT  : SEMIMP
!  ********    - PROGRAMME(S) APPELE(S) :    -
!
!**********************************************************************
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,CIMPLI, FREQ, TETA,
     &                                 COEFWD, COEFWE, COEFWF, COEFWH 
!FROM TOMAWAC MODULE
! CIMPLI          IMPLICITATION COEFFICIENT FOR SOURCE TERMS
! COEFWD          COEFFICIENT D OF YAN WIND GENERATION MODEL
! COEFWE          COEFFICIENT E OF YAN WIND GENERATION MODEL
! COEFWF          COEFFICIENT F OF YAN WIND GENERATION MODEL
! COEFWH          COEFFICIENT H OF YAN WIND GENERATION MODEL
!
      USE INTERFACE_TOMAWAC, EX_QWIND3 => QWIND3
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)   ::    NF  , NDIRE        , NPOIN2
      DOUBLE PRECISION, INTENT(IN)   :: TWOLD(NPOIN2), TWNEW(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: USOLD(NPOIN2), USNEW(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: F(NPOIN2,NDIRE,NF),XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT):: DIRO(NPOIN2) , DIRN(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: BETAN(NPOIN2), BETAO(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT):: TSDER(NPOIN2,NDIRE,NF)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , IFF   , IP
      DOUBLE PRECISION DIREC , CONST
      DOUBLE PRECISION INTERO, INTERN , CPHAS  , DIMPLI
!
!
      DIMPLI= 1.0D0-CIMPLI
!
!
!.....LOOP ON THE DISCRETISED DIRECTIONS
!     """"""""""""""""""""""""""""""""""""""""""""
      DO JP=1,NDIRE
!
!.......PRECOMPUTES THE DIRECTIONAL DEPENDENCES
!       """"""""""""""""""""""""""""""""""""""""""""""
        DIREC=TETA(JP)
        DO IP=1,NPOIN2
          DIRO(IP)=COS(DIREC-TWOLD(IP))
          DIRN(IP)=COS(DIREC-TWNEW(IP))
        ENDDO
!
!.......LOOP ON THE DISCRETISED FREQUENCIES
!       """"""""""""""""""""""""""""""""""""""""""""
        DO IFF = 1,NF
          CONST=DEUPI*FREQ(IFF)
!
!.........COMPUTES THE PROPORTIONALITY COEFFICIENTS BETA
!         """"""""""""""""""""""""""""""""""""""""""""""""
          DO IP = 1,NPOIN2
            CPHAS = CONST / XK(IP,IFF)
            INTERO=USOLD(IP)/CPHAS
            BETAO(IP)=(COEFWD*INTERO**2*DIRO(IP)+COEFWE*INTERO*DIRO(IP)
     &      +COEFWF*DIRO(IP)+COEFWH)*CONST
            INTERN=USNEW(IP)/CPHAS
            BETAN(IP)=(COEFWD*INTERN**2*DIRN(IP)+COEFWE*INTERN*DIRN(IP)
     &      +COEFWF*DIRN(IP)+COEFWH)*CONST
          ENDDO
!
!.........TAKES INTO ACCOUNT THE SOURCE TERM
!         """"""""""""""""""""""""""""""""
          DO IP = 1,NPOIN2
            TSTOT(IP,JP,IFF) = TSTOT(IP,JP,IFF)
     &           + MAX((DIMPLI*BETAO(IP)+CIMPLI*BETAN(IP))*F(IP,JP,IFF)
     &                 ,0.D0)
            TSDER(IP,JP,IFF) = TSDER(IP,JP,IFF) + MAX(BETAN(IP),0.D0)
          ENDDO
!
        ENDDO
!
      ENDDO
!
      RETURN
      END

