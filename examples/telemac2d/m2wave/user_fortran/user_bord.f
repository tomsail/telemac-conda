!                   ********************
                    SUBROUTINE USER_BORD
!                   ********************
!
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,
     & ZF,NBOR,TRA05,TRA06,LIHBOR,LIUBOR,LITBOR,
     & XNEBOR,YNEBOR,NPOIN,NPTFR,NPTFR2,TEMPS,NDEBIT,NCOTE,NVITES,
     & NTRAC,NTRACE,NFRLIQ,NUMLIQ,KENT,KENTU,PROVEL,MASK,MESH,EQUA,
     & NOMIMP)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    USER MODIFIES THE BOUNDARY CONDITIONS ARRAYS
!+                WHEN THEY VARY IN TIME.
!+
!
!history  J-M HERVOUET (LNHE)
!+        27/03/2008
!+        V5P9
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EQUA           |-->| STRING DESCRIBING THE EQUATIONS SOLVED
!| H              |-->| DEPTH AT TIME N
!| HBOR           |<->| PRESCRIBED DEPTH
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LITBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON TRACERS
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| MASK           |-->| BLOCK OF MASKS FOR DIFFERENT BOUNDARY CONDITIONS
!| MESH           |-->| MESH STRUCTURE
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NCOTE          |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED ELEVATION
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| NDEBIT         |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED DISCHARGE
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NOMIMP         |-->| NAME OF LIQUID BOUNDARIES FILE
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NPTFR2         |-->| NUMBER OF QUADRATIC BOUNDARY POINTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| NTRACE         |-->| NUMBER OF BOUNDARIES WITH TRACER PRESCRIBED
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| NVITES         |-->| NUMBER OF BOUNDARIES WITH VELOCITY PRESCRIBED
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| PROVEL         |-->| OPTION FOR VELOCITY PROFILES
!| TBOR           |<--| BLOCK WITH PRESCRIBED VALUES OF TRACERS
!| TEMPS          |-->| TIME IN SECONDS
!| TRA05          |-->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| TRA06          |-->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| U              |-->| X-COMPONENT OF VELOCITY AT TIME N
!| V              |-->| Y-COMPONENT OF VELOCITY AT TIME N
!| UBOR           |<->| X-COMPONENT OF PRESCRIBED VELOCITY
!| VBOR           |<->| Y-COMPONENT OF PRESCRIBED VELOCITY
!| XNEBOR         |-->| X-COMPONENT OF NORMAL VECTOR AT BOUNDARY NODES
!| YNEBOR         |-->| Y-COMPONENT OF NORMAL VECTOR AT BOUNDARY NODES
!| ZF             |-->| BOTTOM TOPOGRAPHY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_USER_BORD => USER_BORD
      USE DECLARATIONS_TELEMAC2D, ONLY: TIDALTYPE,BOUNDARY_COLOUR,
     &                                  T2D_FILES,T2DFO1,LT
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NPTFR,NDEBIT,NCOTE,NVITES,NTRACE
      INTEGER, INTENT(IN) :: KENT,KENTU,NFRLIQ,NTRAC,NPTFR2
      INTEGER, INTENT(IN) :: PROVEL(*)
      INTEGER, INTENT(INOUT) :: LIHBOR(NPTFR),LIUBOR(NPTFR2)
      INTEGER, INTENT(IN) :: NUMLIQ(NPTFR),NBOR(NPTFR2)
      DOUBLE PRECISION, INTENT(IN) :: TEMPS
      DOUBLE PRECISION, INTENT(IN) :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      CHARACTER(LEN=20), INTENT(IN)   :: EQUA
      CHARACTER(LEN=PATH_LEN), INTENT(IN)  :: NOMIMP
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR2,2),VBOR(NPTFR2,2)
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR)
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: H,U,V,TRA05,TRA06,TBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASK,LITBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!  TABLEAUX ISSUS DU FICHIER 26 , DONNANT LES CONDITIONS AUX LIMITES
!
      DOUBLE PRECISION HB(77),PHASEB(77),HC(77),PHASEC(77)
!
      DOUBLE PRECISION PI,AN,ARG,T,W
!
      INTEGER NBORL(77),NPTFRL,I,KK,ID,K
!
!  TABLEAUX DE DONNEES TEMPORELLES
!
      SAVE HB,PHASEB,PHASEC,NBORL,HC
!
!-----------------------------------------------------------------------
!
!  LECTURE SUR LE FICHIER 26 DE HB ET PHASEB, CALCULES
!  PAR INTERPOLATION SUR CHAQUE POINT FRONTIERE
!
      PI = 4.D0*ATAN(1.D0)
      NPTFRL = 77
      IF(LT.EQ.0) THEN
        DO K = 1, NPTFR
          HBOR(K) = 1.D0
        ENDDO
      ENDIF
      IF(TEMPS.EQ.150.D0) THEN
        ID = T2D_FILES(T2DFO1)%LU
        REWIND ID
        DO K= 1, NPTFRL
          READ(ID,*) I,HB(K),PHASEB(K),HC(K),PHASEC(K)
          PHASEB(K) = PI/180.D0*PHASEB(K)
          PHASEC(K) = PI/180.D0*PHASEC(K)
          NBORL(K) = I
        ENDDO
      ENDIF
!
      T = 44714.D0
      W = 2.D0*PI/T
!
      DO K= 1 , NPTFRL
        ARG = MOD (W*TEMPS - PHASEB(K),2.D0*PI)
        AN  = HB(K) * COS(ARG)
        ARG = MOD (2.D0*W*TEMPS - PHASEC(K),2.D0*PI)
        AN  = AN + HC(K) * COS(ARG)
        IF (TEMPS.LT.2500.D0) AN = AN*0.0004D0*TEMPS
        IF(NCSIZE.GT.0) THEN
          DO KK=1,NPTFR
            IF(BOUNDARY_COLOUR%I(KK).EQ.NBORL(K)) THEN
              HBOR(KK) = AN-ZF(NBOR(KK))
            ENDIF
          ENDDO
        ELSE
          HBOR(NBORL(K)) = AN-ZF(NBOR(NBORL(K)))
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
