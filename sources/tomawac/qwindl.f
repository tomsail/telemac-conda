!                       *****************
                        SUBROUTINE QWINDL
!                       *****************
!
     &( TSTOT , USOLD , USNEW , TWOLD , TWNEW , NF    , NDIRE ,
     &  NPOIN2, USN   , USO   , FPMO  , FPMN )
!
!**********************************************************************
! TOMAWAC   V6P3                                   27/06/2011
!**********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE LINEAR WAVE GENERATION
!+               (BY WIND) SOURCE TERM BASED ON CAVALERI AND
!+                MALANOTTE-RIZZOLI (1981)
!
!reference   CAVALERI L. & P. MALANOTTE-RIZZOLI, 1981 :
!+                 "WIND WAVE PREDICTION IN SHALLOW WATER : THEORY AND
!+                  APPLICATIONS". J. GEOPHYS. RES., 86(C5),10,961-975
!
!reference   TOLMAN  (1992) : EFFECT OF NUMERICS ON THE PHYSICS IN
!+                A THIRD-GENERATION WIND-WAVE MODEL, JPO, VOL 22,
!+                PP 1095-1111.
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
!history  J-M HERVOUET (EDF/LNHE)
!+        23/12/2012
!+        V6P3
!+   A first optimisation.
!
!history  J-M HERVOUET (EDF/LNHE)
!+        09/07/2013
!+        V6P3
!+   (1.D0/1.D-90)**4 triggers an overflow, 20 put instead of 90.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FPMN           |<->| WORK TABLE
!| FPMO           |<->| WORK TABLE
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| TWNEW          |-->| WIND DIRECTION AT TIME N+1
!| TWOLD          |-->| WIND DIRECTION AT TIME N
!| USNEW          |-->| FRICTION VELOCITY AT TIME N+1
!| USOLD          |-->| FRICTION VELOCITY AT TIME N
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| USN            |<--| WORK TABLE
!| USO            |<--| WORK TABLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!  APPELS :    - PROGRAMME(S) APPELANT  : SEMIMP
!  ********    - PROGRAMME(S) APPELE(S) :    -
!**********************************************************************
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,GRAVIT,CIMPLI, FREQ, TETA,
     &    T0,T1
!FROM TOMAWAC MODULE
! CIMPLI          IMPLICITATION COEFFICIENT FOR SOURCE TERMS
!
      USE INTERFACE_TOMAWAC, EX_QWINDL => QWINDL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(INOUT) :: FPMO(NPOIN2),FPMN(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TWOLD(NPOIN2),TWNEW(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: USNEW(NPOIN2),USOLD(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: USO(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: USN(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER JP,JF,IP
      DOUBLE PRECISION C1,C2,DIREC,ALPHAN,ALPHAO,SURFREQ4
      DOUBLE PRECISION :: COSDIREC , SINDIREC
      DOUBLE PRECISION, DIMENSION(:), POINTER ::COSTW,SINTW,
     &    EXPFPMO,EXPFPMN
!
!-----------------------------------------------------------------------
!
      COSTW=>T0
      SINTW=>T1
      EXPFPMO=>T0
      EXPFPMN=>T1

      C1 = 1.5D-3/GRAVIT**2
      C2 = GRAVIT/(DEUPI*28.D0)
!
!     ARRAYS DEPENDING ONLY ON POINTS
!
! ABR: ADDED EXPONETIAL HERE
      DO IP=1,NPOIN2
        FPMO(IP)=EXP(-(C2/MAX(USOLD(IP),1.D-20))**4)
      ENDDO
      DO IP=1,NPOIN2
        FPMN(IP)=EXP(-(C2/MAX(USNEW(IP),1.D-20))**4)
      ENDDO

!
!     ARRAYS DEPENDING ONLY ON POINTS AND DIRECTIONS
!     COULD BE OPTIMISED MORE BY DECOMPOSING THE COS...
!
! abr: done
      DO IP=1,NPOIN2
        COSTW(IP)=COS(TWOLD(IP))
      ENDDO
      DO IP=1,NPOIN2
        SINTW(IP)=SIN(TWOLD(IP))
      ENDDO

      DO JP=1,NDIRE
        DIREC=TETA(JP)
        COSDIREC = COS(DIREC)
        SINDIREC = SIN(DIREC)
        DO IP=1,NPOIN2
          USO(IP,JP)=C1*(MAX(USOLD(IP)*(COSDIREC*COSTW(IP)+
     &                                 SINDIREC*SINTW(IP)),0.D0))**4
        ENDDO
      ENDDO

      DO IP=1,NPOIN2
        COSTW(IP)=COS(TWNEW(IP))
      ENDDO
      DO IP=1,NPOIN2
        SINTW(IP)=SIN(TWNEW(IP))
      ENDDO

      DO JP=1,NDIRE
        DIREC=TETA(JP)
        COSDIREC = COS(DIREC)
        SINDIREC = SIN(DIREC)
        DO IP=1,NPOIN2
          USN(IP,JP)=C1*(MAX(USNEW(IP)*(COSDIREC*COSTW(IP)+
     &                                 SINDIREC*SINTW(IP)),0.D0))**4
        ENDDO
      ENDDO


!
!     LOOP ON THE DISCRETISED FREQUENCIES
!
      DO JF=1,NF
        SURFREQ4=1.D0/FREQ(JF)**4
        DO IP=1,NPOIN2
            EXPFPMO(IP)= FPMO(IP)**SURFREQ4
        ENDDO
        DO IP=1,NPOIN2
            EXPFPMN(IP)= FPMN(IP)**SURFREQ4
        ENDDO
        DO JP=1,NDIRE
          DO IP=1,NPOIN2
            ALPHAO=USO(IP,JP)*EXPFPMO(IP)
            ALPHAN=USN(IP,JP)*EXPFPMN(IP)
!           TAKES THE SOURCE TERM INTO ACCOUNT
            TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)
     &                      + (ALPHAO + CIMPLI*(ALPHAN-ALPHAO))
          ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
