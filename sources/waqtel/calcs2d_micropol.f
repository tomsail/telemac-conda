!                     ***************************
                      SUBROUTINE CALCS2D_MICROPOL
!                     ***************************
!
     & (NPOIN,TN,TEXP,TIMP,HPROP,CF,UN,VN,
     &  T1,T2,T3,T4,T5,T6)
!
!***********************************************************************
! WAQTEL   V8P4
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR MICROPOL WAQ PROCESS
!          WAQ PROCESS OF CODE_TRACER (MASCARET SYSTEM)
!
!history  R. ATA
!+        21/09/2014
!+        V7P0
!+       CREATION (VOID)
!history  R. ATA
!+        28/09/2015
!+        V7P1
!+       REAL CREATION
!
!history  S.E. BOURBAN (HRW)
!+        07/06/2017
!+        V7P3
!+        Indexing tracer (IND_*) to avoid conflicting naming convention
!+        between user defined tracers, water quality processes and
!+        ice processes. Introduction of the array RANK_*.
!
!history  S.E. BOURBAN (HRW)
!+        25/09/2017
!+        V7P3
!+        TEXP and TIMP are now additive to account for a variety of
!+        of sources / sinks on a given TRACER
!
!history  C.-T. PHAM
!+        20/11/2019
!+        V8P1
!+        The calculation of the 4th ad 5th equations is with RS/SF
!+        and SEDP not RS and SED=SEDP*SS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |-->| FRICTION COEFFICIENT
!| HPROP          |-->| WATER DEPTH
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES
!| TEXP           |<--| EXPLICIT SOURCE TERMS OF TRACERS
!| TIMP           |<--| IMPLICIT SOURCE TERMS OF TRACERS
!| TN             |-->| TRACERS
!| T1,...,T6      |<->| 2D WORKING STRUCTURES
!| UN,VN          |-->| VELOCITY COMPONENTS AT TIME N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_WAQTEL,ONLY:ERO,TAUR,TAUS,VITCHU_WAQ,CDISTRIB,
     &     CDISTRIB2,RO0,KDESORP,KDESORP2,CCSEDIM,IND_SS,IND_SF,IND_C,
     &     IND_CSS,IND_CSF,IND_CSS2,IND_CSF2,KIN_MICROPOL
      USE INTERFACE_WAQTEL, EX_CALCS2D_MICROPOL => CALCS2D_MICROPOL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER          , INTENT(IN   ) :: NPOIN
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,CF,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,T1,T2,T3,T4,T5,T6,TIMP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION            :: CC,CC2
!
!-----------------------------------------------------------------------
!
!
!     =======================================
!     PRELIMINARY COMPUTATIONS
!     =======================================
!
!     BED SHEAR STRESS (TAUB-STORED IN T1)
!
      CALL TAUB_WAQTEL(CF,RO0,T1,NPOIN,UN,VN)
!
!     DEPOSITION PROBABILITY (SED): STORED IN T2, NOT SEDP
!
      CALL DEPOS_FX(T2,T1,TN%ADR(IND_SS)%P,TAUS,VITCHU_WAQ,NPOIN)
!
!     SEDP = SED/SS: STORED IN T4, NOT SED
!
      CALL OS ('X=C     ', X=T3,C=1.D0)
      CALL DEPOS_FX(T4,T1,T3,TAUS,VITCHU_WAQ,NPOIN)
!
!     EROSION FLUX (RS): STORED IN T3
!
      CALL EROSION_FX(T3,T1,TN%ADR(IND_SF)%P,TAUR,ERO,1.D-10,NPOIN)
!
!
!     =======================================
!     LET'S NOW COMPUTE SOURCE TERMS
!     =======================================
!
!     FIRST TRACER: SUSPENDED LOAD [SS] (IND_SS)
!                                               RS   SED
      CALL OS ('X=Y-Z   ', X=T1,              Y=T3,Z=T2              )
      CALL OVD('X=X+CY/Z', TEXP%ADR(IND_SS)%P%R,T1%R,HPROP%R,
     &                     1.D0, NPOIN,2,0.D0,EPS  )
!
!     SECOND TRACER: BED SEDIMENT [SF] (IND_SF)
!      warning: no advection neither diffusion for this tracer
!                                SED   RS
      CALL OS ('X=Y-Z   ', X=T1,Y=T2,Z=T3              )
      CALL OS ('X=X+Y   ', X=TEXP%ADR(IND_SF)%P, Y=T1  )
!
!     THIRD TRACER: POLLUTANT DENSITY [C] (IND_C)
!
!     implicit part
!     CCSEDIM = CONSTANT L IN THE DOC
!      CALL OS( 'X=X+C   ' ,X=TIMP%ADR(IND_C)%P,C=CCSEDIM             )
      CALL OS( 'X=X+CY  ' ,X=TIMP%ADR(IND_C)%P,Y=HPROP,C=-CCSEDIM)
!     explicit part
      CALL OS( 'X=CY    ' ,X=T1,Y=TN%ADR(IND_CSS)%P,C=KDESORP        )
      CC =-KDESORP*CDISTRIB
      CALL OS( 'X=X+CYZ ' ,X=T1,Y=TN%ADR(IND_C)%P,
     &                     Z=TN%ADR(IND_SS)%P             ,C=CC       )
      CALL OS( 'X=X+Y   ' ,X=TEXP%ADR(IND_C)%P,Y=T1                  )
!
!     FOURTH TRACER: ADSORBED POLLUTANT BY SUSPENDED LOAD [CSS] (IND_CSS)
!
!     implicit part
!      CALL OS( 'X=X+C   ' ,X=TIMP%ADR(IND_CSS)%P,C=CCSEDIM             )
      CALL OS( 'X=X+CY  ' ,X=TIMP%ADR(IND_CSS)%P,Y=HPROP,C=-CCSEDIM)
!     explicit part
      CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(IND_CSS)%P,Y=T1                  )
!      CALL OS( 'X=YZ    ' ,X=T4,Y=T3,Z=TN%ADR(IND_CSF)%P               )
!      CALL OS( 'X=X+CYZ ' ,X=T4,Y=TN%ADR(IND_CSS)%P,Z=T2,C=-1.D0       )
!      CALL OVD('X=Y/Z   ' ,T4%R,T4%R,
!     &                     HPROP%R,0.D0,NPOIN,2,0.D0,EPS)
!      CALL OS( 'X=X+Y   ' ,X=TEXP%ADR(IND_CSS)%P,Y=T4                  )
!                                 RS
      CALL OS( 'X=Y     ' ,X=T1,Y=T3)
      CALL OVD('X=CXY/Z ' ,T1%R,TN%ADR(IND_CSF)%P%R,TN%ADR(IND_SF)%P%R,
     &                     1.D0, NPOIN,2,0.D0,EPS)
!                                 SEDP
      CALL OS( 'X=X-YZ  ' ,X=T1,Y=T4,Z=TN%ADR(IND_CSS)%P)
      CALL OVD('X=X+CY/Z', TEXP%ADR(IND_CSS)%P%R,T1%R,HPROP%R,
     &                     1.D0, NPOIN,2,0.D0,EPS  )
!
      IF(KIN_MICROPOL.EQ.2) THEN
        CALL OS( 'X=CY    ' ,X=T5,Y=TN%ADR(IND_CSS2)%P,C=KDESORP2      )
        CC2 =-KDESORP2*CDISTRIB2
        CALL OS( 'X=X+CY  ' ,X=T5,Y=TN%ADR(IND_CSS)%P,C=CC2            )
        CALL OS( 'X=X+Y   ' ,X=TEXP%ADR(IND_CSS)%P,Y=T5                )
      ENDIF
!
!     FIFTH TRACER: ADSORBED POLLUTANT BY BED SEDIMENT [CFF] (IND_CSF)
!
!      CALL OS( 'X=X+YZ  ' ,X=TEXP%ADR(IND_CSF)%P,Y=TN%ADR(IND_CSS)%P,
!     &                     Z=T2                                        )
!      CALL OS( 'X=X+CYZ ' ,X=TEXP%ADR(IND_CSF)%P,Y=TN%ADR(IND_CSF)%P,
!     &                     Z=T3,C=-1.D0                                )
!      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_CSF)%P,Y=TN%ADR(IND_CSF)%P,
!     &                     C=-CCSEDIM                                  )
!     implicit part
      CALL OS( 'X=X+CY  ' ,X=TIMP%ADR(IND_CSF)%P,Y=HPROP,C=-CCSEDIM)
!     explicit part
      CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(IND_CSF)%P,Y=T1)
      IF(KIN_MICROPOL.EQ.2) THEN
        CALL OS( 'X=CY    ' ,X=T6,Y=TN%ADR(IND_CSF2)%P,C=KDESORP2      )
        CALL OS( 'X=X+CY  ' ,X=T6,Y=TN%ADR(IND_CSF)%P,C=CC2            )
        CALL OS( 'X=X+Y   ' ,X=TEXP%ADR(IND_CSF)%P,Y=T6                )
      ENDIF
!
      IF(KIN_MICROPOL.EQ.2) THEN
!       SIXTH TRACER: ADSORBED POLLUTANT BY SUSPENDED LOAD 2 [CSS2] (IND_CSS2)
!
!       implicit part
        CALL OS( 'X=X+CY  ' ,X=TIMP%ADR(IND_CSS2)%P,Y=HPROP,C=-CCSEDIM)
!       explicit part
        CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(IND_CSS2)%P,Y=T5               )
        CALL OS( 'X=Y     ' ,X=T1,Y=T3)
        CALL OVD('X=CXY/Z ', T1%R,TN%ADR(IND_CSF2)%P%R,
     &                       TN%ADR(IND_SF)%P%R,1.D0, NPOIN,2,0.D0,EPS)
!                                   SEDP
        CALL OS( 'X=X-YZ  ' ,X=T1,Y=T4,Z=TN%ADR(IND_CSS2)%P)
        CALL OVD('X=X+CY/Z', TEXP%ADR(IND_CSS2)%P%R,T1%R,HPROP%R,
     &                       1.D0, NPOIN,2,0.D0,EPS  )
!
!       SEVENTH TRACER: ADSORBED POLLUTANT BY BED SEDIMENT 2 [CFF2] (IND_CSF2)
!
!       implicit part
        CALL OS( 'X=X+CY  ' ,X=TIMP%ADR(IND_CSF2)%P,Y=HPROP,C=-CCSEDIM)
!       explicit part
        CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(IND_CSF2)%P,Y=T1)
        CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(IND_CSF2)%P,Y=T6               )
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
