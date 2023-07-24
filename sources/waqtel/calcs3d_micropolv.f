!                   ****************************
                    SUBROUTINE CALCS3D_MICROPOLV
!                   *****************************
     & (NPOIN2,TN,TEXP,TIMP,ZPROP,CF,UN,VN,T2_1,T2_2,T2_3,T3_1,
     &  T3_2,T3_3)
!
!***********************************************************************
! WAQTEL   V8P4
!***********************************************************************
!
!brief    COMPUTES VOLUMIC SOURCE TERMS FOR MICROPOL WAQ PROCESS IN 3D
!          WAQ PROCESS OF CODE_TRACER (MASCARET SYSTEM)
!
!history  R. ATA
!+        21/05/2016
!+        V7P0
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |-->| FRICTION COEFFICIENT
!| NPOIN2         |-->| NUMBER OF NODES IN THE 2D MESH
!| TEXP           |<--| EXPLICIT SOURCE TERMS OF TRACERS
!| TIMP           |<--| IMPLICIT SOURCE TERMS OF TRACERS
!| TN             |-->| TRACERS
!| T2_1,...,T3_3  |<->| 2D WORKING STRUCTURES
!| T3_1,T3_2      |<->| 3D WORKING STRUCTURES
!| UN,VN          |-->| VELOCITY COMPONENTS AT TIME N
!| ZPROP          |-->| Z COORDINATES FOR 3D NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_WAQTEL,ONLY:TAUS,VITCHU_WAQ,RO0,KDESORP,KDESORP2,
     &     CCSEDIM,IND_SS,IND_SF,IND_C,IND_CSS,IND_CSS2,IND_CSF,
     &     IND_CSF2,RS,CDISTRIB,CDISTRIB2,KIN_MICROPOL
      USE INTERFACE_WAQTEL, EX_CALCS3D_MICROPOLV => CALCS3D_MICROPOLV
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN   ) :: NPOIN2
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,ZPROP,CF,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T2_1,T2_2,T2_3,T3_3,T3_1,T3_2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
      INTEGER I
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION            :: CC,CC2
!
!-----------------------------------------------------------------------
!
!     =======================================
!     PRELIMINARY COMPUTATIONS
!     =======================================
!
!     BED SHEAR STRESS (TAUB-STORED IN T2_1==>2D TABLE)
!
      CALL TAUB_WAQTEL(CF,RO0,T2_1,NPOIN2,UN,VN)
!
!     DEPOSITION PROBABILITY (SED): STORED IN T2_2==>2D TABLE
!
      CALL DEPOS_FX(T2_2,T2_1,TN%ADR(IND_SS)%P,TAUS,VITCHU_WAQ,NPOIN2)
!
!     =======================================
!     LET'S NOW COMPUTE SOURCE TERMS
!     =======================================
!
!     FIRST TRACER: SUSPENDED LOAD [SS] (IND_SS)
!
!     BED SOURCES
      DO I=1,NPOIN2
        TEXP%ADR(IND_SS)%P%R(I)=TEXP%ADR(IND_SS)%P%R(I)-T2_2%R(I)
      ENDDO
!
!     BOTTOM LAYER THICKNESS/2 : STORED IN T2_3==>2D TABLE
      DO I=1,NPOIN2
        T2_3%R(I)=0.5D0*(ZPROP%R(I+NPOIN2)-ZPROP%R(I))
      ENDDO
!     SECOND TRACER: BED SEDIMENT [SF] (IND_SF)
!      warning: no advection neither diffusion for this tracer
!
      DO I=1,NPOIN2
        TEXP%ADR(IND_SF)%P%R(I)= TEXP%ADR(IND_SF)%P%R(I) +
     &  T2_2%R(I)*T2_3%R(I) - RS%R(I)
      ENDDO
!
!     THIRD TRACER: POLLUTANT DENSITY [C] (IND_C)
!
!     implicit part
      CALL OS( 'X=X+C   ' ,X=TIMP%ADR(IND_C)%P,C=CCSEDIM            )
!     explicit part
      CALL OS( 'X=CY    ' ,X=T3_2,Y=TN%ADR(IND_CSS)%P     ,C=KDESORP )
      CC =-KDESORP*CDISTRIB
      CALL OS( 'X=X+CYZ ' ,X=T3_2,Y=TN%ADR(IND_C)%P,
     &                     Z=TN%ADR(IND_SS)%P             ,C=CC       )
      CALL OS( 'X=X+Y   ' ,X=TEXP%ADR(IND_C)%P,Y=T3_2                 )
!
!
!     FOURTH TRACER: ADSORBED POLLUTANT BY SUSPENDED LOAD [CSS] (IND_CSS)
!
!     implicit part
      CALL OS( 'X=X+C   ' ,X=TIMP%ADR(IND_CSS)%P,C=CCSEDIM          )
!     explicit part
      CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(IND_CSS)%P,Y=T3_2 )
!               RS/SF
      CALL OVD('X=Y/Z   ',RS%R,RS%R,TN%ADR(IND_SF)%P%R,1.D0, NPOIN2,2,
     &                    0.D0,EPS)
!
!     SEDP = SED/SS: STORED IN T2_2, NOT SED
!     USE OF 3D WORK ARRAY T3_1 TO SAVE ONE MORE 2D WORK ARRAY
      CALL OS ('X=C     ', X=T3_1,C=1.D0)
      CALL DEPOS_FX(T2_2,T2_1,T3_1,TAUS,VITCHU_WAQ,NPOIN2)
      CALL OS( 'X=0     ' ,X=T3_1 )
!
      DO I=1,NPOIN2
        T3_1%R(I) = RS%R(I)*TN%ADR(IND_CSF)%P%R(I) -
     &              T2_2%R(I)*TN%ADR(IND_CSS)%P%R(I)
      ENDDO
      CALL OS( 'X=X+Y   ' ,X=TEXP%ADR(IND_CSS)%P,Y=T3_1             )
!
      IF(KIN_MICROPOL.EQ.2) THEN
        CALL OS( 'X=CY    ' ,X=T3_2,Y=TN%ADR(IND_CSS2)%P,C=KDESORP2    )
        CC2 =-KDESORP2*CDISTRIB2
        CALL OS( 'X=X+CY  ' ,X=T3_2,Y=TN%ADR(IND_CSS)%P,C=CC2          )
        CALL OS( 'X=X+Y   ' ,X=TEXP%ADR(IND_CSS)%P,Y=T3_2              )
      ENDIF
!     FIFTH TRACER: ADSORBED POLLUTANT BY BED SEDIMENT [CFF] (IND_CSF)
!      warning: no advection neither diffusion for this tracer
!
      DO I=1,NPOIN2
        TEXP%ADR(IND_CSF)%P%R(I) = TEXP%ADR(IND_CSF)%P%R(I) +
     &                       T2_2%R(I)*T2_3%R(I)*TN%ADR(IND_CSS)%P%R(I)-
     &                       RS%R(I)*TN%ADR(IND_CSF)%P%R(I)*T2_3%R(I)
      ENDDO
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_CSF)%P,Y=TN%ADR(IND_CSF)%P,
     &                     C=-CCSEDIM                               )
!
      IF(KIN_MICROPOL.EQ.2) THEN
        CALL OS( 'X=CY    ' ,X=T3_3,Y=TN%ADR(IND_CSF2)%P,C=KDESORP2    )
        CALL OS( 'X=X+CY  ' ,X=T3_3,Y=TN%ADR(IND_CSF)%P,C=CC2          )
        CALL OS( 'X=X+Y   ' ,X=TEXP%ADR(IND_CSF)%P,Y=T3_3              )
      ENDIF
!
      IF(KIN_MICROPOL.EQ.2) THEN
!       SIXTH TRACER: ADSORBED POLLUTANT BY SUSPENDED LOAD 2 [CSS2] (IND_CSS2)
!
!       implicit part
        CALL OS( 'X=X+C   ' ,X=TIMP%ADR(IND_CSS2)%P,C=CCSEDIM          )
!       explicit part
        CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(IND_CSS2)%P,Y=T3_2             )
        CALL OS( 'X=0     ' ,X=T3_1 )
        DO I=1,NPOIN2
          T3_1%R(I) = RS%R(I)*TN%ADR(IND_CSF2)%P%R(I) -
     &                T2_2%R(I)*TN%ADR(IND_CSS2)%P%R(I)
        ENDDO
        CALL OS( 'X=X+Y   ' ,X=TEXP%ADR(IND_CSS2)%P,Y=T3_1             )
!       SEVENTH TRACER: ADSORBED POLLUTANT BY BED SEDIMENT 2 [CFF2] (IND_CSF2)
!
!       implicit part
        CALL OS( 'X=X+CY  ',X=TEXP%ADR(IND_CSF2)%P,Y=TN%ADR(IND_CSF2)%P,
     &                       C=-CCSEDIM                               )
!       explicit part
        DO I=1,NPOIN2
          TEXP%ADR(IND_CSF2)%P%R(I) = TEXP%ADR(IND_CSF2)%P%R(I) +
     &                     T2_2%R(I)*T2_3%R(I)*TN%ADR(IND_CSS2)%P%R(I)-
     &                     RS%R(I)*TN%ADR(IND_CSF2)%P%R(I)*T2_3%R(I)
        ENDDO
        CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(IND_CSF2)%P,Y=T3_3             )
      ENDIF

!
!-----------------------------------------------------------------------
!
      RETURN
      END
