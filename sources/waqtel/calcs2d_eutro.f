!                     ************************
                      SUBROUTINE CALCS2D_EUTRO
!                     ************************
!
     &  (NPOIN,WATTEMP,TN,TEXP,TIMP,RAYEFF,HPROP,T1,T2,T3,T4,
     &   T5,T6,T7,T8,T9,T10,T11,T12,DEBUG,UN,VN)
!
!***********************************************************************
! WAQTEL   V8P1
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR EUTRO WAQ PROCESS
!
!history  R. ATA
!+        21/09/2014
!+        V7P0
!+       CREATION (VOID)
!history  R. ATA
!+        21/09/2015
!+        V7P1
!+       REAL IMPLEMENTATION
!
!history  R. ATA
!+        21/03/2016
!+        V7P2
!+       IMPROVEMENT- REMOVE LOCAL DECLARATIONS
!+       AND ALLOCATIONS
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
!| DEBUG          |-->| IF NE.0 THEN DEBUG MODE
!| HPROP          |-->| WATER DEPTH
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES
!| RAYEFF         |-->| EFFECT OF SUNSHINE ON ALGAE GROWTH
!| T1,..,T12      |<--| WORKING STRUCTURES
!| TN             |-->| TRACER STRUCUTRE
!| TEXP           |<--| EXPLICIT SOURCE TERMES
!| WATTEMP        |-->| WATER TEMPERATURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!-----------------------------------------------------------------------
!***********************************************************************
      USE BIEF
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_WAQTEL,ONLY:CMAX,CTOXIC,IK,K520,O2SATU,K2,ZSD,
     &  I0,KPE,KP,KN,CMORALG,FORMCS,TRESPIR,PROPHOC,DTP,PRONITC,K22,
     &  WLOR,K360,K320,PERNITS,WPOR,WNOR,FORMK2,O2PHOTO,K120,O2NITRI,
     &  DEMBEN,MEXTINC,SECTODAY,IND_T,
     &  IND_PHY,IND_PO4,IND_POR,IND_NO3,IND_NOR,IND_NH4,IND_OL,IND_O2
      USE INTERFACE_WAQTEL, EX_CALCS2D_EUTRO => CALCS2D_EUTRO
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  U        ! TR ! D  ! VITESSE DE L'EAU                             !
! !  WPOR     ! R  !    ! VITESSE DE SEDIMENTATION DU PHOSPHORE ORGANIQ!
! !  WNOR     ! R  !    ! VITESSE DE SEDIMENTATION DE L AZOTE ORGANIQUE!
! !  CMAX     ! R  !    ! TAUX DE CROISSANCE ALGALE MAXIMUM A 20°C     !
! !  ZSD      ! R  !    ! PROFONDEUR DE SECCHI                         !
! !  KPE      ! R  !    ! COEF D EXTINCTION DU RAY SANS PHYTO          !
! !  BETA     ! R  !    ! COEF DE TURBIDITE VEGETALE                   !
! !  IK       ! R  !    ! PARAMETRE DE CALAGE DE LA FORMULE DE SMITH   !
! !  KP       ! R  !    ! CONSTANTE DE DEMI-SATURATION EN PHOSPHATE    !
! !  KN       ! R  !    ! CONSTANTE DE DEMI-SATURATION EN AZOTE        !
! !  ALPHA    ! R  !    ! COEF 1 DE TOXICITE DE L EAU POUR LES ALGUES  !
! !  ALPHA2   ! R  !    ! COEF 2 DE TOXICITE DE L EAU POUR LES ALGUES  !
! !  RP       ! R  !    ! TAUX DE RESP. DE LA BIOMASSE ALGALE A 20°C   !
! !  PROPHOC  ! R  !    ! PROP DE PHOSPHORE DANS LES CELLULES DU PHYTO !
! !  DTP      ! R  !    ! POURCENT DE PHOSPH DIRECT ASSIM DS PHY MORT  !
! !  K320     ! R  !    ! TAUX DE TRANSFORMATION DU POR EN PO4         !
! !  PRONITC  ! R  !    ! PROP D AZOTE DANS LES CELLULES DU PHYTO      !
! !  PERNITS  ! R  !    ! POURCENT D AZOTE DIRECT ASSIM DS PHY MORT    !
! !  K360     ! R  !    ! TAUX DE TRANSFORMATION DU NOR EN NO3         !
! !  M1       ! R  !    ! COEF 1 DE MORTALITE ALGALE A 20°C            !
! !  M2       ! R  !    ! COEF 2 DE MORTALITE ALGALE A 20°C            !
! !  WLOR     ! R  !    ! VITESSE DE SEDIMENTATION DE LA CHARGE ORGANIQ!
! !  K120     ! R  !    ! CINETIQUE DE DEGRADATION DE LA CHARGE ORGANIQ!
! !  K520     ! R  !    ! CINETIQUE DE NITRIFICATION                   !
! !  F        ! R  !    ! QTTE D O2 PRODUITE PAR PHOTOSYNTHESE         !
! !  N        ! R  !    ! QTTE D O2 CONSOMMEE PAR NITRIFICATION        !
! !  BEN      ! R  !    ! DEMANDE BENTHIQUE A 20°C                     !
! !  K2       ! R  !    ! COEFFICIENT DE REAERATION                    !
! !  FORMK2   ! E  !    ! FORMULE DE CALCUL DE K2                      !
! !  CS       ! R  !    ! CONCENTRATION DE SATURATION EN OXYG DE L'EAU !
! !  FORMCS   ! E  !    ! FORMULE DE CALCUL DE CS                      !
! !  RSW      ! R  !    ! COEFFICIENT DE REAERATION AUX SEUILS         !
! !  FORMRS   ! E  !    ! FORMULE DE CALCUL DE R                       !
! !  ARS      ! R  !    ! COEFFICIENT A DES FORMULES DE CALCUL DE R    !
! !  BRS      ! R  !    ! COEFFICIENT B DES FORMULES DE CALCUL DE R    !
! !  NBSEUI   ! E  !    ! NOMBRE DE SEUILS                             !
! !  XSEUI    ! TR !    ! ABSCISSES DES SEUILS                         !
! !  DZS      ! TR !    ! DELTA Z AUX SEUILS                           !
! !           !    !    !                                              !
! !  IF1      ! TR ! D  ! INDIC DE LECTURE DU FICHIER DES PARAMETRES   !
! !___________!____!____!______________________________________________!
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!***********************************************************************
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN   ) :: NPOIN,DEBUG
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TIMP,TEXP,RAYEFF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T1,T2,T3,T4,T5,T6,T7,T8,T9,T10
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T11,T12
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
!
      INTEGER                     :: I
      DOUBLE PRECISION, PARAMETER :: UNSURVINGT=0.05D0
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-6
      DOUBLE PRECISION, PARAMETER :: CORR1=1.065D0
!      DOUBLE PRECISION, PARAMETER :: CORR2=1.0241D0
      DOUBLE PRECISION, PARAMETER :: CORR2=1.025D0
      DOUBLE PRECISION            :: G1,G2,G3,CC,POWER
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 0'
!
!     INITIALISATION
!
!     CS IS STORED IN T2
      CALL OS( 'X=0     ',X=T2)
      CALL OS( 'X=0     ',X=T3)
!     G2 IS STORED IN T6
      CALL OS( 'X=0     ',X=T6)
!     CP IS STORED IN T7
      CALL OS( 'X=0     ',X=T7)
!     DP IS STORED IN T4
      CALL OS( 'X=0     ',X=T4)
!     LNUT IS STORED IN T5 UNTIL ALGAE_GROWTH,
!     THEN RESPIR*G1 IS STORED IN T5 FOR THE 8TH EQUATION
      CALL OS( 'X=0     ',X=T5)
!     RN IS STORED IN T8
      CALL OS( 'X=0     ',X=T8)
!
!     G2 IS STORED IN T6,WE TAKE INTO ACCOUNT VARIABLE TEMPERATURE
!
!      G2 = WATTEMP/20.D0
!      IF( IND_T.GT.0 ) THEN
!        CALL OS('X=CY    ',X=T6,Y=TN%ADR(IND_T)%P,C=UNSURVINGT)
!      ELSE
!        CALL OS('X=C     ',X=T6 ,C=G2)
!      ENDIF
!
!     COMPUTE G3,BENCOR
!
      POWER = WATTEMP-20.D0
      G2    = 1.050D0**POWER
      G3    = 1.047D0**POWER
      DO I=1,NPOIN
        IF( IND_T.GT.0 ) THEN
          POWER = TN%ADR(IND_T)%P%R(I)-20.D0
          G2    = 1.050D0**POWER
          G3    = 1.047D0**POWER
        ENDIF
!       CORR2T AND BENCOR STORED HERE IN T9,T10
        T9%R(I) = CORR2**POWER
        T10%R(I)= DEMBEN*(CORR1**POWER)
!       G2 IS STORED IN T6
        T6%R(I)=G2
!       G3 IS STORED IN T11
        T11%R(I)=G3
      ENDDO
!
!     COMPUTE CS (O2SATU, STORED IN T2)
!
      IF( IND_T.EQ.0 ) THEN
        CALL SATUR_O2(O2SATU,FORMCS,WATTEMP,EPS)
        CALL OS('X=C     ',X=T2,C=O2SATU       )
      ELSE
        DO I=1,NPOIN
          CALL SATUR_O2(T2%R(I),FORMCS,TN%ADR(IND_T)%P%R(I),EPS)
        ENDDO
      ENDIF
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 1'
!
!     RAYEFF WITH SMITH FORMULA
!
      CALL RAY_EFFECT(ZSD,TN%ADR(IND_PHY)%P,NPOIN,MEXTINC,I0,IK,KPE,
     &                RAYEFF,HPROP,T3,T4)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 2'
!
!     COMPUTE LNUT: EFFECTS OF PHOSPHORIOUS AND NITROGENIOUS
!           NUTRIMENTS ON ALGAE GROWTH ==>STORED IN T5
!
!      CALL NUTEFF(T5%R,TN,NPOIN,IND_PO4,IND_NO3,KP,KN)
!
!     NUTEFF DOEST NOT TO NH4 INTO ACCOUNT
      DO I=1,NPOIN
        T5%R(I)= MIN(TN%ADR(IND_PO4)%P%R(I)/(KP+TN%ADR(IND_PO4)%P%R(I)),
     &                (TN%ADR(IND_NO3)%P%R(I)+TN%ADR(IND_NH4)%P%R(I))
     &               /(KN+TN%ADR(IND_NO3)%P%R(I)
     &                   +TN%ADR(IND_NH4)%P%R(I)))
      ENDDO
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 3'
!
!     RATE OF ALGAE GROWTH: CP (STORED IN T7)
!
      CALL ALGAE_GROWTH(T7%R,CMAX,RAYEFF%R,T6,T5%R,CTOXIC(1),NPOIN)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 4'
!
!     RATE OF ALGAE DISAPPEARANCE DP (STORED IN T4) AND MP (STOPCKED IN T12)
!
      CALL ALGAE_DEATH(T4%R,T12%R,CMORALG,TN%ADR(IND_PHY)%P%R,TRESPIR,
     &                  T6,CTOXIC(2),NPOIN)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 5'
!
!     COMPUTE K2
!
      CALL REAER(FORMK2,K2,K22,NPOIN,1,UN,VN,HPROP,EPS)
!
!     COMPUTE RS (RSW:DONE IN DIFSOU)
!
!
!     COMPUTE RN: PROPORTION OF NITROGEN ASSIMILATED AS NH4(STORED IN T8)
!
      CALL OV( 'X=Y+Z   ' ,T1%R,TN%ADR(IND_NH4)%P%R,TN%ADR(IND_NO3)%P%R,
     &          0.D0,NPOIN)
      CALL OVD('X=Y/Z   ' ,T8%R,TN%ADR(IND_NH4)%P%R,T1%R,0.D0,
     &          NPOIN ,2,0.D0,EPS )
!
!     RESPIR*G1 IS STORED IN T5 FOR THE 8TH EQUATION
      G1 = WATTEMP/20.D0
      IF( IND_T.GT.0 ) THEN
        CALL OS('X=CY    ',X=T5,Y=TN%ADR(IND_T)%P,C=UNSURVINGT*TRESPIR)
      ELSE
        CALL OS('X=C     ',X=T5 ,C=G1*TRESPIR)
      ENDIF
!
!     LET'S NOW COMPUTE SOURCE TERMS
!     -------------------------------
!
!     FIRST TRACER [PHY] (IND_PHY)
!
      CALL OS( 'X=Y-Z   ' ,X=T1                 ,Y=T7,Z=T4)
!      CALL OS( 'X=YZ    ' ,X=TEXP%ADR(IND_PHY)%P,Y=T1,
!     &                     Z=TN%ADR(IND_PHY)%P)
      CALL OS( 'X=X+CYZ ' ,X=TEXP%ADR(IND_PHY)%P,Y=T1,
     &                     Z=TN%ADR(IND_PHY)%P,C=SECTODAY )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 6'
!
!     SECOND TRACER [PO4] (IND_PO4)
!
      CALL OS( 'X=CY    ' ,X=T1,Y=T4                    ,C=DTP      )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T7                                )
      CALL OS( 'X=CXY   ' ,X=T1,Y=TN%ADR(IND_PHY)%P     ,C=PROPHOC  )
      CALL OS( 'X=CYZ   ' ,X=T3,Y=TN%ADR(IND_POR)%P,Z=T6,C=K320     )
!      CALL OS( 'X=Y+Z   ' ,X=TEXP%ADR(IND_PO4)%P,Y=T1,Z=T3          )
      CALL OS( 'X=X+Y   ' ,X=T1,Y=T3                                )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_PO4)%P,Y=T1   ,C=SECTODAY )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 7'
!
!     THIRD TRACER [POR] (IND_POR)
!
      G2=PROPHOC*(1.D0-DTP)
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T4,Z=TN%ADR(IND_PHY)%P,C=G2       )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T3                                )
!      CALL OVD('X=C/Y   ' ,T3%R,HPROP%R,TN%ADR(IND_POR)%P%R,WPOR,
!     &          NPOIN ,2,0.D0,EPS                                   )
      CALL OVD('X=CY/Z  ' ,T3%R,TN%ADR(IND_POR)%P%R,HPROP%R,WPOR,
     &         NPOIN ,2,0.D0,EPS )
!      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(IND_POR)%P,Y=T1,Z=T3          )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T3                                )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_POR)%P,Y=T1   ,C=SECTODAY )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 8'
!
!     FOURTH TRACER [NO3] (IND_NO3)
!
      CALL OS( 'X=Y+C   ' ,X=T1,Y=T8                      ,C=-1.D0  )
      CALL OS( 'X=CXYZ  ' ,X=T1,Y=T7,Z=TN%ADR(IND_PHY)%P  ,C=PRONITC)
      CALL OS( 'X=CYZ   ' ,X=T3,Y=TN%ADR(IND_NH4)%P  ,Z=T6,C=K520)
!      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(IND_NO3)%P,Y=T3,Z=T1          )
      CALL OS( 'X=X+Y   ' ,X=T3,Y=T1                                )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_NO3)%P,Y=T3   ,C=SECTODAY )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 9'
!
!     FIFTH TRACER [NOR] (IND_NOR)
!
      G2=PRONITC*(1.D0-PERNITS)
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T4,Z=TN%ADR(IND_PHY)%P,C=G2       )
      CALL OS( 'X=CYZ   ' ,X=T3,Y=TN%ADR(IND_NOR)%P  ,Z=T6,C=K360)
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T3                                )
      CALL OVD('X=CY/Z  ' ,T3%R,TN%ADR(IND_NOR)%P%R,HPROP%R,WNOR,
     &         NPOIN ,2,0.D0,EPS )
!      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(IND_NOR)%P,Y=T1,Z=T3          )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T3                                )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_NOR)%P,Y=T1   ,C=SECTODAY )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 10'
!
!     SIXTH TRACER [NH4] : AMMONIACAL LOAD (IND_NH4)
!
!     IMPLICIT PART
!      CALL OS( 'X=CYZ   ' ,X=TIMP%ADR(IND_NH4)%P,Y=T6,Z=HPROP,C=-K520)
      CALL OS( 'X=X+CYZ ' ,X=TIMP%ADR(IND_NH4)%P,
     &                     Y=T6,Z=HPROP,   C=-K520*SECTODAY)
!     EXPLICIT PART
      CALL OS( 'X=CY    ' ,X=T1,Y=T4                      ,C=PERNITS )
      CALL OS( 'X=YZ    ' ,X=T3,Y=T7,Z=T8                            )
      CALL OS( 'X=C(Y-Z)' ,X=T3,Y=T1,Z=T3                 ,C=PRONITC )
      CALL OS( 'X=XY    ' ,X=T3,Y=TN%ADR(IND_PHY)%P                  )
      CALL OS( 'X=CYZ   ' ,X=T1,Y=TN%ADR(IND_NOR)%P  ,Z=T6,C=K360    )
!      CALL OS( 'X=Y+Z   ' ,X=TEXP%ADR(IND_NH4)%P,Y=T1,Z=T3           )
      CALL OS( 'X=X+Y   ' ,X=T1,Y=T3                                )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_NH4)%P,Y=T1   ,C=SECTODAY )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 11'
!
!     SEVENTH TRACER [L]: ORGANIC LOAD (IND_OL)
!
!     IMPLICIT PART
!      CALL OS( 'X=CYZ   ' ,X=TIMP%ADR(IND_OL)%P,Y=T11,Z=HPROP,C=-K120)
      CALL OS( 'X=X+CYZ ' ,X=TIMP%ADR(IND_OL)%P,
     &                     Y=T11,Z=HPROP,C=-K120*SECTODAY )
!     EXPLICIT PART
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T12,Z=TN%ADR(IND_PHY)%P,C=O2PHOTO  )
      CALL OVD('X=CY/Z  ' ,T3%R,TN%ADR(IND_OL)%P%R,HPROP%R,WLOR,
     &          NPOIN ,2,0.D0,EPS                                    )
!      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(IND_OL)%P,Y=T1,Z=T3            )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T3                                 )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_OL)%P,Y=T1   ,C=SECTODAY   )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 12'
!
!     EIGTH TRACER: DISSOLVED O2 (IND_O2)
!
!      CALL OS( 'X=Y+C   ' ,X=T1                 ,Y=T7,C=-TRESPIR     )
      CALL OS( 'X=Y-Z   ' ,X=T1                 ,Y=T7,Z=T5           )
!      CALL OS( 'X=CYZ   ' ,X=TEXP%ADR(IND_O2)%P,Y=T1,
      CALL OS( 'X=CYZ   ' ,X=T4,Y=T1,
     &                     Z=TN%ADR(IND_PHY)%P,C=O2PHOTO)
!     -nK520g2[NH4]
      CC=O2NITRI*K520
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T6,Z=TN%ADR(IND_NH4)%P,C=CC        )
!      CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(IND_O2)%P,Y=T1                 )
      CALL OS( 'X=X-Y   ' ,X=T4,Y=T1                                 )
!     K120g3[L]
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T11,Z=TN%ADR(IND_OL)%P,C=K120)
!      CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(IND_O2)%P,Y=T1                 )
      CALL OS( 'X=X-Y   ' ,X=T4,Y=T1                                 )
!     K2g4(Cs-[O2])
      CALL OS( 'X=Y-Z   ' ,X=T1,Y=T2,Z=TN%ADR(IND_O2)%P              )
      CALL OS( 'X=CXYZ  ' ,X=T1,Y=T9,Z=K2,C=1.D0                     )
!      CALL OS( 'X=X+Y   ' ,X=TEXP%ADR(IND_O2)%P,Y=T1                 )
      CALL OS( 'X=X+Y   ' ,X=T4,Y=T1                                )
!     -BEN/h
      CALL OVD('X=Y/Z   ' ,T3%R,T10%R,HPROP%R,0.D0,
     &          NPOIN ,2,0.D0,EPS )
!      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_O2)%P,Y=T3,C=-DEMBEN       )
!      CALL OS( 'X=X+CY  ' ,X=T4,Y=T3                ,C=-DEMBEN       )
      CALL OS( 'X=X-Y   ' ,X=T4,Y=T3                                 )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_O2)%P,Y=T4,C=SECTODAY      )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 14'
!
!-----------------------------------------------------------------------
!
      RETURN
      END
