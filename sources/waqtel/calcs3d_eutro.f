!                     ************************
                      SUBROUTINE CALCS3D_EUTRO
!                     ************************
!
     &  (NPOIN3,NPOIN2,NPLAN,WATTEMP,TN,TEXP,TIMP,RAYEFF,HPROP,
     &   ZPROP,T1,T2_1,T2_2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,DEBUG,
     &   UN,VN,DT)
!
!***********************************************************************
! WAQTEL   V8P4
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR EUTRO WAQ 3d PROCESS
!
!history  R. ATA
!+        21/03/2016
!+        V7P2
!+       CREATION (VOID)
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
!| DT             |-->| TIME STEP
!| HPROP          |-->| WATER DEPTH AFTER PROPAGATION (2D STRUCTURE)
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| NPOIN2         |-->| TOTAL NUMBER OF MESH NODES
!| NPOIN3         |-->| TOTAL NUMBER OF MESH NODES
!| RAYEFF         |-->| EFFECT OF SUNSHINE ON ALGAE GROWTH
!| T1,..,T12      |<->| 3D WORKING STRUCTURES
!| T2_1,T2_2      |<->| 2D WORKING STRUCTURES
!| TN             |-->| TRACER STRUCUTRE
!| TEXP           |<--| EXPLICIT SOURCE TERMES
!| VOLU2D         |-->| BASES AREA (NON ASSEMBLED)
!| WATTEMP        |-->| WATER TEMPERATURE
!| ZPROP          |-->| 3D MESH NODE POSTIONS AFTER PROPAGATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!-----------------------------------------------------------------------
!***********************************************************************
      USE BIEF
      USE DECLARATIONS_WAQTEL,ONLY:CMAX,CTOXIC,IK,K520,O2SATU,K2,
     &  ZSD,I0,KPE,KP,KN,CMORALG,FORMCS,TRESPIR,PROPHOC,DTP,PRONITC,
     &  K22,WLOR,K360,K320,PERNITS,WPOR,WNOR,FORMK2,O2PHOTO,K120,
     &  O2NITRI,DEMBEN,MEXTINC,SECTODAY,IND_T,
     &  IND_PHY,IND_PO4,IND_POR,IND_NO3,IND_NOR,IND_NH4,IND_OL,IND_O2
      USE INTERFACE_WAQTEL, EX_CALCS3D_EUTRO => CALCS3D_EUTRO
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  U        ! TR ! D  ! VITESSE DE L'EAU                             !
! !  J        ! TR ! D  ! INDEX FOR THE PLAN NUMBER                 !
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
      INTEGER          , INTENT(IN   ) :: NPOIN3,NPOIN2,NPLAN
      INTEGER          , INTENT(IN   ) :: DEBUG
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP,DT
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,ZPROP,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP,RAYEFF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T1,T2_1,T3,T4,T5,T6,T7,T8,T9
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T10,T11,T12,T2_2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
!
      INTEGER                     :: I,J
      DOUBLE PRECISION, PARAMETER :: UNSURVINGT=0.05D0
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-10
      DOUBLE PRECISION, PARAMETER :: CORR1=1.065D0
!      DOUBLE PRECISION, PARAMETER :: CORR2=1.0241D0
      DOUBLE PRECISION, PARAMETER :: CORR2=1.025D0
      DOUBLE PRECISION            :: G1,G2,G3,CC,POWER
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN EUTRO, STEP 0'
!
!     INITIALISATION
!
!     CS IS STORED IN T2_1 ==> 2D TABLE
      CALL OS( 'X=0     ',X=T2_1)
!     CS IS STORED IN T2_2 ==> 2D TABLE
      CALL OS( 'X=0     ',X=T2_2)
!     G2 IS STORED IN T6 ==> 3D TABLE
      CALL OS( 'X=0     ',X=T6)
!     CP IS STORED IN T7
      CALL OS( 'X=0     ',X=T7)
!     DP IS STORED IN T4
      CALL OS( 'X=0     ',X=T4)
!     DEPTH IS STORED IN T5 FOR 3D UNTIL RAY_EFFECT_3D,
!     THEN LNUT IS STORED IN T5 AFTER RAY_EFFECT_3D UNTIL ALGAE_GROWTH,
!     THEN RESPIR*G1 IS STORED IN T5 FOR THE 8TH EQUATION
      CALL OS( 'X=0     ',X=T5)
!     RN IS STORED IN T8
      CALL OS( 'X=0     ',X=T8)
!
!     G2 IS STORED IN T6,WE TAKE INTO ACCOUNT FOR VARIABLE TEMPERATURE
!
!      G2 = WATTEMP/20.D0
!      IF( IND_T.GT.0 ) THEN
!        CALL OS('X=CY    ',X=T6,Y=TN%ADR(IND_T)%P,C=UNSURVINGT)
!      ELSE
!        CALL OS('X=C     ',X=T6 ,C=G2)
!      ENDIF
!
!     COMPUTE G2,G3,BENCOR
!
      POWER = WATTEMP-20.D0
      G2    = 1.050D0**POWER
      G3    = 1.047D0**POWER
      DO I=1,NPOIN3
        IF( IND_T.GT.0 ) THEN
          POWER=TN%ADR(IND_T)%P%R(I)-20.D0
          G2   =1.050D0**POWER
          G3   =1.047D0**POWER
        ENDIF
!       CORR2T AND BENCOR STORED HERE IN T9,T10
        T9%R(I)=CORR2**POWER
        T10%R(I)=DEMBEN*(CORR1**POWER)
!       G2 IS STORED IN T6
        T6%R(I)=G2
!       G3 IS STORED IN T11
        T11%R(I)=G3
      ENDDO
!
!     COMPUTE CS (O2SATU, STORED IN T2_1): O2 SATURATION DENSITY OF WATER
!
      IF( IND_T.EQ.0 ) THEN
        CALL SATUR_O2(O2SATU,FORMCS,WATTEMP,EPS)
        CALL OS('X=C     ',X=T2_1,C=O2SATU       )
      ELSE
        DO I=1,NPOIN2
          J=(NPLAN-1)*NPOIN2+I
          CALL SATUR_O2(T2_1%R(I),FORMCS,TN%ADR(IND_T)%P%R(J),EPS)
        ENDDO
      ENDIF
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 1'
!
!     RAYEFF WITH SMITH FORMULA GENERALISED OVER THE VERTICAL
!
      CALL RAY_EFFECT_3D(ZSD,TN%ADR(IND_PHY)%P,NPOIN2,NPLAN,MEXTINC,I0,
     &                   IK,KPE,RAYEFF,ZPROP%R,T3,T4,T5)
!
!     COMPUTE LNUT: EFFECTS OF PHOSPHORIOUS AND NITROGENIOUS
!     NUTRIMENTS ON ALGAE GROWTH ==> STORED IN T5
!
!      CALL NUTEFF(T5%R,TN,NPOIN3,IND_PO4,IND_NO3,KP,KN)
!
!     NUTEFF DOEST NOT TAKE NH4 INTO ACCOUNT
      DO I=1,NPOIN3
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
      CALL ALGAE_GROWTH(T7%R,CMAX,RAYEFF%R,T6,T5%R,CTOXIC(1),NPOIN3)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 4'
!
!     RATE OF ALGAE DISAPPEARANCE DP (STORED IN T4) AND MP (STOPCKED IN T12)
!
      CALL ALGAE_DEATH(T4%R,T12%R,CMORALG,TN%ADR(IND_PHY)%P%R,TRESPIR,
     &                 T6,CTOXIC(2),NPOIN3)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 5'
!
!     COMPUTE K2
!
      CALL REAER(FORMK2,K2,K22,NPOIN2,NPLAN,UN,VN,HPROP,EPS)
!
!     COMPUTE RN: PROPORTION OF NITROGEN ASSIMILATED AS NH4(STORED IN T8)
!
      CALL OS( 'X=Y+Z   ' ,X=T1,Y=TN%ADR(IND_NH4)%P,Z=TN%ADR(IND_NO3)%P)
      CALL OVD('X=Y/Z   ' ,T8%R,TN%ADR(IND_NH4)%P%R,T1%R,0.D0,
     &          NPOIN3 ,2,0.D0,EPS )
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
!     ------------------------------
!
!     FIRST TRACER [PHY] (IND_PHY)
!
      CALL OS( 'X=Y-Z   ' ,X=T1                 ,Y=T7,Z=T4)
!      CALL OS( 'X=YZ    ' ,X=TEXP%ADR(IND_PHY)%P,Y=T1,
!     &                     Z=TN%ADR(IND_PHY)%P)
      CALL OS( 'X=X+CYZ ' ,X=TEXP%ADR(IND_PHY)%P,Y=T1,
     &                     Z=TN%ADR(IND_PHY)%P  ,C=SECTODAY )
!      CALL OS( 'X=YZ    ' ,X=T3                 ,Y=T1,
!     &                     Z=TN%ADR(IND_PHY)%P              )
!      CALL OS( 'X=CY    ' ,X=TEXP%ADR(IND_PHY)%P,Y=T3,C=SECTODAY )
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
!      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(IND_POR)%P,Y=T1,Z=T3          )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T3                                )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_POR)%P,Y=T1   ,C=SECTODAY )
!     SURFACE SOURCES
      DO I=1,NPOIN2
        J=(NPLAN-1)*NPOIN2+I
        TEXP%ADR(IND_POR)%P%R(J) = TEXP%ADR(IND_POR)%P%R(J) -
     &          SECTODAY * WPOR
      ENDDO
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
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_NO3)%P,Y=T3    ,C=SECTODAY )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 9'
!
!     FIFTH TRACER [NOR] (IND_NOR)
!
      CC=PRONITC*(1.D0-PERNITS)
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T4,Z=TN%ADR(IND_PHY)%P,C=CC       )
      CALL OS( 'X=CYZ   ' ,X=T3,Y=TN%ADR(IND_NOR)%P  ,Z=T6,C=K360   )
!      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(IND_NOR)%P,Y=T1,Z=T3          )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T3                                 )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_NOR)%P,Y=T1    ,C=SECTODAY )
!     surface sources
      DO I=1,NPOIN2
        J=(NPLAN-1)*NPOIN2+I
        TEXP%ADR(IND_NOR)%P%R(J) = TEXP%ADR(IND_NOR)%P%R(J) -
     &          SECTODAY * WNOR
      ENDDO
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 10'
!
!     SIXTH TRACER [NH4] : AMMONIACAL LOAD (IND_NH4)
!
!     IMPLICIT PART
!      CALL OS( 'X=CY    ' ,X=TIMP%ADR(IND_NH4)%P,Y=T6,C=K520         )
      CALL OS( 'X=X+CY  ' ,X=TIMP%ADR(IND_NH4)%P,Y=T6,C=K520*SECTODAY )
!      CALL OS( 'X=CY    ' ,X=T1                 ,Y=T6,C=K520         )
!      CALL OS( 'X=X+CY  ' ,X=TIMP%ADR(IND_NH4)%P,Y=T1,C=SECTODAY     )
!     EXPLICIT PART
      CALL OS( 'X=CY    ' ,X=T1,Y=T4                      ,C=PERNITS )
      CALL OS( 'X=YZ    ' ,X=T3,Y=T7,Z=T8                            )
      CALL OS( 'X=C(Y-Z)' ,X=T3,Y=T1,Z=T3                 ,C=PRONITC )
      CALL OS( 'X=XY    ' ,X=T3,Y=TN%ADR(IND_PHY)%P                  )
      CALL OS( 'X=CYZ   ' ,X=T1,Y=TN%ADR(IND_NOR)%P  ,Z=T6,C=K360    )
!      CALL OS( 'X=Y+Z   ' ,X=TEXP%ADR(IND_NH4)%P,Y=T1,Z=T3           )
      CALL OS( 'X=X+Y   ' ,X=T1,Y=T3                                 )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_NH4)%P,Y=T1    ,C=SECTODAY )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 11'
!
!     SEVENTH TRACER [L]: ORGANIC LOAD (IND_OL)
!
!     implicit part
!      CALL OS( 'X=CY    ' ,X=TIMP%ADR(IND_OL)%P,Y=T11,C=K120)
      CALL OS( 'X=X+CY  ' ,X=TIMP%ADR(IND_OL)%P,Y=T11,C=K120*SECTODAY)
!      CALL OS( 'X=CY    ' ,X=T1                ,Y=T11,C=K120)
!      CALL OS( 'X=X+CY  ' ,X=TIMP%ADR(IND_OL)%P,Y=T1,C=SECTODAY      )
!     explicit part
!      CALL OS( 'X=CYZ   ' ,X=TEXP%ADR(IND_OL)%P,Y=T12,
!     &                     Z=TN%ADR(IND_PHY)%P,C=O2PHOTO             )
      CALL OS( 'X=X+CYZ ' ,X=TEXP%ADR(IND_OL)%P,Y=T12,
     &                     Z=TN%ADR(IND_PHY)%P,C=O2PHOTO*SECTODAY   )
!      CALL OS( 'X=CYZ   ' ,X=T1                ,Y=T12,
!     &                     Z=TN%ADR(IND_PHY)%P,C=O2PHOTO             )
!      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_OL)%P,Y=T1,C=SECTODAY      )
!     surface terms
      DO I=1,NPOIN2
        J=(NPLAN-1)*NPOIN2+I
!       - FLOR/H = WLOR[L]/H
        TEXP%ADR(IND_OL)%P%R(J) = TEXP%ADR(IND_OL)%P%R(J) -
     &       SECTODAY * WLOR*TN%ADR(IND_OL)%P%R(J)
      ENDDO
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 12'
!
!     EIGTH TRACER: DISSOLVED O2 (IND_O2)
!
!      CALL OS( 'X=Y+C   ' ,X=T1                 ,Y=T7,C=-TRESPIR     )
      CALL OS( 'X=Y-Z   ' ,X=T1                 ,Y=T7,Z=T5           )
!      CALL OS( 'X=CYZ   ' ,X=TEXP%ADR(IND_O2)%P,Y=T1,
      CALL OS( 'X=CYZ   ' ,X=T4                ,Y=T1,
     &                     Z=TN%ADR(IND_PHY)%P,C=O2PHOTO             )
!     -nK520g2[NH4]
      CC=O2NITRI*K520
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T6,Z=TN%ADR(IND_NH4)%P,C=CC        )
!      CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(IND_O2)%P,Y=T1                )
      CALL OS( 'X=X-Y   ' ,X=T4                ,Y=T1                 )
!     K120g3[L]
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T11,Z=TN%ADR(IND_OL)%P,C=K120)
!      CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(IND_O2)%P,Y=T1                )
      CALL OS( 'X=X-Y   ' ,X=T4                ,Y=T1                 )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_O2)%P,Y=T4,C=SECTODAY      )

!     surface sources
      DO I=1,NPOIN2
        J=(NPLAN-1)*NPOIN2+I
        T2_2%R(I)=0.5D0*(ZPROP%R(J)-ZPROP%R(J-NPOIN2))
!       K2g4(Cs-[O2]) - BEN/H
        TEXP%ADR(IND_O2)%P%R(J) = TEXP%ADR(IND_O2)%P%R(J)
     &    + SECTODAY * ( K2%R(I)*T9%R(J) *
     &               MAX( (T2_1%R(I)-TN%ADR(IND_O2)%P%R(J)),0.D0 )
     &    - T10%R(J) * T2_2%R(I) )
      ENDDO
!
      DO I=1,NPOIN3
        TEXP%ADR(IND_O2)%P%R(I) = MAX(TEXP%ADR(IND_O2)%P%R(I) ,
     &                                -TN%ADR(IND_O2)%P%R(I)/DT)
      ENDDO
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 14'
!
!-----------------------------------------------------------------------
!
      RETURN
      END
