!                     **************************
                      SUBROUTINE CALCS2D_BIOMASS
!                     **************************
!
     &  (NPOIN,WATTEMP,TN,TEXP,RAYEFF,HPROP,T1,T2,T3,T4,T5,T6,DEBUG)
!
!***********************************************************************
! WAQTEL   V8P1
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR PHYTOPLANKTONIC BIOMASS WAQ PROCESS
!
!history  R. ATA
!+        21/09/2014
!+        V7P0
!+       CREATION
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
!| T1,..,T7       |<--| WORKING STRUCTURES
!| TN             |-->| TRACER STRUCUTRE
!| TEXP           |<--| EXPLICIT SOURCE TERMES
!| WATTEMP        |-->| WATER TEMPERATURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!-----------------------------------------------------------------------
!***********************************************************************
      USE BIEF
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_WAQTEL,ONLY: CMAX,CTOXIC,IK,ZSD,I0,MEXTINC,KP,KN,
     &  CMORALG,TRESPIR,PROPHOC,DTP,PRONITC,K360,K320,PERNITS,
     &  WPOR,WNOR,SECTODAY,KPE,
     &  IND_T,IND_PHY,IND_NO3,IND_PO4,IND_NOR,IND_POR
      USE INTERFACE_WAQTEL, EX_CALCS2D_BIOMASS => CALCS2D_BIOMASS
!-----------------------------------------------------------------------
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  WPOR     ! R  !    ! VITESSE DE SEDIMENTATION DU PHOSPHORE ORGANIQ!
! !  WNOR     ! R  !    ! VITESSE DE SEDIMENTATION DE L AZOTE ORGANIQUE!
! !  CMAX     ! R  !    ! TAUX DE CROISSANCE ALGALE MAXIMUM A 20degC   !
! !  ZSD      ! R  !    ! PROFONDEUR DE SECCHI                         !
! !  KPE      ! R  !    ! COEF DE TURBIDITE VEGETALE                   !
! !  IK       ! R  !    ! PARAMETRE DE CALAGE DE LA FORMULE DE SMITH   !
! !  KP       ! R  !    ! CONSTANTE DE DEMI-SATURATION EN PHOSPHATE    !
! !  KN       ! R  !    ! CONSTANTE DE DEMI-SATURATION EN AZOTE        !
! !  ALPHA    ! R  !    ! COEF 1 DE TOXICITE DE L EAU POUR LES ALGUES  !
! !  ALPHA2   ! R  !    ! COEF 2 DE TOXICITE DE L EAU POUR LES ALGUES  !
! !  RP       ! R  !    ! TAUX DE RESP. DE LA BIOMASSE ALGALE A 20° C  !
! !  PROPHOC  ! R  !    ! PROP DE PHOSPHORE DANS LES CELLULES DU PHYTO !
! !  DTP      ! R  !    ! POURCENT DE PHOSPH DIRECT ASSIM DS PHY MORT  !
! !  K320     ! R  !    ! TAUX DE TRANSFORMATION DU POR EN PO4         !
! !  PRONITC  ! R  !    ! PROP D AZOTE DANS LES CELLULES DU PHYTO      !
! !  PERNITS  ! R  !    ! POURCENT D AZOTE DIRECT ASSIM DS PHY MORT    !
! !  K360     ! R  !    ! TAUX DE TRANSFORMATION DU NOR EN NO3         !
! !  M1       ! R  !    ! COEF 1 DE MORTALITE ALGALE A 20° C           !
! !  M2       ! R  !    ! COEF 2 DE MORTALITE ALGALE A 20° C           !
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
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,RAYEFF,T1,T2,T3,T4,T5,T6
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     LOCAL VARIABLES
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION, PARAMETER :: UNSURVINGT=0.05D0
      DOUBLE PRECISION            :: G1
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 0'
!
!-----------------------------------------------------------------------
!
!    INITIALISATION
!
!     CP IS STORED IN T3
      CALL OS( 'X=0     ',X=T3)
!     DP IS STORED IN T4
      CALL OS( 'X=0     ',X=T4)
!     LNUT IS STORED IN T5
      CALL OS( 'X=0     ',X=T5)
!
!     G1 IS STOCKED IN T6, WE TAKE INTO ACCOUNT VARIABLE TEMPERATURE
!
      G1 = WATTEMP/20.D0
      IF( IND_T.GT.0 )THEN
        CALL OS('X=CY    ',X=T6,Y=TN%ADR(IND_T)%P,C=UNSURVINGT)
      ELSE
        CALL OS('X=C     ',X=T6,C=G1)
      ENDIF
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 1'
!
!     RAYEFF WITH SMITH FORMULA
!
      CALL RAY_EFFECT(ZSD,TN%ADR(IND_PHY)%P,NPOIN,MEXTINC,I0,IK,KPE,
     &                RAYEFF,HPROP,T1,T2)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 2'
!
!     COMPUTE LNUT
!
      CALL NUTEFF(T5%R,TN,NPOIN,IND_PO4,IND_NO3,KP,KN)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 3'
!
!     RATE OF ALGAE GROWTH
!
      CALL ALGAE_GROWTH(T3%R,CMAX,RAYEFF%R,T6,T5%R,CTOXIC(1),NPOIN)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 4'
!
!     RATE OF ALGAE DISAPPEARANCE
!
      CALL ALGAE_DEATH(T4%R,T1%R,CMORALG,TN%ADR(IND_PHY)%P%R,TRESPIR,T6,
     &                 CTOXIC(2),NPOIN)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 5'
!
!-----------------------------------------------------------------------
!
!     LET'S NOW COMPUTE SOURCE TERMS
!
!     FIRST TRACER [PHY] (IND_PHY)
!
      CALL OS( 'X=Y-Z   ' ,X=T1                 ,Y=T3,Z=T4)
!      CALL OS( 'X=YZ    ' ,X=TEXP%ADR(IND_PHY)%P,Y=T1,
!     &                     Z=TN%ADR(IND_PHY)%P)
      CALL OS( 'X=X+CYZ ' ,X=TEXP%ADR(IND_PHY)%P,Y=T1,
     &                     Z=TN%ADR(IND_PHY)%P,C=SECTODAY)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 6'
!
!     SECOND TRACER [PO4] (IND_PO4)
!
      CALL OS( 'X=CY    ' ,X=T1,Y=T4                    ,C=DTP      )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T3                                )
      CALL OS( 'X=CXY   ' ,X=T1,Y=TN%ADR(IND_PHY)%P     ,C=PROPHOC  )
      CALL OS( 'X=CYZ   ' ,X=T2,Y=TN%ADR(IND_POR)%P,Z=T6,C=K320     )
!
!      CALL OS( 'X=Y+Z   ' ,X=TEXP%ADR(IND_PO4)%P,Y=T1,Z=T2          )
      CALL OS( 'X=X+Y   ' ,X=T1,Y=T2                                )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_PO4)%P,Y=T1   ,C=SECTODAY )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 7'
!
!     THIRD TRACER [POR] (IND_POR)
!
      G1=PROPHOC*(1.D0-DTP)
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T4,Z=TN%ADR(IND_PHY)%P,C=G1       )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T2                                )
      CALL OVD('X=CY/Z  ' ,T2%R,TN%ADR(IND_POR)%P%R,HPROP%R,WPOR,
     &          NPOIN ,2,0.D0,EPS                                   )
!      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(IND_POR)%P,Y=T1,Z=T2          )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T2                                )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_POR)%P,Y=T1   ,C=SECTODAY )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 8'
!
!     FOURTH TRACER [NO3] (IND_NO3)
!
      CALL OS( 'X=CY    ' ,X=T1,Y=T4                      ,C=PERNITS)
      CALL OS( 'X=C(Y-Z)' ,X=T1,Y=T1                 ,Z=T3,C=PRONITC)
      CALL OS( 'X=XY    ' ,X=T1,Y=TN%ADR(IND_PHY)%P                 )
      CALL OS( 'X=CYZ   ' ,X=T2,Y=TN%ADR(IND_NOR)%P  ,Z=T6,C=K360   )
!
!      CALL OS( 'X=Y+Z   ' ,X=TEXP%ADR(IND_NO3)%P,Y=T1,Z=T2          )
      CALL OS( 'X=X+Y   ' ,X=T1,Y=T2                                )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_NO3)%P,Y=T1   ,C=SECTODAY )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 9'
!
!     FIFTH TRACER [NOR] (IND_NOR)
!
      G1=PRONITC*(1.D0-PERNITS)
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T4,Z=TN%ADR(IND_PHY)%P,C=G1       )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T2                                )
      CALL OVD('X=CY/Z  ' ,T2%R,TN%ADR(IND_NOR)%P%R,HPROP%R,WNOR,
     &         NPOIN ,2,0.D0,EPS )
!
!      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(IND_NOR)%P,Y=T1,Z=T2          )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T2                                )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_NOR)%P,Y=T1   ,C=SECTODAY )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 10'
!
!-----------------------------------------------------------------------
!
      RETURN
      END
