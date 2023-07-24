!                     **************************
                      SUBROUTINE CALCS3D_BIOMASS
!                     **************************
!
     &  (NPOIN3,NPOIN2,NPLAN,WATTEMP,TN,TEXP,RAYEFF,ZPROP,
     &   T1,T2,T3,T4,T5,T6)
!
!***********************************************************************
! WAQTEL   V8P4
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR PHYTOPLANKTONIC BIOMASS WAQ PROCESS
!
!history  R. ATA
!+        21/03/2016
!+        V7P2
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
!| DT             |-->| TIME STEP
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| NPOIN2         |-->| TOTAL NUMBER OF MESH NODES
!| NPOIN3         |-->| TOTAL NUMBER OF MESH NODES
!| RAYEFF         |-->| EFFECT OF SUNSHINE ON ALGAE GROWTH
!| T1,..,T6       |<->| 3D WORKING STRUCTURES
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
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_WAQTEL,ONLY: CMAX,CTOXIC,IK,ZSD,I0,MEXTINC,KP,KN,
     &  CMORALG,TRESPIR,PROPHOC,DTP,PRONITC,K360,K320,PERNITS,
     &  WPOR,WNOR,SECTODAY,KPE,
     &  IND_T,IND_PHY,IND_NO3,IND_PO4,IND_NOR,IND_POR
      USE INTERFACE_WAQTEL, EX_CALCS3D_BIOMASS => CALCS3D_BIOMASS
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
      INTEGER          , INTENT(IN   ) :: NPOIN2,NPOIN3
      INTEGER          , INTENT(IN   ) :: NPLAN
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,ZPROP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,RAYEFF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T1,T2,T3,T4,T5,T6
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     LOCAL VARIABLES
      INTEGER                     :: I,J
      DOUBLE PRECISION, PARAMETER :: UNSURVINGT=0.05D0
      DOUBLE PRECISION            :: G1
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!    INITIALISATION
!
!     DEPTH IS STORED IN T3 FOR 3D UNTIL RAY_EFFECT_3D,
!     THEN CP IS STORED IN T3 AFTER RAY_EFFECT ==> 3D TABLE
      CALL OS( 'X=0     ',X=T3)
!     DP IS STORED IN T4 ==> 3D TABLE
      CALL OS( 'X=0     ',X=T4)
!     LNUT IS STORED IN T5 ==> 3D TABLE
      CALL OS( 'X=0     ',X=T5)
!
!     G1 IS STORED IN T6, WE TAKE INTO ACCOUNT VARIABLE TEMPERATURE
!
      G1 = WATTEMP/20.D0
      IF( IND_T.GT.0 )THEN
        CALL OS('X=CY    ',X=T6,Y=TN%ADR(IND_T)%P,C=UNSURVINGT)
      ELSE
        CALL OS('X=C     ',X=T6,C=G1)
      ENDIF
!
!     RAYEFF WITH SMITH FORMULA GENERALISED OVER THE VERTICAL
!
      CALL RAY_EFFECT_3D(ZSD,TN%ADR(IND_PHY)%P,NPOIN2,NPLAN,MEXTINC,I0,
     &                   IK,KPE,RAYEFF,ZPROP%R,T1,T2,T3)
!
!     COMPUTE LNUT
!
      CALL NUTEFF(T5%R,TN,NPOIN3,IND_PO4,IND_NO3,KP,KN)
!
!     RATE OF ALGAE GROWTH
!
      CALL ALGAE_GROWTH(T3%R,CMAX,RAYEFF%R,T6,T5%R,CTOXIC(1),NPOIN3)
!
!     RATE OF ALGAE DISAPPEARANCE
!
      CALL ALGAE_DEATH(T4%R,T1%R,CMORALG,TN%ADR(IND_PHY)%P%R,TRESPIR,T6,
     &                  CTOXIC(2),NPOIN3)
!
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
!     SECOND TRACER [PO4] (IND_PO4)
!
      CALL OS( 'X=CY    ' ,X=T1,Y=T4                    ,C=DTP      )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T3                                )
      CALL OS( 'X=CXY   ' ,X=T1,Y=TN%ADR(IND_PHY)%P     ,C=PROPHOC  )
      CALL OS( 'X=CYZ   ' ,X=T2,Y=TN%ADR(IND_POR)%P,Z=T6,C=K320     )
!      CALL OS( 'X=Y+Z   ' ,X=TEXP%ADR(IND_PO4)%P,Y=T1,Z=T2          )
      CALL OS( 'X=X+Y   ' ,X=T1,Y=T2                                )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_PO4)%P,Y=T1   ,C=SECTODAY )
!
!     THIRD TRACER [POR] (IND_POR)
!
      G1=PROPHOC*(1.D0-DTP)
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T4,Z=TN%ADR(IND_PHY)%P,C=G1       )
!      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(IND_POR)%P,Y=T1,Z=T2          )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T2                                )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_POR)%P,Y=T1   ,C=SECTODAY )
!     SURFACE SOURCES
      DO I=1,NPOIN2
        J=(NPLAN-1)*NPOIN2+I
        TEXP%ADR(IND_POR)%P%R(J) = TEXP%ADR(IND_POR)%P%R(J) -
     &          SECTODAY * WPOR
      ENDDO
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
!     FIFTH TRACER [NOR] (IND_NOR)
!
      G1=PRONITC*(1.D0-PERNITS)
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T4,Z=TN%ADR(IND_PHY)%P,C=G1       )
!      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(IND_NOR)%P,Y=T1,Z=T2          )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T2                                )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(IND_NOR)%P,Y=T1   ,C=SECTODAY )
      DO I=1,NPOIN2
        J=(NPLAN-1)*NPOIN2+I
        TEXP%ADR(IND_NOR)%P%R(J) = TEXP%ADR(IND_NOR)%P%R(J) -
     &          SECTODAY * WNOR
      ENDDO
!
!     MASS BALANCE: MASS ADDED BY EXPLICIT TERMS
!                   (IMPLICIT PART IS ADDED IN CVDFTR)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
