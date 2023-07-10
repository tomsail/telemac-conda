Module M_INIT_VAR_SORTIE_COURLIS_S

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       08/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :  Module declarant les types et definissant les structures
!  --------    pour les variables a sortir (i.e. a stocker ou a imprimer
!              sur listing)
!
!=========================================================================

!=========================== Declarations ================================

!.. Modules importes ..
!----------------------

  use M_PRECISION

! Definition de 2 structures qui permettront de realiser des boucles
! sur les variables a stocker ou a imprimer sur listing

! 1. Definition de la structure contenant toutes les informations constantes
! sur les variables a sortir.

!..................... definition du type de la structure VAR_NOM_T .....!
!                                                                        !
  integer , parameter :: LEN_NOM_LONG = 39                               !
  integer , parameter :: LEN_NOM_A4   =  4                               !
  integer , parameter :: LEN_UNITE    =  6                               !
!                                                                        !
  type VAR_NOM_T                                                         !
     sequence                                                            !
     logical                 :: Obligatoire                              !
     character(LEN_NOM_LONG) :: NomLong                                  !
     character(LEN_NOM_A4)   :: NomA4                                    !
     character(LEN_UNITE)    :: Unite                                    !
     integer                 :: Precision                                !
     logical                 :: DependantTemps                           !
  end type VAR_NOM_T                                                     !
!                                                                        !
!............. fin de la definition du type de la structure VAR_NOM_T ...!


! 2. Definition de la structure contenant toutes les informations dependantes
! de la simulation sur les variables a sortir.

!....................... definition du type de la structure GDR_STO_T ...!
!                                                                        !
  type GDR_STO_T                                                         !
     sequence                                                            !
     logical                             :: ASortir                      !
     real(DOUBLE), dimension(:), pointer :: Valeur                       !
  end type GDR_STO_T                                                     !
!                                                                        !
!............. fin de la definition du type de la structure GDR_STO_T ...!

Contains

  Subroutine  INIT_VAR_SORTIE_COURLIS_S  (  &
    Var_nom          ,  & ! Structure sur les informations constantes
    Gdr              ,  & ! Structure sur les informations non constantes
    X                ,  & ! Maillage
    Zsurf            ,  & ! Cote de la surface libre
    Vitesse          ,  & ! Vitesse moyenne par section
    SurfMouil        ,  & ! Surface mouillee
    PerimMouil       ,  & ! Perimetre mouille
    CVase            ,  & ! Concentration des vases en suspension
    CSable           ,  & ! Concentration des sables en suspension
    Zref             ,  & ! Cote des points bas des interfaces sedimentaires
    NbInterface      ,  & ! Nombre d'interfaces sedimentaires
    QVase            ,  & ! Flux de depot des vases par couche (> 0 depot, < 0 erosion)
    QSable           ,  & ! Flux de depot des sables par couche (> 0 depot, < 0 erosion)
    TauHMoy          ,  & ! Contrainte hydraulique moyenne dans la section
    TauHMax          ,  & ! Contrainte hydraulique maximale dans la section
    TauEMoy          ,  & ! Contrainte hydraulique effective moyenne ds section
    TauEMax          ,  & ! Contrainte hydraulique effective maximale ds section
    CeqMoy           ,  & ! Concentration d'equilibre des sables moyenne dans la section
    DeltaSurfaceSed  ,  & ! Variation de la surface sedimentaire
    DepotCumulCouche ,  & ! Depot cumule /profil et /couche (> 0 depot, < 0 erosion)
    VarASortir       ,  & ! Drapeaux sur les variables a sortir
    PhaseSimulation  )    ! Variable indiquant la phase de la simulation

    !*************************************************************************
    !  PROGICIEL : COURLIS           Ch. BERTIER
    !
    !  VERSION : 4.0       08/2003    Copyright EDF-CETMEF
    !
    !*************************************************************************
    !=========================================================================
    !
    !  Fonction :  Module declarant les types et definissant les structures
    !  --------    pour les variables a sortir (i.e. a stocker ou a imprimer
    !        sur listing)
    !
    !=========================================================================

    !
    !============================= Declarations ==============================

    !.. Modules importes ..
    !----------------------

    use M_INDEX_VARIABLE_COURLIS_C    ! Constantes servant a reperer les variables
    use M_CONSTANTES_CALCUL_C         ! Constante servant a reperer la phase de calcul

    !.. Declarations explicites ..
    !-----------------------------

    implicit none

    !.. Arguments ..
    !---------------

    type(VAR_NOM_T), dimension(:)  , intent(  out) :: Var_nom
    type(GDR_STO_T), dimension(:)  , intent(  out) :: Gdr
    real(DOUBLE)   , dimension(:)  , pointer       :: X
    real(DOUBLE)   , dimension(:)  , pointer       :: Zsurf, Vitesse, SurfMouil, PerimMouil
    real(DOUBLE)   , dimension(:)  , pointer       :: CVase, CSable, QVase, QSable
    real(DOUBLE)   , dimension(:,:), pointer       :: Zref
    real(DOUBLE)   , dimension(:)  , pointer       :: TauHMoy, TauHMax, TauEMoy, TauEMax, CeqMoy
    real(DOUBLE)   , dimension(:)  , pointer       :: DeltaSurfaceSed
    real(DOUBLE)   , dimension(:,:), pointer       :: DepotCumulCouche
    integer                        , intent(in   ) :: NbInterface
    logical        , dimension(:)  , intent(in   ) :: VarASortir
    integer                        , intent(in   ) :: PhaseSimulation

    integer :: k
    character*2, dimension(10) :: i_in_letter
    data i_in_letter / '1','2','3','4','5','6','7','8','9','10'/

    !============================ Initialisations ===========================

    label_initialisation : If (PhaseSimulation == PHASE_INITIALISATION) Then

      !.....................     initialisation du tableau de structure Var_nom ........................
      Var_nom(VARCO_X      ) = VAR_NOM_T(.true. ,"Maillage                         ","X   ","m     ",2,.false.)
      Var_nom(VARCO_Vit    ) = VAR_NOM_T(.true. ,"Vitesse                          ","VIT ","m/s   ",3,.true. )
      Var_nom(VARCO_Sm     ) = VAR_NOM_T(.true. ,"Surface Mouillee                 ","SMOU","m2    ",2,.true. )
      Var_nom(VARCO_Pm     ) = VAR_NOM_T(.true. ,"Perimetre Mouille                ","PMOU","m     ",2,.true. )
      Var_nom(VARCO_Zsurf  ) = VAR_NOM_T(.true. ,"Cote de la surface libre         ","ZSUR","m     ",3,.true. )
      Var_nom(VARCO_CVase  ) = VAR_NOM_T(.true. ,"Concentration en vase            ","CVAS","g/l   ",4,.true. )
      Var_nom(VARCO_CSable ) = VAR_NOM_T(.true. ,"Concentration en sable           ","CSAB","g/l   ",4,.true. )
      Var_nom(VARCO_Zdur   ) = VAR_NOM_T(.true. ,"Cote des fonds durs libre        ","ZDUR","m     ",3,.false.)
      Var_nom(VARCO_Qvase  ) = VAR_NOM_T(.true. ,"Flux de depot des vases          ","FLUV","kg/m/s",4,.true. )
      Var_nom(VARCO_Qsable ) = VAR_NOM_T(.true. ,"Flux de depot des sables         ","FLUS","kg/m/s",4,.true. )
      Var_nom(VARCO_DSurSed) = VAR_NOM_T(.true. ,"Variation de surface sedimentaire","DSUR","m2    ",2,.true. )
      Var_nom(VARCO_TauHMax) = VAR_NOM_T(.true. ,"Contrainte locale maximale       ","THMA","N/m2  ",3,.true. )
      Var_nom(VARCO_TauHMoy) = VAR_NOM_T(.true. ,"Contrainte locale moyenne        ","THMO","N/m2  ",3,.true. )
      Var_nom(VARCO_TauEMax) = VAR_NOM_T(.true. ,"Contrainte effective maximale    ","TEMA","N/m2  ",3,.true. )
      Var_nom(VARCO_TauEMoy) = VAR_NOM_T(.true. ,"Contrainte effective moyenne     ","TEMO","N/m2  ",3,.true. )
      Var_nom(VARCO_CeqMoy ) = VAR_NOM_T(.true. ,"Concentration d equilibre moyenne","CEQM","g/l   ",4,.true. )

      Do k = 1, NbInterface-1
          Var_nom(VARCO_Zref(k)) = VAR_NOM_T(.true.                                 , &
                                             "Cote de l interface "//i_in_letter(k) , &
                                             "ZF"//i_in_letter(k),"m     ",3,.true. )

          Var_nom(VARCO_DepC(k)) = VAR_NOM_T(.true.                                       , &
                                             "Depot cumule de la couche "//i_in_letter(k) , &
                                             "MDE"//i_in_letter(k),"tonnes",3,.true.      )
      Enddo

    End if label_initialisation

    !============================ Instructions ==============================

    ! Une variable a sortir est :
    ! - soit une variable     obligatoire `Var_nom(:)%Obligatoire == .true.'
    ! - soit une variable non obligatoire mais qui a ete choisie comme etant a
    !   sortir `Var_nom(:)%Obligatoire == .false. .and. VarASortir(:) == .true.'

    Gdr(:)%ASortir = Var_nom(:)%Obligatoire .or. VarASortir(:)

    Gdr(VARCO_X      )%Valeur     => X
    Gdr(VARCO_Vit    )%Valeur     => Vitesse
    Gdr(VARCO_Sm     )%Valeur     => SurfMouil
    Gdr(VARCO_Pm     )%Valeur     => PerimMouil
    Gdr(VARCO_Zsurf  )%Valeur     => Zsurf
    Gdr(VARCO_CVase  )%Valeur     => CVase
    Gdr(VARCO_CSable )%Valeur     => CSAble
    Gdr(VARCO_Zdur   )%Valeur     => Zref(NbInterface,:)
    Gdr(VARCO_Qvase  )%Valeur     => QVase
    Gdr(VARCO_Qsable )%Valeur     => QSAble
    Gdr(VARCO_DSurSed)%Valeur     => DeltaSurfaceSed
    Gdr(VARCO_TauHMax)%Valeur     => TauHMax
    Gdr(VARCO_TauHMoy)%Valeur     => TauHMoy
    Gdr(VARCO_TauEMax)%Valeur     => TauEMax
    Gdr(VARCO_TauEMoy)%Valeur     => TauEMoy
    Gdr(VARCO_CeqMoy )%Valeur     => CeqMoy
    Do k = 1, NbInterface-1
        Gdr(VARCO_Zref(k))%Valeur   => Zref(k,:)
        Gdr(VARCO_DepC(k))%Valeur   => DepotCumulCouche(k,:)
    Enddo

  End Subroutine INIT_VAR_SORTIE_COURLIS_S

End Module M_INIT_VAR_SORTIE_COURLIS_S
