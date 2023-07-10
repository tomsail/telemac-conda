Module M_MY_GLOBAL_VAR_SED

  use M_PRECISION

  implicit none

  integer      :: NIteSed = 0                      ! Nombre d'iterations de calcul sedimentaire
!  real(DOUBLE) :: fracH = 0.05D0                   ! MS2018 ==> devient un mot-cle
  real(DOUBLE) :: fracH                            ! Pourcentage pour le critere de planimetrage
  real(DOUBLE) :: absolute_clip                    ! clip absolu en cas de desactivation du clipping
  logical      :: optionPente                      ! MS2018 choix entre une pente locales ou non
  real(DOUBLE) :: dm, d16, d84, porosite           ! MS2018 diametre moyen, d16 et d84 pour lefort et recking
  logical      :: bedload_option                   ! bedload option
  logical      :: suspension_option                ! bedload option
  logical      :: sediment_slide_option            ! sediment slide option
  logical      :: clipping_option                  ! planim clipping option
  logical      :: without_voids                    ! prise en compte de la porosite
  logical      :: debug_bedload                    ! debug bedload
  logical      :: qstar_option                     ! option for Lefort2014 (Ks over Krwith qstar or not)
  integer      :: bedload_transport_law            ! bedload transport law

  integer      :: recking_morpho                   ! option for type of morphology river (1 for pool-riffle, 2 for others)
  logical      :: recking_q_star_option            ! option to use expression of Tau with Q (True) or with Rh (false)

  real(DOUBLE) :: equilibrium_slope                ! valeur de la pente d'equilibre pour l'option cl amont avec formule
  logical      :: equilibrium_slope_option         ! option cl amont calcule avec la formule de transport et une pente

  logical      :: strickler_slope_option           ! option Strickler pente energie
  logical      :: round_slope_option               ! option arrondi pente energie
  integer      :: round_precision

  real(DOUBLE), dimension(:)  , pointer :: Vsed   => null()      ! Volume de sediments transportes
  real(DOUBLE), dimension(:)  , pointer :: Hsed   => null()      ! Epaisseur de sediments deposes ou erodes
  real(DOUBLE), dimension(:)  , pointer :: myZsl  => null()      ! Cote de la surface libre
  real(DOUBLE), dimension(:)  , pointer :: mySm   => null()      ! Surface mouillee
  real(DOUBLE), dimension(:,:), pointer :: DeltaH => null()

End Module M_MY_GLOBAL_VAR_SED

