!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

SUBROUTINE CALCS_BIOMASS( RNU , S , &
                          Nbsect , NBTRA , Nbsing , &
                          Q , A , H , RH , ST , C , &
                          SA , T , TParph , TMeteo , &
                          DT , Erreur )

!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - N.GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
!  FONCTION : MODELE DE QUALITE D'EAU BIOMASS
!  --------
!
!       CE SOUS PROGRAMME CALCULE LES TERMES SOURCES IMPLICITES
!       ET EXPLICITES, VOLUMIQUES ET SURFACIQUES,
!       UTILISEES DANS L'EQUATION DE CONSERVATION DU TRACEUR
!
!       POUR UN PROBLEME DE BIOMASSE PHYTOPLANCTONIQUE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! ! NBTRA     ! E  ! M  ! NOMBRE DE TRACEURS                           !
! !  Q        ! TR ! D  ! DEBIT                                        !
! !  A        ! TR ! D  ! SECTION MOUILLEE                             !
! !  Z        ! TR ! D  ! HAUTEUR D EAU                                !
! !  RH       ! TR ! D  ! RAYON HYDRAULIQUE                            !
! !  ST       ! TR ! D  ! STRICKLER                                    !
! !  IM       ! E  ! M  ! NOMBRE DE SECTIONS DE CALCUL                 !
! !  C        ! TR ! D  ! CONCENTRATIONS                               !
! !  SVA      ! TR ! D  ! TERMES SOURCES VOLUMIQUE AJOUTES             !
! !  SSA      ! TR ! D  ! TERME SOURCE SURFACIQUE  AJOUTES             !
! !  T        !  R ! D  ! TEMPS                                        !
! !  DT       !  R ! D  ! PAS DE TEMPS                                 !
!  RESULTATS------------------------------------------------------------
! !  RNUV     ! TR ! D  ! TERMES SOURCES VOLUMIQUES IMPLICITES         !
! !  RNUS     ! TR ! D  ! TERME SOURCE SURFACIQUE IMPLICITES           !
! !  SV       ! TR ! D  ! TERMES SOURCES  EXPLICITES                   !
! !___________!____!____!______________________________________________!
!                               COMMON
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  NMSCAL   ! E  ! M  ! NOMBRE MAXIMUM DE SECTIONS DE CALCUL         !
! !  NMTRA    ! E  ! M  ! NOMBRE MAXIMUM DE TRACEURS                   !
! !___________!____!____!______________________________________________!
!                          VARIABLES INTERNES
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  WPOR     ! R  !    ! VITESSE DE SEDIMENTATION DU PHOSPHORE ORGANIQ!
! !  WNOR     ! R  !    ! VITESSE DE SEDIMENTATION DE L AZOTE ORGANIQUE!
! !  CMAX     ! R  !    ! TAUX DE CROISSANCE ALGALE MAXIMUM A 20° C  !
! !  PS       ! R  !    ! PROFONDEUR DE SECCHI                         !
! !  KPE      ! R  !    ! COEF D EXTINCTION DU RAY SANS PHYTO          !
! !  BETA     ! R  !    ! COEF DE TURBIDITE VEGETALE                   !
! !  IK       ! R  !    ! PARAMETRE DE CALAGE DE LA FORMULE DE SMITH   !
! !  KP       ! R  !    ! CONSTANTE DE DEMI-SATURATION EN PHOSPHATE    !
! !  KN       ! R  !    ! CONSTANTE DE DEMI-SATURATION EN AZOTE        !
! !  ALPHA1   ! R  !    ! COEF 1 DE TOXICITE DE L EAU POUR LES ALGUES  !
! !  ALPHA2   ! R  !    ! COEF 2 DE TOXICITE DE L EAU POUR LES ALGUES  !
! !  RP       ! R  !    ! TAUX DE RESP. DE LA BIOMASSE ALGALE A 20° C  !
! !  FP       ! R  !    ! PROP DE PHOSPHORE DANS LES CELLULES DU PHYTO !
! !  DTP      ! R  !    ! POURCENT DE PHOSPH DIRECT ASSIM DS PHY MORT  !
! !  K1       ! R  !    ! TAUX DE TRANSFORMATION DU POR EN PO4         !
! !  FN       ! R  !    ! PROP D AZOTE DANS LES CELLULES DU PHYTO      !
! !  DTN      ! R  !    ! POURCENT D AZOTE DIRECT ASSIM DS PHY MORT    !
! !  K2       ! R  !    ! TAUX DE TRANSFORMATION DU NOR EN NO3         !
! !  M1       ! R  !    ! COEF 1 DE MORTALITE ALGALE A 20° C           !
! !  M2       ! R  !    ! COEF 2 DE MORTALITE ALGALE A 20° C           !
! !           !    !    !                                              !
! !  IF1      ! TR ! D  ! INDIC DE LECTURE DU FICHIER DES PARAMETRES   !
! !___________!____!____!______________________________________________!
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!***********************************************************************

   USE  M_PRECISION
   USE  M_PARAMETRE_C
   USE  M_CONSTANTES_TRACER_T
   USE  M_PARAMETRES_QUALITE_EAU_T
   USE  M_METEO_T
   USE  M_ERREUR_T
   USE  M_INTERPOLATION_S

   IMPLICIT NONE

   REAL(DOUBLE), DIMENSION(:,:) ,intent(inout) :: RNU , S , SA
   REAL(DOUBLE), DIMENSION(:)   ,intent(in   ) :: Q , A , H , ST , RH
   REAL(DOUBLE), DIMENSION(:,:) ,intent(inout) :: C
   INTEGER                                     :: Nbsect , NBTRA , nbsing
   REAL(DOUBLE)                                :: T , DT
   !
   !  DECLARATION DES PARAMETRES PHYSIQUES
   !
   TYPE (PARAMETRES_QUALITE_EAU_T )         ::  TPARPH
   TYPE (METEO_T)                           ::  TMeteo
   TYPE (ERREUR_T)                          ::  Erreur
   !
   !  VARIABLES LOCALES
   !
   REAL(DOUBLE), DIMENSION (Nbsect)        :: RAY
   REAL(DOUBLE), DIMENSION (Nbsect)        :: LNUT , IH , CP , DP , MP , KE
   REAL(DOUBLE), DIMENSION (Nbsect,nbtra)  :: SS , SV
   REAL(DOUBLE) :: WPOR , WNOR , CMAX , PS , KPE , BETA , RP
   REAL(DOUBLE) :: IK , KP , KN , ALPHA1 , ALPHA2 , FP , DTP , K1
   REAL(DOUBLE) :: FN , DTN , K2 , M1 , M2 , CORRT , TEMP , I0
   INTEGER I ,  K
   INTEGER :: IF1 = 0
   !
   !  FONCTIONS
   !
   INTRINSIC DEXP, DLOG
   !
   SAVE WPOR , WNOR , CMAX , PS , KPE , BETA , IK , KP , KN , ALPHA1 , ALPHA2
   SAVE FP , DTP , K1 , FN , DTN , K2 , M1 , M2 , IF1
   !
   ! --------------------------------------------------------------------
   ! C1 : BIOMASSE PHYTOPLANCTONIQUE PHY  << ug/l >> (micro-gramme/litre)
   ! C2 : PHOSPHORE MINERAL ASSIMILABLE PO4  << mg/l >>
   ! C3 : PHOSPHORE MINERAL NON ASSIMILABLE POR  << mg/l >>
   ! C4 : AZOTE MINERAL ASSIMILABLE NO3  << mg/l >>
   ! C5 : AZOTE MINERAL NON ASSIMILABLE NOR  << mg/l >>
   ! --------------------------------------------------------------------

   !
   ! -I- INITIALISATIONS
   !

   ! 1) AFFECTATIONS DES PARAMETRES PHYSIQUES
   !
   IF(IF1.EQ.0) THEN
      WPOR   = TPARPH%ParQual_eau(1)
      WNOR   = TPARPH%ParQual_eau(2)
      CMAX   = TPARPH%ParQual_eau(3)
      PS     = TPARPH%ParQual_eau(4)
      KPE    = TPARPH%ParQual_eau(5)
      BETA   = TPARPH%ParQual_eau(6)
      IK     = TPARPH%ParQual_eau(7)
      KP     = TPARPH%ParQual_eau(8)
      KN     = TPARPH%ParQual_eau(9)
      ALPHA1 = TPARPH%ParQual_eau(10)
      ALPHA2 = TPARPH%ParQual_eau(11)
      RP     = TPARPH%ParQual_eau(12)
      FP     = TPARPH%ParQual_eau(13)
      DTP    = TPARPH%ParQual_eau(14)
      K1     = TPARPH%ParQual_eau(15)
      FN     = TPARPH%ParQual_eau(16)
      DTN    = TPARPH%ParQual_eau(17)
      K2     = TPARPH%ParQual_eau(18)
      M1     = TPARPH%ParQual_eau(19)
      M2     = TPARPH%ParQual_eau(20)
      IF1    = 1
   ENDIF

   !
   !   Interpolation temporelle des donnees meteo
   !
   CALL INTERPOLATION_S( TEMP , T , 1 , TMeteo%Temps , TMeteo%Temp , size(TMeteo%Temps) , Erreur )
   CALL INTERPOLATION_S( I0 , T , 1 , TMeteo%Temps , TMeteo%I0 , size(TMeteo%Temps) , Erreur )

   !
   ! 2) CALCULS PRELIMINAIRES
   !
   CORRT = TEMP / 20.D0

   DO I = 1 , Nbsect
      IF( ABS(PS).GT.EPS15 ) THEN
         KE(I) = 1.7D0 / PS
      ELSE
         ! ke    = kpe + Beta*[PHY]
         KE(I) = KPE + BETA * C(I,1)
      ENDIF

      IH(I) = I0 * DEXP( -KE(I) * H(I) )
      RAY(I)= 1.D0 / ( KE(I) * H(I) ) * DLOG( ( I0 + DSQRT( IK**2 + I0**2 ) ) / &
                     ( IH(I) + DSQRT( IK**2 + IH(I)**2 ) ) )

      ! Lnut = min( [PO4] /(KP+[PO4] ) , [NO3] /(KN+[NO3] ) )
      LNUT(I) = DMIN1( C(I,2) / ( KP + C(I,2) ) , C(I,4) / ( KN + C(I,4) ) )

      ! MP= m1 + m2*[PHY]  + alpha2
      CP(I) = CMAX * RAY(I) * CORRT * LNUT(I) * ALPHA1
      MP(I) = M1 + M2 * C(I,1) + ALPHA2
      DP(I) = ( RP + MP(I) ) * CORRT
   ENDDO

   !
   !----------------------------------------------------------------------
   ! -II- CALCUL DES TERMES SOURCES
   !
   DO I = 1 , Nbsect
      !
      ! TRACEUR 1 : [PHY] Biomasse phytoplanctonique
      !
      SV(I,1) = ( CP(I) - DP(I) ) * C(I,1) / 86400.D0
      SS(I,1) = 0.D0
      !
      ! TRACEUR 2 : [PO4] Phosphore mineral assimilable
      !
      SV(I,2) = ( FP * ( DTP * DP(I) - CP(I) ) * C(I,1) &
               + K1 * CORRT * C(I,3) ) / 86400.D0
      SS(I,2) = 0.D0
      !
      ! TRACEUR 3 : [POR] Phosphore mineral non assimilable
      !
      SV(I,3) = ( FP * ( 1.D0 - DTP ) * DP(I) * C(I,1) &
               - K1 * CORRT * C(I,3) ) / 86400.D0
      SS(I,3) = - WPOR * C(I,3)
      !
      ! TRACEUR 4 : [NO3] Azote mineral assimilable
      !
      SV(I,4) = ( FN * ( DTN * DP(I) - CP(I) ) * C(I,1) &
               + K2 * CORRT * C(I,5) ) / 86400.D0
      SS(I,4) = 0.D0
      !
      ! TRACEUR 5 : [NOR] Azote mineral non assimilable
      !
      SV(I,5) = ( FN * (1.D0 - DTN ) * DP(I) * C(I,1) &
               - K2 * CORRT * C(I,5) ) / 86400.D0
      SS(I,5) = - WNOR * C(I,5)
   ENDDO

   !
   !----------------------------------------------------------------------
   ! -III- ASSEMBLAGE DES TERMES SOURCES
   !       (sources volumiques, surfaciques et ajoutees par l'utilisateur)
   !
   DO K = 1 , NBTRA
      DO I = 1 , nbsect
         S(I,K) = SV(I,K) + SS(I,K) / H(I) + SA(I,K)
         RNU(I,K) = 0.D0
      ENDDO
   ENDDO

   RETURN
END SUBROUTINE CALCS_BIOMASS

subroutine GET_TAB_VAR_CALCS_BIOMASS(i, tabNomVar, tabDescriptionVar)
   integer , intent(inout)                                  :: i                 ! indiceTableaux
   character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele ou de l'etat
   character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele ou de l'etat

   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.WPOR"
   tabDescriptionVar(i) ="Water Quality BIOMASS: vitesse de sedimentation du phosphore organique"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.WNOR"
   tabDescriptionVar(i) ="Water Quality BIOMASS: vitesse de sedimentation de l azote organique"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.CMAX"
   tabDescriptionVar(i) ="Water Quality BIOMASS: taux de croissance algale maximum a 20 deg C"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.PS"
   tabDescriptionVar(i) ="Water Quality BIOMASS: profondeur de Secchi"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.KPE"
   tabDescriptionVar(i) ="Water Quality BIOMASS: coef d extinction du ray sans phyto"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.BETA"
   tabDescriptionVar(i) ="Water Quality BIOMASS: coef de turbidite vegetale"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.IK"
   tabDescriptionVar(i) ="Water Quality BIOMASS: parametre de calage de la formule de Smith"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.KP"
   tabDescriptionVar(i) ="Water Quality BIOMASS: constante de demi-saturation en phosphate"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.KN"
   tabDescriptionVar(i) ="Water Quality BIOMASS: constante de demi-saturation en azote"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.ALPHA1"
   tabDescriptionVar(i) ="Water Quality BIOMASS: coef 1 de toxicite de l eau pour les algues"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.ALPHA2"
   tabDescriptionVar(i) ="Water Quality BIOMASS: coef 2 de toxicite de l eau pour les algues"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.RP"
   tabDescriptionVar(i) ="Water Quality BIOMASS: taux de resp. de la biomasse algale a 20 deg C"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.FP"
   tabDescriptionVar(i) ="Water Quality BIOMASS: prop de phosphore dans les cellules du phyto"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.DTP"
   tabDescriptionVar(i) ="Water Quality BIOMASS: pourcent de phosph direct assim ds phy mort"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.K1"
   tabDescriptionVar(i) ="Water Quality BIOMASS: taux de transformation du POR en PO4"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.FN"
   tabDescriptionVar(i) ="Water Quality BIOMASS: prop d azote dans les cellules du phyto"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.DTN"
   tabDescriptionVar(i) ="Water Quality BIOMASS: pourcent d azote direct assim ds phy mort"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.K2"
   tabDescriptionVar(i) ="Water Quality BIOMASS: taux de transformation du NOR en NO3"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.M1"
   tabDescriptionVar(i) ="Water Quality BIOMASS: coef 1 de mortalite algale a 20 deg C"
   i=i+1
   tabNomVar(i)         ="Model.Tracer.ParPhy.BIOMASS.M2"
   tabDescriptionVar(i) ="Water Quality BIOMASS: coef 2 de mortalite algale a 20 deg C"
   i=i+1

end subroutine GET_TAB_VAR_CALCS_BIOMASS

function GET_TYPE_VAR_CALCS_BIOMASS(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
   implicit none

   integer                          :: GET_TYPE_VAR_CALCS_BIOMASS ! different de 0 si erreur
   character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
   character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
   character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
   logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_TRACER sur la variable
   integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
   character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

   GET_TYPE_VAR_CALCS_BIOMASS = 0
   TypeVar               = ""
   Categorie             = "MODEL"
   Modifiable            = .TRUE.
   dimVar                = 0
   MessageErreur         = ""

   if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.WPOR') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.WNOR') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.CMAX') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.PS') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.KPE') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.BETA') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.IK') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.KP') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.KN') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.ALPHA1') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.ALPHA2') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.RP') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.FP') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   elseif ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.DTP') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   elseif ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.K1') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   elseif ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.FN') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   elseif ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.DTN') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   elseif ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.K2') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   elseif ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.M1') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   elseif ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.M2') then
      TypeVar = 'DOUBLE'
      dimVar                = 0
   else
      GET_TYPE_VAR_CALCS_BIOMASS = 1
      TypeVar = "?"
      Categorie             = "MODEL"
      Modifiable            = .false.
      dimVar                = -1
      MessageErreur         = "GET_TYPE_VAR_CALCS_BIOMASS - Unknown variable name"
   end if

end function GET_TYPE_VAR_CALCS_BIOMASS

function GET_TAILLE_VAR_CALCS_BIOMASS(ParQual_Eau, NomVar, taille1, MessageErreur)

   use M_PRECISION

   implicit none

   integer                          :: GET_TAILLE_VAR_CALCS_BIOMASS ! different de 0 si erreur
   real(DOUBLE), dimension(*), intent(in):: ParQual_Eau
   character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
   integer          , intent(out)   :: taille1                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
   character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

   GET_TAILLE_VAR_CALCS_BIOMASS = 0
   taille1                 = 0
   MessageErreur           = ""

   if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.WPOR') then
      taille1 = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.WNOR') then
      taille1 = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.CMAX') then
      taille1 = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.PS') then
      taille1 = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.KPE') then
      taille1 = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.BETA') then
      taille1 = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.IK') then
      taille1 = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.KP') then
      taille1 = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.KN') then
      taille1 = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.ALPHA1') then
      taille1 = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.ALPHA2') then
      taille1 = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.RP') then
      taille1 = 0
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.FP') then
      taille1 = 0
   elseif ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.DTP') then
      taille1 = 0
   elseif ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.K1') then
      taille1 = 0
   elseif ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.FN') then
      taille1 = 0
   elseif ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.DTN') then
      taille1 = 0
   elseif ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.K2') then
      taille1 = 0
   elseif ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.M1') then
      taille1 = 0
   elseif ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.M2') then
      taille1 = 0
   else
      GET_TAILLE_VAR_CALCS_BIOMASS = 1
      taille1                 = 0
      MessageErreur           = "GET_TAILLE_VAR_CALCS_BIOMASS - Unknown variable name"
   end if

end function GET_TAILLE_VAR_CALCS_BIOMASS

function GET_DOUBLE_CALCS_BIOMASS(ParQual_Eau, NomVar, index1, valeur, MessageErreur)

   use M_PRECISION

   implicit none
   integer                            :: GET_DOUBLE_CALCS_BIOMASS    ! different de 0 si erreur
   real(DOUBLE), dimension(*), intent(in) :: ParQual_Eau                   ! Instance du type derive dont on souhaite recuperer la valeur
   character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
   integer,                intent(in) :: index1                     ! valeur du 1er indice
   real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
   character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

   GET_DOUBLE_CALCS_BIOMASS = 0
   valeur                = -9999999.9999
   MessageErreur          = ""

   if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.WPOR') then
      valeur = ParQual_Eau(1)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.WNOR') then
      valeur = ParQual_Eau(2)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.CMAX') then
      valeur = ParQual_Eau(3)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.PS') then
      valeur = ParQual_Eau(4)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.KPE') then
      valeur = ParQual_Eau(5)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.BETA') then
      valeur = ParQual_Eau(6)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.IK') then
      valeur = ParQual_Eau(7)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.KP') then
      valeur = ParQual_Eau(8)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.KN') then
      valeur = ParQual_Eau(9)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.ALPHA1') then
      valeur = ParQual_Eau(10)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.ALPHA2') then
      valeur = ParQual_Eau(11)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.RP') then
      valeur = ParQual_Eau(12)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.FP') then
      valeur = ParQual_Eau(13)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.DTP') then
      valeur = ParQual_Eau(14)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.K1') then
      valeur = ParQual_Eau(15)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.FN') then
      valeur = ParQual_Eau(16)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.DTN') then
      valeur = ParQual_Eau(17)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.K2') then
      valeur = ParQual_Eau(18)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.M1') then
      valeur = ParQual_Eau(19)
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.M2') then
      valeur = ParQual_Eau(20)
   else
      GET_DOUBLE_CALCS_BIOMASS = 1
      valeur                = -9999999.9999
      MessageErreur         = "GET_DOUBLE_CALCS_BIOMASS - Unknown variable name"
   end if
end function GET_DOUBLE_CALCS_BIOMASS

function SET_DOUBLE_CALCS_BIOMASS(ParQual_Eau, NomVar, index1, valeur, MessageErreur)

   use M_PRECISION

   implicit none
   integer                            :: SET_DOUBLE_CALCS_BIOMASS    ! different de 0 si erreur
   real(DOUBLE), dimension(*), intent(inout) :: ParQual_Eau                   ! Instance du type derive dont on souhaite recuperer la valeur
   character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
   integer,                intent(in) :: index1                     ! valeur du 1er indice
   real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
   character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

   SET_DOUBLE_CALCS_BIOMASS = 0
   MessageErreur          = ""

   if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.WPOR') then
      ParQual_Eau(1) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.WNOR') then
      ParQual_Eau(2) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.CMAX') then
      ParQual_Eau(3) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.PS') then
      ParQual_Eau(4) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.KPE') then
      ParQual_Eau(5) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.BETA') then
      ParQual_Eau(6) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.IK') then
      ParQual_Eau(7) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.KP') then
      ParQual_Eau(8) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.KN') then
      ParQual_Eau(9) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.ALPHA1') then
      ParQual_Eau(10) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.ALPHA2') then
      ParQual_Eau(11) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.RP') then
      ParQual_Eau(12) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.FP') then
      ParQual_Eau(13) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.DTP') then
      ParQual_Eau(14) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.K1') then
      ParQual_Eau(15) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.FN') then
      ParQual_Eau(16) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.DTN') then
      ParQual_Eau(17) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.K2') then
      ParQual_Eau(18) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.M1') then
      ParQual_Eau(19) = valeur
   else if ( NomVar == 'Model.Tracer.ParPhy.BIOMASS.M2') then
      ParQual_Eau(20) = valeur
   else
      SET_DOUBLE_CALCS_BIOMASS = 1
      MessageErreur         = "SET_DOUBLE_CALCS_BIOMASS - Unknown variable name"
   end if
end function SET_DOUBLE_CALCS_BIOMASS
