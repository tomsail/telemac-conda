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

module M_TRACER_I
!***********************************************************************
! PROGICIEL : TRACER       S.MANDELKERN - E.LEHMANN - N.GOUTAL - M.LUCK
!
! VERSION : V8P4R0            EDF-CEREMA
!***********************************************************************

   interface

   subroutine TRACER( &
                      ! Resultats
                  C , & ! Concentration des traceurs
                      !  Donnees hydrauliques
          Q , A , B , & ! Donnees hydrauliques (lits mineur+majeur)
  Q_ANT,A_ANT, B_ANT, & ! Donnees hydrauliques au pas de temps precedent
            RH , ST , & ! Rayon hydraulique et frottement lit mineur
             Qinjec , & !
               ZREF , & !
                      !  Donnees TRACER
           Cond_Lim , & ! Cond lim amont interpolees
      Source_Tracer , & ! Termes sources ajoutees aux traceurs
           ConsTrac , & ! Constantes lies au transport-diffusion
    Modele_Qual_Eau , & ! Modele de qualite d'eau considere
        PAR_QualEau , & ! Parametres lies au modele de qualite d'eau
              Meteo , & ! Donnees meteo (pour les modeles Biomass, Eutro et Thermic)
           NodeTrac , & ! Connectivite Tracer
                      !  Modele
                  X , & ! Abscisses des sections de calcul
             Nbtrac , & ! Nombre de traceurs
             Nbsect , & ! Dimension spatiale des tableaux 
        Singularite , & ! Singularite
            Connect , & ! Table de connectivite
            message , &
                      !  Donnees temporelles
              TEMPS , & ! Temps
                      !  Bilan de Masse
              MASSE , & ! Masse de traceur
    FLUMAS , FLUENT , & ! Flux de traceur
    FLUSOR , FLUSRC , & !
                      !  Etats
                 DT , & ! Pas de temps
              IPASS , & ! Phase de calcul
       UniteListing , & ! Unite du fichier listing tracer
    ImpressionBilan , & ! Logique pour le bilan
          NbCourant , & ! Nombre de courant max
             Erreur )

   !***********************************************************************
   !
   !  FONCTION :
   !  --------
   !
   !  Programme principal du module de tracage
   !
   !
   !   SOUS-PROGRAMMES APPELANT :  SUPERVISEUR
   !   -----------------------
   !
   !   SOUS-PROGRAMMES APPELES :   RESEQU, CALK, CALCSA, CALCS_*, BILMAS
   !   -----------------------
   !
   !***********************************************************************

   use M_PRECISION
   use M_PARAMETRE_C
   use M_CONNECT_T
   use M_SINGULARITE_T
   use M_FICHIER_T
   use M_RESEQU_I
   use M_BILMAS_I
   use M_CONSTANTES_TRACER_T
   use M_CONSTANTES_CALCUL_C
   use M_SOURCE_TRACER_T
   use M_PARAMETRES_QUALITE_EAU_T
   use M_COND_LIM_TRACER_T
   use M_CONSTANTES_CALCUL_TRACER_C
   use M_CALCSA_I
   use M_CALCS_Rien_I
   use M_CALCS_O2_I
   use M_CALCS_BIOMASS_I
   use M_CALCS_EUTRO_I
   use M_CALCS_MICROPOL_I
   use M_CALCS_THERMIC_I
   use M_PARPHY_I
   use M_METEO_T
   use M_NODE_TRACER_T
   use M_ERREUR_T
   use M_TRAITER_ERREUR_I
   use M_MESSAGE_TRACER_C

   !==============================================================
   !           Declarations 
   !==============================================================

   !.. Declaration Implicite ..
   implicit none

   !.. Arguments ..
   ! RESULTAT
   real(DOUBLE), dimension(:,:), intent(inout) :: C
   ! DONNEES HYDRAULIQUES
   real(DOUBLE), dimension(:)  , intent(in)    :: A
   real(DOUBLE), dimension(:)  , intent(in)    :: Q
   real(DOUBLE), dimension(:)  , intent(in)    :: B
   real(DOUBLE), dimension(:)  , intent(in)    :: QINJEC
   real(DOUBLE), dimension(:)  , intent(in)    :: RH, ZREF, ST
   real(DOUBLE), dimension(:)  , intent(in)    :: Q_ANT, A_ANT, B_ANT
   ! DONNEES TRACER
   integer, intent(in)                                     :: Modele_Qual_Eau
   type (CONSTANTES_TRACER_T) ,dimension(:), intent(inout) :: ConsTrac
   type (PARAMETRES_QUALITE_EAU_T)         , intent(inout) :: Par_QualEau
   type (METEO_T)                          , intent(inout) :: Meteo
   type (NODE_TRACER_T)                    , intent(inout) :: NodeTrac
   type (SOURCE_TRACER_T)     ,dimension(:), intent(inout) :: Source_Tracer
   type (COND_LIM_TRACER_T)   ,dimension(:), intent(inout) :: Cond_Lim
   type (Singularite_T)       ,dimension(:), intent(in   ) :: Singularite
   type (Erreur_T)                         , intent(inout) :: Erreur
   type (Fichier_T)                        , intent(in   ) :: message
   integer                                 , intent(in   ) :: UniteListing
   logical                                 , intent(in   ) :: ImpressionBilan
   real(DOUBLE)               , dimension(:)   :: NbCourant
   ! MODELE
   real(DOUBLE), dimension(:)   ,intent(in)    :: X
   type(CONNECT_T)              ,intent(in   ) :: CONNECT
   integer                      ,intent(in)    :: Nbsect
   integer                      ,intent(inout) :: Nbtrac
   ! DONNEES TEMPORELLES
   real(DOUBLE)                 ,intent(in)    :: TEMPS
   ! ETATS     
   real(DOUBLE)                 ,intent(in   ) :: DT
   integer                      ,intent(in)    :: IPASS
   ! BILAN DE MASSE 
   real(DOUBLE), dimension (:,:),intent(inout) :: MASSE,FLUMAS,FLUENT,FLUSOR,FLUSRC

   end subroutine TRACER

   end interface

end module M_TRACER_I
