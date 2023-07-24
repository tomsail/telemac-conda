!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET.
!
!   MASCARET is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>
!

subroutine LEC_GEOM( &
                Profil , & ! Profils geometriques
                NbBief , & ! Nombre de biefs
           ProfDebBief , & ! Premiers profils de chaque bief
           ProfFinBief , & ! Derniers profils de chaque bief
                 Ecart , & ! Ecart entre branches
   FrottParoiVerticale , & ! Frottement sur les parois verticales
              Prof_Abs , & ! Abscisse absolue
                 Noyau , & ! Noyau de calcul
        impression_geo , & ! Impression de la geometrie
          UniteListing , & ! Unite logique fichier listing
           FichierGeom , & ! Fichier geometrie
            FormatGeom , & ! Format du fichier geometrie
                Erreur ) ! Erreur

! *********************************************************************
! PROGICIEL : MASCARET         S. MANDELKERN
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!
!  FONCTION :
!  --------
!
!          LECTURE DU FICHIER GEOMETRIE
!
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!                FichierGeo    : Fichier des profils geometriaues
!                UniteListing  : Fichier listing
!
!   SOUS PROGRAMME APPELANT :
!   -------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!                LEC_GEOM_V2P0 : LECTURE DES PROFILS AU FORMET LIDO 2.0
!                LEC_GEOM_V3P0 : LECTURE DES PROFILS AU FORMET LIDO 3.0
!
!
!   COMMENTAIRES :
!   ------------

   !=========================== Declarations ================================
   use M_PRECISION
   use M_MESSAGE_C           ! Messages d'erreur
   use M_PARAMETRE_C         ! Parametres
   use M_CONSTANTES_CALCUL_C ! formats geometrie disponibles
   use M_PROFIL_T         ! Definition du type PROFIL_T
   use M_FICHIER_T        ! Definition du type FICHIER_T
   use M_ERREUR_T         ! type ERREUR_T
   use M_TRAITER_ERREUR_I ! Module-procedure de traitement des erreurs
   use M_LEC_GEOM_V2P0_I  ! Interface de sous-programme
   use M_LEC_GEOM_V3P0_I  ! Interface de sous-programme
   use M_INTERPOLATION_S  ! sou_programme d'interpolation de donnees

   !.. Implicit Declarations ..
   implicit none

   !.. Formal Arguments ..
   type(PROFIL_T), dimension(:) , pointer       :: Profil
   integer                      , intent(  out) :: NbBief
   integer       , dimension(:) , pointer       :: ProfDebBief
   integer       , dimension(:) , pointer       :: ProfFinBief
   type(FICHIER_T)              , intent(in   ) :: FichierGeom
   logical                      , intent(in   ) :: FrottParoiVerticale
   logical                      , intent(in   ) :: Prof_Abs
   logical                      , intent(in   ) :: impression_geo
   integer                      , intent(in   ) :: UniteListing
   integer                      , intent(in   ) :: Noyau
   real(DOUBLE)                 , intent(in   ) :: Ecart ! denominateur commun
                                                         ! des abscisses de debut de bief
   integer                      , intent(in   ) :: FormatGeom
   type(ERREUR_T)               , intent(inout) :: Erreur

   ! variables locales
   !character(132) :: !arbredappel_old
   integer        :: iprof       ! compteur sur les profils
   integer        :: ibief       ! compteur sur les biefs
   integer        :: i           ! compteur
   integer        :: j           ! compteur
   integer        :: retour      ! code de retour d'erreur
   integer        :: num_point_g !
   integer        :: num_point_d !
   integer        :: prof_deb    ! premier profil d'un bief
   real(DOUBLE)   :: delta_abscisse_rel  ! Ecart depuis l'origine du bief
   integer        :: nb_prof     ! nombre de profils
   integer        :: num_bief_prec ! Numero du bief precedent
   real(DOUBLE)   :: z_ref       ! Point bas du lit

   !=========================== Instructions ================================
   !===============
   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   retour = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_GEOM'

   !=====================
   ! LECTURE DES FICHIERS
   !=====================
   if( FormatGeom == FORMAT_GEOM_LIDOV2P0 ) then
      call LEC_GEOM_V2P0        ( &
          Profil                , & ! Resultats
          FrottParoiVerticale   , & ! Donnees non modifiees
          FichierGeom           , &
          UniteListing          , &
          Erreur                  )
      if( Erreur%Numero /= 0 ) then
         return
      end if

   else if( FormatGeom == FORMAT_GEOM_LIDOV3P0 ) then
      call LEC_GEOM_V3P0           ( &
         Profil                    , &
         FrottParoiVerticale       , & ! Donnees non modifiees
         FichierGeom               , &
         FormatGeom                , &
         UniteListing              , &
         Erreur                    )
      if( Erreur%Numero /= 0 ) then
         return
      end if
   end if

   !===========
   ! CONTROLES
   !===========
   nb_prof = size(Profil)
   Icvp    = 13

   !------------------------------
   ! Controle du nombre de profils
   !------------------------------
   if( nb_prof <= 1 ) then
      Erreur%Numero = 83
      Erreur%ft     = err_83
      Erreur%ft_c   = err_83c
      call TRAITER_ERREUR( Erreur , nb_prof )
      return
   end if

   !===========================================================
   !  DETERMINATION DE L'ABSCISSE CALCUL DES PROFILS DU FICHIER
   !  ET DES NUMEROS DES PROFILS DE DEBUT ET DE FIN DES BIEFS
   !===========================================================
   !-------------------------------------
   ! Premiere phase de comptage des biefs
   !-------------------------------------
   ibief =1

   !-----------------------
   ! Boucle sur les profils
   !-----------------------
   do iprof = 2 , nb_prof
      !---------------------
      ! Si on change de bief
      !---------------------
      if( Profil(iprof)%NumBief /= Profil(iprof-1)%NumBief ) then
         ibief = ibief + 1
      end if
   end do

   NbBief = ibief

   !---------------------------
   ! Seconde phase d'allocation
   !---------------------------
   if(.not.associated(ProfDebBief)) allocate( ProfDebBief(NbBief) , stat = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'ProfDebBief' )
      return
   end if

   if(.not.associated(ProfFinBief)) allocate( ProfFinBief(NbBief) , stat = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR  (Erreur, 'ProfFinBief')
      return
   end if

   !------------------------------
   ! Troisieme phase d'affectation
   !------------------------------
   prof_deb       = 1
   ProfDebBief(1) = prof_deb

   ! Cas du premier profil
   !----------------------
   Profil(1)%AbsAbs = Profil(1)%AbsRel

   !-----------------------
   ! Boucle sur les profils
   !-----------------------
   do iprof = 2 , nb_prof
      !---------------------
      ! Si on change de bief
      !---------------------
      if( Profil(iprof)%NumBief /= Profil(iprof-1)%NumBief ) then
         ProfDebBief(Profil(iprof  )%NumBief) = iprof
         ProfFinBief(Profil(iprof-1)%NumBief) = iprof-1
         prof_deb                             = iprof
         !-------------------------------------------------
         ! Controle que le bief precedent a plus d'1 profil
         !-------------------------------------------------
         num_bief_prec = Profil(iprof-1)%NumBief
         if( ProfFinBief(num_bief_prec) == ProfDebBief(num_bief_prec) ) then
            Erreur%Numero = 85
            Erreur%ft     = err_85
            Erreur%ft_c   = err_85c
            call TRAITER_ERREUR( Erreur , num_bief_prec )
            return
         end if

         !---------------------------------------------
         ! Abscisse absolue du premier profil d'un bief
         !---------------------------------------------
         if( Prof_Abs ) then
            Profil(iprof)%AbsAbs = Profil(iprof)%AbsRel
         else
            Profil(iprof)%AbsAbs = ( INT( ( Profil(iprof - 1)%AbsAbs ) / Ecart ) + 1 ) * Ecart
        endif

        !---------------------------
        ! Si on est sur le meme bief
        !---------------------------
      else
         !---------------------------------
         ! difference d'abscisses relatives
         !---------------------------------
         delta_abscisse_rel = Profil(iprof)%AbsRel - Profil(prof_deb)%AbsRel
         !-------------------
         ! Abscisses absolues
         !-------------------
         Profil(iprof)%AbsAbs = Profil(prof_deb)%AbsAbs + delta_abscisse_rel
      end if

      !-----------------------
      ! Pour le dernier profil
      !-----------------------

      if( iprof == size(Profil) ) then
         ProfFinBief(Profil(iprof)%NumBief) = iprof
      endif
   end do


   !----------------------------
   ! Calcul des Zref aux profils
   !----------------------------
   do iprof = 1 , size(Profil)
      z_ref = INFINI
      do i = 1 , size(Profil(iprof)%X)
         z_ref = dmin1( z_ref , Profil(iprof)%Y(i) )
      end do
      Profil(iprof)%ZRef = z_ref
   end do

   !==========================================================
   !
   !       I M P R E S S I O N   D E S   D O N N E E S
   !
   !==========================================================
   sortie_listing: if (impression_geo) then

      ! IMPRESSION DES ProfilS
      ! ----------------------
      write(UniteListing,10001)
      write(UniteListing,10037) nb_prof

      do I = 1 , nb_prof
         write(UniteListing,10002) Profil(I)%Nom , Profil(I)%AbsRel , Profil(I)%AbsAbs , Profil(I)%Zref
         num_point_g = Profil(I)%LimiteMin(1)
         num_point_d = Profil(I)%LimiteMin(2)
         write(UniteListing,10003) I , Profil(I)%X(num_point_g) , Profil(I)%X(num_point_d)
         if( FormatGeom == FORMAT_GEOM_LIDOV2P0 ) then
            num_point_g = Profil(I)%LimiteMaj(1)
            num_point_d = Profil(I)%LimiteMaj(2)
            write(UniteListing,10004) Profil(I)%X(num_point_g) , Profil(I)%X(num_point_d)
         endif
         write(UniteListing,10005) Profil(I)%ZRive(1) , Profil(I)%ZRive(2)
         write(UniteListing,10006) size(Profil(I)%X)
         write(UniteListing,'(10X,A)') ' ABSCISSES :'
         write(UniteListing,10008) (Profil(I)%X(J) , J = 1 , size(Profil(I)%X))
         write(UniteListing,'(10X,A)') ' ORDONNEES :'
         write(UniteListing,10008) (Profil(I)%Y(J) , J = 1 , size(Profil(I)%X))
      end do

      write (UniteListing,10011)

      ! IMPRESSION DES INDICES DES PREMIERS ET
      ! DERNIERS PROFILS DE CHAQUE BIEF
      ! --------------------------------------
      write (UniteListing,10011)

      do ibief = 1 , NbBief
         write(UniteListing,'(" BIEF ",i3,"  NUMERO DU 1ER PROFIL     =",i4,"  ABSCISSE CALCUL : X = ",f12.3)') &
                ibief, ProfDebBief(ibief), Profil(ProfDebBief(ibief))%AbsAbs
         write(UniteListing,'(" BIEF ",i3,"  NUMERO DU DERNIER PROFIL =",i4,"  ABSCISSE CALCUL : X = ",f12.3)') &
                ibief, ProfFinBief(ibief), Profil(ProfFinBief(ibief))%AbsAbs
      end do

      write(UniteListing,10011)

   end if sortie_listing


   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

   ! Formats d ecriture
   ! ------------------

   10001 format (/,'PROFILS GEOMETRIQUES',/,21('-'),/)
   10037 format ('NOMBRE DE PROFILS = ',i5)
   10002 format ( &
         /,1x,90('-'),/,5x,'PROFIL  ',a12,6x,'  ABSC RELATIF =',f12.2,5x,   &
         '  ABSC CALCUL =',f12.2,/,5x,20('-'),6x,'   ZREF      =',f12.2)
   10003 format (                                                           &
         '  J=',i4,' LIMITES LIT MINEUR : ',2f8.2)
   10004 format (                                                           &
         '         LIMITES LIT MAJEUR : ',2f8.2)
   10005 format (8x,' COTE RIVE GAUCHE : ',f8.2,' COTE RIVE DROITE ',f8.2)
   10006 format (8x,' NOMBRE DE POINTS DU PROFIL : ',i5)
   10008 format (8x,10f8.2)
   10011 format (1x,90('*'))

end subroutine LEC_GEOM
