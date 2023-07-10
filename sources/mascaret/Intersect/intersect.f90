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

subroutine Intersect( &
                      ! Resultats
                      ZREF                , & ! Tableau des cotes de ref aux sections
                      RGC                 , & ! Cotes de la rive gauche
                      RDC                 , & ! Cotes de la rive droite
                      CF1                 , & ! Coefficient de frottement mineur
                      CF2                 , & ! Coefficient de frottement majeur
                      ! Donnees
                      Profil              , & ! Profils geometriques
                      ! Modele
                      X                   , & ! Abscisses des sections de calcul
                      IDT                 , & ! Positionement des sections / profils
                      XDT                 , & ! Positionement des sections / profils
                      Connect             , & ! Connectivite du reseau
                      Extremite           , & ! Extremite libre
                      ! Parametres
                      TypeMaillage        , & ! Type de calcul du maillage
                      ImpressionPlani     , & ! flag d'impression
                      UniteListing        , & ! Unite logique fichier listing
                      FormatGeom          , & ! Format du fichier geometrie utilise
                      InterpLinCoeffFrott , & ! Flag d'interpolation lineaire des coeff de frottement
                      PhaseSimulation     , & ! Phase de la simulation
                      Erreur  )               ! Erreur

! ******************************************************************
! PROGICIEL : MASCARET             S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!
! CALCUL DU BILAN ENTREE - SORTIE DANS UN CASIER
!*******************************************************************
!
!  FONCTION :
!  --------
!
!               INTERPOLATION DES CARACTERISTIQUES DES SECTIONS
!               EN FONCTION DES PROFILS
!*******************************************************************
   !
   ! Ecriture sur fichier listing : UniteListing
   !
   !============================= Declarations ===========================
   use M_PRECISION
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_PARAMETRE_C         ! EPS1
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONNECT_T           ! Type CONNECT_T : connectivite du reseau
   use M_ERREUR_T            ! type ERREUR_T
   use M_EXTREMITE_T         ! Type EXTREMITE_T
   use M_PROFIL_T            ! Definition du type PROFIL_T
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs

   Implicit none

   ! ... Arguments ...
   ! Resultats
   real(DOUBLE)   , dimension(:)    , pointer       :: ZREF
   real(DOUBLE)   , dimension(:)    , pointer       :: RGC, RDC
   real(DOUBLE)   , dimension(:)    , pointer       :: CF1
   real(DOUBLE)   , dimension(:)    , pointer       :: CF2
   ! Donnees
   type(PROFIL_T) , dimension(:)    , intent(in   ) :: Profil
   ! Modele
   real(DOUBLE)   , dimension(:)    , intent(in   ) :: X
   integer        , dimension(:)    , intent(in   ) :: IDT
   real(DOUBLE)   , dimension(:)    , intent(in   ) :: XDT
   type(CONNECT_T)                  , intent(in   ) :: Connect
   type(EXTREMITE_T) , dimension(:) , intent(inout) :: Extremite
   ! Parametres
   integer                          , intent(in   ) :: TypeMaillage
   logical                          , intent(in   ) :: ImpressionPlani
   integer                          , intent(in   ) :: UniteListing
   logical                          , intent(in   ) :: InterpLinCoeffFrott
   integer                          , intent(in   ) :: FormatGeom
   integer                          , intent(in   ) :: PhaseSimulation
   type(ERREUR_T)                   , intent(inout) :: Erreur

   ! ... Scalaires locaux ...
   integer      :: i
   integer      :: isect               ! compteur
   integer      :: iprof               ! compteur
   integer      :: iext                ! compteur sur les extremites libres
   integer      :: ibief               ! compteur sur les branches
   integer      :: num_sect            ! numero d'une section
   real(DOUBLE) :: delta_x
   real(DOUBLE) :: delta_zref
   logical      :: debut_bief          ! indicateur de debut de bief
   integer      :: borne_prof          ! point de depart de la boucle
   logical      :: v2p0_pas_assoc      ! flag
   integer      :: retour              ! code de retour d'erreur
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================
   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   retour = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>INTERSECT'
   v2p0_pas_assoc = .false.

   if( PhaseSimulation == PHASE_INITIALISATION ) then
      ! Allocations
      !------------
      if(.not.associated(RGC)) allocate( RGC(size( X )) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'RGC' )
         return
      end if
      if(.not.associated(RDC)) allocate( RDC(size( X )) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'RDC' )
         return
      end if
      if(.not.associated(ZREF)) allocate( ZREF(size( X )) , STAT = retour )
      if (retour /= 0) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'ZREF' )
         return
      end if

      if( FormatGeom == FORMAT_GEOM_LIDOV2P0 .and. TypeMaillage /= TYPE_MAILLAGE_PRECEDENT) then

         v2p0_pas_assoc = .true.
         if(.not.associated(CF1)) allocate( CF1(size( X )) , STAT = retour )
         if( retour /= 0 ) then
           Erreur%Numero = 5
           Erreur%ft     = err_5
           Erreur%ft_c   = err_5c
           call TRAITER_ERREUR( Erreur , 'CF1' )
           return
         end if
         if(.not.associated(CF2)) allocate( CF2(size( X )) , STAT = retour )
         if( retour /= 0 ) then
           Erreur%Numero = 5
           Erreur%ft     = err_5
           Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'CF2' )
           return
         end if

      endif

   endif ! de if PhaseSimulation

   do isect= 1 , size( X )
      Iprof = IDT(isect)

      ZREF(isect) = Profil(iprof)%Zref                               &
                   + ( Profil(iprof + 1)%Zref - Profil(iprof)%Zref ) &
                   * XDT(isect)

      RGC(isect)  = Profil(iprof)%ZRive(1)                                   &
                   + ( Profil(iprof + 1)%ZRive(1) - Profil(iprof)%ZRive(1) ) &
                   * XDT(isect)

      RDC(isect)  = Profil(iprof)%ZRive(2)                                   &
                   + ( Profil(iprof + 1)%ZRive(2) - Profil(iprof)%ZRive(2) ) &
                   * XDT(isect)

      if( v2p0_pas_assoc ) then
         if( InterpLinCoeffFrott ) then
            CF1(isect) = Profil(iprof)%CoeffFrottMin                                        &
                        + ( Profil(iprof + 1)%CoeffFrottMin - Profil(iprof)%CoeffFrottMin ) &
                        * XDT(isect)

            CF2(isect) = Profil(iprof)%CoeffFrottMaj                                        &
                        + ( Profil(iprof + 1)%CoeffFrottMaj - Profil(iprof)%CoeffFrottMaj ) &
                        * XDT(isect)
         else
            CF1(isect) = Profil(iprof)%CoeffFrottMin
            CF2(isect) = Profil(iprof)%CoeffFrottMaj
         endif
      endif

      borne_prof = iprof

   end do

   if( ImpressionPlani ) then
      write( UniteListing , 2030 )
      if( v2p0_pas_assoc ) then
         do i = 1 , size( X )
            write( UniteListing , 2050 ) I , X(I) , ZREF(I) , RGC(I) , RDC(I) ,&
                                         CF1(I) , CF2(I) , XDT(I) , IDT(I)
         end do
      else
         do i = 1 , size( X )
            write( UniteListing , 2040 ) I , X(I) , ZREF(I) , RGC(I) , RDC(I) , XDT(I) , IDT(I)
         end do
      end if
   endif

   !-------------------------------------------------
   ! Calcul de la pente du fond aux extremites libres
   !-------------------------------------------------
   do iext = 1 , size( Connect%NumSectionExtLibre )
      num_sect = Connect%NumSectionExtLibre(iext)
      ! test de la position debut/fin de branche
      !-----------------------------------------
      debut_bief = .false.
      do ibief = 1 , size( Connect%OrigineBief )
         if( num_sect == Connect%OrigineBief(ibief) ) then
            debut_bief = .true.
            exit
         endif
      end do

      ! calcul de delta x et delta zref
      !--------------------------------
      if( debut_bief ) then
         delta_x    = dabs( X(Connect%NumSectionExtLibre(iext)) - &
                         X(Connect%NumSectionExtLibre(iext) + 1) )

         delta_zref = dabs( ZREF(Connect%NumSectionExtLibre(iext)) - &
                            ZREF(Connect%NumSectionExtLibre(iext) + 1) )
      else
         delta_x    = dabs( X(Connect%NumSectionExtLibre(iext)) - &
                            X(Connect%NumSectionExtLibre(iext) - 1) )
         delta_zref = dabs( ZREF(Connect%NumSectionExtLibre(iext)) - &
                            ZREF(Connect%NumSectionExtLibre(iext) - 1) )
      endif

      ! calcul de la pente
      !-------------------
      Extremite(iext)%PenteFond = delta_zref / delta_x

   end do

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

   ! ... Formats ...

 2030 FORMAT(/,'INTERPOLATION LINEAIRE DANS CHAQUE SECTION DE CALCUL',/,&
               '----------------------------------------------------',/)
 2040 FORMAT(1X,'I=',I4,' X=',F10.2,' ZREF=',F9.2,&
             ' RGC=',F9.2,' RDC=',F9.2,&
             ' XDT=',F5.3,' IDT=',I4)
 2050 FORMAT(1X,'I=',I4,' X=',F10.2,' ZREF=',F7.2,&
             ' RGC=',F9.2,' RDC=',F9.2,&
             ' CF1=',F9.2,' CF2=',F9.2,&
             ' XDT=',F5.3,' IDT=',I4)

end subroutine Intersect
