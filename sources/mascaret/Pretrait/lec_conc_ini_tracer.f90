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

subroutine LEC_CONC_INI_TRACER( &
                 UniteListing , &
            ImpressionConcIni , &
               FichierConcIni , &
                            X , & ! Maillage
                 TypeMaillage , & ! Mode de calcul du maillage
                            C , & ! Concentrations initiales des traceurs
                      nb_trac , & ! nombre de traceurs
                      Connect , & ! Table de connectivite du reseau
                       Profil , & ! Profils geometriques
                      unitNum , & ! Unite logique du fichier .xcas
                       Erreur ) ! Erreur

!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - M. LUCK
!!                           F. ZAOUI
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
!  FONCTION : LECTURE DU FICHIER DES CONCENTRATIONS INITIALES
!  --------
!             INTERPOLATION AUX SECTIONS DE CALCUL SI NECESSAIRE
!             SI TypeMaillage = TYPE_MAILLAGE_PRECEDENT :
!             DEFINITION DES ABSCISSES DES SECTIONS DE CALCUL.
!
!----------------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :       - Fichier listing (UniteListing)
!   ----------------------
!
!   SOUS-PROGRAMME(S) APPELANT(S) :  - PRETRAIT_TRACER
!   -----------------------------
!   SOUS-PROGRAMME(S) APPELE(S)   :  - LEC_FIC_CONC_INI_TRACER
!   ---------------------------      - INTERPOLATION_S
!
!***********************************************************************

   !============================= Declarations ============================
   use M_PRECISION
   use M_FICHIER_T            ! Definition du type FICHIER_T et UniteListing
   use M_ERREUR_T             ! Definition du type ERREUR_T
   use M_CONNECT_T            ! Type CONNECT_T
   use M_PROFIL_T             ! Type PROFIL_T
   use M_PARAMETRE_C
   use M_MESSAGE_C            ! Definition des messages d'erreur
   use M_MESSAGE_TRACER_C
   use M_CONSTANTES_CALCUL_C  ! Definition des formats de fichiers de sortie
   use M_LEC_FIC_CONC_INI_TRACER_I
   use M_INTERPOLATION_S       ! Interface du sous programme INTERPOLATION_S
   use M_ABS_ABS_S             ! Calcul de l'abscisse absolue
   use M_TRAITER_ERREUR_I      ! Traitement des erreurs
   use M_XCAS_S

   !.. Declarations explicites ..
   implicit none

   !.. Arguments ..
   real(DOUBLE)       , dimension(:)   , pointer       :: X
   real(DOUBLE)       , dimension(:,:) , pointer       :: C
   type(CONNECT_T)                     , intent(in   ) :: Connect
   type(PROFIL_T)     , dimension(:)   , intent(in   ) :: Profil
   integer                                             :: nb_trac              ! nombre de traceurs
   type(FICHIER_T)                     , intent(inout) :: FichierConcIni
   integer                             , intent(in   ) :: TypeMaillage
   logical                             , intent(in   ) :: ImpressionConcIni
   integer            , intent(in   )                  :: UniteListing
   integer, intent(in)                                 :: unitNum
   type(ERREUR_T)                      , intent(inout) :: Erreur
   integer                             , parameter     :: ORDRE_INTERP = 1  ! ordre d'interpolation
   ! Variables locales
   real(DOUBLE)    , dimension(:)  , pointer :: x_ini    ! maillage lu
   real(DOUBLE)    , dimension(:,:), pointer :: c_ini    ! concentration lue
   integer      :: i,j,nb_prof,nb_bief
   integer      , dimension(size(Connect%OrigineBief))   :: ProfDebBief
   integer      , dimension(size(Connect%OrigineBief))   :: ProfFinBief
   integer      :: TypeEntreeConcInit
   integer      :: nb_sect              ! nombre de sections de calculs
   logical      :: interpoler           ! test necessite interpolatione
   integer      :: RETOUR               ! code de retour des fonctions intrinseques
   real(DOUBLE) :: sigma
   character(len=256)  :: pathNode
   character(len=8192) :: line

   !============================ Instructions ==============================
   ! INITIALISATION
   ! --------------
   Erreur%Numero      = 0
   retour             = 0
   ! arbredappel_old    = trim(Erreur%arbredappel)
   ! Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LEC_CONC_INI_TRACER'

   ! RECONSTRUCTION DES STRUCTURES ProfDebBief, ProfFinBief
   ! ------------------------------------------------------
   nb_prof = size(Profil)
   nb_bief = size(Connect%OrigineBief)

   ! .. detection des profils debut et fin de bief
   ProfDebBief(Profil(1)%NumBief) = 1
   do j = 2 , nb_prof
      if( Profil(j)%NumBief.ne.Profil(j-1)%NumBief ) then
         ProfDebBief(Profil(j)%NumBief)   = j
         ProfFinBief(Profil(j-1)%NumBief) = j - 1
      endif
   enddo
   ProfFinBief(Profil(nb_prof)%NumBief) = nb_prof

   ! Type d'entree des concentrations initiales
   pathNode = 'parametresTraceur/parametresConcentrationsInitialesTraceur/modeEntree'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) TypeEntreeConcInit

   if( TypeEntreeConcInit == SAISIE_PAR_FICHIER ) then
      pathNode = 'parametresTraceur/parametresConcentrationsInitialesTraceur/fichConcInit'
      FichierConcIni%Nom = xcasReader(unitNum, pathNode)
      if( ImpressionConcIni ) write(UniteListing,1100) FichierConcIni%Nom
   else
      if(ImpressionConcIni) write(UniteListing,1200)
   endif

   !================================================
   ! LECTURE OU SAISIE DES CONCENTRATIONS INITIALES
   !================================================
   if( TypeEntreeConcInit == SAISIE_PAR_FICHIER ) then
      !-----------------------------------------
      ! Lecture dans le fichier (format Opthyca)
      !-----------------------------------------
      call LEC_FIC_CONC_INI_TRACER( &
                            x_ini , & ! Tableau des abscisses initiales
                            c_ini , & ! Tableau des cotes initiales
                          nb_trac , & ! nombre de traceurs
                   FichierConcIni , & ! fichier de la ligne d'eau initiale
                           Erreur   &
                                  )
      if( Erreur%Numero /= 0 ) then
         return
      end if

   else

      Erreur%Numero = 510
      Erreur%ft   = err_510
      Erreur%ft_c = err_510c
      call TRAITER_ERREUR (Erreur, 'les concentrations initiales')
      return

   endif

   !===============================================
   ! ALLOCATION DES POINTEURS AUX BONNES DIMENSIONS
   !===============================================
   nb_sect = size(X)

   do j = 1 , nb_trac
      allocate( C(nb_sect,j) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'C' )
         return
      end if
   enddo

   !========================
   ! AFFECTATION DES VALEURS
   !========================
   nb_sect = size(X)
   interpoler = .false.

   if( nb_sect == size(x_ini) ) then
      sigma = 0.d0
      do i = 1 , nb_sect
         sigma = sigma + ( X(i) - x_ini(i) )
      end do
      if( dabs(sigma) >= EPS5 ) then
         interpoler = .true.
      endif
   else
      interpoler = .true.
   endif

   !-----------------------------------------------------
   ! LE MAILLAGE EST DIFFERENT DE CELUI DES CONC INIT
   ! NECESSITE D'INTERPOLER AUX SECTIONS DE CALCUL
   !-----------------------------------------------------

   if( interpoler ) then

      boucle_d_interpolation :  do i = 1 , nb_sect

         do j = 1 , nb_trac

            CALL INTERPOLATION_S( &
                   C(I,j)       , &
                   X(I)         , &
                   ORDRE_INTERP , &
                   x_ini        , &
                   c_ini(:,j)   , &
                   size(x_ini)  , & ! taille de x_ini
                   Erreur       )

            if( Erreur%Numero /= 0 ) then
               return
            end if

         enddo

      end do boucle_d_interpolation

   !----------------------------------------------------
   ! LE MAILLAGE EST LE MEME QUE CELUI DES CONC INIT
   ! PAS BESOIN D'INTERPOLER AUX SECTIONS DE CALCUL
   !----------------------------------------------------
   else  ! de if interpoler

      do j = 1 , nb_trac
         C(:,j) = c_ini(:,j)
      enddo

   endif  ! de if interpoler

   !------------------------------------
   ! ECRITURE DES CONC INIT
   !------------------------------------
   if( ImpressionConcIni ) then
      write(UniteListing ,2000) nb_sect
      write(UniteListing ,2001)
      write(UniteListing ,2002) ( X(i) , i = 1 , nb_sect )
      do j = 1 , nb_trac
         write(UniteListing ,2003) j
         write(UniteListing ,2002) (C(i,j),i=1,nb_sect)
      enddo
   endif

   !---------------------------------------------------------
   ! De-allocation des tableaux locaux x_ini, c_ini a nb_sect
   !---------------------------------------------------------
   deallocate( x_ini , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'x_ini' )
      return
   end if

   deallocate( c_ini , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'c_ini' )
      return
   end if

   !----------------------
   ! Fin du sous-programme
   !----------------------

   ! Erreur%arbredappel = arbredappel_old

   return

   1100 FORMAT( 'Mode de saisie par fichier - Nom du fichier    : ' , A )
   1200 FORMAT( 'Mode de saisie par clavier                       '   )
   2000 FORMAT( 'Nombre de sections de calcul = ',I5,/)
   2001 FORMAT( 'Abscisses des sections de calcul :'/)
   2002 FORMAT( 10(f10.2,1X))
   2003 FORMAT( 'Concentration du traceur no ',I1,'( C(I) , I=1,nb_sect ) :'/)

   return

end subroutine LEC_CONC_INI_TRACER
