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

Subroutine LEC_FIC_LOI_TRACER( &
                FicLoiTracer , & ! Fic. contenant une evolution temporelle de conc
                   LoiTracer , & ! Concentration initiale des traceurs
                  UniteTemps , & ! unite de temps des chroniques temporelles
                      NumLoi , & ! Numero de loi
                      nbtrac , & ! Nombre de traceurs
                      Erreur )

!*****************************************************************************
! PROGICIEL : TRACER         M. LUCK
!
! VERSION : V8P4R0              EDF-CEREMA
!*****************************************************************************
!  Fonction : Lecture du fichier contenant une loi de type temps-concentration
!  --------
!
!  Sous-programme appelant : Lec_Loi_Tracer
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!=============================================================================
!
!  Commentaires : Description du fichier lu
!  ------------
! Toute ligne de commentaire commence par le caractere "#"
!
! Exemple :
!
!    # ESSAI
!    # Concentration en traceur i (g/l)
!    #    T        C
!     10.00     1.00
!     20.00     1.10
!     30.00     1.20
!     35.00     1.00
!     46.00     1.10
!     54.00     1.20
!=============================================================================

   !============================ Declarations ===================================
   use M_PRECISION         ! Definition de la precision DOUBLE ou SIMPLE
   use M_LIRE_CHAINE_S     ! Lecture de lignes de commentaire du fichier
   use M_FICHIER_T         ! Definition du type FICHIER_T
   use M_LOI_TRACER_T      ! Definition du type LOI_TRACER_T
   use M_ERREUR_T          ! Definition du type ERREUR_T
   use M_MESSAGE_C         ! Messages d'erreur
   use M_MESSAGE_TRACER_C
   use M_TRAITER_ERREUR_I  ! Traitement de l'erreur

   !.. Implicit Declarations ..
   implicit none

   ! Constantes
   integer, parameter :: LEN_CHAINE = 80
   character(1) , parameter :: CHAINE_COMMENTAIRE = "#" ! Caractere commentaire qui
                                                        ! debute une ligne commentaire
   ! Variables d'entree
   type(FICHIER_T) , intent(in   ) :: FicLoiTracer
   integer         , intent(in   ) :: NumLoi
   ! Variables de sortie
   type(LOI_TRACER_T), intent(  out) :: LoiTracer
   integer           , intent(  out) :: UniteTemps
   integer           , intent(in   ) :: nbtrac
   ! Variables locales
   integer                           :: iPts        ! Compteur de points
   integer                           :: j
   integer                           :: NbPts       ! Nombre de points decrivant la loi
   integer                           :: UniteLoi    ! Unite logique du fichier des lois
   integer                           :: rang        ! position du mot cle sur la ligne
   character(72)                     :: txt         ! chaine de caractere temporaire
   character(LEN_CHAINE)             :: chaine      ! Chaine contenant une ligne du fichier
   character(1)                      :: ChaineVar
   ! Traitement des erreurs
   integer                           :: retour      ! code de retour des fonctions d'e/s
   type(ERREUR_T) , intent(inout)    :: Erreur

   !============================ Instructions ===============================
   Erreur%Numero      = 0
   ! arbredappel_old    = trim(Erreur%arbredappel)
   ! Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LEC_FIC_LOI_TRACER'

   !=========================================================================
   ! PREMIERE LECTURE DU FICHIER AFIN DE CONNAITRE LE NOMBRE DE LOIS
   ! ET DE DECELER LES ERREURS
   !=========================================================================
   ! Ouverture du fichier a lire
   ! ---------------------------
   UniteLoi = FicLoiTracer%Unite
   open( unit = UniteLoi , file = FicLoiTracer%Nom , access = 'SEQUENTIAL' , &
         action = 'READ' , form = 'FORMATTED' , iostat = retour , &
         position = 'rewind' , status = 'OLD' )

   If( retour /= 0 ) Then
      Erreur%Numero = 3
      Erreur%ft     = err_3
      Erreur%ft_c   = err_3c
      call TRAITER_ERREUR( Erreur , trim(FicLoiTracer%Nom) )
      return
   End if

   ! LECTURE DE LA PREMIERE LIGNE (LES LIGNES DE COMMENTAIRE NE SONT PAS LUES)
   !--------------------------------------------------------------
   call LIRE_CHAINE_S( chaine , FicLoiTracer , CHAINE_COMMENTAIRE , retour )
   If( retour /= 0 ) Then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , trim(FicLoiTracer%Nom) )
      return
   End if

   rang = scan(chaine,'SsMmHhJj')
   ! Message d'erreur si erreur a la ligne d'unite de temps
   If( rang == 0 ) Then
      Erreur%Numero = 579
      Erreur%ft     = err_579
      Erreur%ft_c   = err_579c
      call TRAITER_ERREUR( Erreur , trim(FicLoiTracer%Nom) )
      return
   Endif

   ! ===============================================================
   ! Definition du nombre de points devrivant la loi
   ! ===============================================================
   NbPts = 0

   ! lecture jusqu'a la fin du fichier
   LabelDimLois: Do While( retour == 0 )
      NbPts = NbPts + 1
      call LIRE_CHAINE_S( chaine , FicLoiTracer , CHAINE_COMMENTAIRE , retour )
   End do LabelDimLois

   ! Si Erreur de lecture ==> message d'erreur
   !------------------------------------------
   If( retour > 0 ) Then
      Erreur%Numero = 11
      Erreur%ft     = err_11
      Erreur%ft_c   = err_11c
      call TRAITER_ERREUR( Erreur , trim(FicLoiTracer%Nom) , NbPts )
      return
   Endif

   ! Stockage du nombre de points decrivant la loi
   !---------------------------------------------
   NbPts = NbPts - 1

   ! Controle du nombre de points minimum
   !-------------------------------------
   write(txt,'(a,i3)') 'Nombre de Pts decrivant la loi tracer' , NumLoi
   If( NbPts < 2 ) Then
      Erreur%Numero = 508
      Erreur%ft     = err_508
      Erreur%ft_c   = err_508c
      call TRAITER_ERREUR( Erreur ,trim(txt) , '> ou egal a 2' )
      return
   End if

   ! Allocations de memoire du tableau de la variable "Temps"
   !---------------------------------------------------------
   allocate( LoiTracer%Temps(NbPts) , STAT = retour )
   If( retour /= 0 ) Then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'LoiTracer%Temps' )
      return
   End if

   ! Allocations de memoire du tableau de la variable "Conc"
   !---------------------------------------------------------
   allocate( LoiTracer%Conc(NbPts,nbtrac) , STAT = retour )
   If( retour /= 0 ) Then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'LoiTracer%Conc' )
      return
   End if

   !=========================================================================
   ! 2-EME LECTURE : LECTURE EFFECTIVE DES LOIS DE CONCENTRATION
   !=========================================================================
   ! Retour au debut du fichier
   rewind (UniteLoi)

   ! Le cas echeant, lecture de l'unite de temps
   call LIRE_CHAINE_S( chaine , FicLoiTracer , CHAINE_COMMENTAIRE , retour )
   read(chaine(rang:rang+1),*,IOSTAT = retour) ChaineVar

   ! Si erreur de 2eme lecture de la ligne d'unite de temps ==> message d'erreur
   !----------------------------------------------------------------------------
   If( retour /= 0 ) Then
      Erreur%Numero = 579
      Erreur%ft     = err_579
      Erreur%ft_c   = err_579c
      call TRAITER_ERREUR( Erreur , trim(FicLoiTracer%Nom) )
      return
   End if

   ! Stockage de l'unite de la variable 'Temps' donnee en saisie
   !-------------------------------------------------------
   Select case( ChaineVar )
      Case ("S","s")
         UniteTemps = 1
      Case ("M","m")
         UniteTemps = 2
      Case ("H","h")
         UniteTemps = 3
      Case ("J","j")
         UniteTemps = 4
   End select

   ! Lecture des points decrivant la loi de concentration
   !-------------------------------------------------------
   Do iPts = 1 , NbPts
      !Lecture
      call LIRE_CHAINE_S( chaine , FicLoiTracer , CHAINE_COMMENTAIRE , retour )
      read(chaine,*,IOSTAT = retour) LoiTracer%Temps(iPts) , (LoiTracer%Conc(iPts,j),j=1,nbtrac)
      ! Traitement d'erreur
      If( retour /= 0 ) Then
         Erreur%Numero = 11
         Erreur%ft     = err_11
         Erreur%ft_c   = err_11c
         call TRAITER_ERREUR( Erreur , trim(FicLoiTracer%Nom) , iPts )
         return
      End if
   End do

   !=========================================================================
   ! TRAITEMENT DE FIN DE SOUS-PROGRAMME
   !=========================================================================
   close(UniteLoi)

   !Erreur%arbredappel = arbredappel_old

   !=========================================================================
   ! FIN DU SOUS-PROGRAMME
   !=========================================================================

   return

End Subroutine LEC_FIC_LOI_TRACER
