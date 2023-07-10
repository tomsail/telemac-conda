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

module M_LEC_FIC_LOI_TRACER_I
!***********************************************************************
! PROGICIEL : TRACER         M.LUCK
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   interface

   Subroutine LEC_FIC_LOI_TRACER( &
                   FicLoiTracer , & ! Fic. contenant une evolution temporelle de conc
                      LoiTracer , & ! Concentration initiale des traceurs
                     UniteTemps , & ! unite de temps des chroniques temporelles
                         NumLoi , & ! Numero de loi
                         nbtrac , & ! Nombre de traceurs
                         Erreur )

   !=========================================================================
   !  Fonction : Lecture du fichier contenant une loi de type temps-concentration
   !  --------
   !
   !  Sous-programme appelant : Lec_Loi_Tracer
   !  -----------------------
   !
   !  Sous-programme appele : 
   !  ---------------------
   !=========================================================================
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
   !=========================================================================


   !============================ Declarations =============================== 
   use M_PRECISION          ! Definition de la precision DOUBLE ou SIMPLE
   use M_LIRE_CHAINE_S      ! Lecture de lignes de commentaire du fichier
   use M_FICHIER_T          ! Definition du type FICHIER_T
   use M_LOI_TRACER_T       ! Definition du type LOI_TRACER_T
   use M_ERREUR_T           ! Definition du type ERREUR_T
   use M_MESSAGE_C          ! Messages d'erreur
   use M_MESSAGE_TRACER_C
   use M_TRAITER_ERREUR_I   ! Traitement de l'errreur

   !.. Implicit Declarations .. 
   implicit none

   ! Constantes
   integer, parameter      :: LEN_CHAINE = 80
   character(1), parameter :: CHAINE_COMMENTAIRE = "#" ! Caractere commentaire qui debute une ligne commentaire
   ! Variables d'entree
   type(FICHIER_T) , intent(in   ) :: FicLoiTracer
   integer , intent(in   ) :: NumLoi
   ! Variables de sortie
   type(LOI_TRACER_T), intent(  out) :: LoiTracer
   integer           , intent(  out) :: UniteTemps
   integer           , intent(in   ) :: nbtrac
   ! Traitement des erreurs
   type(ERREUR_T)            , intent(inout) :: Erreur

   end subroutine LEC_FIC_LOI_TRACER

   end interface

end module M_LEC_FIC_LOI_TRACER_I
