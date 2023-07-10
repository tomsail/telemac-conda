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

module M_MESSAGE_CASIER_C
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSEE    C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

! === CE MODULE CONTIENT LES MESSAGES D'ERREUR LONGS ET COURTS 
! === PROPRES A CASIER

!=========================== Instructions ==============================

!-------------------------------------------------------------------------
character (*), parameter ::                                             &
banniere = '("=========================================================================")'
!---------------------------------------------------------------------------
character (*), parameter ::                                             &
err_3  = '("Erreur a l''ouverture du fichier : ",A,/,                   &
         & "en lecture.",/,                                            &
         & "Le fichier n''a pas ete trouve au chemin specifie.",/,      &
         & "Verifier sa presence.")'
character (*), parameter ::                                             &
err_3c = '("Erreur a l''ouverture du fichier ",A," en lecture")'
!-------------------------------------------------------------------------
character (*), parameter ::                                             &
err_4  = '("Erreur a l''ouverture du fichier : ",A,/,                   &
         & "en ecriture.",/,                                            &
         & "Verifier l''existence du chemin specifie.")'
character (*), parameter ::                                             &
err_4c = '("Erreur a l''ouverture du fichier ",A," en ecriture")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_5  = '("Erreur d''allocation du tableau ",A,".",/,                     &
         & "Verifier la place memoire disponible.")'
character (*), parameter ::                                                &
err_5c = '("Erreur d''allocation du tableau ",A)'
!----------------------------------------------------------------------------
character (*), parameter ::                                                &
err_6  = '("Erreur de de-allocation du tableau ",A,".")'
character (*), parameter ::                                             &
err_6c = '("Erreur de de-allocation du tableau ",A)'
!---------------------------------------------------------------------------
character (*), parameter ::                                                &
err_77  = '("Erreur en lecture du fichier geometrie",/,                    &
          & "a la lecture d''une ligne de commentaire.")'
character (*), parameter ::                                                &
err_77c = '("Erreur de lecture du fichier geometrie",                      &
          & " en lecture d''une ligne de commentaire")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_78  = '("Erreur de lecture du fichier geometrie",/,                    &
          & "Le mot-cle ""CASIER"" n''a pas ete trouve pour le premier casier.",/,&
          & "Verifier la presence du mot-cle a cet endroit du fichier.")'
character (*), parameter ::                                                &
err_78c = '("Mot cle non trouve pour le premier casier")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_79  = '("Erreur en lecture du fichier geometrie",/,                    &
          & "a la lecture des valeurs du ",i3,"eme casier du fichier.")'
character (*), parameter ::                                                &
err_79c = '("Erreur en lecture dans le ",i3,"eme casier")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_80  = '("Erreur en lecture du fichier geometrie",/,                    &
          & "a la lecture des valeurs du ",i3,"eme casier,",/,             &
          & "au ",i5,"eme point.")'
character (*), parameter ::                                                &
err_80c = '("Erreur de lecture dans le ",i3,"eme casier, au ",i5,"eme point")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_81  = '("Erreur en lecture du fichier geometrie",/,                    &
          & "Le nombre de casiers est different du nombre de casiers lu",/,&
          & "dans le fichier cas.")'
character (*), parameter ::                                                &
err_81c = '("Nombre de casier different du nombre lu dans le fichier cas.")'
!-------------------------------------------------------------------------
character (*), parameter ::                                                &
err_800   = '("Erreur de calcul relative au casier numero ",i4,/,          &
          & "Division par zero dans la resolution Gauss-Seidel",/,         &
          & "Sous-programme ",A,".")'
character (*), parameter ::                                                &
err_800c  = '("Erreur de calcul casier ",i4,"division par zero dans ",A,".")'
!----------------------------------------------------------------------------
character (*), parameter ::                                                &
err_801   = '("L''algorithme de Gauss-Seidel dans le sous-",/,             & 
          & "programme ",A, "n''a pas converge.",/,                        & 
          & "L''erreur vaut ",f2.8,)' 
character (*), parameter ::                                                & 
err_801c  = '("Algo de Gauss-Seidel dans",A,"non convergent; erreur",f2.8,".")'
!----------------------------------------------------------------------------
character (*), parameter ::                                                 &
err_802   = '("Erreur de calcul relative a la liaison numero ",i4,/,        &
          & "Division par zero dans le calcul de la vitesse d echange.",/,  &
          & "La cote de la liaison est egale a la cote moyenne des casiers.")'
character (*), parameter ::                                                 &
err_802c  = '("Erreur de calcul liaison ",i4,"; division par zero.")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                                  &
err_803   = '("Erreur de calcul relative au casier numero ",i4,/,            &
          & "On ne peut pas calculer sa surface et son volume par ",A,".",/, &
          & "La cote du casier est egale a la cote du fond.",/,              &
          & "Il faut utiliser les lois Z_S et Z_V pour Z=Zfond.")'
character (*), parameter ::                                                  &
err_803c  = '("Erreur de calcul casier ",i4,"; impossible d utiliser ",A,".")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                                &
err_900   = '("Erreur d''implementation du fichier des parametres.",/,     &
          & "L''option ",A,/,                                              & 
          & "est incompatible avec le nombre de ",A,".")'              
character (*), parameter ::                                                &
err_900c  = '("Option ",A," incompatible avec le nombre de ",A,".")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                                &
err_901   = '("Erreur de saisie des parametres.",/,                        &
          & "La cote du casier ",i5," est inferieure a la cote du fond.")'
character (*), parameter ::                                                &
err_901c  = '("La cote du casier ",i5," est inferieure a la cote du fond.")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                                &
err_902   = '("Erreur de saisie des parametres.",/,                        &
          & "Le pas de planimetrage du casier ",i5," ne peut pas etre <=0.")'
character (*), parameter ::                                                &
err_902c  = '("Le pas de planimetrage du casier ",i5," ne peut pas etre <=0.")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                                &
err_9021   = '("Erreur de saisie des parametres.",/,                        &
          & "Le nombre de cotes de planimetrage du casier ",i5," doit etre",/,&
          & "au moins egal a 2.")'              
character (*), parameter ::                                                &
err_9021c  = '("nb de cotes de planimetrage du casier ",i5," inferieur a 2!")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_903   = '("Erreur de saisie des parametres pour le casier ",i5,/,     &
          & "Certaines liaisons casier-casier ne sont associees a aucun casier.")'
character (*), parameter ::                                               &
err_903c  = '("Casier ",i3,"erreur de numerotation des liaisons.")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_904   = '("Erreur de saisie des parametres pour le casier ",i5,".",/,     &
          & "Le numero du casier associe ",i5," est superieur au nombre",/,&
          & "de casiers.")'              
character (*), parameter ::                                                &
err_904c  = '("Casier ",i3,"mauvaise numerotation du casier associe",i3,".")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                                &
err_905   = '("Erreur de saisie des parametres pour le casier ",i5,".",/,  &
          & "Certaines liaisons casier-casier ne sont pas numerotees.")'
character (*), parameter ::                                                &
err_905c  = '("Erreur de numerotation des liaisons pour le casier",i3,".")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_906   = '("Erreur de saisie des parametres pour le casier ",i5,".",/,     &
          & "Le numero de la liaison associee ",i5," est superieur au nombre",/,&
          & "de liaisons.")'              
character (*), parameter ::                                                &
err_906c  = '("Casier ",i3,"mauvaise numerotation de la liaison associee",i3,".")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_907   = '("Erreur de saisie des parametres pour la liaison ",i5,".",/,&
          & "Le type de la liaison doit etre compris entre 1 et 4.")'
character (*), parameter ::                                               &
err_907c  = '("Le type de la liaison ",i5," doit etre compris entre 1 et 4.")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_908   = '("Erreur de saisie des parametres pour la liaison ",i5,".",/,&
          & "La nature de la liaison doit etre egale a ",i5," ou ",i5,".")'
character (*), parameter ::                                               &
err_908c  = '("La nature de la liaison ",i5," doit etre egale a ",i5," ou ",i5,".")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_9091   = '("Erreur de saisie des parametres pour la liaison ",i5,".",/,&
          & "Le numero du bief associe doit etre strictement positif.")'
character (*), parameter ::                                               &
err_9091c  = '("Liaison ",i3,"bief associe de numero negatif ou nul.")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_909   = '("Erreur de saisie des parametres pour la liaison ",i5,".",/,&
          & "Le numero des casiers doit etre compris entre 1 et ",i3,".")'
character (*), parameter ::                                               &
err_909c  = '("Liaison",i3,"numero des casiers entre 1 et ",i3,".")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_910   = '("Erreur de saisie des parametres pour la liaison ",i5,".",/,&
          & "L''abscisse correspondante ",f15.2," doit etre comprise entre",/,&
          & ,f15.2," et ",f15.2,".")'
character (*), parameter ::                                               &
err_910c  = '("Liaison ",i3,"abscisse ",f15.2,"entre ",f15.2," et ",f15.2,".")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_911   = '("Erreur de saisie des parametres pour la liaison ",i5,".",/,&
          & "La liaison est de type ",A,". Le champ ",A"," doit obligatoirement",/,&
          & "etre renseigne et non nul.")'
character (*), parameter ::                                               &
err_911c  = '("La liaison ",i5," est de type ",A,". Le champ ",A," ne peut etre nul.")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_912   = '("Erreur de saisie des parametres pour la liaison ",i5,".",/,&
          & "La liaison est de type seuil. Le champ coefficient noye/denoye",/,&
          & "doit obligatoirement etre different de 1.")'
character (*), parameter ::                                               &
err_912c  = '("liaison ",i5," type seuil. Coef. noye denoye different de 1.")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_913   = '("Erreur de saisie des parametres pour le casier ",i5,".",/,&
          & "La premiere cote a rentrer pour les lois Z_S et Z_V et la cote &
          & "du fond du casier, soit ",f12.3,.")'
character (*), parameter ::                                               &
err_913c  = '("casier ",i3,"1ere cote des lois Z_S et Z_V doit etre la cote du fond.")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_914   = '("Erreur de saisie des parametres pour le casier ",i5,".",/,&
          & "La valeur Loi_Z_",A,"(",i5,",2) ne peut pas etre nulle.")'
character (*), parameter ::                                               &
err_914c  = '("casier ",i5,"; valeur Loi_Z_",A,"(",i5,",2) nulle.")'
!err_914c = '("test")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_915   = '("Erreur de saisie des parametres pour le casier ",i5,".",/,&
          & "La valeur ",A," est obligatoirement nulle puisqu''elle",/,  &
          & "corespond a la cote du fond du casier.")'
character (*), parameter ::                                               &
err_915c  = '("casier ",i5,"; la valeur ",A," est obligatoirement nulle.")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_916   = '("Erreur de saisie des parametres pour le casier ",i5,".",/,&
          & "Les cotes Loi_Z_S(",i5,",1) et Loi_Z_V(",i5,",1) doivent etre",/,&
          & "egales.")'
character (*), parameter ::                                               &
err_916c  = '("casier ",i5,"; il faut Loi_Z_S(",i5,",1) = Loi_Z_V(",i5,",1).")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_917   = '("Erreur de saisie des parametres pour le casier ",i5,".",/,&
          & "Les cotes des lois Z_S et Z_V doivent etre rangees par ordre croissant.")'
character (*), parameter ::                                               &
err_917c  = '("Lois Z_S et Z_V, casier ",i5," : cotes par ordre croissant.")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_918   = '("Erreur de saisie des parametres pour le casier ",i5,".",/,&
          & "Le pas de planimetrage doit etre constant.")'
character (*), parameter ::                                               &
err_918c  = '("casier ",i3,"Pas de planimetrage non constant.")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                                &
err_919   = '("Erreur de saisie des parametres.",/,                        &
          & "Le nb de cotes de planimetrage du casier ",i5," ne peut pas",/,&
		  & "etre <=0.")'              
character (*), parameter ::                                                &
err_919c  = '("Nb de cotes de planimetrage du casier ",i5," <=0.")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_1000  = '("Erreur de numerotation des liaisons casier-casier ",/,     &
          & "pour le casier ",i5,".",/,                                   &
          & "La liaison ",i5," n''est pas une liaison casier-casier.")'
character (*), parameter ::                                               &
err_1000c  = '("casier ",i3,"erreur dans le numero de la liaison ",i3,".")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_1010  = '("Erreur de saisie des parametres pour l''apport ",i5,".",/, &
          & "Le numero du casier recepteur doit etre compris entre 1 et ",i5,".")'
character (*), parameter ::                                               &
err_1010c  = '("Apport ",i3,"casier associe entre 1 et ",i3,".")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_1020  = '("Erreur de saisie des parametres pour l''apport ",i5,".",/, &
          & "Le numero de la loi associee doit etre compris entre 1 et ",i5,".")'
character (*), parameter ::                                               &
err_1020c  = '("Apport ",i3,"loi associee entre 1 et ",i3,".")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_1030  = '("Erreur de saisie des parametres pour l''apport ",i5,".",/, &
          & "La loi associee n''est pas de type hydrogramme.")'
character (*), parameter ::                                               &
err_1030c  = '("La loi associee a l''apport ",i5," n''est pas un hydrogramme.")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_2000  = '("Erreur de lecture pour le casier ",i5,".",/,               &
          & "Le nombre de points frontiere doit au moins etre egal a 4.")'
character (*), parameter ::                                               &
err_2000c  = '("Casier ",i5,", nombre de points frontiere au moins egal a 4.")'
!-----------------------------------------------------------------------------
character (*), parameter ::                                               &
err_2001  = '("Erreur de lecture pour le casier ",i5,".",/,               &
          & "Le nombre de points interieur doit etre non nul.")'
character (*), parameter ::                                               &
err_2001c  = '("Casier ",i5,", nombre de points interieur nul!")'
character (*), parameter ::                                               &
err_2007  = '("Erreur de definition des points frontiere pour le casier ",i5".",/, &
          & "Le dernier point frontiere doit etre identique au premier",/,&
          & "pour assurer la fermeture du casier.")'
character (*), parameter ::                                               &
err_2007c  = '("Attention le casier ",i5," n''est pas ferme.")'
!-----------------------------------------------------------------------------

end module M_MESSAGE_CASIER_C