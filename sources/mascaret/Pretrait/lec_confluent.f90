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

subroutine LEC_CONFLUENT( &
                  Confluent , & ! Tableau des lois confluents 2D
                    Connect , & ! Table de connectivite du reseau
               UniteListing , & ! Unite logique fichier listing
                      Noyau , & ! Noyau de calcul
                   unitNum  , & ! Unite logique .xcas
                     Erreur   & ! Erreur
                           )

! *********************************************************************
! PROGICIEL : MASCARET         A. LEBOSSE
!                              S. MANDELKERN
!                              F. ZAOUI
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_CONFLUENT_T         ! Type CONFLUENT_T
   use M_CONNECT_T           ! Type CONNECT_T
   use M_ERREUR_T            ! Type ERREUR_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_PARAMETRE_C        ! Parametres
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc
   use M_XCAS_S

   implicit none

   ! Arguments
   type(CONFLUENT_T) , dimension(:)  , pointer       :: Confluent
   type(CONNECT_T  )                 , intent(in   ) :: Connect
   integer                           , intent(in   ) :: UniteListing
   integer                           , intent(in   ) :: Noyau
   integer, intent(in)                               :: unitNum
   type(ERREUR_T)                    , intent(inout) :: Erreur
   ! Variables locales
   integer :: nb_confluent ! Nombre de confluents 2D
   integer :: nb_affluent  ! Nombre d'affluents du confluent
   integer :: iconf        ! compteur sur les confluents
   integer :: ibief        ! compteur sur les biefs
   integer :: iafflu       ! compteur sur les affluents
   integer :: ip, is,if
   integer :: retour       ! code de retour des fonctions intrinseques
   real(DOUBLE) ::  xproj,yproj,delta1,delta2,ordo,theta
   real(double), allocatable :: rtab1(:),rtab2(:),rtab3(:)
   character(len=256)  :: pathNode
   character(len=8192) :: line
   !character(132) :: !arbredappel_old

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_CONFLUENT'

   pathNode = 'parametresConfluents/nbConfluents'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) nb_confluent

   if( nb_confluent < 0 ) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Nombre de confluents' )
      return
   end if

   if (UniteListing >0) write(UniteListing,11100)
   if (UniteListing >0) write(UniteListing,11110) nb_confluent

   if( nb_confluent > 0 ) then

      if(.not.associated(Confluent)) allocate( Confluent(nb_confluent) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Confluent' )
         return
      end if

       pathNode = 'parametresConfluents/confluents'
       line = xcasReader(unitNum, pathNode)

       DO iconf = 1 , nb_confluent

         pathNode = 'structureParametresConfluent/nbAffluent'
         if(iconf.eq.1) then
           line = xcasReader(unitNum, pathNode, 0)
         else
           line = xcasReader(unitNum, pathNode, 1)
         endif
         read(unit=line, fmt=*) Confluent(iconf)%NbAffluent

         nb_affluent = Confluent(iconf)%NbAffluent

         pathNode = 'nom'
         Confluent(iconf)%Nom = xcasReader(unitNum, pathNode, 0)

         if (UniteListing >0) write(UniteListing,11120) iconf, Confluent(iconf)%Nom, Confluent(iconf)%NbAffluent

         allocate( rtab1(nb_affluent) , STAT = retour )
         if( retour /= 0 ) then
             Erreur%Numero = 5
             Erreur%ft     = err_5
             Erreur%ft_c   = err_5c
             call TRAITER_ERREUR( Erreur , 'rtab1' )
             return
         end if
         allocate( rtab2(nb_affluent) , STAT = retour )
         if( retour /= 0 ) then
             Erreur%Numero = 5
             Erreur%ft     = err_5
             Erreur%ft_c   = err_5c
             call TRAITER_ERREUR( Erreur , 'rtab2' )
             return
         end if
         allocate( rtab3(nb_affluent) , STAT = retour )
         if( retour /= 0 ) then
             Erreur%Numero = 5
             Erreur%ft     = err_5
             Erreur%ft_c   = err_5c
             call TRAITER_ERREUR( Erreur , 'rtab3' )
             return
         end if

         if(.not.associated(Confluent(iconf)%AbscisseAfflu)) &
                        allocate( Confluent(iconf)%AbscisseAfflu(nb_affluent) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'Confluent%AbscisseAfflu' )
            return
         end if

         if(.not.associated(Confluent(iconf)%OrdonneeAfflu)) &
                   allocate( Confluent(iconf)%OrdonneeAfflu(nb_affluent) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'Confluent%OrdonneeAfflu' )
            return
         end if

         if(.not.associated(Confluent(iconf)%AngleAfflu)) &
                allocate( Confluent(iconf)%AngleAfflu(nb_affluent) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'Confluent%AngleAfflu' )
            return
         end if

         if(.not.associated(Confluent(iconf)%Isec)) &
                 allocate( Confluent(iconf)%Isec(nb_affluent) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'Confluent%Isec' )
            return
         end if

         if(.not.associated(Confluent(iconf)%Isecvo)) &
                allocate( Confluent(iconf)%Isecvo(nb_affluent) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'Confluent%Isecvo' )
            return
         end if

         if(.not.associated(Confluent(iconf)%Finbie)) &
                  allocate( Confluent(iconf)%Finbie(nb_affluent) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'Confluent%Finbie' )
            return
         end if

         pathNode = 'abscisses'
         line = xcasReader(unitNum, pathNode, 0)
         read(unit=line, fmt=*) rtab1

         pathNode = 'ordonnees'
         line = xcasReader(unitNum, pathNode, 0)
         read(unit=line, fmt=*) rtab2

         pathNode = 'angles'
         line = xcasReader(unitNum, pathNode, 0)
         read(unit=line, fmt=*) rtab3

         do iafflu = 1 , nb_affluent

            Confluent(iconf)%AbscisseAfflu(iafflu) = rtab1(iafflu)
            Confluent(iconf)%OrdonneeAfflu(iafflu) = rtab2(iafflu)
            Confluent(iconf)%AngleAfflu(iafflu) = rtab3(iafflu)

            if( Confluent(iconf)%AngleAfflu(iafflu) < -90. .or. Confluent(iconf)%AngleAfflu(iafflu) > 270.) then
               Erreur%Numero = 348
               Erreur%ft     = err_348
               Erreur%ft_c   = err_348c
               call TRAITER_ERREUR( Erreur , iafflu , iconf , Confluent(iconf)%AngleAfflu(iafflu) )
               return
            end if

            if (UniteListing >0) write(UniteListing,11130) iafflu, &
                          Confluent(iconf)%AbscisseAfflu(iafflu), &
                          Confluent(iconf)%OrdonneeAfflu(iafflu), &
                          Confluent(iconf)%AngleAfflu(iafflu)

            if( Noyau == NOYAU_MASCARET ) then
               ! Determination des sections de calcul associees a chaque point
               ibief = Connect%NumBiefConfluence(iconf,iafflu)
               is    = Connect%NumSectionConfluence(iconf,iafflu)
               ip    = Connect%OrigineBief(ibief)
               if    = Connect%FinBief(ibief)
               if( is == ip ) then
                  Confluent(Iconf)%Isec(iafflu)   = ip
                  Confluent(Iconf)%Isecvo(iafflu) = ip + 1
                  Confluent(iconf)%Finbie(iafflu) = 0
               else
                  Confluent(Iconf)%Isec(Iafflu)   = if
                  Confluent(Iconf)%Isecvo(Iafflu) = if - 1
                  Confluent(iconf)%Finbie(Iafflu) = 1
               endif
               if (UniteListing >0) write(UniteListing,*) iafflu, &
                          Confluent(iconf)%Isec(iafflu), &
                          Confluent(iconf)%Isecvo(iafflu), &
                          Confluent(iconf)%Finbie(iafflu)
               ! Transformation degres ----> Radians
               Confluent(iconf)%AngleAfflu(iafflu) = Confluent(iconf)%AngleAfflu(iafflu)*PI/180._DOUBLE
            endif
         end do ! boucle sur les affluents

         if( Noyau == NOYAU_MASCARET ) then
            Delta1 = Confluent(iconf)%AngleAfflu(2)-Confluent(iconf)%AngleAfflu(1)
            if( Delta1<0._DOUBLE ) Delta1 = Delta1 + 2._DOUBLE * PI
            Delta2 = Confluent(iconf)%AngleAfflu(3) - Confluent(iconf)%AngleAfflu(1)
            if( Delta2 < 0._DOUBLE ) Delta2 = Delta2 + 2._DOUBLE * PI
            if( Delta2 < Delta1 ) then
               !   les affluents ne sont pas dans le sens trigonometrique
               !    on effectue une symetrie axiale autour de (1) pour les modifier
               theta = Confluent(iconf)%AngleAfflu(1)
               ordo  = dsin(theta) * Confluent(iconf)%AbscisseAfflu(1) - dcos(theta) * Confluent(iconf)%OrdonneeAfflu(1)
               do iafflu = 2 , 3
                  xproj = dcos(theta) * dcos(theta) * Confluent(iconf)%AbscisseAfflu(iafflu) &
                        + dsin(theta) * dcos(theta) * Confluent(iconf)%OrdonneeAfflu(iafflu) &
                        + dsin(theta) * ordo

                  yproj = dcos(theta) * dsin(theta) * Confluent(iconf)%AbscisseAfflu(iafflu) &
                        + dsin(theta) * dsin(theta) * Confluent(iconf)%OrdonneeAfflu(iafflu) &
                        - dcos(theta) * ordo

                  Confluent(iconf)%AbscisseAfflu(iafflu) = 2.D0 * xproj - Confluent(iconf)%AbscisseAfflu(iafflu)
                  Confluent(iconf)%OrdonneeAfflu(iafflu) = 2.D0 * yproj - Confluent(iconf)%OrdonneeAfflu(iafflu)
                  Confluent(iconf)%AngleAfflu(iafflu)    = 2.D0 * theta - Confluent(iconf)%AngleAfflu(iafflu)

                  if( Confluent(iconf)%AngleAfflu(iafflu) < 0._DOUBLE ) then
                     Confluent(iconf)%AngleAfflu(iafflu) = Confluent(iconf)%AngleAfflu(iafflu) + 2.D0 * PI
                  endif
                  if (UniteListing >0) write(UniteListing,11130) iafflu, &
                          Confluent(iconf)%AbscisseAfflu(iafflu), &
                          Confluent(iconf)%OrdonneeAfflu(iafflu), &
                          Confluent(iconf)%AngleAfflu(iafflu)
               end do ! boucle sur les affluents
            endif
         end if ! du noyau

         deallocate(rtab1)
         deallocate(rtab2)
         deallocate(rtab3)

      end do   ! boucle sur les confluents

   !--------------------
   ! Si pas de confluent
   !--------------------
   else

      if(.not.associated(Confluent)) allocate( Confluent(0) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Confluent' )
         return
      end if
   endif    ! de if(nb_affluant > 0)

   !Erreur%arbredappel = !arbredappel_old

   return

   ! Formats

   11100 format (/,'CONFLUENTS',/, &
               &  '----------',/)
   11110 format ('Nombre de confluents : ',i3)
   11120 format (/,'Confluent ',i3,' Nom : ',A,' Nombre d''affluents : ',i2)
   11130 format ('Affluent ',i3,' Abscisse = ',f12.3,' Ordonnee = ',     &
                f12.3,' Angle = ',f12.3)

end subroutine LEC_CONFLUENT
