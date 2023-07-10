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

subroutine POST_IMP_TRACER( &
                        X , &
         CTraceur, Nbtrac , &
           MASSE , FLUMAS , & ! Masse de traceur
          FLUENT , FLUSOR , & ! Flux de traceur
                   FLUSRC , &
                NbCourant , & ! Nombre de courant max
                  Connect , & ! Dimension spatiale
     FichierListingTracer , &
    ImpressionConcListing , & ! Logique pour les impressions
    ImpressionBilanTracer , &
                    TEMPS , &
          PhaseSimulation , &
                   Erreur )

!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
!  FONCTION :   IMPRESSION DES VALEURS DES VARIABLES TRACEUR
!  --------     SUR LISTING
!
!  SOUS PROGRAMMES APPELANT(S) : SUPERVISEUR
!  ---------------------------
!  SOUS PROGRAMMES APPELE(S) :   Neant
!  -------------------------
!***********************************************************************

   !============================= Declarations ===========================
   use M_PRECISION
   use M_ERREUR_T
   use M_MESSAGE_C
   use M_TRAITER_ERREUR_I
   use M_FICHIER_T
   use M_CONNECT_T
   use M_CONSTANTES_CALCUL_C

   implicit none

   !.. Arguments ..
   type(ERREUR_T)  ,                  intent(inout) :: Erreur
   type(FICHIER_T) ,                  intent(in   ) :: FichierListingTracer
   logical         ,                  intent(in   ) :: ImpressionConcListing,ImpressionBilanTracer
   type(CONNECT_T) ,                  intent(in   ) :: Connect
   real(DOUBLE)    , dimension(:,:) , intent(in   ) :: Ctraceur
   real(DOUBLE)    , dimension(:)   , intent(in   ) :: X
   real(DOUBLE)    , dimension(:,:) , intent(inout) :: MASSE,FLUMAS,FLUENT,FLUSOR,FLUSRC
   real(DOUBLE)    , dimension(:)   , intent(in   ) :: NbCourant
   integer         ,                  intent(in   ) :: PhaseSimulation
   real(DOUBLE)    ,                  intent(in   ) :: TEMPS
   integer                                          :: Nbtrac
   !Variables locales
   real (DOUBLE) , dimension (Nbtrac) :: Masse_totale , Appsrc , Entrees , Sorties
   ! attention : tableaux dimensionnes a Nbtracmax
   real (DOUBLE) , dimension (10), save :: Qtte_entr , Qtte_sort , Qtte_src
   integer ult , itrac , isect , ibief , nbbief , iext , nb_ext , n_sect_deb , n_sect_fin
   character(132) :: fmt_ligne
   character(2) , dimension(10) :: i_in_letter
   data i_in_letter / '1','2','3','4','5','6','7','8','9','10'/

   !============================ Instructions ==============================
   !
   ! INITIALISATION
   ! ==============
   Erreur%Numero = 0
   !arbredappel_old = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>POST_IMP_TRACER'

   if( PhaseSimulation.eq.PHASE_INITIALISATION ) then
      do itrac = 1 , Nbtrac
         Qtte_entr(itrac) = 0.d0
         Qtte_sort(itrac) = 0.d0
         Qtte_src(itrac)  = 0.d0
      enddo
   endif

   nbbief = size(Connect%OrigineBief)
   nb_ext = size(Connect%NumBiefExtLibre)
   ult    = FichierListingTracer%Unite

   write (ult,*)
   write (ult,*)
   write (ult,"('******** TEMPS ******** = ',f15.3,' s')") TEMPS
   write (ult,*)
   write (ult,*)

   ! ECRITURE DES DONNEES AU COURS DU TEMPS (et au pas de temps 0)
   ! =============================================================

   do ibief = 1 , nbbief

      n_sect_deb = Connect%OrigineBief(ibief)
      n_sect_fin = Connect%FinBief(ibief)

      write(ult, &
        '(/" BIEF NUMERO ",I2,5X," I1 = ",I4," I2 = ",I4,/1X,39("=")/)') &
        ibief , n_sect_deb , n_sect_fin

      if( ImpressionConcListing ) then

         write(fmt_ligne, '("(A4,",I2,"A6)")') Nbtrac + 1
         write(ult,fmt_ligne) "I","X",("C"//I_in_letter(itrac),itrac=1,Nbtrac)

         do isect = n_sect_deb , n_sect_fin
            write(ult,30000) isect , X(isect) , (Ctraceur(isect,itrac),itrac=1,Nbtrac)
         enddo

      endif

      write (ult,*)
      write (ult,*)
      write (ult,"(A,f6.3)")  "Nombre de courant maximal dans le bief au temps t = ",NbCourant(ibief)
      write (ult,*)
      write (ult,*)

      if( ImpressionBilanTracer ) then

         write(ult,*) "----- BILAN DES TRACEURS POUR LE BIEF ",ibief," -----"
         write (ult,"(A)") "( Grandeurs exprimees en [Unite Traceur] x m3 )"
         write (ult,*)

         do itrac = 1 , Nbtrac
            write (ult,20000) itrac
            write (ult,"(A,f20.8)") "Masse du traceur dans le bief au temps t       = " , Masse(ibief,itrac)
            write (ult,"(A,f20.8)") "Masse entree dans le bief entre t-dt et t      = " , Fluent(ibief,itrac)
            write (ult,"(A,f20.8)") "Masse sortie du bief entre t-dt et t           = " , Flusor(ibief,itrac)
            write (ult,"(A,f20.8)") "Masse apportee par les sources entre t-dt et t = " , Flusrc(ibief,itrac)
            write (ult,*)
         enddo

      endif

   enddo

   ! BILAN DES QUANTITES DE TRACEURS SUR LE DOMAINE ET DEPUIS LE TEMPS T0
   ! ====================================================================
   if( ImpressionBilanTracer ) then

      write (ult,*) "----- BILAN DES TRACEURS SUR LE DOMAINE -----"
      write (ult,"(A)") "( Grandeurs exprimees en [Unite Traceur] x m3 )"
      write (ult,*)

      do itrac = 1 , nbtrac

         Masse_totale(itrac) = 0.d0
         Appsrc(itrac)       = 0.d0
         Entrees(itrac)      = 0.d0
         Sorties(itrac)      = 0.d0

         do ibief = 1 , nbbief

             Masse_totale(itrac) = Masse_totale(itrac) + Masse(ibief,itrac)
             Appsrc(itrac)       = Appsrc(itrac) + Flusrc(ibief,itrac)

         enddo

         do iext = 1 , nb_ext
            ibief = Connect%NumBiefExtLibre(iext)
            if( Connect%NumSectionExtLibre(iext) == Connect%OrigineBief(ibief) ) then
               ! Extremites amont => entrees
               Entrees(itrac) = Entrees(itrac) + Fluent(ibief,itrac)
            else
               ! Extremites aval => sorties
               Sorties(itrac) = Sorties(itrac) + Flusor(ibief,itrac)
            endif
         enddo

         write(ult,20000) itrac
         write(ult,"(A,f20.8)") "Masse du traceur dans le domaine au temps t      = " , Masse_totale(itrac)
         write(ult,"(A,f20.8)") "Masse entree dans le domaine entre t-dt et t     = " , Entrees(itrac)
         write(ult,"(A,f20.8)") "Masse sortie du domaine entre t-dt et t          = " , Sorties(itrac)
         write(ult,"(A,f20.8)") "Masse apportee par les sources entre t-dt et t   = " , Appsrc(itrac)

         ! Calcul des quantites de traceur entree / sortie / issue des sources depuis le temps initial
         Qtte_entr(itrac) = Qtte_entr(itrac) + Entrees(itrac)
         Qtte_sort(itrac) = Qtte_sort(itrac) + Sorties(itrac)
         Qtte_src(itrac)  = Qtte_src(itrac)  + Appsrc(itrac)
         write(ult,"(A,f20.8)") "Masse entree dans le domaine depuis t0           = " , Qtte_entr(itrac)
         write(ult,"(A,f20.8)") "Masse sortie du domaine depuis t0                = " , Qtte_sort(itrac)
         write(ult,"(A,f20.8)") "Masse apportee par les sources depuis t0         = " , Qtte_src(itrac)

         ! Remise a zero des flux

         do ibief = 1 , nbbief
            Fluent(ibief,itrac) = 0.d0
            Flusor(ibief,itrac) = 0.d0
            Flusrc(ibief,itrac) = 0.d0
         enddo

      enddo

   endif ! de if(ImpressionBilanTracer)

   !fin des traitements d erreur

   ! Erreur%arbredappel = arbredappel_old

   20000 FORMAT('   TRACEUR No ',I4,/,3X,15('='))
   30000 FORMAT(i6,2X,f10.2,2X,10(e13.5))

   return

end subroutine POST_IMP_TRACER
