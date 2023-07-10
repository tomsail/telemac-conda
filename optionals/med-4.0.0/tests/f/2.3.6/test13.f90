!*  This file is part of MED.
!*
!*  COPYRIGHT (C) 1999 - 2019  EDF R&D, CEA/DEN
!*  MED is free software: you can redistribute it and/or modify
!*  it under the terms of the GNU Lesser General Public License as published by
!*  the Free Software Foundation, either version 3 of the License, or
!*  (at your option) any later version.
!*
!*  MED is distributed in the hope that it will be useful,
!*  but WITHOUT ANY WARRANTY; without even the implied warranty of
!*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!*  GNU Lesser General Public License for more details.
!*
!*  You should have received a copy of the GNU Lesser General Public License
!*  along with MED.  If not, see <http://www.gnu.org/licenses/>.
!*

! ******************************************************************************
! * - Nom du fichier : test13.f90
! *
! * - Description : lecture des equivalences dans un maillage MED.
! *
! ******************************************************************************

program test13
  
  implicit none
  include 'med.hf'
!
!
  integer*8    fid
  integer      ret,cret
  character*32 maa
  integer      mdim,nequ,ncor
  integer, allocatable, dimension(:) :: cor
  character*32  equ
  character*200 des
  integer       i,j,k
  character*255 argc
  integer, parameter :: MED_NBR_MAILLE_EQU = 8
  integer,parameter  :: typmai(MED_NBR_MAILLE_EQU) =  (/ MED_POINT1,MED_SEG2,   &
                                                      &  MED_SEG3,MED_TRIA3,    &
                                                      &  MED_TRIA6,MED_QUAD4,   &
                                                      &  MED_QUAD8,MED_POLYGONE/)

   integer,parameter :: typfac(MED_NBR_GEOMETRIE_FACE+1) = (/MED_TRIA3,MED_TRIA6,       &
					&	 MED_QUAD4,MED_QUAD8, MED_POLYGONE/)
   integer,parameter ::typare(MED_NBR_GEOMETRIE_ARETE) = (/MED_SEG2,MED_SEG3/)
   character*200 desc
   integer type
 
   print *,"Indiquez le fichier med a decrire : "
   !!read(*,*) argc
   argc = "test12.med"

   !  ** Ouverture du fichier en lecture seule **
   call efouvr(fid,argc,MED_LECTURE, cret)
   print *,cret
   
     
   !  ** Lecture des infos sur le premier maillage **
   if (cret.eq.0) then
      call efmaai(fid,1,maa,mdim,type,desc,cret)
      print *,"Maillage de nom : ",maa," et de dimension : ", mdim
   endif
   print *,cret


   !  ** Lecture du nombre d'equivalence  **
   if (cret.eq.0) then
      call efnequ(fid,maa,nequ,cret)
      if (cret.eq.0) then
         print *,"Nombre d'equivalences : ",nequ
      endif
   endif
 
   !** Lecture de toutes les equivalences **
   if (cret.eq.0) then
      do i=1,nequ
         print *,"Equivalence numero : ",i
         !** Lecture des infos sur l'equivalence **
         if (cret.eq.0) then
            call efequi(fid,maa,i,equ,des,cret)
         endif
         print *,cret
         if (cret.eq.0) then
            print *,"Nom de l'equivalence : ",equ          
            print *,"Description de l'equivalence : ",des 
         endif

         !** Lecture des correspondances sur les differents types d'entites **
         if (cret.eq.0) then
	    !** Les noeuds **
            call efncor(fid,maa,equ,MED_NOEUD,0,ncor,cret)
            print *,"Il y a ",ncor," correspondances sur les noeuds "
            if (ncor > 0) then
               allocate(cor(ncor*2),STAT=ret)
               call efequl(fid,maa,equ,cor,ncor,MED_NOEUD,0,cret)
               do j=0,(ncor-1)
                  print *,"Correspondance ",j+1," : ",cor(2*j+1)," et ",cor(2*j+2)
               end do
               deallocate(cor)
            end if
	    
	    !** Les mailles : on ne prend pas en compte les mailles 3D **

            do j=1,MED_NBR_MAILLE_EQU
               call efncor(fid,maa,equ,MED_MAILLE,typmai(j),ncor,cret)
               print *,"Il y a ",ncor," correspondances sur les mailles ",typmai(j)
               if (ncor > 0 ) then
                  allocate(cor(2*ncor),STAT=ret)
                  call efequl(fid,maa,equ,cor,ncor,MED_MAILLE,typmai(j),cret)
                  do k=0,(ncor-1)
                     print *,"Correspondance ",k+1," : ",cor(2*k+1)," et ",cor(2*k+2)
                  end do
                  deallocate(cor)
               endif
            end do

	    ! ** Les faces **
            do j=1,MED_NBR_GEOMETRIE_FACE+1
               call efncor(fid,maa,equ,MED_FACE,typfac(j),ncor,cret)
               print *,"Il y a ",ncor," correspondances sur les faces ",typfac(j)
               if (ncor > 0 ) then
                  allocate(cor(2*ncor),STAT=ret)
                  call efequl(fid,maa,equ,cor,ncor,MED_FACE,typfac(j),cret)
                  do k=0,(ncor-1)
                     print *,"Correspondance ",k+1," : ",cor(2*k+1)," et ",cor(2*k+2)
                  end do
                  deallocate(cor)
               endif
            end do

	    ! **  Les aretes **
            do j=1,MED_NBR_GEOMETRIE_ARETE
               call efncor(fid,maa,equ,MED_ARETE,typare(j),ncor,cret)
               print *,"Il y a ",ncor," correspondances sur les aretes ",typare(j)
               if (ncor > 0 ) then
                  allocate(cor(2*ncor),STAT=ret)
                  call efequl(fid,maa,equ,cor,ncor,MED_ARETE,typare(j),cret)
                  do k=0,(ncor-1)
                     print *,"Correspondance ",k+1," : ",cor(2*k+1)," et ",cor(2*k+2)
                  end do
                  deallocate(cor)
               endif
            end do

         end if
      end do
   end if

!  ** Fermeture du fichier   **
   call efferm (fid,cret)
   print *,cret

!  ** Code retour
   call efexit(cret)
   
 end program test13
	




