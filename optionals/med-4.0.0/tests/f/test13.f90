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
  include 'med.hf90'
!
!
  integer*8    fid
  integer      ret,cret
  character*64 maa
  integer      mdim,nequ,ncor,sdim
  integer, allocatable, dimension(:) :: cor
  character*64  equ
  character*200 desc,des
  integer       i,j,k
  character*255 argc
  integer,parameter :: MY_NOF_DESCENDING_FACE_TYPE =  5
  integer,parameter :: MY_NOF_DESCENDING_EDGE_TYPE =  2


  integer, parameter :: MED_NBR_MAILLE_EQU = 8
  integer,parameter  :: typmai(MED_NBR_MAILLE_EQU) =  (/ MED_POINT1,MED_SEG2,   &
                                                      &  MED_SEG3,MED_TRIA3,    &
                                                      &  MED_TRIA6,MED_QUAD4,   &
                                                      &  MED_QUAD8,MED_POLYGON/)

   integer,parameter :: typfac(MY_NOF_DESCENDING_FACE_TYPE) = (/MED_TRIA3,MED_TRIA6,       &
					&	 MED_QUAD4,MED_QUAD8, MED_POLYGON/)
   integer,parameter ::typare(MY_NOF_DESCENDING_EDGE_TYPE) = (/MED_SEG2,MED_SEG3/)
   integer type
   character(16)  :: dtunit
   integer nstep, stype, atype
   character*16 nomcoo(3)   
   character*16 unicoo(3)
   integer nctcor,nstepc


   !  ** Ouverture du fichier en lecture seule **
   call mfiope(fid,'test12.med',MED_ACC_RDONLY, cret)
   print *,cret
   
     
   !  ** Lecture des infos sur le premier maillage **
   if (cret.eq.0) then
      call mmhmii(fid,1,maa,sdim,mdim,type,desc,dtunit,stype,nstep,atype,nomcoo,unicoo,cret)
      print *,"Maillage de nom : ",maa," et de dimension : ", mdim
   endif
   print *,cret


   !  ** Lecture du nombre d'equivalence  **
   if (cret.eq.0) then
      call meqneq(fid,maa,nequ,cret)
      if (cret.eq.0) then
         print *,"Nombre d'equivalence : ",nequ
      endif
   endif

 
   !** Lecture de toutes les equivalences **
   if (cret.eq.0) then
      do i=1,nequ
         print *,"Equivalence numero : ",i
         !** Lecture des infos sur l'equivalence **
         if (cret.eq.0) then
            call meqeqi(fid,maa,i,equ,des,nstepc,nctcor,cret)
         endif
         print *,cret
         if (cret.eq.0) then
            print *,"Nom de l'equivalence : ",equ          
            print *,"Description de l'equivalence : ",des
            print *,"Nombre de pas de temps sur l'equivalence : ",nstepc
            print *,"Nombre de correspondance sur MED_NO_IT, MED_NO_DT : ", nctcor 
         endif

         !** Lecture des correspondances sur les differents types d'entites **
         if (cret.eq.0) then
	    !** Les noeuds **
            call meqcsz(fid,maa,equ,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,ncor,cret)
            print *,cret
            print *,"Il y a ",ncor," correspondances sur les noeuds "
            if (ncor > 0) then
               allocate(cor(ncor*2),STAT=ret)
               call meqcor(fid,maa,equ,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,cor,cret)
               do j=0,(ncor-1)
                  print *,"Correspondance ",j+1," : ",cor(2*j+1)," et ",cor(2*j+2)
               end do
               deallocate(cor)
            end if
	    
!!$	    !** Les mailles : on ne prend pas en compte les mailles 3D **

            do j=1,MED_NBR_MAILLE_EQU
               call meqcsz(fid,maa,equ,MED_NO_DT,MED_NO_IT,MED_CELL,typmai(j),ncor,cret)
               print *,"Il y a ",ncor," correspondances sur les mailles ",typmai(j)
               if (ncor > 0 ) then
                  allocate(cor(2*ncor),STAT=ret)
                  call meqcor(fid,maa,equ,MED_NO_DT,MED_NO_IT,MED_CELL,typmai(j),cor,cret)
                  do k=0,(ncor-1)
                     print *,"Correspondance ",k+1," : ",cor(2*k+1)," et ",cor(2*k+2)
                  end do
                  deallocate(cor)
               endif
            end do

!!$	    ! ** Les faces **
            do j=1,MY_NOF_DESCENDING_FACE_TYPE
               call meqcsz(fid,maa,equ,MED_NO_DT,MED_NO_IT,MED_DESCENDING_FACE,typmai(j),ncor,cret)
               print *,"Il y a ",ncor," correspondances sur les faces ",typfac(j)
               if (ncor > 0 ) then
                  allocate(cor(2*ncor),STAT=ret)
                  call meqcor(fid,maa,equ,MED_NO_DT,MED_NO_IT,MED_DESCENDING_FACE,typfac(j),cor,cret)
                  do k=0,(ncor-1)
                     print *,"Correspondance ",k+1," : ",cor(2*k+1)," et ",cor(2*k+2)
                  end do
                  deallocate(cor)
               endif
            end do

!!$	    ! **  Les aretes **
            do j=1,MY_NOF_DESCENDING_EDGE_TYPE
               call meqcsz(fid,maa,equ,MED_NO_DT,MED_NO_IT,MED_DESCENDING_EDGE,typare(j),ncor,cret)
               print *,"Il y a ",ncor," correspondances sur les aretes ",typare(j)
               if (ncor > 0 ) then
                  allocate(cor(2*ncor),STAT=ret)
                  call meqcor(fid,maa,equ,MED_NO_DT,MED_NO_IT,MED_DESCENDING_EDGE,typare(j),cor,cret)
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
   call mficlo(fid,cret)
   print *,cret

!  ** Code retour
   call efexit(cret)
   
 end program test13
	




