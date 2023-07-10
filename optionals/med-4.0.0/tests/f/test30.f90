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
! * - Nom du fichier : test30.f90
! *
! * - Description : lecture des joints dans un maillage MED.
! *
! ******************************************************************************

program test30
  
  implicit none
  include 'med.hf90'
!
!
  integer*8    fid
  integer      ret,cret,edim
  character*64 maa,maadst,corr,jnt
  integer      mdim,njnt,ncor,domdst,nc,nent
  character*64  equ,ent, nodenn, nodent
  character*200 des, dcornn, dcornt
  integer       i,j,k
  character*255 argc
  character*200 desc
  integer type
  integer nstep,stype,atype
  character*16 nomcoo(2)
  character*16 unicoo(2)
  character*16 dtunit
  integer entlcl,geolcl, entdst, geodst
  
  data nodent /"CorresTria3"/
  data nodenn /"CorresNodes"/
  
  argc = "test29.med"
  
  !  ** Ouverture du fichier en lecture seule **
  call mfiope(fid,argc,MED_ACC_RDONLY, cret)
  print '(I1)',cret
  
     
  !  ** Lecture des infos sur le premier maillage **
  if (cret.eq.0) then
      call mmhmii(fid,1,maa,edim,mdim,type,desc,dtunit,stype,nstep,atype,nomcoo,unicoo,cret)
      print '(A,A,A,I3)',"Maillage de nom : ",maa
   endif
   print '(I1)',cret
   

   !  ** Lecture du nombre de joints **
   if (cret.eq.0) then
      call  msdnjn(fid,maa,njnt,cret)
      if (cret.eq.0) then
         print '(A,I3)',"Nombre de joints : ",njnt
      endif
   endif
   
   !** Lecture de tous les joints **
   if (cret.eq.0) then
      do i=1,njnt
         print '(A,I3)',"Joint numero : ",i
         !** Lecture des infos sur le joint **
         if (cret.eq.0) then
            call msdjni(fid,maa,i,jnt,des,domdst,maadst,nstep,ncor,cret)
         endif
         print '(I1)',cret
         if (cret.eq.0) then
            print '(A,A)',"Nom du joint                              : ",jnt          
            print '(A,A)' ,"Description du joint                     : ",des 
            print '(A,I3)',"Domaine en regard                        : ",domdst
            print '(A,A)' ,"Maillage en regard                       : ",maadst
            print '(A,I3)',"Nombre de sequence                       : ",nstep
            print '(A,I3)',"Nombre de correspondance (NO_DT,NO_IT)   : ",ncor
         endif
         
         do nc=1,ncor
            call msdszi(fid,maa,jnt,MED_NO_DT,MED_NO_IT,nc,entlcl,geolcl,entdst,geodst,ncor,cret)
            print '(I3)',cret
            if (cret>=0) then
               call affCorr(fid,maa,jnt,entlcl,geolcl,entdst,geodst)
            endif
         enddo

         
      end do
   end if

!  ** Fermeture du fichier   **
   call mficlo (fid,cret)
   print '(I2)',cret
   
!   call flush(6)


!  ** Code retour
   call efexit(cret)
   
 end program test30
	

 subroutine affCorr(fid,maa,jnt,entlcl,geolcl,entdst,geodst)
   
   implicit none
   include 'med.hf90'

   character*(*) maa,jnt
   character*200 des;
   integer*8 fid
   integer ret,cret,ncor,ntypnent,i,j,nent,ntypent
   integer entlcl,geolcl, entdst, geodst
   integer, allocatable, dimension(:) :: cortab

   
   call msdcsz(fid,maa,jnt,MED_NO_DT,MED_NO_IT,entlcl,geolcl,entdst,geodst,ncor,cret)
   print '(I3,i5)',cret,ncor
           

   !** Lecture des correspondances sur les differents types d'entites connus a priori **
   if (cret.eq.0) then

      print '(A,I4,A,I4,A,I4,A,I4,A)','correspondance entre les types : (',entlcl,'/',geolcl,') et (',entdst,'/',geodst,')'
      print '(A,I4)','nombre de type de couples d''entite en regard ',ncor

!      call flush(6)
      
      allocate(cortab(ncor*2),STAT=ret)
      call msdcrr(fid,maa,jnt,MED_NO_DT,MED_NO_IT,entlcl,geolcl,entdst,geodst,cortab,cret)
      do j=0,(ncor-1)
         print '(A,I3,A,I4,A,I4)',"Correspondance ",j+1," : ",cortab(2*j+1)," et ",cortab(2*j+2)
      end do
      deallocate(cortab)
   end if


         
   return
 end subroutine affCorr



