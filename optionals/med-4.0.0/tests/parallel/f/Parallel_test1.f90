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
! * - Nom du fichier : Parallel_test1.f90
! *
! * - Description : lecture de champs de resultats MED en parallele
! *
! ***************************************************************************** 


program parallel_test1

  implicit none
  include 'med.hf90'
  include 'mpif.h'

  integer  ret, fid
  integer  USER_INTERLACE,USER_MODE
  integer*4 com,ioe,rank,nprocs
  integer  info,com4_8
  integer  nent 
  integer  nvent
  integer  ncent
  integer  start, stride, count, bsize, lbsize, resd
  character*64 :: pflname
  integer*8 flt(1)
  real*8,   allocatable,dimension(:) :: val
  integer   i,j,k

  com4_8=MPI_COMM_WORLD
  info=MPI_INFO_NULL

  call MPI_INIT(ioe)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,ioe)
  call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ioe)

  !  ** ouverture du fichier **
  call mpfope(fid, 'NENT-942_NVAL-008_NCST-007.med', MED_ACC_RDONLY,com4_8, info, ret)

  if (ret .ne. 0) then 
   print *,"Erreur à l'ouverture du fichier"
   print *,"Process n° ",rank,"/",nprocs," ret :",ret
   call efexit(ret)
  endif

  nent     = 942
  nvent    = 008
  ncent    = 007
  pflname  = ""
  bsize  = nent/nprocs
! Etant donné que l'on affecte qu'un bloc par processus lbsize vaut toujours 0
  lbsize = 0
  start  = rank*(bsize)+1
  count  = 1
  stride = bsize
  resd = 0
  if (rank.eq.(nprocs-1) ) then
     resd   = nent-(nprocs*bsize)
     bsize = bsize + resd
  endif
  print *,"myrank :",rank," resd", resd," bsize ",bsize," lbsize",lbsize

  call mfrall(1,flt,ret)
  if (ret .ne. 0) then 
   print *,"Erreur à l'allocation du filtre"
   print *,"Process n° ",rank,"/",nprocs," ret :",ret
   call efexit(ret)
  endif

  call mfrblc (fid, nent, nvent, ncent, &
       & MED_ALL_CONSTITUENT, MED_FULL_INTERLACE,MED_COMPACT_STMODE ,MED_ALLENTITIES_PROFILE, &
       & start, stride, count, bsize, lbsize, flt, ret)

  if (ret .ne. 0) then 
   print *,"Erreur à la définition du filtre"
   print *,"Process n° ",rank,"/",nprocs," ret :",ret
   call efexit(ret)
  endif

  allocate(val(bsize*nvent*ncent),STAT=ret) 
  val(:)=-1.1

  call mfdrar   ( fid, "NENT-942_NVAL-008_NCST-007_NBL-001",&
       & MED_NO_DT, MED_NO_IT, MED_CELL, MED_TRIA6,& 
       & flt(1), val, ret )
  if (ret .ne. 0) then 
   print *,"Erreur à la lecture du champ résultat"
   print *,"Process n° ",rank,"/",nprocs," ret :",ret
   call efexit(ret)
  endif

  open(40+rank)  
  do i=0,bsize-1
     do j=0,nvent-1
        do k=0,ncent-1
           write(40+rank,'(1X,F10.3,1X)',ADVANCE='NO') val(i*(ncent*nvent)+j*ncent+k+1)
        enddo
        write(40+rank,'(A)') "/"
     enddo
     write(40+rank,'(A)') "//"
  enddo
  close(40+rank)

  deallocate(val)

  call mfrdea(1,flt,ret)
  if (ret .ne. 0) then 
   print *,"Erreur à la desallocation du filtre"
   print *,"Process n° ",rank,"/",nprocs," ret :",ret
   call efexit(ret)
  endif

  print *,"Process n° ",rank,"/",nprocs," ret :",ret

!  call MPI_BARRIER(com,ioe)

  call mficlo(fid,ret)

  call MPI_FINALIZE(ioe)

end program parallel_test1
