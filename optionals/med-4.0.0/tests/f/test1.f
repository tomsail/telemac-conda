C*  This file is part of MED.
C*
C*  COPYRIGHT (C) 1999 - 2019  EDF R&D, CEA/DEN
C*  MED is free software: you can redistribute it and/or modify
C*  it under the terms of the GNU Lesser General Public License as published by
C*  the Free Software Foundation, either version 3 of the License, or
C*  (at your option) any later version.
C*
C*  MED is distributed in the hope that it will be useful,
C*  but WITHOUT ANY WARRANTY; without even the implied warranty of
C*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C*  GNU Lesser General Public License for more details.
C*
C*  You should have received a copy of the GNU Lesser General Public License
C*  along with MED.  If not, see <http://www.gnu.org/licenses/>.
C*

C******************************************************************************
C * - Description : open/close med files
C *
C *****************************************************************************
      program test1
C     
      implicit none
      include 'med.hf'
C
C     
      integer cret
      integer*8 fid

      character*255 fname
      character*200 des
      parameter (des = "Ceci est un courte description"
     1     // " du fichier test1.med")  
      
C     Creation du fichier "test1.med"
      call mfiope(fid,'test1.med',MED_ACC_RDWR, cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur à la création du fichier'
         call efexit(-1)
      endif      

C     Ecriture d'un en-tete dans le fichier
      call mficow(fid,des,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur à ecriture en-tete'
         call efexit(-1)
      endif      
      
C     Lecture de la taille du nom de fichier "test1.med"
      call mfinam(fid,"", cret)
      print *,cret
      if (cret .le. 0 ) then
         print *,'Erreur à la lecture de la taille du nom de fichier'
         call efexit(-1)
      endif      

C     Lecture du nom de fichier "test1.med"
      call mfinam(fid,fname, cret)
      print *,cret
      if (cret .le. 0 ) then
         print *,'Erreur à la lecture du nom de fichier'
         call efexit(-1)
      endif      
      print *,fname

C     Fermeture du fichier
      call mficlo(fid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur à la fermeture du fichier'
         call efexit(-1)
      endif      

C     Re-ouverture du fichier en lecture seule      
      call mfiope(fid,'test1.med',MED_ACC_RDONLY, cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur ouverture du fichier en lecture'
         call efexit(-1)
      endif      

C     Fermeture du fichier
      call mficlo(fid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur à la fermeture du fichier'
         call efexit(-1)
      endif      

      end
