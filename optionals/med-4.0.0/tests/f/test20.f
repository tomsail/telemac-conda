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

C ******************************************************************************
C * - Nom du fichier : test20.f
C *
C * - Description : montage/demontage de fichiers MED. 
C *
C ******************************************************************************
      program test20
C     
      implicit none
      include 'med.hf'
C
C
      integer*8 fid, mid, mid2
      integer cret, ncha, nmaa
      integer i, ncomp, type
      character*16  comp(3), unit(3), dtunit
      character*64  nomcha,nommaa
      integer lmesh, ncst
C
C     ** Ouverture du fichier test20-0.med en mode lecture ajout
      call mfiope(fid,'test20-0.med',MED_ACC_RDEXT, cret)
      print *,cret 
      if (cret .ne. 0 ) then
         print *,'Erreur ouverture du fichier'
         call efexit(-1)
      endif      
      print *,'On ouvre le fichier test20-0.med'
C
C     ** Lecture du nombre de champ
      call mfdnfd(fid,ncha,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur lecture du nombre de champ'
         call efexit(-1)
      endif      
      print *,'Nombre de champs dans test20-0.med : ',ncha
C
C     ** Montage du fichier test10-0.med (acces aux champs et maillages)
      call mfiomn(fid, 'test10-0.med', MED_FIELD, mid, cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur montage du fichier'
         call efexit(-1)
      endif      
      print *,'On monte les champs du fichier test10-0.med'
C
C     ** Lecture du nombre de champs apres montage
      call mfdnfd(fid,ncha,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur lecture du nombre de champs'
         call efexit(-1)
      endif      
      print *,'Nombre de champs dans test20-0.med apres montage : ',ncha
C
C     ** Acces a tous les champs de test10.med a travers le point de 
C     ** montage
C  
      do 10 i = 1,ncha
C
C        ** Lecture du nombre de composante dans le champ
         call mfdnfc(fid,i,ncomp,cret)
         print *,cret
         if (cret .ne. 0 ) then
            print *,'Erreur lecture du nombre de composante'
            call efexit(-1)
         endif      
C
 10   continue
C    
C
C     ** Demontage de test10-0.med
      call mfioun(fid, mid, MED_FIELD, cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur demontage du fichier'
         call efexit(-1)
      endif      
      print *,'On demonte le fichier test10-0.med'
C
C     ** Lecture du nombre de champ apres demontage
      call mfdnfd(fid,ncha,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur lecture du nombre de champ'
         call efexit(-1)
      endif      
      print *,'Nombre de champs apres demontage : ',ncha
C
C     ** Fermeture du fichier
      call mficlo(fid,cret)
      print *, cret
      if (cret .ne. 0 ) then
         print *,'Erreur fermeture du fichier'
         call efexit(-1)
      endif      
      print *,'On ferme le fichier test20-0.med'
C

C     * Phase 2 : Test de montage de champs et de maillages
C                 dans un fichier vierge
      
C     ** Creation du fichier test20.med
      call mfiope(fid,'test20.med',MED_ACC_RDWR,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation du fichier'
         call efexit(-1)
      endif      
      print *,'Creation du fichier test20.med'
C
C     ** Montage du fichier test20-0.med (acces aux maillages)
      call mfiomn(fid, 'test20-0.med', MED_MESH, mid, cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur montage du fichier'
         call efexit(-1)
      endif      
      print *,'On monte le fichier test20-0.med'
C
C     ** Lecture du nombre de maillage apres montage
      call mmhnmh(fid,nmaa,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur lecture du nombre de maillage'
         call efexit(-1)
      endif      
      print *,'Nombre de maillage apres montage : ', nmaa
C
C     ** Montage du fichier test10-0.med (acces aux champs)
      call mfiomn(fid, 'test10-0.med', MED_FIELD, mid2, cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur montage du fichier'
         call efexit(-1)
      endif      
      print *,'On monte le fichier test10-0.med'
C
C     ** Lecture du nombre de champs apres montage
      call mfdnfd(fid,ncha,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur lecture du nombre de champ'
         call efexit(-1)
      endif      
      print *,'Nombre de champ  apres montage : ',ncha
C
C     ** Demontage de test10.med
      call mfioun(fid, mid2,MED_FIELD,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur demontage du fichier'
         call efexit(-1)
      endif      
      print *,'On demonte test10.med'
C
C     ** Demontage de test20-0.med
      call mfioun(fid, mid,MED_MESH,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur demontage du fichier'
         call efexit(-1)
      endif      
      print *,'On demonte test20-0.med'
C
C     ** Fermeture du fichier
      call mficlo(fid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur fermeture du fichier'
         call efexit(-1)
      endif      
      print *,'Fermeture du fichier test20.med'
C
      end
C
