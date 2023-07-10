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
      character*16  comp(3), unit(3)
      character*32  nom
C
C     ** Ouverture du fichier test2.med en mode lecture ajout
      call efouvr(fid,'test2.med',MED_LECTURE_AJOUT, cret)
      print *,cret 
      if (cret .ne. 0 ) then
         print *,'Erreur ouverture du fichier'
         call efexit(-1)
      endif      
      print *,'On ouvre le fichier test2.med'
C
C     ** Lecture du nombre de champ
      call efncha(fid,0,ncha,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur lecture du nombre de champ'
         call efexit(-1)
      endif      
      print *,'Nombre de champs dans test2.med : ',ncha
C
C     ** Montage du fichier test10.med (acces aux champs)
      call efmont(fid,'test10.med',MED_CHAMP,mid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur montage du fichier'
         call efexit(-1)
      endif      
      print *,'On monte les champs du fichier test10.med'
C
C     ** Lecture du nombre de champ apres montage
      call efncha(fid,0,ncha,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur lecture du nombre de champ'
         call efexit(-1)
      endif      
      print *,'Nombre de champs dans test2.med apres montage : ',ncha
C
C     ** Acces a tous les champs de test10.med a travers le point de 
C     ** montage
C
      do 10 i = 1,ncha
C
C        ** Lecture du nombre de composante dans le champ
         call efncha(fid,i,ncomp,cret)
         print *,cret
         if (cret .ne. 0 ) then
            print *,'Erreur lecture du nombre de composante'
            call efexit(-1)
         endif      
C
C           ** Lecture des informations sur le champ
         call efchai(fid,i,nom,type,comp,unit,ncomp,cret)
         print *,cret
         if (cret .ne. 0 ) then
            print *,'Erreur lecture des infos sur le champ'
            call efexit(-1)
         endif      
         print *,'Champ de nom ',nom
         print *,' avec ', ncomp, ' composantes'
C
 10   continue
C    
C
C     ** Demontage de test10.med
      call efdemo(fid,mid,MED_CHAMP,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur demontage du fichier'
         call efexit(-1)
      endif      
      print *,'On demonte le fichier test10.med'
C
C     ** Lecture du nombre de champ apres demontage
      call efncha(fid,0,ncha,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur lecture du nombre de champ'
         call efexit(-1)
      endif      
      print *,'Nombre de champs apres demontage : ',ncha
C
C     ** Fermeture du fichier
      call efferm(fid,cret)
      print *, cret
      if (cret .ne. 0 ) then
         print *,'Erreur fermeture du fichier'
         call efexit(-1)
      endif      
      print *,'On ferme le fichier test2.med'
C
C     ** Creation du fichier test20.med
      call efouvr(fid,'test20.med',MED_LECTURE_ECRITURE,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation du fichier'
         call efexit(-1)
      endif      
      print *,'Creation du fichier test20.med'
C
C     ** Montage du fichier test2.med (acces aux maillages)
      call efmont(fid,'test2.med',MED_MAILLAGE,mid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur montage du fichier'
         call efexit(-1)
      endif      
      print *,'On monte le fichier test2.med'
C
C     ** Lecture du nombre de maillage apres montage
      call efnmaa(fid,nmaa,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur lecture du nombre de maillage'
         call efexit(-1)
      endif      
      print *,'Nombre de maillage apres montage : ', nmaa
C
C     ** Montage du fichier test10.med (acces aux champs)
      call efmont(fid,'test10.med',MED_CHAMP,mid2,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur montage du fichier'
         call efexit(-1)
      endif      
      print *,'On monte le fichier test10.med'
C
C     ** Lecture du nombre de champs apres montage
      call efncha(fid,0,ncha,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur lecture du nombre de champ'
         call efexit(-1)
      endif      
      print *,'Nombre de champ  apres montage : ',ncha
C
C     ** Demontage de test10.med
      call efdemo(fid,mid2,MED_CHAMP,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur demontage du fichier'
         call efexit(-1)
      endif      
      print *,'On demonte test10.med'
C
C     ** Demontage de test2.med
      call efdemo(fid,mid,MED_MAILLAGE,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur demontage du fichier'
         call efexit(-1)
      endif      
      print *,'On demonte test2.med'
C
C     ** Fermeture du fichier
      call efferm(fid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur fermeture du fichier'
         call efexit(-1)
      endif      
      print *,'Fermeture du fichier test20.med'
C
      end
C
