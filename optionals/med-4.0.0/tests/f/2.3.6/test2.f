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
C * - Nom du fichier : test2.f
C *
C * - Description : exemples de creations de maillage MED
C *
C ******************************************************************************
      program test2
C     
      implicit none
      include 'med.hf'
C
C
      integer cret,ret
      integer*8 fid

      character*200 des

C  ** verifie que le fichier test1.med est au bon format **
      call effoco('test1.med',cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur à la vérification du format'
         call efexit(-1)
      endif      

C  ** Ouverture en mode de lecture du fichier test1.med
      call efouvr(fid,'test1.med',MED_LECTURE, cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur ouverture du fichier en lecture'
         call efexit(-1)
      endif      

C  ** Lecture de l'en-tete du fichier
      call effien (fid, MED_FICH_DES,des,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur lecture en-tete du fichier'
         call efexit(-1)
      endif      
      print *,"DESCRIPTEUR DE FICHIER : ",des

 
C  ** Fermeture du fichier test1.med 
      call efferm (fid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur fermeture du fichier'
         call efexit(-1)
      endif      

      
C  ** Ouverture en mode de creation du fichier test2.med
      call efouvr(fid,'test2.med',MED_LECTURE_ECRITURE, cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation du fichier'
         call efexit(-1)
      endif      
      
C  **  Creation du maillage maa1 de type MED_NON_STRUCTURE
C  **  et de dimension 3
C  ** attention le ../test3 de V3.0 supposait une dimension 2
C  ** ce qui propoquait un écrasement de mdim lors du traitement
C  ** des chaines unites et nom des axes. 
      call efmaac(fid,'maa1',3,
     &     MED_NON_STRUCTURE,
     &     'un premier maillage',ret)
      cret = cret + ret
C     **  Creation du nom universel
      call efunvc(fid,'maa1',ret)
      cret = cret + ret
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation du maillage'
         call efexit(-1)
      endif      
      
C  **  Creation du maillage maa2 de type MED_NON_STRUCTURE
C  **  et de dimension 2
      call efmaac(fid,'maa2',2,
     &     MED_NON_STRUCTURE,
     &     'un second maillage',ret)
      cret = cret + ret 
C  **  Ecriture de la dimension de l'espace : maillage
C  **  de dimension 2 dans un espace de dimension 3
      call efespc(fid,'maa2',3,ret)
      cret = cret + ret
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation du maillage'
         call efexit(-1)
      endif      

C  ** Creation du maillage maa3 de type MED_STRUCTURE
C  **  et de dimension 1
      call efmaac(fid,'maa3',1,
     &     MED_STRUCTURE,
     &     'un troisieme  maillage',ret)
      cret = cret + ret
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation du maillage'
         call efexit(-1)
      endif      

C **  Fermeture du fichier
      call efferm (fid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur fermeture du fichier'
         call efexit(-1)
      endif      
C      
      end 





