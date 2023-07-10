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
      integer hdfok, medok
      integer fexist, accok
      character*16 nomcoo(2)
      character*16 unicoo(2)
      data  nomcoo /"x","y"/, unicoo /"cm","cm"/


C  ** verifie que le fichier test1.med existe et est accessible  **
      call mfiexi('test1.med', MED_ACC_RDONLY, fexist, accok, cret)
      print *,"fexistf :",fexist, " ; accok :", accok

C  ** verifie que le fichier test1.med est au bon format **
      call mficom('test1.med',hdfok,medok,cret)
      print *,hdfok, medok, cret
      if (cret .ne. 0 ) then
         print *,'Erreur à la vérification du format'
         call efexit(-1)
      endif      

C  ** Ouverture en mode de lecture du fichier test1.med
      call mfiope(fid,'test1.med',MED_ACC_RDONLY, cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur ouverture du fichier en lecture'
         call efexit(-1)
      endif      

C  ** Lecture de l'en-tete du fichier
      call mficor(fid,des,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur lecture en-tete du fichier'
         call efexit(-1)
      endif      
      print *,"DESCRIPTEUR DE FICHIER : ",des

 
C  ** Fermeture du fichier test1.med 
      call mficlo(fid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur fermeture du fichier'
         call efexit(-1)
      endif      

      
      call mfiexi('test2.med', MED_ACC_RDWR, fexist, accok, cret)
      print *,"fexistf :",fexist, " ; accok :", accok

C     ** Ouverture en mode de creation du fichier test2.med
      call mfiope(fid,'test2.med',MED_ACC_RDWR, cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation du fichier'
         call efexit(-1)
      endif      
      
C  **  Creation du maillage maa1 de type non structure
C  **  et de dimension 2
      call mmhcre(fid,'maa1',2,2,
     &     MED_UNSTRUCTURED_MESH,'un premier maillage', 
     &     "",MED_SORT_DTIT,MED_CARTESIAN,nomcoo,unicoo,ret)
      print *,cret
      cret = cret + ret
C     **  Creation du nom universel
      call mmhunw(fid,'maa1',ret)
      cret = cret + ret
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation du maillage'
         call efexit(-1)
      endif      
      
C  **  Creation du maillage maa2 de type non structure
      call mmhcre(fid,'maa2',2,2,
     &     MED_UNSTRUCTURED_MESH,'un second maillage', 
     &     "",MED_SORT_DTIT,MED_CARTESIAN,nomcoo,unicoo,ret)
      cret = cret + ret 
      if (cret .ne. 0 ) then
         print *,'Erreur creation du maillage'
         call efexit(-1)
      endif      
    

C  ** Creation du maillage maa3 de type structure
      call mmhcre(fid,'maa3',2,2,
     &     MED_STRUCTURED_MESH,'un troisieme maillage', 
     &     "",MED_SORT_DTIT,MED_CARTESIAN,nomcoo,unicoo,ret)
      cret = cret + ret
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation du maillage'
         call efexit(-1)
      endif      

C **  Fermeture du fichier
      call mficlo(fid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur fermeture du fichier'
         call efexit(-1)
      endif      
C      
      end 





