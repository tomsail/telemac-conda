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
C* - Nom du fichier : test21.f
C*
C* - Description : ecriture de valeurs scalaires numeriques dans un fichier MED
C*
C ******************************************************************************
      program test21
C     
      implicit none
      include 'med.hf'
C
      integer*8 fid
      integer cret
      character*16 edtuni,dtunit1
      character*32 nom1, nom2
      character*200 desc1, desc2
      integer vali1, vali2
      real*8 valr1,dt
C
      parameter (nom1="VariableEntiere")
      parameter (nom2="VariableFlottante")
      data desc1 / "Une premiere description" /
      data desc2 / "Une seconde description" /
      parameter (vali1 = 56,vali2 = -789)
      parameter (valr1 = 67.98D0)
    
      parameter (edtuni="                "
     1         ,dtunit1="ms")
C
C     
C     Creation du fichier test21.med
C
      call efouvr(fid,'test21.med',MED_LECTURE_ECRITURE,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation du fichier'
         call efexit(-1)
      endif      
      print *,'Creation du fichier test21.med'
C
C     Creation d'une variable scalaire entiere 
C
      call efscac(fid,nom1,MED_INT,desc1,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation variable scalaire'
         call efexit(-1)
      endif      
      print *,'Creation d une variable scalaire entiere'
C
C     Ecriture d'une valeur sans pas de temps ni numero d'ordre
C
      dt =0.0D0
      call efscee(fid,nom1,vali1,MED_NOPDT,edtuni,dt,MED_NONOR,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur ecriture valeur scalaire'
         call efexit(-1)
      endif      
      print *,'Ecriture valeur entiere sans pas de temps'
C
C     Ecriture d'une valeur avec pas de temps et sans numero d'ordre
C
      dt = 5.5D0
      call efscee(fid,nom1,vali2,1,dtunit1,dt,MED_NONOR,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur ecriture valeur scalaire'
         call efexit(-1)
      endif      
      print *,'Ecriture valeur entiere avec pas de temps'
C
C     Creation d'une variable scalaire flottante
C
      call efscac(fid,nom2,MED_FLOAT64,desc2,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation variable sclaire'
         call efexit(-1)
      endif      
      print *,'Creation d une variable scalaire flottante'
C
C     Ecriture d'une valeur flottante avec pas de temps et numero d'ordre
C
      call efscfe(fid,nom2,valr1,1,dtunit1,dt,2,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur ecriture valeur scalaire'
         call efexit(-1)
      endif      
      print *,'Ecriture valeur entiere avec pas de temps'
C
C     Fermeture du fichier      
C
      call efferm(fid,cret)
      if (cret .ne. 0 ) then
         print *,'Erreur fermeture du fichier'
         call efexit(-1)
      endif      
      print *,cret
      print *,'Fermeture du fichier test21.med'
C
      end
C
