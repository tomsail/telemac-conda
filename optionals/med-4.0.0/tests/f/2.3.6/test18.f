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
C * - Nom du fichier : test18.f
C *
C * - Description : routines de test de la conformite d'une fichier MED.
C *
C ******************************************************************************
      program test18
C     
      implicit none
      include 'med.hf'
C
C
      integer*8    fid

      integer      cret
      integer      maj,min,rel
      
C **  Creation du fichier test18.med
      call efouvr(fid,'test18.med',MED_LECTURE_ECRITURE, cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation du fichier'
         call efexit(-1)
      endif      
      print *,'- Creation du fichier'

C **  Fermeture du fichier
      call efferm (fid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur fermeture du fichier'
         call efexit(-1)
      endif      
      print *,'- Fermeture du fichier'

C ** Quelle version de la bibliotheque est utilisee ? **
      call efvedo(maj,min,rel,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur lecture version med'
         call efexit(-1)
      endif      
      print *,'- Version MED utilisee : ',maj,'.',min,'.',rel
 
C ** Conformite du format HDF  **
      call effoco('test18.med',cret)
      print *,cret
      if (cret .eq. 0) then
         print *,'- Format HDF conforme'
      else
         print *,'- Format HDF non conforme'
         call efexit(-1)
      endif   

C ** Conformite de la bibliotheque MED
      call efveco('test18.med',cret)
      print *,cret
      if (cret .eq. 0) then
         print *,'- Version MED du fichier conforme'
      else
         print *,'- Version MED du fichier non conforme'
         call efexit(-1)
      endif           

C **  Ouverture du fichier test18.med en lecture seule
      call efouvr(fid,'test18.med',MED_LECTURE, cret)
      if (cret .ne. 0 ) then
         print *,'Erreur ouverture du fichier'
         call efexit(-1)
      endif      
      print *,'- Ouverture du fichier'

C **  Lecture de la version de MED utilisee pour creer le fichier ? **
      call efveli(fid,maj,min,rel,cret)
      if (cret .ne. 0 ) then
         print *,'Erreur lecture version med du fichier'
         call efexit(-1)
      endif      
      print *,cret
      print *,'- Fichier cree avec MED V',maj,'.',min,'.',rel
         
C **  Fermeture du fichier
      call efferm (fid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur fermeture du fichier'
         call efexit(-1)
      endif      
      print *,'- Fermeture du fichier'
C 
      end 


