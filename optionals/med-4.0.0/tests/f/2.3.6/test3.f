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
C * - Nom du fichier : test3.f
C *
C * - Description : lecture des informations sur les maillages dans un fichier
C*                  MED.
C *
C ******************************************************************************
      program test3
C     
      implicit none
      include 'med.hf'
C
C
      integer*8     fid
      integer       cret,cres,type,cnu
      character*32  maa
      character*80  nomu
      character*200 desc
      integer       nmaa,i,mdim,edim
      
C ** Ouverture du fichier en lecture seule
      call efouvr(fid,'test2.med',MED_LECTURE, cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur ouverture du fichier en lecture'
         call efexit(-1)
      endif      

C ** lecture du nombre de maillage                      **
      call efnmaa(fid,nmaa,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur lecture du nombre de maillage'
         call efexit(-1)
      endif      
      print *,'Nombre de maillages = ',nmaa

C ** lecture des infos sur les maillages : **
C ** - nom, dimension, type,description
C ** - options : nom universel, dimension de l'espace
      do i=1,nmaa  
         call efmaai(fid,i,maa,mdim,type,desc,cret)
         edim = -1
         call efespl(fid,maa,edim,cres)
         call efunvl(fid,maa,nomu,cnu)
         print *,cret
         if (cret .ne. 0 ) then
            print *,'Erreur acces au maillage'
            call efexit(-1)
         endif      
         print '(A,I1,A,A4,A,I1,A,A65,A65)','maillage '
     &        ,i,' de nom ',maa,' et de dimension ',mdim,
     &        ' de description ',desc
         if (type.eq.MED_NON_STRUCTURE) then
            print *,'Maillage non structure'
         else
            print *,'Maillage structure'
         endif
         if (cres.eq.0) then
            print *,'Dimension espace ', edim
         else
            print *,'Dimension espace ', mdim
         endif
         if (cnu.eq.0) then
            print *,'Nom universel : ',nomu
         else
            print *,'Pas de nom universel'
         endif
      enddo         
         
C **  fermeture du fichier
      call efferm (fid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur fermeture du fichier'
         call efexit(-1)
      endif      
C
      end 

