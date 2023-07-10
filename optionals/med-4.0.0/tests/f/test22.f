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
C* - Nom du fichier : test22.f
C*
C* - Description : lecture des valeurs scalaires numeriques dans un fichier MED
C ******************************************************************************
      program test22
C     
      implicit none
      include 'med.hf'
C
      integer*8 fid
      integer cret
      character*16 dtunit
      character*64 nom
      character*200 desc
      integer vali
      real*8 valr,dt
      integer n,npdt,i,j,type,numdt,numo
C     
C     Ouverture du fichier test21.med en lecture seule
C
      call mfiope(fid,'test21.med',MED_ACC_RDONLY, cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur ouverture du fichier'
         call efexit(-1)
      endif      
      print *,'Ouverture du fichier test21.med'
C
C     Lecture du nombre de variable scalaire
C
      call  mprnpr(fid,n,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur lecture du nombre de variable'
         call efexit(-1)
      endif      
      print *,'Nombre de variables scalaires : ',n
C
C     Lecture des infos (type,description) propres 
C     a chaque variable
C
      do 10 i=1,n
         call mprpri(fid,i,nom,type,desc, 
     &               dtunit,npdt,cret)
         print *,cret
         if (cret .ne. 0 ) then
            print *,'Erreur lecture des infos'
            call efexit(-1)
         endif      
         print *,'- Scalaire de nom : ',nom
         if (type.eq.MED_FLOAT64) then
            print *,'  de type flottant' 
         else
            print *,'  de type entier'
         endif
         print *,'  Description associee : ',desc
         print *,'  Nombre de valeurs : ',npdt
         print *,'  Unite : ',dtunit
C
C     Pour chaque scalaire, on regarde les valeurs associees
C     eventuellement a un pas de temps et/ou un numero d'ordre 
C
         do 20 j=1,npdt
            call mprcsi(fid,nom,j,numdt,numo,dt,cret)
            print *,cret
            if (cret .ne. 0 ) then
               print *,'Erreur infos pas de temps'
               call efexit(-1)
            endif      
            print *,'   Valeur ', j
C
            if (numdt.eq.MED_NO_DT) then
               print *,'     - Aucun pas de temps'
            else
               print *,'     - Pas de temps de numero ',numdt
               print *,'      de valeur : ',dt
            endif
C
            if (numo.eq.MED_NO_IT) then
               print *,'     - Aucun numero ordre'
            else
               print *,'     - Numero ordre : ',numo
            endif
C
            if (type.eq.MED_FLOAT64) then
C           ** Lecture de la valeur flottante associee 
C           ** au pas de temps
               call mprrvr(fid,nom,numdt,numo,valr,cret)
               print *,cret
               if (cret .ne. 0 ) then
                  print *,'Erreur lecture valeur'
                  call efexit(-1)
               endif      
               print *,'     - Valeur : ',valr
            else
C           ** Lecture de la valeur entiere associee 
C           ** au pas de temps
               call mprivr(fid,nom,numdt,numo,vali,cret)
               print *,cret
               if (cret .ne. 0 ) then
                  print *,'Erreur lecture valeur'
                  call efexit(-1)
               endif      
               print *,'     - Valeur : ',vali
            endif
C
 20      continue
C
 10   continue
C
C     Fermeture du fichier      
C
      call mficlo(fid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur fermeture du fichier'
         call efexit(-1)
      endif      
      print *,'Fermeture du fichier test21.med'
C
      end
