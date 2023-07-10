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
C * - Nom du fichier : test19.f
C *
C * - Description : conversion groupes => familles
C *
C *****************************************************************************
      program test19
C     
      implicit none
      include 'med.hf'
C
C
      integer cret
      integer*8 fid

      character *32 maa
      parameter (maa = "maillage_test19")
      character*200 des
      parameter (des = "un maillage pour test19")
      integer mdim 
      parameter (mdim = 2)
C     Donnees de tests pour MEDgro2FamCr() 
C     Les noeuds/mailles sont numerotes de 1 a 5 et les
C     groupes de 1 a 3.
C     Au depart, on a :
C     - G1 : 1,2
C     - G2 : 3,4,6
C     - G3 : 1,4
C     Au retour, on foit avoir 4 familles de noeuds + 4 familles de mailles 
C     + la famille 0 dans le fichier :
C     - F0 : 5       - groupes : aucun groupe par defaut (convention habituelle).
C     - F1 : 1       - groupes : G1,G3  
C     - F2 : 2       - groupes : G1
C     - F3 : 3,6     - groupes : G2
C     - F4 : 4       - groupes : G2,G3
C  
      integer ngroup 
      parameter (ngroup = 3)
      integer nent 
      parameter (nent = 6)
      character*80 nomgro(ngroup)
      integer ent(7)
      integer ind(ngroup+1)
      integer ngeo
      parameter (ngeo = 3)
      integer geo(ngeo)
      integer indgeo(ngeo+1)
      character*200 attdes,gro
      integer attval,attide
      integer typgeo
      integer indtmp
C
      data nomgro    / "GROUPE1","GROUPE2","GROUPE3"    /
      data ent       /  1,2, 3,4,6, 1,4                 /
      data ind       /  1,   3,     6,   8              /
      data geo       /  MED_SEG2, MED_TRIA3, MED_TETRA4 /
      data indgeo    /  1,4,6,7 /
C      
C     ** Creation du fichier test19.med
      call efouvr(fid,'test19.med',MED_LECTURE_ECRITURE, cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation du fichier'
         call efexit(-1)
      endif      
      print *,'Creation du fichier test19.med'
C
C     ** Creation du maillage
      call efmaac(fid,maa,mdim,MED_NON_STRUCTURE,des,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation du maillage'
         call efexit(-1)
      endif      
      print *,'Creation du maillage'
C
C     ** Creation de la famille 0
      call effamc(fid,maa,'FAMILLE_0',0,attide,attval,attdes,0,gro,0,
     &               cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation de la famille 0'
         call efexit(-1)
      endif      
      print *,'Creation de la famille 0'
C
C     ** Creation des familles de noeuds
      call efg2fc(fid,maa,nomgro,ind,ngroup,ent,nent,MED_NOEUD,
     &               typgeo,indtmp,0,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation des familles de noeud'
         call efexit(-1)
      endif      
      print *,'Creation des familles de noeuds dans test19.med'
C
C     ** Creation des familles de mailles
      call efg2fc(fid,maa,nomgro,ind,ngroup,ent,nent,MED_MAILLE,
     &               geo,indgeo,ngeo,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation des familles de maille'
         call efexit(-1)
      endif      
      print *,'Creation des familles de mailles dans test19.med'
C      
C     ** Fermeture du fichier
      call efferm (fid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur fermeture du fichier'
         call efexit(-1)
      endif      
      print *,'Fermeture du fichier'
C
      end
