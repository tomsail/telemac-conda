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

C     ******************************************************************************
C     * - Nom du fichier : test14.f
C     *
C     * - Description : ecriture des noeuds d'un maillage MED
C     *                 a l'aide des routines de niveau 2
C     *                 MED - equivalent a test4.f
C     *
C     ******************************************************************************
      program test14           
C     
      implicit none             
      include 'med.hf'
C      
      integer*8 fid
      integer cret
C     ** la dimension du maillage **
      integer mdim,sdim
C     ** nom du maillage de longueur maxi MED_TAILLE_NOM ** 
      character*64 maa
C     ** le nombre de noeuds **
      integer   nnoe
      parameter (mdim=2,maa="maa1",nnoe=4,sdim=2)
C     ** table des coordonnees  
      real*8 coo(mdim*nnoe)
C     ** tables des noms et des unites des coordonnees 
      character*16 nomcoo(mdim), unicoo(mdim) 
C     ** tables des noms, numeros, numeros de familles des noeuds
C     autant d'elements que de noeuds - les noms ont pout longueur
C     MED_TAILLE_PNOM : 8  **
      character*16 nomnoe(nnoe)
      integer numnoe(nnoe), nufano(nnoe)
      real*8 dt
      parameter(dt=0.0)

      data   coo /0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0/
      data   nomcoo /"x","y"/, unicoo /"cm","cm"/
      data   nomnoe /"nom1","nom2","nom3","nom4"/
      data   numnoe /1,2,3,4/,nufano /0,1,2,2/
      
C  ** Creation du fichier test14.med  **
      call mfiope(fid,'test14.med',MED_ACC_RDWR, cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur creation du fichier'
         call efexit(-1)
      endif      

C     ** Creation du maillage  **
      call mmhcre(fid,maa,mdim,sdim,MED_UNSTRUCTURED_MESH,
     &     'un maillage pour test14',"",MED_SORT_DTIT,
     &     MED_CARTESIAN,nomcoo,unicoo,cret) 
      print *,cret  
      if (cret .ne. 0 ) then
         print *,'Erreur creation du maillage'
         call efexit(-1)
      endif      
      
C     ** Ecriture des noeuds d'un maillage MED : 
C     - Des coordonnees en mode MED_FULL_INTERLACE : (X1,Y1,X2,Y2,X3,Y3,...) 
C     dans un repere cartesien 
C     - Des noms (optionnel dans un fichier MED) 
C     - Des numeros (optionnel dans un fichier MED) 
C     - Des numeros de familles des noeuds **	  
      call mmhnow(fid,maa,MED_NO_DT,MED_NO_IT,dt,MED_FULL_INTERLACE, 
     &            nnoe,coo,MED_TRUE,nomnoe,MED_TRUE,numnoe,
     &            MED_TRUE,nufano,cret)    
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur ecriture des noeuds'
         call efexit(-1)
      endif      
      
C     ** Fermeture du fichier **
      call mficlo(fid,cret)
      print *,cret
      if (cret .ne. 0 ) then
         print *,'Erreur fermeture du fichier'
         call efexit(-1)
      endif      
C
      end 
	


