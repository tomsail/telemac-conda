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

C       *******************************************************************************
C       * - Nom du fichier : test26.f
C       *
C       * - Description : lecture de mailles MED_POLYEDRE dans le maillage MED
C       *                 du fichier test25.med   
C       *
C       ******************************************************************************
	program test26
C       
	implicit none
	include 'med.hf'
C	
	integer*8 fid
	integer cret,mdim,nmaa,npoly,i,j,k,l,nfindex
        integer edim,nstep,stype,atype, chgt, tsf
	integer nfaces, nnoeuds
	integer ind1, ind2
	character*64 maa
 	character*200 desc
	integer n
	parameter (n=2)
        integer np,nf,np2,nf2,taille,tmp
	parameter (np=3,nf=9,np2=3,nf2=8)
	integer indexp(np),indexf(nf)
	integer conn(24)
	integer indexp2(np2),indexf2(nf2)
	integer conn2(nf2)
	character*16 nom(n)
	integer num(n),fam(n)
	integer type
	character*16 nomcoo(3)
	character*16 unicoo(3)
	character(16)  :: dtunit
C
C       Ouverture du fichier test25.med en lecture seule
	call mfiope(fid,'test25.med',MED_ACC_RDONLY, cret)
        print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ouverture du fichier'
	   call efexit(-1)
	endif      
        print *,'Ouverture du fichier test25.med'
C
C       Combien de maillage
	call mmhnmh(fid,nmaa,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur lecture du nombre de maillage'
	   call efexit(-1)
	endif      
	print *,'Nombre de maillages : ',nmaa
C   
C       Lecture de toutes les mailles MED_POLYEDRE
C       dans chaque maillage
	do 10 i=1,nmaa
C
C          Info sur chaque maillage
	   call mmhmii(fid,i,maa,edim,mdim,type,desc,
     &	               dtunit,stype,nstep,atype,
     &		       nomcoo,unicoo,cret)
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur infos maillage'
	      call efexit(-1)
	   endif      
	   print *,'Maillage : ',maa
	   print *,'Dimension : ',mdim
C     
C          Combien de mailles polyedres a partir de la taille du tableau
C          d'indexation des faces en connectivite nodale
           call mmhnme(fid,maa,MED_NO_DT,MED_NO_IT,
     &                 MED_CELL,MED_POLYHEDRON,MED_INDEX_FACE,MED_NODAL,
     &                 chgt,tsf,nfindex,cret) 
	   npoly = nfindex - 1
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur lecture nombre de polyedre'
	      call efexit(-1)
	   endif      
	   print *,'Nombre de mailles MED_POLYEDRE : ',npoly
C
C          Taille des connectivites et du tableau d'indexation des faces
C          en connectivite nodale
           call mmhnme(fid,maa,MED_NO_DT,MED_NO_IT,
     &                 MED_CELL,MED_POLYHEDRON,
     &                 MED_INDEX_NODE,MED_NODAL,
     &                 chgt,tsf,taille,cret) 
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur infos sur les polyedres'
	      call efexit(-1)
	   endif      
	   print *,'Taille de la connectivite : ',taille
	   print *,'Taille du tableau indexf : ', nfindex
C
C          Lecture de la connectivite en mode nodal
	   call mmhphr(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,
     &                 MED_NODAL,indexp,indexf,conn,cret)
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur lecture connectivites polyedres'
	      call efexit(-1)
	   endif      
	   print *,'Lecture de la connectivite des polyedres'
	   print *,'Connectivite nodale'
C
C          Lecture de la connectivite en mode descendant
	   call mmhphr(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,
     &                 MED_DESCENDING,indexp2,indexf2,conn2,cret)
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur lecture connectivite des polyedres'
	      call efexit(-1)
	   endif      
	   print *,'Lecture de la connectivite des polyedres'
	   print *,'Connectivite descendante'
C
C          Lecture des noms
	   call mmhear(fid,maa,MED_NO_DT,MED_NO_IT,
     &                 MED_CELL,MED_POLYHEDRON,nom,cret)
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur lecture noms des polyedres'
	      call efexit(-1)
	   endif      
	   print *,'Lecture des noms'
C
C          Lecture des numeros
	   call mmhfnr(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,
     &                 MED_POLYHEDRON,num,cret)
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur lecture des numeros des polyedres'
	      call efexit(-1)
	   endif      
	   print *,'Lecture des numeros'
C
C          Lecture des numeros de familles
	   call mmhfnr(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,
     &                 MED_POLYHEDRON,fam,cret)
	   print *,cret
	   if (cret .ne. 0 ) then
	      print *,'Erreur lecture numeros de famille polyedres'
	      call efexit(-1)
	   endif      
	   print *,'Lecture des numeros de famille'
C
C          Affichage des resultats
	   print *,'Affichage des resultats'
	   do 20 j=1,npoly
C
	      print *,'>> Maille polyhedre ',j
	      print *,'---- Connectivite nodale    ---- : '
	      nfaces = indexp(j+1) - indexp(j)
C             ind1 = indice dans "indexf" pour acceder aux
C             numeros des faces 
	      ind1 = indexp(j)
	      do 30 k=1,nfaces
C                ind2 = indice dans "conn" pour acceder au premier noeud 
		 ind2 = indexf(ind1+k-1)
		 nnoeuds = indexf(ind1+k) - indexf(ind1+k-1)
		 print *,'   - Face ',k
		 do 40 l=1,nnoeuds
		    print *,'   ',conn(ind2+l-1)
 40		 continue
 30	      continue
	      print *,'---- Connectivite descendante ---- : '
	      nfaces = indexp2(j+1) - indexp2(j)
C             ind1 = indice dans "conn2" pour acceder aux faces
	      ind1 = indexp2(j)
	      do 50 k=1,nfaces
		 print *,'   - Face ',k
		 print *,'   => Numero : ',conn2(ind1+k-1)
		 print *,'   => Type   : ',indexf2(ind1+k-1)
 50	      continue
	      print *,'---- Nom                    ---- : ',nom(j)
	      print *,'---- Numero                 ----:  ',num(j)
	      print *,'---- Numero de famille      ---- : ',fam(j)
C       
 20	   continue
C
 10	continue
C
C       Fermeture du fichier
	call mficlo(fid,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur fermeture du fichier'
	   call efexit(-1)
	endif      
	print *,'Fermeture du fichier'
C
        end
