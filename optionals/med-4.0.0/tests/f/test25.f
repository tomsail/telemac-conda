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
C       * - Nom du fichier : test25.f
C       *
C       * - Description : ecriture de mailles MED_POLYEDRE dans un maillage MED
C       *
C       ******************************************************************************
	program test25
C       
	implicit none
	include 'med.hf'
C	
	integer*8 fid
	integer cret,mdim, sdim
	parameter  (mdim = 3, sdim = 3)
	character*64 maa 	
        integer n
	parameter (n=2)
C       Connectivite nodale
	integer np,nf
	parameter (nf=9,np=3)
	integer indexp(np),indexf(nf)
	integer conn(24)
C       Connectivite descendante
	integer np2,nf2
	parameter (nf2=8,np2=3)
	integer indexp2(np2),indexf2(nf2)
	integer conn2(nf2)
	character*16 nom(n)
	integer num(n),fam(n)
C       ** tables des noms et des unites des coordonnees    **
C          profil : (dimension)                            **
	character*16 nomcoo(3)
	character*16 unicoo(3)
C
	data indexp / 1,5,9 /
	data indexf / 1,4,7,10,13,16,19,22,25 /
	data conn / 1,2,3,4,5,6,7,8,9,10,11,12,13,14,
     &              15,16,17,18,19,20,21,22,23,24 /    
	data indexp2 / 1,5,9 /
	data indexf2 / MED_TRIA3,MED_TRIA3,MED_TRIA3,MED_TRIA3,
     &                 MED_TRIA3,MED_TRIA3,MED_TRIA3,MED_TRIA3 /
	data conn2 / 1,2,3,4,5,6,7,8 /
	data nom  / "poly1", "poly2"/ 
	data num  / 1,2 /, fam / 0,-1 /
	data maa /"maa1"/
	data nomcoo /"x","y","z"/, unicoo /"cm","cm","cm"/

C       ** Creation du fichier test25.med  **
	call mfiope(fid,'test25.med',MED_ACC_RDWR, cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du fichier'
	   call efexit(-1)
	endif      
	print *,'Creation du fichier test25.med'

C       ** Creation du maillage          **
	call mmhcre(fid,maa,mdim,sdim,
     &     MED_UNSTRUCTURED_MESH,'un maillage pour test 25', 
     &     "",MED_SORT_DTIT,MED_CARTESIAN,nomcoo,unicoo,cret)
	if (cret .ne. 0 ) then
	   print *,'Erreur creation du maillage'
	   call efexit(-1)
	endif      
	print *,cret
	print *,'Creation du maillage'

C       ** Ecriture des connectivites des mailles polyedres en mode nodal **
	call mmhphw(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,MED_CELL,
     &              MED_NODAL,np,indexp,nf,indexf,conn,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture connectivite des polyedres'
	   call efexit(-1)
	endif      
	print *,'Ecriture des connectivites des mailles
     & de type MED_POLYEDRE'
	print *,'Description nodale'

C       ** Ecriture des connectivites des mailles polyedres en mode descendant **
	call mmhphw(fid,maa,MED_NO_DT,MED_NO_IT,MED_UNDEF_DT,MED_CELL,
     &              MED_DESCENDING,np2,indexp2,nf2,indexf2,conn2,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture connectivite des polyedres'
	   call efexit(-1)
	endif      
	print *,'Ecriture des connectivites des mailles 
     & de type MED_POLYEDRE'
	print *,'Description descendante'

C       ** Ecriture des noms des mailles polyedres          **
	call mmheaw(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,
     &              MED_POLYHEDRON,n,nom,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture noms des polyedres'
	   call efexit(-1)
	endif      
	print *,'Ecriture des noms des polyedress'

C       ** Ecriture des numeros des mailles polyedres **
        call mmhenw(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,
     &              MED_POLYHEDRON,n,num,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture numeros des polyedres'
	   call efexit(-1)
	endif      
	print *,'Ecriture des numeros des polyedres'

C	** Ecriture des numeros des familles des segments  **
        call mmhfnw(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,
     &              MED_POLYHEDRON,n,fam,cret)	 
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur ecriture numeros de familles polyedres'
	   call efexit(-1)
	endif      
	print *,'Ecriture des numeros de familles des polyedres'

C       ** Fermeture du fichier                            **
	call mficlo(fid,cret)
	print *,cret
	if (cret .ne. 0 ) then
	   print *,'Erreur fermeture du fichier'
	   call efexit(-1)
	endif      
	print *,'Fermeture du fichier'
C
	end
