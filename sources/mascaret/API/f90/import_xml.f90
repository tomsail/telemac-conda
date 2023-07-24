!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET.
!
!   MASCARET is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>
!

! *********************************************************************
! PROGICIEL : MASCARET       Fabrice Zaoui
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************


!......................................................................
! Import d'un modele Mascaret en XML
! .....................................................................
subroutine import_xml(errorCode, Identifiant, NomFichier, importModele)

   use M_APIMASCARET_STATIC
   use M_MASCARET_T
   use M_MODELE_MASCARET_T

   implicit none

   integer, intent(out)                        :: errorCode        ! different de 0 si erreur
   integer, intent(in )                        :: Identifiant         ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   character(len=255), intent(in )             :: NomFichier          ! Nom du fichier XML  contenant le Modele Mascaret
   logical, intent(in)                         :: importModele        ! si vrai import du modele, sinon import de l'etat

   character(len=14) :: baliseModeleEtat
   character(len=7) :: nameType
   character(len=7) :: nameType2
   logical :: mascaretFile ! Le fichier est bien un fichier mascaret
   character(len=8192) :: line
   integer             :: unitNum
   character(len=64)   :: varName
   character(len=64)   :: varType
   integer             :: varDimension
   integer j

   mascaretFile = .false.

   if(importModele)then
       baliseModeleEtat = "MASCARET_Model"
       nameType = '<Model.'
       nameType2 = '</Model'
   else
       baliseModeleEtat = "MASCARET_State"
       nameType = '<State.'
       nameType2 = '</State'
   endif

   errorCode = TEST_INIT_AND_ID(Identifiant, 'IMPORT_XML')
   if (errorCode > 0 ) then
        RETURN
   end if

   unitNum = 123
   open(unit=unitNum, file=NomFichier, status="old", action="read", iostat=errorCode)
   if(errorCode.ne.0) then
       ptrMsgsErreurs(Identifiant) = 'IMPORT_XML - Unable to open the .xml file ' // NomFichier
       return
   endif

   read(unitNum, '(A)', iostat=errorCode) line
   read(unitNum, '(A)', iostat=errorCode) line

   if(index(line, baliseModeleEtat).eq.0) then
       if(importModele) then
           ptrMsgsErreurs(Identifiant) = 'IMPORT_XML - The file is not a Mascaret model'
       else
           ptrMsgsErreurs(Identifiant) = 'IMPORT_XML - The file is not a Mascaret state'
       endif
       errorCode = 2
       close(unitNum)
       return
   endif

   do
       read(unitNum, '(A)', iostat=errorCode) line
       if(errorCode > 0) then
           ptrMsgsErreurs(Identifiant) = 'IMPORT_XML - Unable to read from the .xml file ' // NomFichier
           errorCode = 2
           close(unitNum)
           return
       elseif(errorCode < 0) then
           exit
       else
           j = index(line, nameType)
           if(j.gt.0) then
               call tagxml(line(j:), varName, varType, varDimension)
               write(*,*) varName, varType, varDimension
               if(index(varType, 'INT').gt.0) then
                   call readInt(unitNum, identifiant, line(j:), varName, varDimension, nameType2)
               elseif(index(varType, 'DOUBLE').gt.0) then
                   call readDouble(unitNum, identifiant, line(j:), varName, varDimension, nameType2)
               elseif(index(varType, 'BOOL').gt.0) then
                   call readBool(unitNum, identifiant, line(j:), varName, varDimension, nameType2)
               elseif(index(varType, 'STRING').gt.0) then
                   call readString(unitNum, identifiant, line(j:), varName, varDimension, nameType2)
               endif
           endif
       endif
   end do

   if(errorCode < 0) errorCode = 0  ! EOF normal condition

   geometrieModifiee(Identifiant) = .false.

   close(unitNum)

contains

    subroutine xmeshGrid(d, ind)
        integer, intent(in)                                 :: d(3)
        integer, intent(inout), dimension(:,:), allocatable :: ind
        integer :: i, j, k, ii

        ii = 0
        do i = 1, d(1)
            if(d(2).eq.0) then
                ii = ii + 1
                ind(ii, 1) = i
            else
                do j = 1, d(2)
                    if(d(3).eq.0) then
                        ii = ii + 1
                        ind(ii, 1) = i
                        ind(ii, 2) = j
                    else
                        do k = 1, d(3)
                            ii = ii + 1
                            ind(ii, 1) = i
                            ind(ii, 2) = j
                            ind(ii, 3) = k
                        end do
                    endif
                end do
            endif
        end do
        return
    end subroutine xmeshGrid

    subroutine tagxml(line, varName, varType, varDimension)
        implicit none
        character(len=8192), intent(in) :: line
        character(len=64), intent(out)  :: varName
        character(len=64), intent(out)  :: varType
        integer, intent(out)            :: varDimension
        integer :: i, j
        character :: d

        ! Variable name
        i = scan(line, ' ')
        varName = line(2:i-1)

        ! Variable type
        i = index(line, 'type=')
        j = index(line, 'description=')
        if(j.eq.0) then
            j = index(line, 'dimension=')
        endif
        varType = line(i+6:j-3)

        ! Variable dimension
        j = index(line, 'dimension=')
        d = line(j+11:j+11)
        read(d, '(i1)') varDimension

    end subroutine tagxml

    subroutine readInt(unitNum, identifiant, line, varName, varDimension, nameType2)
        implicit none
        integer, intent(in)                :: unitNum
        integer, intent(in )               :: identifiant
        character(len=8192), intent(inout) :: line
        character(len=64), intent(in)      :: varName
        integer, intent(in)                :: varDimension
        character(len=7), intent(in)       :: nameType2
        integer :: valint
        integer :: i, j, ii, size, prodsize, d(3)
        integer, dimension(:,:), allocatable :: ind
        integer :: errorCode

        if(varDimension.eq.0) then
            i = index(line, 'dimension=')
            j = i+14
            i = index(line, nameType2)
            read(line(j:i-1), '(i6)') valint
            call set_int_mascaret(errorCode, identifiant, trim(varName), &
              0, 0, 0, valint)
        else
            prodsize = 1
            d(1:3) = 0
            do ii = 1, varDimension
                read(unitNum, '(A)', iostat=errorCode) line
                i = index(line, 'taille=')
                j = index(line, '">')
                read(line(i+8:j-1), '(i6)') size
                prodsize = prodsize * size
                d(ii) = size
            end do
            if(prodsize.gt.0) then
                allocate(ind(prodsize, 3))
                ind(:,:) = 0
                call xmeshGrid(d, ind)
                ii = 0
                do
                    read(unitNum, '(A)', iostat=errorCode) line
                    i = index(line, '<v>')
                    if(i.ne.0) then
                        j = index(line, '</v>')
                        read(line(i+3:j-1), '(i6)') valint
                        ii = ii + 1
                        call set_int_mascaret(errorCode, identifiant, trim(varName), &
                          ind(ii,1), ind(ii,2), ind(ii,3), valint)
                        if(ii.eq.prodsize) exit
                    endif
                end do
                deallocate(ind)
            endif
        endif
    end subroutine readInt

    subroutine readDouble(unitNum, identifiant, line, varName, varDimension, nameType2)
        implicit none
        integer, intent(in)                :: unitNum
        integer, intent(in )               :: identifiant
        character(len=8192), intent(inout) :: line
        character(len=64), intent(in)      :: varName
        integer, intent(in)                :: varDimension
        character(len=7), intent(in)       :: nameType2
        double precision :: valdouble
        integer :: i, j, ii, size, prodsize, d(3)
        integer, dimension(:,:), allocatable :: ind
        integer :: errorCode

        if(varDimension.eq.0) then
            i = index(line, 'dimension=')
            j = i+14
            i = index(line, nameType2)
            read(line(j:i-1), '(g32.4)') valdouble
            call set_double_mascaret(errorCode, identifiant, trim(varName), &
              0, 0, 0, valdouble)
        else
            prodsize = 1
            d(1:3) = 0
            do ii = 1, varDimension
                read(unitNum, '(A)', iostat=errorCode) line
                i = index(line, 'taille=')
                j = index(line, '">')
                read(line(i+8:j-1), '(i6)') size
                prodsize = prodsize * size
                d(ii) = size
            end do
            if(prodsize.gt.0) then
                allocate(ind(prodsize, 3))
                ind(:,:) = 0
                call xmeshGrid(d, ind)
                ii = 0
                do
                    read(unitNum, '(A)', iostat=errorCode) line
                    i = index(line, '<v>')
                    if(i.ne.0) then
                        j = index(line, '</v>')
                        read(line(i+3:j-1), '(g32.4)') valdouble
                        ii = ii + 1
                        call set_double_mascaret(errorCode, identifiant, trim(varName), &
                          ind(ii,1), ind(ii,2), ind(ii,3), valdouble)
                        if(ii.eq.prodsize) exit
                    endif
                end do
                deallocate(ind)
            endif
        endif
    end subroutine readDouble

    subroutine readBool(unitNum, identifiant, line, varName, varDimension, nameType2)
        implicit none
        integer, intent(in)                :: unitNum
        integer, intent(in )               :: identifiant
        character(len=8192), intent(inout) :: line
        character(len=64), intent(in)      :: varName
        integer, intent(in)                :: varDimension
        character(len=7), intent(in)       :: nameType2
        logical :: valbool
        character(len=4) :: frenchVal
        integer :: i, j, ii, size, prodsize, d(3)
        integer, dimension(:,:), allocatable :: ind
        integer :: errorCode

        if(varDimension.eq.0) then
            i = index(line, 'dimension=')
            j = i+14
            i = index(line, nameType2)
            frenchVal = line(j:i-1)
            if(index(frenchVal, 'VRAI').ne.0) then
                valbool = .true.
            else
                valbool = .false.
            endif
            call set_bool_mascaret(errorCode, identifiant, trim(varName), &
              0, 0, 0, valbool)
        else
            prodsize = 1
            d(1:3) = 0
            do ii = 1, varDimension
                read(unitNum, '(A)', iostat=errorCode) line
                i = index(line, 'taille=')
                j = index(line, '">')
                read(line(i+8:j-1), '(i6)') size
                prodsize = prodsize * size
                d(ii) = size
            end do
            if(prodsize.gt.0) then
                allocate(ind(prodsize, 3))
                ind(:,:) = 0
                call xmeshGrid(d, ind)
                ii = 0
                do
                    read(unitNum, '(A)', iostat=errorCode) line
                    i = index(line, '<v>')
                    if(i.ne.0) then
                        j = index(line, '</v>')
                        frenchVal = line(i+3:j-1)
                        if(index(frenchVal, 'VRAI').ne.0) then
                            valbool = .true.
                        else
                            valbool = .false.
                        endif
                        ii = ii + 1
                        call set_bool_mascaret(errorCode, identifiant, trim(varName), &
                          ind(ii,1), ind(ii,2), ind(ii,3), valbool)
                        if(ii.eq.prodsize) exit
                    endif
                end do
                deallocate(ind)
            endif
        endif
    end subroutine readBool

    subroutine readString(unitNum, identifiant, line, varName, varDimension, nameType2)
        implicit none
        integer, intent(in)                :: unitNum
        integer, intent(in )               :: identifiant
        character(len=8192), intent(inout) :: line
        character(len=64), intent(in)      :: varName
        integer, intent(in)                :: varDimension
        character(len=7), intent(in)       :: nameType2
        character(len=128) :: valstring
        integer :: i, j, ii, size, prodsize, d(3)
        integer, dimension(:,:), allocatable :: ind
        integer :: errorCode

        if(varDimension.eq.0) then
            i = index(line, 'dimension=')
            j = i+14
            i = index(line, nameType2)
            valstring = line(j:i-1)
            call set_string_mascaret(errorCode, identifiant, trim(varName), &
                0, 0, 0, trim(valstring))
        else
            prodsize = 1
            d(1:3) = 0
            do ii = 1, varDimension
                read(unitNum, '(A)', iostat=errorCode) line
                i = index(line, 'taille=')
                j = index(line, '">')
                read(line(i+8:j-1), '(i6)') size
                prodsize = prodsize * size
                d(ii) = size
            end do
            if(prodsize.gt.0) then
                allocate(ind(prodsize, 3))
                ind(:,:) = 0
                call xmeshGrid(d, ind)
                ii = 0
                do
                    read(unitNum, '(A)', iostat=errorCode) line
                    i = index(line, '<v>')
                    if(i.ne.0) then
                        j = index(line, '</v>')
                        valstring = line(i+3:j-1)
                        ii = ii + 1
                        call set_string_mascaret(errorCode, identifiant, trim(varName), &
                            ind(ii,1), ind(ii,2), ind(ii,3), trim(valstring))
                        if(ii.eq.prodsize) exit
                    endif
                end do
                deallocate(ind)
            endif
        endif
    end subroutine readString
end subroutine import_xml
