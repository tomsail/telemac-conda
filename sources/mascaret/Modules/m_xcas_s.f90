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
module m_xcas_s
!
!  Mascaret Software
!
!  Author(s): Fabrice Zaoui
!
!  Comments: A reader for Mascaret .xcas file
!     Please do not use for anything else (not a xml parser)
!
! VERSION : V8P4R0              EDF-CEREMA
!
  implicit none

    contains
        subroutine splitString(pathNode, Nodes)
            character(len=256), intent(in) :: pathNode
            character(len=32), intent(out), dimension(5) :: Nodes
            character(len=256) :: rpathNode
            integer :: i

            ! Initialization
            rpathNode = pathNode
            Nodes(:) = ''

            ! Node #1
            i = index(rpathNode, '/')
            if(i.ne.0) then
                Nodes(1) = rpathNode(1:i-1)
                rpathNode = rpathNode(i+1:)
                ! Node #2
                i = index(rpathNode, '/')
                if(i.ne.0) then
                    Nodes(2) = rpathNode(1:i-1)
                    rpathNode = rpathNode(i+1:)
                    ! Node #3
                    i = index(rpathNode, '/')
                    if(i.ne.0) then
                        Nodes(3) = rpathNode(1:i-1)
                        rpathNode = rpathNode(i+1:)
                        ! Node #4
                        i = index(rpathNode, '/')
                        if(i.ne.0) then
                            Nodes(4) = rpathNode(1:i-1)
                            rpathNode = rpathNode(i+1:)
                            ! Node #5
                            i = index(rpathNode, '/')
                            if(i.ne.0) then
                                Nodes(5) = rpathNode(1:i-1)
                                rpathNode = rpathNode(i+1:)
                            else
                                Nodes(5) = rpathNode
                            endif
                        else
                            Nodes(4) = rpathNode
                        endif
                    else
                        Nodes(3) = rpathNode
                    endif
                else
                    Nodes(2) = rpathNode
                endif
            else
                Nodes(1) = rpathNode
            endif
            return
        end subroutine splitString

        function xcasReader(unitNum, pathNode, move)

            implicit none
            integer, intent(in) :: unitNum
            character(len=256), intent(in) :: pathNode
            integer, optional :: move
            character(len=32), dimension(5) :: Nodes
            character(len = 8192) :: xcasReader
            character(len=8192) :: line
            integer             :: i, j
            integer             :: errorCode
            integer             :: jump
            logical             :: goback

            ! Initialization
            goback = .false.
            if(present(move)) then
              if(move.eq.2) then
                goback = .true.
                jump = 0
              else
                jump = move
              endif
            else
              jump = -1
            endif

            if(jump.eq.-1) rewind(unitNum)

            ! Nodes
            call splitString(pathNode, Nodes)

            ! Search
            j = 0
            iloop: do i = 1, 5
                if(len(trim(Nodes(i))) > 0) then  ! avoid direct comparison
                    loop: do
                        read(unitNum, '(A)', iostat=errorCode) line
                        j = j + 1
                        if(errorCode > 0) then
                            write(*,*) 'Unable to read the Mascaret .xcas file'
                            stop
                        elseif(errorCode < 0) then
                            exit iloop
                        else
                            if(index(line, trim(Nodes(i))).gt.0) then
                              if(jump.le.0) then
                                exit
                              else
                                jump = 0
                              endif
                            endif
                        endif
                    end do loop
                endif
            end do iloop

            ! Get results
            if(errorCode.eq.0) then
              if(index(line,'</').gt.0) then
                i = index(line, '>')
                line = line(i+1:)
                i = index(line, '<')
                xcasReader = line(1:i-1)
              else
                xcasReader = line
              endif
            elseif(errorCode.lt.0) then
              xcasReader = ''
            endif

            ! Re-positioning
            if(goback.eqv..true.) then
               do i = 1, j
                 backspace(unitNum)
               end do
            endif

        end function xcasReader
end module m_xcas_s
