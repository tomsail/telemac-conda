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

subroutine dgtsl( &
                  n , &
                  c , &
                  d , &
                  e , &
                  b , &
               info )

!*****************************************************************************80
!
!! DGTSL solves a general tridiagonal linear system.
!
!  Modified:
!
!    17 May 2005
!
!  Author:
!
!    FORTRAN90 translation by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!  Parameters:
!
!    Input, integer N, the order of the tridiagonal matrix.
!
!    Input/output, real ( kind = 8 ) C(N), contains the subdiagonal of the
!    tridiagonal matrix in entries C(2:N).  On output, C is destroyed.
!
!    Input/output, real ( kind = 8 ) D(N).  On input, the diagonal of the
!    matrix.  On output, D is destroyed.
!
!    Input/output, real ( kind = 8 ) E(N), contains the superdiagonal of the
!    tridiagonal matrix in entries E(1:N-1).  On output E is destroyed.
!
!    Input/output, real ( kind = 8 ) B(N).  On input, the right hand side.
!    On output, the solution.
!
!    Output, integer INFO, error flag.
!    0, normal value.
!    K, the K-th element of the diagonal becomes exactly zero.  The 
!       subroutine returns if this error condition is detected.
!
   use M_PRECISION
   use M_PARAMETRE_C
   implicit none

   integer n
   real (double ) b(n)
   real (double ) c(n)
   real (double ) d(n)
   real (double ) e(n)
   real (double ) t
   !  real ( kind = 8 ) b(n)
   !  real ( kind = 8 ) c(n)
   !  real ( kind = 8 ) d(n)
   !  real ( kind = 8 ) e(n)
   integer info
   integer k
   !  real ( kind = 8 ) t

   !print*,'b'
   !print*,(b(k),k=1,n)
   !print*,'c'
   !print*,(c(k),k=1,n)
   !print*,'d'
   !print*,(d(k),k=1,n)
   !print*,'e'
   !print*,(e(k),k=1,n)

   info = 0
   c(1) = d(1)

   if( 2 <= n ) then

      d(1) = e(1)
      e(1) = 0.0D+00
      e(n) = 0.0D+00

      do k = 1 , n - 1
         !
         !  Find the larger of the two rows.
         !
         if ( abs ( c(k) ) <= abs ( c(k+1) ) ) then
            !
            !  Interchange rows.
            !
            t      = c(k+1)
            c(k+1) = c(k)
            c(k)   = t

            t      = d(k+1)
            d(k+1) = d(k)
            d(k)   = t

            t      = e(k+1)
            e(k+1) = e(k)
            e(k)   = t

            t      = b(k+1)
            b(k+1) = b(k)
            b(k)   = t

         end if

         !
         !  Zero elements.
         !
         if( abs(c(k)).lt.EPS15 ) then
            info = k
            return
         end if

         t = -c(k+1) / c(k)
         c(k+1) = d(k+1) + t * d(k)
         d(k+1) = e(k+1) + t * e(k)
         e(k+1) = 0.0D+00
         b(k+1) = b(k+1) + t * b(k)

      end do

   end if

   if ( abs(c(n)).lt.EPS15 ) then
      info = n
      return
   end if

   !
   !  Back solve.
   !
   b(n) = b(n) / c(n)

   if( 1 < n ) then

      b(n-1) = ( b(n-1) - d(n-1) * b(n) ) / c(n-1)

      do k = n-2, 1, -1
         b(k) = ( b(k) - d(k) * b(k+1) - e(k) * b(k+2) ) / c(k)
      end do

   end if

   return

end subroutine dgtsl
