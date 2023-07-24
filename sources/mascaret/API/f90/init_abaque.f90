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
! PROGICIEL : MASCARET       J.-M. LACOMBE
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************
  !.................................................................................................................................
  ! Initialise le tableau des abaques pour eviter de lire un fichier
  ! .................................................................................................................................
  subroutine INIT_ABAQUE(Abaque)
    implicit none
    real(8)    , dimension(6,6,5) , intent(inout) :: Abaque

    ! Abaque 1a
    Abaque(1,1,:) = (/1.038, 0.916, 0.824, 0.717, 0.595/)
    Abaque(1,2,:) = (/2.430, 1.710, 1.374, 1.000, 0.687/)
    Abaque(1,3,:) = (/3.450, 2.321, 1.740, 1.190, 0.748/)
    Abaque(1,4,:) = (/4.336, 2.809, 2.076, 1.374, 0.794/)
    Abaque(1,5,:) = (/5.069, 3.236, 2.351, 1.511, 0.855/)
    Abaque(1,6,:) = (/5.679, 3.603, 2.595, 1.618, 0.916/)

    ! Abaque 1b
    Abaque(2,1,:) = (/3.000, 2.192, 1.650, 1.158, 0.739/)
    Abaque(2,2,:) = (/3.360, 2.360, 1.773, 1.207, 0.739/)
    Abaque(2,3,:) = (/3.768, 2.537, 1.872, 1.256, 0.764/)
    Abaque(2,4,:) = (/4.089, 2.709, 1.970, 1.305, 0.764/)
    Abaque(2,5,:) = (/4.483, 2.857, 2.067, 1.330, 0.764/)
    Abaque(2,6,:) = (/4.852, 3.030, 2.192, 1.379, 0.764/)

    ! Abaque 1c
    Abaque(3,1,:) = (/1.551, 0.837, 0.653, 0.408, 0.204/)
    Abaque(3,2,:) = (/1.800, 1.080, 0.735, 0.449, 0.225/)
    Abaque(3,3,:) = (/2.060, 1.204, 0.816, 0.469, 0.245/)
    Abaque(3,4,:) = (/2.367, 1.357, 0.918, 0.531, 0.265/)
    Abaque(3,5,:) = (/2.714, 1.531, 1.020, 0.571, 0.265/)
    Abaque(3,6,:) = (/2.939, 1.735, 1.143, 0.633, 0.286/)

    ! Abaque 2a
    Abaque(4,1,:) = (/0.0361,0.089, 0.180, 0.483, 1.630/)
    Abaque(4,2,:) = (/0.0505,0.115, 0.224, 0.563, 1.770/)
    Abaque(4,3,:) = (/0.058, 0.144, 0.267, 0.649, 1.900/)
    Abaque(4,4,:) = (/0.072, 0.173, 0.310, 0.715, 2.040/)
    Abaque(4,5,:) = (/0.087, 0.198, 0.349, 0.767, 2.170/)
    Abaque(4,6,:) = (/0.110, 0.224, 0.389, 0.858, 2.310/)

    ! Abaque 2b
    Abaque(5,1,:) = (/1.680, 2.540, 3.480, 5.350, 9.210/)
    Abaque(5,2,:) = (/1.760, 2.700, 3.670, 5.610, 9.660/)
    Abaque(5,3,:) = (/1.870, 2.880, 3.890, 5.910,10.120/)
    Abaque(5,4,:) = (/1.980, 3.030, 4.090, 6.200,10.590/)
    Abaque(5,5,:) = (/2.100, 3.200, 4.280, 6.510,11.040/)
    Abaque(5,6,:) = (/2.210, 3.330, 4.480, 6.770,11.510/)

    ! Abaque 2c
    Abaque(6,1,:) = (/0.450, 0.760, 1.100, 1.800, 3.370/)
    Abaque(6,2,:) = (/0.530, 0.860, 1.230, 1.990, 1.080/)
    Abaque(6,3,:) = (/0.600, 0.970, 1.360, 2.190, 1.080/)
    Abaque(6,4,:) = (/0.670, 1.070, 1.490, 2.400, 4.350/)
    Abaque(6,5,:) = (/0.740, 1.170, 1.620, 2.600, 4.680/)
    Abaque(6,6,:) = (/0.800, 1.260, 1.760, 2.790, 5.010/)

  end subroutine INIT_ABAQUE
