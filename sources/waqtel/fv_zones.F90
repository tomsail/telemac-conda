!###############################################################################
!#                                                                             #
!# fv_zones.F90                                                                #
!#                                                                             #
!# Interface for FV (Finite Volume) Model to AED2 modules.                     #
!#   Designed for TUFLOW-FV, released by BMT-WBM:                              #
!#   http://www.tuflow.com/Tuflow%20FV.aspx                                    #
!#                                                                             #
!# This is a support module to allow ability for benthic/sediment zones in     #
!# AED2, including zone-averaging                                              #
!#                                                                             #
!#   -----------------------------------------------------------------------   #
!#                                                                             #
!# Developed by :                                                              #
!#     AquaticEcoDynamics (AED) Group                                          #
!#     School of Earth & Environment                                           #
!# (C) The University of Western Australia                                     #
!#                                                                             #
!# Copyright by the AED-team @ UWA under the GNU Public License - www.gnu.org  #
!#                                                                             #
!#   -----------------------------------------------------------------------   #
!#                                                                             #
!# Created Apr 2015                                                            #
!#                                                                             #
!###############################################################################

#if defined HAVE_AED2
#include "aed2.h"
!#include "aed2_common.h"
#endif

#ifndef DEBUG
#define DEBUG      0
#endif

!###############################################################################
MODULE fv_zones
!-------------------------------------------------------------------------------
#if defined HAVE_AED2
   USE aed2_common
   USE DECLARATIONS_SPECIAL

   IMPLICIT NONE

   PUBLIC init_zones, calc_zone_areas, copy_to_zone, copy_from_zone
   PUBLIC compute_zone_benthic_fluxes !, distribute_flux_from_zone
   PUBLIC zone

   !#--------------------------------------------------------------------------#
   !# Module Data

   !# Arrays for environmental variables not supplied externally.
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone
   AED_REAL,DIMENSION(:,:),ALLOCATABLE,TARGET :: zone_cc
   AED_REAL,DIMENSION(:,:),ALLOCATABLE,TARGET :: zone_cc_diag
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_area
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_temp
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_salt
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_rho
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_height
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_extc
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_tss
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_par
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_wind
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_rain ! JC not sure it makes sense
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_I_0
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_taub
   INTEGER, DIMENSION(:),  ALLOCATABLE        :: zone_count, zm

   AED_REAL,DIMENSION(:,:),ALLOCATABLE,TARGET :: flux_pelz
   AED_REAL,DIMENSION(:,:),ALLOCATABLE,TARGET :: flux_benz

   INTEGER :: nZones, nwq_var, nben_var

!#####################################################

CONTAINS
!===============================================================================


!###############################################################################
SUBROUTINE init_zones(nCols, mat_id, n_aed2_vars, n_vars, n_vars_ben)
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in) :: nCols
   INTEGER,DIMENSION(:,:),INTENT(in) :: mat_id
   INTEGER,INTENT(in) :: n_aed2_vars, n_vars, n_vars_ben
!
!LOCALS
   INTEGER :: cType, nTypes
   INTEGER :: col, zon
   INTEGER,DIMENSION(:),ALLOCATABLE :: mat_t
!
!-------------------------------------------------------------------------------
!BEGIN
   ALLOCATE(mat_t(nCols)) ; ALLOCATE(zm(nCols))
   !# The new form of zones
   cType = mat_id(1,1) ; nTypes = 1 ; mat_t(nTypes) = mat_id(1,1)
   DO col=1, ubound(mat_id,2)
      !# use the bottom index to fill the array
      IF ( cType /= mat_id(1,col) ) THEN
         DO zon=1,nTypes
            IF ( mat_t(zon) .eq. mat_id(1,col) ) THEN
               cType = mat_id(1,col)
               EXIT
            ENDIF
         ENDDO
      ENDIF
      IF ( cType /= mat_id(1,col) ) THEN
         nTypes = nTypes + 1
         mat_t(nTypes) = mat_id(1,col)
         cType = mat_id(1,col)
         zon = nTypes
      ENDIF
      zm(col) = zon
   ENDDO
   WRITE(LU, *) "Number of mats = ", nTypes, " = ", mat_t(1:nTypes)
   DEALLOCATE(mat_t)
   nZones = nTypes

   ALLOCATE(zone(nZones))
   ALLOCATE(zone_area(nZones))
   ALLOCATE(zone_temp(nZones))
   ALLOCATE(zone_salt(nZones))
   ALLOCATE(zone_rho(nZones))
   ALLOCATE(zone_height(nZones))

   ALLOCATE(zone_extc(nZones))
   ALLOCATE(zone_tss(nZones))
   ALLOCATE(zone_par(nZones))
   ALLOCATE(zone_wind(nZones))
   ALLOCATE(zone_rain(nZones)) ! JC
   ALLOCATE(zone_I_0(nZones))
   ALLOCATE(zone_taub(nZones))

   ALLOCATE(zone_count(nZones))

   ALLOCATE(zone_cc(n_vars, nZones))
   ALLOCATE(zone_cc_diag(n_aed2_vars-n_vars, nZones))

   ALLOCATE(flux_pelz(n_vars, nZones))
   ALLOCATE(flux_benz(n_vars, nZones))

   nwq_var = n_vars
   nben_var = n_vars_ben
END SUBROUTINE init_zones
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE calc_zone_areas(nCols, temp, salt, h, area, wnd, rho, extcoeff, I_0, par, tss, active, rain)!, bathy ,rain JC
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in) :: nCols
   AED_REAL,DIMENSION(:),INTENT(in) :: temp, salt, h, area, wnd, rain
   AED_REAL,DIMENSION(:),INTENT(in) :: rho, extcoeff, I_0, par, tss
   LOGICAL,DIMENSION(:), INTENT(in) :: active
!
!LOCALS
   INTEGER :: col, zon
!
!-------------------------------------------------------------------------------
!BEGIN
   zone_area = zero_
   zone_temp = zero_
   zone_salt = zero_
   zone_height = zero_
   zone_wind = zero_
   zone_rain = zero_ ! JC
   zone_count = 0

   DO col=1, nCols
      IF (.NOT. active(col)) CYCLE

      zon = zm(col)
      zone_area(zon)   = zone_area(zon) + area(col)
      zone_temp(zon)   = zone_temp(zon) + temp(col)
      zone_salt(zon)   = zone_salt(zon) + salt(col)
      zone_rho(zon)    = zone_rho(zon) + rho(col)
      zone_height(zon) = zone_height(zon) + h(col)
      zone_extc(zon)   = zone_extc(zon) + extcoeff(col)
      zone_tss(zon)    = zone_tss(zon) + tss(col)
      zone_par(zon)    = zone_par(zon) + par(col)
      zone_wind(zon)   = zone_wind(zon) + wnd(col)
      zone_rain(zon)   = zone_rain(zon) + rain(col) !JC
      zone_I_0(zon)    = zone_I_0(zon) + I_0(col)
    ! zone_taub(zon)   = zone_taub(zon) + col_taub

      zone_count(zon)  = zone_count(zon) + 1
   ENDDO

   zone_temp = zone_temp / zone_count
   zone_salt = zone_salt / zone_count

   zone_rho  =  zone_rho / zone_count
   zone_extc = zone_extc / zone_count
   zone_tss  =  zone_tss / zone_count
   zone_par  =  zone_par / zone_count
   zone_wind = zone_wind / zone_count
   zone_rain = zone_rain / zone_count ! JC
   zone_I_0  =  zone_I_0 / zone_count
   zone_taub = zone_taub / zone_count

END SUBROUTINE calc_zone_areas
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE copy_to_zone(nCols, cc, area, active, benth_map)
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in)  :: nCols
   AED_REAL,INTENT(in) :: cc(:,:)       !# (n_vars, n_layers)
   AED_REAL,DIMENSION(:),INTENT(in) :: area
   LOGICAL,DIMENSION(:), INTENT(in) :: active
   INTEGER,DIMENSION(:), INTENT(in) :: benth_map
!
!LOCALS
   INTEGER :: col, zon, bot
!
!-------------------------------------------------------------------------------
!BEGIN
   DO col=1, nCols
      IF (.NOT. active(col)) CYCLE

      bot = benth_map(col)
      zon = zm(col)

      zone_cc(1:nwq_var,zon) = zone_cc(1:nwq_var,zon) + (cc(1:nwq_var,bot) * area(col) / zone_area(zon))
   ENDDO
END SUBROUTINE copy_to_zone
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE copy_from_zone(nCols, cc, area, active, benth_map)
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in)     :: nCols
   AED_REAL,INTENT(inout) :: cc(:,:)       !# (n_vars, n_layers)
   AED_REAL,DIMENSION(:),INTENT(in) :: area
   LOGICAL,DIMENSION(:), INTENT(in) :: active
   INTEGER,DIMENSION(:), INTENT(in) :: benth_map
!
!LOCALS
   INTEGER :: col, zon, bot
!
!-------------------------------------------------------------------------------
!BEGIN
   DO col=1, nCols
      IF (.NOT. active(col)) CYCLE

      bot = benth_map(col)
      zon = zm(col)

      cc(nwq_var+1:nwq_var+nben_var,bot) = zone_cc(nwq_var+1:nwq_var+nben_var,zon)
   ENDDO
END SUBROUTINE copy_from_zone
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE STOPIT(message)
!-------------------------------------------------------------------------------
!ARGUMENTS
   CHARACTER(*) :: message
!-------------------------------------------------------------------------------
   PRINT *,message
   STOP
END SUBROUTINE STOPIT
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE define_column_zone(column, zon, n_aed2_vars)
!-------------------------------------------------------------------------------
!ARGUMENTS
   TYPE (aed2_column_t), INTENT(inout) :: column(:)
   INTEGER, INTENT(in) :: zon, n_aed2_vars
!
!LOCALS
   INTEGER :: av
   INTEGER :: v, d, sv, sd, ev
   TYPE(aed2_variable_t),POINTER :: tvar
!
!-------------------------------------------------------------------------------
!BEGIN
   v = 0 ; d = 0; sv = 0; sd = 0 ; ev = 0
   DO av=1,n_aed2_vars

      IF ( .NOT. aed2_get_var(av, tvar) ) STOP "Error getting variable info"

      IF ( tvar%extern ) THEN !# global variable
         ev = ev + 1
         SELECT CASE (tvar%name)
            CASE ( 'temperature' ) ; column(av)%cell => zone_temp
            CASE ( 'salinity' )    ; column(av)%cell => zone_salt
            CASE ( 'density' )     ; column(av)%cell => zone_rho
            CASE ( 'layer_ht' )    ; column(av)%cell => zone_height
            CASE ( 'layer_area' )  ; column(av)%cell_sheet => zone_area(zon)
            CASE ( 'rain' )        ; column(av)%cell_sheet => zone_rain(zon) ! JC
       !    CASE ( 'rainloss' )    ; column(av)%cell_sheet => zone_rainloss(zon) ! JC
            CASE ( 'material' )    ; column(av)%cell_sheet => zone(zon)
       !    CASE ( 'bathy' )       ; column(av)%cell_sheet => zone_bathy(zon)! Does it mean this var is already pointed to the correct variable of TFFV? JC
            CASE ( 'extc_coef' )   ; column(av)%cell => zone_extc
            CASE ( 'tss' )         ; column(av)%cell => zone_tss
       !    CASE ( 'par' )         ; column(av)%cell => zone_par
       !    CASE ( 'par' )         ; IF (link_ext_par) THEN
       !                                column(av)%cell => lpar(top:bot)
       !                             ELSE
       !                                column(av)%cell => par(top:bot)
       !                             ENDIF
       !    CASE ( 'nir' )         ; column(av)%cell => zone_nir
       !    CASE ( 'uva' )         ; column(av)%cell => zone_uva
       !    CASE ( 'uvb' )         ; column(av)%cell => zone_uvb
            CASE ( 'sed_zone' )    ; column(av)%cell_sheet => zone(zon)
            CASE ( 'wind_speed' )  ; column(av)%cell_sheet => zone_wind(zon)
            CASE ( 'par_sf' )      ; column(av)%cell_sheet => zone_I_0(zon)
       !    CASE ( 'taub' )        ; column(av)%cell_sheet => zone_taub
       !    CASE ( 'air_temp' )    ; column(av)%cell_sheet => zone_air_temp(zon)
            CASE DEFAULT ; CALL STOPIT("ERROR: external variable "//trim(tvar%name)//" not found.")
         END SELECT
      ELSEIF ( tvar%diag ) THEN  !# Diagnostic variable
         d = d + 1
         IF ( tvar%sheet ) THEN
            column(av)%cell_sheet => zone_cc_diag(d,zon)
         ELSE
            column(av)%cell => zone_cc_diag(d,zon:zon)
         ENDIF
      ELSE    !# state variable
         IF ( tvar%sheet ) THEN
            sv = sv + 1
            IF ( tvar%bot ) THEN
               column(av)%cell_sheet => zone_cc(nwq_var+sv, zon)
            ELSEIF ( tvar%top ) THEN
    !          column(av)%cell_sheet => zone_cc(nwq_var+sv, top)
            ENDIF
            column(av)%flux_ben => flux_benz(nwq_var+sv, zon)
    !       column(av)%flux_atm => flux_atm(nwq_var+sv)
         ELSE
            v = v + 1
            column(av)%cell => zone_cc(v, 1:1)
            column(av)%flux_pel => flux_pelz(v, 1:1)
            column(av)%flux_ben => flux_benz(v, zon)
    !       column(av)%flux_atm => flux_atm(v)
         ENDIF
      ENDIF
   ENDDO
END SUBROUTINE define_column_zone
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE compute_zone_benthic_fluxes(n_aed2_vars, dt)
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in) :: n_aed2_vars
   AED_REAL,INTENT(in):: dt
!
!LOCALS
   INTEGER :: zon, v
   TYPE (aed2_column_t) :: column(n_aed2_vars)
!
!-------------------------------------------------------------------------------
!BEGIN
   DO zon=1, nZones
      CALL define_column_zone(column, zon, n_aed2_vars)!, nwq_var)

      CALL aed2_calculate_benthic(column, 1)
      DO v=nwq_var+1,nwq_var+nben_var
         zone_cc(v, 1) = zone_cc(v, 1) + dt*flux_benz(v, zon);
      ENDDO
   ENDDO
END SUBROUTINE compute_zone_benthic_fluxes
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#endif
!===============================================================================
END MODULE fv_zones
