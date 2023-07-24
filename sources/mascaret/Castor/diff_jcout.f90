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

SUBROUTINE DIFF_JCOUT(grmin, grmaj, z, q, qinjec, &
& pcsing, zinit, dk, z_cal, ic)
!
! *********************************************************************
! PROGICIEL : MASCARET         F. DEMANGEON
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!
! FONCTION :
! --------
!
! CALCUL DU GRADIENT DE LA FONCTION DE COUT
!
! *********************************************************************

  USE M_PRECISION
  USE M_PARAMETRE_C
  USE M_MESSAGE_C
  USE M_NUM_BIEF_S
  USE M_FROUDE_S
  USE M_TRAITER_ERREUR_I
  USE M_STRICK_I
  USE M_PERMAT_I
  USE M_XINDIC_S
  USE M_SHARE_VAR
  USE M_DIFF_Z_CF12_FWD_SARAP_VAR
  IMPLICIT NONE

!
!.. Arguments ..
!---------------
!
! TABLEAU  DIMENSIONNE  A NbSect
!
  DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: z
!
! TABLEAU  DIMENSIONNE  A NbSect
!
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: q
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qinjec
  DOUBLE PRECISION, INTENT(INOUT) :: zinit
! TABLEAUX DIMENSIONNES A Nbsect
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: pcsing
!
! VARIABLES CASTOR
!
  INTEGER, INTENT(IN) :: ic
! gradient dimensionne au nombre de zones de frottemenr
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: grmin, grmaj
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: z_cal
  DOUBLE PRECISION, INTENT(INOUT) :: dk
!
! VARIABLES LOCALES
!
  INTEGER :: l, isec, iext, num_bief, k
  LOGICAL :: limite_libre
  DOUBLE PRECISION, DIMENSION(:), POINTER :: zcout
  DOUBLE PRECISION, DIMENSION(:), POINTER :: zdtemp
  INTEGER :: j
  INTEGER :: r
  INTEGER :: indice
  INTEGER :: indice2
  INTRINSIC SIZE
!
! INITIALISATIONS
! ---------------
  ALLOCATE(zdtemp(nb_mes(ic)))
  ALLOCATE(zcout(nb_mes(ic)))

  q(1) = extremite(1)%ptq(1)
  DO isec=connect%originebief(1)+1,connect%finbief(1)
    q(isec) = q(isec-1) + qinjec(isec)
  END DO
! On cherche l'extremite libre qui correspond
! a la section Connect%FinBief(noeud_bief)
  limite_libre = .false.
  DO iext=1,SIZE(connect%numsectionextlibre(:))
    IF (connect%numsectionextlibre(iext) .EQ. connect%finbief(1)) THEN
! iext : NUMERO DE LA LIMITE LIBRE CORRESPONDANTE
      limite_libre = .true.
      GOTO 100
    END IF
  END DO
 100 IF (limite_libre) THEN
! --  ON PART D'UNE LIMITE LIBRE --
    zinit = extremite(iext)%ptz(1)
  ELSE
! --  ON PART D'UN NOEUD --
    zinit = z(connect%finbief(1))
  END IF
  num_bief = 1
  CALL STRICK()
  z = 0.D0
! CALCUL DE -2*(Z_MES-Z_CAL)
  CALL PERMAT(z, q, zinit, x, zref, cf1, cf2, pcsing, idt, xdt, &
&              profil, profilplan, f1, connect, num_bief, nb_sect, &
&              singularite, modelelit, impressioncalcul, unitelisting, &
&              temps, loifrottement, cqmv, decentrement, erreur)
  DO l=1,nb_mes(ic)
     j = i_mesu(ic,l)
     z_cal(l) = z(j)
     zcout(l) = -2.D0*(z_mesu(ic,l)-z_cal(l))
  ENDDO
!
! LIT MINEUR
  IF (iestim .EQ. 1 .OR. iestim .EQ. 3) THEN
    zdtemp = 0.D0
    dcf1_fwd = 0.D0
    DO k=1,nb_zone_frottement
      CALL XINDIC_S( indice , calage_frott(k)%abscdeb_zone_frott , X , Erreur )
      CALL XINDIC_S( indice2 , calage_frott(k)%abscfin_zone_frott , X , Erreur )
      DO j = indice, indice2
        dcf1_fwd(j)=1.D0
      ENDDO
      CALL DIFF_Z_CF12_FWD_SARAP(diff_z_fwd, dcf1_fwd, dcf2_fwd)
      DO l=1,nb_mes(ic)
        r = i_mesu(ic,l)
        zdtemp(l) = diff_z_fwd(r)*zcout(l)
      ENDDO
      dcf1_fwd = 0.D0
      grmin(k) = SUM(zdtemp)
    ENDDO
  ENDIF
!
! LIT MAJEUR
  IF (iestim .EQ. 2 .OR. iestim .EQ. 3) THEN
    zdtemp = 0.D0
    dcf2_fwd = 0.D0
    DO k=1,nb_zone_frottement
      CALL XINDIC_S( indice , calage_frott(k)%abscdeb_zone_frott , X , Erreur )
      CALL XINDIC_S( indice2 , calage_frott(k)%abscfin_zone_frott , X , Erreur )
      DO j = indice, indice2
        dcf2_fwd(j)=1.D0
      ENDDO
      CALL DIFF_Z_CF12_FWD_SARAP(diff_z_fwd, dcf1_fwd, dcf2_fwd)
      DO l=1,nb_mes(ic)
        r = i_mesu(ic,l)
        zdtemp(l) = diff_z_fwd(r)*zcout(l)
      ENDDO
        dcf2_fwd = 0.D0
        grmaj(k) = SUM(zdtemp)
    ENDDO
  ENDIF

  DEALLOCATE(zcout)
  DEALLOCATE(zdtemp)

END SUBROUTINE DIFF_JCOUT
