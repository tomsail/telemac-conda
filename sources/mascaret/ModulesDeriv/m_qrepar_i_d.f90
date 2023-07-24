MODULE M_QREPAR_I_D
  INTERFACE 
      SUBROUTINE QREPAR_D(sommedebitance, sommedebitanced, zaval, zavald&
&       , numpassage, q, qd, z, zd, zref, x, cf1, cf1d, cf2, cf2d, idt, &
&       xdt, profil, profil_plan, numconfluence, connect, modelelit, &
&       epsil, dzprev, unitelisting, loifrottement, erreur)
        USE M_PRECISION
        USE M_CONNECT_T
        USE M_PROFIL_T
        USE M_PROFIL_PLAN_T
        USE M_ERREUR_T
        USE M_INTERPOLATION_S_D
        USE M_RHSBP_S_D
        USE M_TRAITER_ERREUR_I
        USE M_REPAR_I_D
        IMPLICIT NONE
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: sommedebitance
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: sommedebitanced
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: zaval
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: zavald
        INTEGER, INTENT(INOUT) :: numpassage
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: q
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qd
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: z
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: zd
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1d
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2d
        INTEGER, DIMENSION(:), INTENT(IN) :: idt
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
        TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
        TYPE(PROFIL_PLAN_T), INTENT(IN) :: profil_plan
        INTEGER, INTENT(IN) :: numconfluence
        TYPE(CONNECT_T), INTENT(IN) :: connect
        INTEGER, INTENT(IN) :: modelelit
        DOUBLE PRECISION, INTENT(OUT) :: epsil
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: dzprev
        INTEGER, INTENT(IN) :: unitelisting
        INTEGER, INTENT(IN) :: loifrottement
        TYPE(ERREUR_T), INTENT(INOUT) :: erreur
      END SUBROUTINE QREPAR_D
  END INTERFACE
END MODULE M_QREPAR_I_D

