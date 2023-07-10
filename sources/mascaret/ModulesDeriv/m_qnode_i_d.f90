MODULE M_QNODE_I_D
  INTERFACE 
      SUBROUTINE QNODE_D(q, qd, z, zd, numconfluence, numpassage, &
&       connect, erreur)
        USE M_PRECISION
        USE M_CONNECT_T
        USE M_ERREUR_T
        USE M_TRAITER_ERREUR_I
        IMPLICIT NONE
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: q
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qd
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: z
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: zd
        INTEGER, INTENT(IN) :: numconfluence
        INTEGER, INTENT(IN) :: numpassage
        TYPE(CONNECT_T), INTENT(IN) :: connect
        TYPE(ERREUR_T), INTENT(INOUT) :: erreur
      END SUBROUTINE QNODE_D
  END INTERFACE
END MODULE M_QNODE_I_D

