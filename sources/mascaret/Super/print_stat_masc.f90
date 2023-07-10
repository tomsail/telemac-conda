! Affichage quelques statistiques sur le calcul
subroutine print_stat_masc(TitreCas,sz1,sz2,sz3,sz4,sz5,sz6,sz7,sz8,sz9,sz10,Noyau,DT,CourantObj,TempsMaximum,NbPasTemps,  &
           CritereArret,PastempsVariable,wSg,OptionCasier,Impli_Trans,Boussinesq,NoConvection)
   use M_PRECISION
   use M_CONSTANTES_CALCUL_C
   character(len=*) :: TitreCas
   logical :: PastempsVariable,wSg,OptionCasier,Impli_Trans,Boussinesq,NoConvection
   integer :: sz1,sz2,sz3,sz4,sz5,sz6,sz7,sz8,sz9,sz10
   integer :: Noyau,NbPasTemps,CritereArret
   real(double) :: DT,CourantObj,TempsMaximum

   print *,'Study name : '//TRIM(TitreCas)
   print *
   print *,'Hydraulic statistics (prior solve phase)'
   print *,'   ------ Geometric parameters ------'
   print *,'   Number of reach(es)       = ',sz1
   print *,'   Number of cross-sections  = ',sz2
   print *,'   Number of open boundaries = ',sz3
   print *,'   Number of junction(s)     = ',sz4
   print *,'   Number of inflow(s)       = ',sz5
   print *,'   Number of lateral weir(s) = ',sz6
   if(OptionCasier) then
      print *,'   Number of storage area(s) = ',sz7
      print *,'   Number of link(s)         = ',sz8
   else
      print *,'   Number of storage area(s) = ',0
      print *,'   Number of link(s)         = ',0
   endif
   if (wSg.eqv..false.) then
      print *,'   Number of dam(s)/weir(s)  = ',0
   else
      print *,'   Number of dam(s)/weir(s)  = ',sz9
   endif
   print *,'   ------ Numerical parameters ------'
   print *,'   Number of 1D nodes (mesh) = ',sz10
   print *,'   Computation Kernel        = ',Noyau
   write(*,123) '    Initial time step         = ',DT
   if(PastempsVariable.eqv..true.) then
      print *,'   Variable time step?       = ',1
      write(*,123) '    Courant number            = ',CourantObj
   else
      print *,'   Variable time step?       = ',0
   endif
   if(Noyau.eq.NOYAU_MASCARET) then
       if(Impli_Trans.eqv..true.) then
           print *,'   Implicit Scheme?          = ',1
       else
           print *,'   Implicit Scheme?          = ',0
       endif
       if(Boussinesq.eqv..true.) then
           print *,'   Non Hydrostatic?          = ',1
       else
           print *,'   Non Hydrostatic?          = ',0
       endif
   endif
   if(Noyau.eq.NOYAU_REZODT) then
       if(NoConvection.eqv..true.) then
           print *,'   Reduced Momentum Equation?= ',1
       else
           print *,'   Reduced Momentum Equation?= ',0
       endif
   endif
   if(CritereArret.eq.temps_maximum) then
      write(*,123) '    Simulation time           = ',TempsMaximum
   elseif(CritereArret.eq.NOMBRE_DE_PAS_TEMPS_MAXIMUM) then
      print *,'   Number of time steps      = ',NbPasTemps
   endif
   print *

   return
   123 format(A,ES12.1)
end subroutine print_stat_masc
