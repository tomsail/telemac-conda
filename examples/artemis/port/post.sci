stacksize(200000000)


Typedepl=['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw'];
Title1=['SURGE', 'SWAY', 'HEAVE', 'ROLL', 'PITCH', 'YAW'];
Title2=['DX', 'DY', 'DZ', 'DRX', 'DRY', 'DRZ'];

SL=1
kd=1

// Chargement des données
TAB1=fscanfMat('angles.dat');
N1=size(TAB1,1)



pi=4.*atan(1.0)


POINT =zeros(1,N1)
AUTO  =zeros(1,N1)
FIXE  =zeros(1,N1)
INCIDF=zeros(1,N1)

 for i=1:N1
  POINT(i) =TAB1(i,1)
  AUTO(i)  =TAB1(i,1+1)
  FIXE(i)  =TAB1(i,1+2)
  INCIDF(i)=TAB1(i,1+3)
 end 

 h(kd)=figure();
 scf(h(kd));
 plot(POINT(:),AUTO(:),'-b','thickness',2)
 plot(POINT(:),FIXE(:),'-r','thickness',2)
 plot(POINT(:),INCIDF(:),'--g','thickness',2)
 xgrid;
 xtitle('INCIDENCE DES VAGUES PAR RAPPORT A LA NORMALE','POINT FRONTIERE','degres')
 legend(['ANGLES AUTOMATIQUES','ANGLE IMPOSES','INCIDENCE iteration 1'],a=2);

