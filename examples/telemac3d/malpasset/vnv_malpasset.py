
"""
Validation script for malpasset
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 3
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # malpasset scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_malpasset-murd_p2.cas')


        # malpasset parallel mode
        cas = TelemacCas('t3d_malpasset-murd_p2.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_malpasset-murd_p2_par.cas',
                       cas=cas)

        del cas


        # malpasset scalar mode
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_malpasset-char_p2.cas')


        # malpasset parallel mode
        cas = TelemacCas('t3d_malpasset-char_p2.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_malpasset-char_p2_par.cas',
                       cas=cas)

        del cas


        # malpasset scalar mode
        self.add_study('vnv_5',
                       'telemac3d',
                       't3d_malpasset-murd_p6.cas')


        # malpasset parallel mode
        cas = TelemacCas('t3d_malpasset-murd_p6.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_6',
                       'telemac3d',
                       't3d_malpasset-murd_p6_par.cas',
                       cas=cas)

        del cas


        # malpasset parallel mode
        cas = TelemacCas('t3d_malpasset-fine_p2.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_8',
                       'telemac3d',
                       't3d_malpasset-fine_p2_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_malpasset-murd_p2.slf',
                            eps=[2.2E-1, 4.E0, 4.E0, 1.E0, 6.E-1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_malpasset-murd_p2.slf',
                            eps=[2.2E-1, 4.E0, 4.E0, 1.E0, 9.1E-1])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[2.E-1, 4.E0, 4.E0, 1.E0, 9.E-1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T3DRES',
                            'f3d_malpasset-char_p2.slf',
                            eps=[1.4E0, 3.0E0, 2.3E0, 5.E-1, 1.6E0])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T3DRES',
                            'f3d_malpasset-char_p2.slf',
                            eps=[1.E0, 3.0E0, 2.3E0, 5.E-1, 1.6E0])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[2.E0, 3.0E0, 2.3E0, 5.E-1, 1.6E0])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:T3DRES',
                            'f3d_malpasset-murd_p6.slf',
                            eps=[3.E-1, 5.5E0, 3.6E0, 1.E0, 1.1E0])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:T3DRES',
                            'f3d_malpasset-murd_p6.slf',
                            eps=[3.E-1, 5.8E0, 4.3E0, 1.1E0, 1.1E0])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_5:T3DRES',
                            'vnv_6:T3DRES',
                            eps=[3.E-1, 6.0E0, 4.3E0, 1.1E0, 1.1E0])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_8:T3DRES',
                            'f3d_malpasset-fine_p2.slf',
                            eps=[2.E-1, 2.9E0, 2.6E0, 8.E-1, 3.E-1])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot2d import plot2d_scalar_filled_contour,plot2d_triangle_mesh
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        from postel.deco_vnv import decoVNV
        import matplotlib.pyplot as plt
        from os import path
        import numpy as np

        # Getting geometry file
        geo, _ = self.get_study_res('vnv_1:T3DGEO', load_bnd=True, module="T3D")
        geo_large, _ = self.get_study_res('vnv_8:T3DGEO', load_bnd=True, module="T3D")

        # Plotting mesh
        vnv_plot2d('',
                   geo,
                   plot_mesh=True,
                   fig_size=(13, 7),
                   fig_name='img/Mesh_small',
                   annotate_bnd=True)

        vnv_plot2d('',
                   geo,
                   plot_mesh=True,
                   fig_size=(14, 8),
                   xlim=[4500., 5500.],
                   ylim=[3900., 4500.],
                   fig_name='img/Mesh_small_dam',
                   annotate_bnd=True)

        vnv_plot2d('',
                   geo_large,
                   plot_mesh=True,
                   fig_size=(13, 7),
                   fig_name='img/Mesh_large',
                   annotate_bnd=True)

        #----------------------------------------------------------------------
        # Plotting BOTTOM at 0
        #
        # Physical model points locations
        P6 = [4947, 4289]
        P7 = [5717, 4407]
        P8 = [6775, 3869]
        P9 = [7128, 3162]
        P10 = [8585, 3443]
        P11 = [9674, 3085]
        P12 = [10939, 3044]
        P13 = [11724, 2810]
        P14 = [12723, 2485]

        # Transformers locations
        A = [5550, 4400]
        B = [11900, 3250]
        C = [13000, 2700]
        point_list = [A, B, C, P6, P7, P8, P9, P10, P11, P12, P13, P14]
        point_labels = ['A', 'B', 'C', 'P6', 'P7', 'P8', 'P9', 'P10', \
                        'P11', 'P12', 'P13', 'P14']

        # Plot bottom with point locations
        plt.style.use('default')
        plt.rcParams.update(decoVNV)
        fig, ax = plt.subplots(1, 1, figsize=(14, 6))
        mesh = geo.tri
        bottom = geo.get_data_value('BOTTOM', 0)
        plot2d_triangle_mesh(ax, mesh, color='k', linewidth=0.1, alpha=.5)
        plot2d_scalar_filled_contour(fig, ax, mesh, bottom,
                                     vmin=-20, vmax=100, nv=13,
                                     x_label='x (m)',y_label='y (m)',
                                     extend='both',
                                     data_name='bottom (m)', colorbar=True)
        for i, pt in enumerate(point_list):
            lab = point_labels[i]
            if i > 2:
                plt.plot(pt[0], pt[1], marker='x', markersize=6, color='k')
                plt.annotate(lab, xy=pt, xytext=(pt[0]+100, pt[1]-200))
            else:
                plt.plot(pt[0], pt[1], marker='d', markersize=8, color='r')
                plt.annotate(lab, xy=pt, xytext=(pt[0]-200, pt[1]+100))
        fig_name = "img/bathymetry"
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
        plt.close('all')

        #----------------------------------------------------------------------
        RFO1 = self.get_study_file('vnv_1:T3DRFO')
        RFO2 = self.get_study_file('vnv_3:T3DRFO')
        RFO3 = self.get_study_file('vnv_5:T3DRFO')
        RFO4 = self.get_study_file('vnv_8:T3DRFO')[0]
        P_RFO1 = self.get_study_file('vnv_2:T3DRFO')[0]
        P_RFO2 = self.get_study_file('vnv_4:T3DRFO')[0]
        P_RFO3 = self.get_study_file('vnv_6:T3DRFO')[0]
        P_RFO4 = self.get_study_file('vnv_8:T3DRFO')[0]

        def TreatObs(RFOs,outFile) :
           import numpy as np
           import codecs

           Nobs = 9
           Nschemes = len(RFOs)
           MaxObs = [84.2-43.9, 49.1-34.5, 54.0-30.0, 40.2-27.4, 34.9-23.1, 27.4-19.1, 21.5-11.4, 16.1-9.3, 12.9-7.5]
           MaxNum = np.zeros((Nobs,Nschemes))
           RelativeErr = np.zeros((Nobs,Nschemes))
           fIn = []
           for ff in range(Nschemes):
               fIn.append(codecs.open(RFOs[ff],"r",encoding='utf-8'))

           for ff in range(Nschemes):
               startSave = 0
               while(startSave == 0):
                   line=fIn[ff].readline()
                   if(line.startswith(" MAXIMUM WATER DEPTHS")):
                       startSave = 1
               for i in range(Nobs):
                   dataString = ((fIn[ff].readline()).split("=")[-1]).split("M")[0]
                   MaxNum[i,ff] = float(dataString)
                   RelativeErr[i,ff] = (MaxNum[i,ff] - MaxObs[i])/MaxObs[i]*100
               fIn[ff].close()


           #Write in Out file
           print(" "*8+"~> Writting "+outFile)
           f = codecs.open(outFile,"w",encoding='utf-8')
           for i in range(Nobs):
               #WRITE RELATIVE ERRORS COLORED
               if(RelativeErr[i,0] < 0):
                   charColor = '%.2f {\color{red} (%.2f\%%)}'
               else:
                   charColor = '%.2f {\color{blue} (%.2f\%%)}'

               f.write(charColor % (MaxNum[i,0],RelativeErr[i,0]))

               for n in range(1,Nschemes-1):
                   #WRITE RELATIVE ERRORS COLORED
                   if(RelativeErr[i,n] < 0):
                       charColor = '& %.2f {\color{red} (%.2f\%%)}'
                   else:
                       charColor = '& %.2f {\color{blue} (%.2f\%%)}'

                   f.write(charColor % (MaxNum[i,n],RelativeErr[i,n]))


               #WRITE RELATIVE ERRORS COLORED
               charEnd = '\\\\ \n'
               if(i==Nobs-1):
                   charEnd = '\n'
               if(RelativeErr[i,Nschemes-1] < 0):
                   charColor = '& %.2f  {\color{red} (%.2f\%%)} ' + charEnd
               else:
                   charColor = '& %.2f  {\color{blue} (%.2f\%%)} ' + charEnd

               f.write(charColor % (MaxNum[i,Nschemes-1],RelativeErr[i,Nschemes-1]))

           f.close

        TreatObs([RFO1, RFO2, RFO3, RFO4], 'img/MaxLevels_schemes.txt')


        def TreatTimeObs(RFOs,outFile) :
           import numpy as np
           import codecs

           Nschemes = len(RFOs)
           nTimeObs = 2
           TimeObs = [1140.0, 1320.0]
           TimeT2D = np.zeros((Nschemes,2))
           RelativeErr = np.zeros((Nschemes,2))
           fIn = []
           for ff in range(Nschemes):
               fIn.append(open(RFOs[ff],"r"))

           for ff in range(Nschemes):
               for line in fIn[ff]:
                   if(line.startswith("TIME FROM A TO B")):
                       dataString = (line.split(":")[-1]).split("S")[0]
                       TimeT2D[ff,0] = float(dataString)
                   RelativeErr[ff,0] = (TimeT2D[ff,0] - TimeObs[0])/(TimeObs[0])*100
                   if(line.startswith("TIME FROM A TO C")):
                      dataString = (line.split(":")[-1]).split("S")[0]
                      TimeT2D[ff,1] = float(dataString)
                   RelativeErr[ff,1] = (TimeT2D[ff,1] - TimeObs[1])/(TimeObs[1])*100
               fIn[ff].close()
           #Write in Out file
           print(" "*8+"~> Writting "+outFile)
           f = codecs.open(outFile,"w",encoding='utf-8')

           for i in range(nTimeObs):
           #WRITE RELATIVE ERRORS COLORED
               if(RelativeErr[0,i] < 0):
                   charColor = '%.1f {\color{red} (%.1f\%%)}'
               else:
                   charColor = '%.1f {\color{blue} (%.1f\%%)}'
               f.write(charColor % (TimeT2D[0,i],RelativeErr[0,i]))
               for n in range(1,Nschemes-1):
                   #WRITE RELATIVE ERRORS COLORED
                   if(RelativeErr[n,i] < 0):
                       charColor = '& %.1f {\color{red} (%.1f\%%)}'
                   else:
                       charColor = '& %.1f {\color{blue} (%.1f\%%)}'
                   f.write(charColor % (TimeT2D[n,i],RelativeErr[n,i]))

               #WRITE RELATIVE ERRORS COLORED
               charEnd = '\\\\ \n'
               if(i==nTimeObs-1):
                   charEnd = '\n'
               if(RelativeErr[Nschemes-1,i] < 0):
                   charColor = '& %.1f  {\color{red} (%.1f\%%)} ' + charEnd
               else:
                   charColor = '& %.1f  {\color{blue} (%.1f\%%)} ' + charEnd

               f.write(charColor % (TimeT2D[Nschemes-1,i],RelativeErr[Nschemes-1,i]))


           f.close()

        TreatTimeObs([RFO1, RFO2, RFO3, RFO4], 'img/waveTimes_schemes.txt')


        def nonRegression(ErrFileName, massFile, fileStatus, oldVar, newVar) :
           import numpy as np
           import matplotlib.pyplot as plt
           import math
           import codecs

           masses = np.genfromtxt(massFile)
           npoin = len(newVar)
           errL1 = 0.
           errL2 = 0.
           errLinf = -100.

           #Denominateurs
           errL1Denom = 0.
           errL2Denom = 0.
           errLinfDenom = 0.

           #Masse
           masseSum = 0.

           for i in range(npoin):
               errL1 += np.abs(oldVar[i] - newVar[i])*masses[i]
               errL2 += np.abs(oldVar[i] - newVar[i])**2*masses[i]
               errLinf = max(errLinf, abs(oldVar[i] - newVar[i]))

               #Denominateurs
               errL1Denom += np.abs(oldVar[i])*masses[i]
               errL2Denom += np.abs(oldVar[i])**2*masses[i]
               errLinfDenom = max(errLinfDenom, abs(oldVar[i]))

               #MASSE
               masseSum+= masses[i]

           errL1 = errL1/masseSum
           errL2 = np.sqrt(errL2/masseSum)

           errL1Denom = errL1Denom/masseSum
           errL2Denom = np.sqrt(errL2Denom/masseSum)

           errL1Relative = errL1/errL1Denom
           errL2Relative = errL2/errL2Denom
           errLinfRelative = errLinf/errLinfDenom

           print(" "*8+"~> Writting "+ErrFileName)
           ErrFile = codecs.open(ErrFileName, fileStatus, encoding='utf-8')
           ErrFile.write('%10.3e' % errL1Relative)
           ErrFile.write('& %10.3e' % errL2Relative)
           ErrFile.write('& %10.3e \\\\ \n' % errLinfRelative)

           ErrFile.close()

        mass_small = path.join(self.get_vnv_working_dir('vnv_1'), 'mass.txt')

        res_vnv_1_t3dhyd = TelemacFile(self.get_study_file('vnv_1:T3DHYD'))
        res_vnv_2_t3dhyd = TelemacFile(self.get_study_file('vnv_2:T3DHYD'))
        res_vnv_3_t3dhyd = TelemacFile(self.get_study_file('vnv_3:T3DHYD'))
        res_vnv_4_t3dhyd = TelemacFile(self.get_study_file('vnv_4:T3DHYD'))
        res_vnv_5_t3dhyd = TelemacFile(self.get_study_file('vnv_5:T3DHYD'))
        res_vnv_6_t3dhyd = TelemacFile(self.get_study_file('vnv_6:T3DHYD'))
        res_vnv_8_t3dhyd = TelemacFile(self.get_study_file('vnv_8:T3DHYD'))

        h1 = res_vnv_1_t3dhyd.get_data_value('WATER DEPTH', -1)
        h2 = res_vnv_3_t3dhyd.get_data_value('WATER DEPTH', -1)
        h3 = res_vnv_5_t3dhyd.get_data_value('WATER DEPTH', -1)
        h4 = res_vnv_8_t3dhyd.get_data_value('WATER DEPTH', -1)
        P_h1 = res_vnv_2_t3dhyd.get_data_value('WATER DEPTH', -1)
        P_h2 = res_vnv_4_t3dhyd.get_data_value('WATER DEPTH', -1)
        P_h3 = res_vnv_6_t3dhyd.get_data_value('WATER DEPTH', -1)

        nonRegression('SeqPar.txt', mass_small, 'w', h1, P_h1)
        nonRegression('SeqPar.txt', mass_small, 'a', h2, P_h2)
        nonRegression('SeqPar.txt', mass_small, 'a', h3, P_h3)

        def simuTime(RFOs,P_RFOs,outFile) :
           import numpy as np
           import matplotlib.pyplot as plt
           import codecs

           Nschemes =  len(RFOs)
           SeqTime = []
           ParTime = []
           fSeq = []
           fPar = []
           for ff in range(Nschemes):
               if(RFOs[ff]==''):
                   fSeq.append('')
               else:
                   fSeq.append(codecs.open(RFOs[ff],"r",encoding='utf-8'))
               if(P_RFOs[ff]==''):
                   fPar.append('')
               else:
                   fPar.append(codecs.open(P_RFOs[ff],"r",encoding='utf-8'))

           for ff in range(Nschemes):
               if(fSeq[ff]==''):
                   SeqTime.append('-')
               else:
                   for line in fSeq[ff]:
                       if(line.startswith(" ELAPSE")):
                           SeqTime.append(line.split(":")[-1])
                   fSeq[ff].close()

               if(fPar[ff]==''):
                   ParTime.append('-')
               else:
                   for line in fPar[ff]:
                       if(line.startswith(" ELAPSE")):
                           ParTime.append(line.split(":")[-1])

                   fPar[ff].close()


           #Write in Out file
           print(" "*8+"~> Writting "+outFile)
           f = codecs.open(outFile,"w",encoding='utf-8')

           f.write(SeqTime[0].split("H")[-1])
           for n in range(1,Nschemes-1):
               f.write('& ' + (SeqTime[n].split("H")[-1]))
           f.write('& ' + (SeqTime[Nschemes-1].split("H")[-1]) + ' \\\\ \n')


           f.write(ParTime[0].split("H")[-1])
           for n in range(1,Nschemes-1):
               f.write('& ' + (ParTime[n].split("H")[-1]))
           f.write('& ' + (ParTime[Nschemes-1].split("H")[-1]) + '\n')

           f.close()

        simuTime([RFO1,RFO2,RFO3,RFO4], [P_RFO1,P_RFO2,P_RFO3,P_RFO4], 'img/TimesSeqPar_schemes.txt')

        def volumeCons(RFOs,outFile) :
           import numpy as np
           import matplotlib.pyplot as plt
           import codecs

           Nschemes = len(RFOs)
           Ntimes = 21
           Times = []
           Volumes = []
           RelativeErr = np.zeros((Nschemes))
           VolumeLost = np.zeros((Nschemes))
           fIn = []
           for ff in range(Nschemes):
               fIn.append(open(RFOs[ff],"r"))

           for ff in range(Nschemes):
               startSave = 0
               while(startSave == 0):
                   line=fIn[ff].readline()
                   if(line.startswith(" VOLUMES")):
                       startSave = 1

               line = fIn[ff].readline()
               Volumes.append([])
               while(line.startswith("TIME")):
                   dataString = line.split()
                   volumeString = dataString[-2]
                   Volumes[ff].append(float(volumeString))
                   line = fIn[ff].readline()

               VolumeLost[ff] = float(line.split()[-1])

               line = fIn[ff].readline()
               RelativeErr[ff] = float(line.split()[-1])

               fIn[ff].close()

           #Write in Out file
           print(" "*8+"~> Writting "+outFile)
           f = codecs.open(outFile,"w",encoding='utf-8')

           f.write('%.2E ' % (VolumeLost[0]))
           for n in range(1,Nschemes-1):
               f.write('& %.2E ' % (VolumeLost[n]))
           f.write('& %.2E \\\\ \n' % (VolumeLost[Nschemes-1]))

           #WRITE RELATIVE ERRORS COLORED
           if(RelativeErr[0] < 0):
               charColor = '{\color{red} %.2E}'
           else:
               charColor = '{\color{blue} %.2E}'
           f.write(charColor % RelativeErr[0])

           for n in range(1,Nschemes-1):
               if(RelativeErr[n] < 0):
                   charColor = '& {\color{red} %.2E}'
               else:
                   charColor = '& {\color{blue} %.2E}'
               f.write(charColor % RelativeErr[n])

           if(RelativeErr[Nschemes-1] < 0):
               charColor = '& {\color{red} %.2E} \n'
           else:
               charColor = '& {\color{blue} %.2E} \n'
           f.write(charColor % RelativeErr[Nschemes-1])

           f.close()

        volumeCons([RFO1,RFO2,RFO3], 'img/volErrors_schemes.txt')
        volumeCons([P_RFO1,P_RFO2,P_RFO3,P_RFO4], 'img/volErrors_schemes_Parallel.txt')

        def VolTime(RFOs,legend,figName) :
           import numpy as np
           import matplotlib.pyplot as plt
           from os import mkdir

           nschemes = len(RFOs)

           #PARAM
           ntimes = 21
           t = np.zeros((ntimes))
           t[0] = 0.
           dtGraph = 200.
           for i in range(1,ntimes):
               t[i] = t[i-1] + dtGraph

           Volumes = np.zeros((ntimes,nschemes))
           fIn = []
           for ff in range(nschemes):
               fIn.append(open(RFOs[ff],"r"))

           for ff in range(nschemes):
               startSave = 0
               while(startSave == 0):
                   line=fIn[ff].readline()
                   if(line.startswith(" VOLUMES")):
                       startSave = 1

               for i in range(ntimes):
                   line = fIn[ff].readline()
                   dataString = line.split()
                   volumeString = dataString[-2]
                   Volumes[i,ff] = float(volumeString)

               fIn[ff].close()

           ##########################ADIM#################################
           H0 = 55.  #Initial water dam
           L = 18000  #domain length
           T = 4000 #Duration

           t[:] = t[:]/T
           for ff in range(nschemes):
               Volumes[:,ff] = Volumes[:,ff]/Volumes[0,ff]
           ###############################################################

           clr = ['r','g','y','m','orange','b']
           fig = plt.figure()

           for s in range(nschemes):
               plt.plot(t,Volumes[:,s],clr[s])

           plt.legend(legend, fontsize=24, ncol=2, loc=0, fancybox=True, framealpha=0.5)
           plt.xlabel("t/T",fontsize = 26)
           plt.ylim(0.99,1.01)
           plt.ylabel("Volume / Initial Volume",fontsize = 26)
           plt.xticks(fontsize = 26)
           plt.yticks(fontsize = 26)

           fig.tight_layout()
           print(" "*8+"~> Plotting "+figName)
           plt.savefig(figName,format='png',dpi=400)
           plt.close('all')

        VolTime([RFO1,RFO2,RFO3,RFO4], ['MURD P2', 'CHAR P2','MURD P6','FINE P2'],'img/VolTime.png')

        def negCheck(Hs,outFile) :
           import numpy as np
           import matplotlib.pyplot as plt
           import codecs

           nschemes=len(Hs)
           mins = np.zeros((nschemes))

           for s in range(nschemes):
               mins[s] = min(Hs[s][:])

           #Write in Out file
           print(" "*8+"~> Writting "+outFile)
           f = codecs.open(outFile,"w",encoding='utf-8')
           f.write('%.2E ' % (mins[0]))
           for s in range(1,nschemes-1):
               f.write('&amp; %.2E ' % (mins[s]))
           f.write('&amp; %.2E \\\\ \n' % (mins[nschemes-1]))
           f.close()


        min_h1 = np.zeros(res_vnv_1_t3dhyd.ntimestep)
        min_h2 = np.zeros(res_vnv_1_t3dhyd.ntimestep)
        min_h3 = np.zeros(res_vnv_1_t3dhyd.ntimestep)
        min_h4 = np.zeros(res_vnv_1_t3dhyd.ntimestep)
        for i in range(res_vnv_1_t3dhyd.ntimestep):
            min_h1[i] = min(res_vnv_1_t3dhyd.get_data_value('WATER DEPTH', i))
            min_h2[i] = min(res_vnv_3_t3dhyd.get_data_value('WATER DEPTH', i))
            min_h3[i] = min(res_vnv_5_t3dhyd.get_data_value('WATER DEPTH', i))
            min_h4[i] = min(res_vnv_8_t3dhyd.get_data_value('WATER DEPTH', i))


        negCheck([min_h1,min_h2,min_h3,min_h4], 'hmins.txt')


        vnv_plot1d_polylines('WATER DEPTH',
                             [res_vnv_1_t3dhyd, res_vnv_3_t3dhyd,
                              res_vnv_5_t3dhyd, res_vnv_8_t3dhyd],
                             record=-1,
                             poly=[[4634., 4132.84], [4589.81, 4393.22]],
                             legend_labels=['MURD P2', 'CHAR P2',
                                            'MURD P6', 'FINE P6'],
                             fig_size=(10,10),
                             fig_name='img/WaterDepth_Dam_amont')

        vnv_plot1d_polylines('WATER DEPTH',
                             [res_vnv_1_t3dhyd, res_vnv_3_t3dhyd,
                              res_vnv_5_t3dhyd, res_vnv_8_t3dhyd],
                             record=-1,
                             poly=[[4884.95, 4161.82], [4846.39, 4362.44]],
                             legend_labels=['MURD P2', 'CHAR P2',
                                            'MURD P6', 'FINE P6'],
                             fig_size=(10, 10),
                             fig_name='img/WaterDepth_Dam_aval')

        bottom = res_vnv_1_t3dhyd.get_data_value('BOTTOM', 0)

        for record in [0, 1, 2, 3, 12]:

            res = res_vnv_1_t3dhyd

            water_depth = res.get_data_value('WATER DEPTH', record)

            fig, ax = plt.subplots(1, 1, figsize=(20, 5))

            plot2d_scalar_filled_contour(fig, ax, res.tri, bottom, data_name='bottom',
                                         levels=np.arange(0, 1000, 500.),
                                         cmap_name="Greys")

            plot2d_scalar_filled_contour(fig, ax, res.tri, water_depth, data_name='water depth',
                                         levels=np.arange(1.0, 55.0, 5.0),
                                         cmap_name="jet")

            ax.set_xlim(3000, 12500)
            ax.set_ylim(2000, 5500)

            fig_name = 'img/WD{}_MURD_P2'.format(record*200)
            print(" "*8+"~> Plotting "+fig_name)
            plt.savefig(fig_name)
            plt.close('all')

        for res, name in [(res_vnv_3_t3dhyd, 'CHAR_P2'),
                          (res_vnv_5_t3dhyd, 'MURD_P6'),
                          (res_vnv_8_t3dhyd, 'FINE_P2')]:
            record = 2

            water_depth = res.get_data_value('WATER DEPTH', record)
            bottom = res.get_data_value('BOTTOM', 0)

            fig, ax = plt.subplots(1, 1, figsize=(20, 5))

            plot2d_scalar_filled_contour(fig, ax, res.tri, bottom, data_name='bottom',
                                         levels=np.arange(0, 1000, 500.),
                                         cmap_name="Greys")

            plot2d_scalar_filled_contour(fig, ax, res.tri, water_depth, data_name='water depth',
                                         levels=np.arange(1.0, 55.0, 5.0),
                                         cmap_name="jet")

            ax.set_xlim(3000, 12500)
            ax.set_ylim(2000, 5500)

            fig_name = 'img/WD400_{}'.format(name)
            print(" "*8+"~> Plotting "+fig_name)
            plt.savefig(fig_name)
            plt.close('all')


        res_vnv_1_t3dhyd.close()
        res_vnv_2_t3dhyd.close()
        res_vnv_3_t3dhyd.close()
        res_vnv_4_t3dhyd.close()
        res_vnv_5_t3dhyd.close()
        res_vnv_6_t3dhyd.close()
        res_vnv_8_t3dhyd.close()

        res = TelemacFile(self.get_study_file('vnv_5:T3DRES'))

        for plane in [1, 3, 6]:
            vnv_plot2d('ELEVATION Z',
                       res,
                       plane=plane-1,
                       filled_contours=True,
                       record=1,
                       vectors=True,
                       vectors_scale=100,
                       xlim=[4500, 5400],
                       ylim=[3900, 4550],
                       fig_size=(20, 5),
                       fig_name='img/Velocity100_MURD_P6_plane{}'.format(plane))

            vnv_plot2d('VELOCITY',
                       res,
                       plane=plane-1,
                       filled_contours=True,
                       record=1,
                       xlim=[4500, 5400],
                       ylim=[3900, 4550],
                       fig_size=(20, 5),
                       fig_name='img/VelocityMap100_MURD_P6_plane{}'.format(plane))

        res.close()
