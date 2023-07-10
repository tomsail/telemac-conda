
"""
Validation script for tracer_wet_dry
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
        self.rank = 2
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # tracer wetting and drying scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_tracerwetdry.cas')


        # tracer wetting and drying parallel mode
        cas = TelemacCas('t3d_tracerwetdry.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_tracerwetdry_par.cas',
                       cas=cas)

        del cas


        # tracer wetting and drying scalar mode
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_tracerwetdry-char.cas')


        # tracer wetting and drying parallel mode
        cas = TelemacCas('t3d_tracerwetdry-char.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_tracerwetdry-char_par.cas',
                       cas=cas)

        del cas


        # tracer wetting and drying scalar mode
        self.add_study('vnv_5',
                       'telemac3d',
                       't3d_tracerwetdry-murd.cas')


        # tracer wetting and drying parallel mode
        cas = TelemacCas('t3d_tracerwetdry-murd.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_6',
                       'telemac3d',
                       't3d_tracerwetdry-murd_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_tracerwetdry.slf',
                            eps=[1.E-8, 1.E-8, 1.E-9, 1.E-11, 1.E-7])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_tracerwetdry.slf',
                            eps=[1.E-8, 1.E-8, 1.E-9, 1.E-11, 1.E-7])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-8, 1.E-8, 1.E-9, 1.E-11, 1.E-7])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T3DRES',
                            'f3d_tracerwetdry.slf',
                            eps=[7.E-5, 3.E-3, 2.E-3, 3.E-5, 0.9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T3DRES',
                            'f3d_tracerwetdry.slf',
                            eps=[7.E-5, 3.E-3, 2.E-3, 3.E-5, 0.9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[1.E-8, 1.E-8, 1.E-9, 1.E-11, 1.E-7])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:T3DRES',
                            'f3d_tracerwetdry.slf',
                            eps=[1.E-5, 6.E-4, 4.E-4, 1.E-5, 0.06])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:T3DRES',
                            'f3d_tracerwetdry.slf',
                            eps=[1.E-5, 6.E-4, 4.E-4, 1.E-5, 0.06])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_5:T3DRES',
                            'vnv_6:T3DRES',
                            eps=[1.E-8, 1.E-8, 1.E-9, 1.E-11, 1.E-7])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        from postel.plot1d import plot1d
        from os import path
        import matplotlib.pyplot as plt
        #TODO: Replace 111 by ntime steps
        # Getting files
        vnv_1_t3dgeo = self.get_study_file('vnv_1:T3DGEO')
        res_vnv_1_t3dgeo = TelemacFile(vnv_1_t3dgeo)


        # Simutime
        vnv_1_t3drfo = self.get_study_file('vnv_1:T3DRFO')
        vnv_2_t3drfo = self.get_study_file('vnv_2:T3DRFO')[0]
        vnv_3_t3drfo = self.get_study_file('vnv_3:T3DRFO')
        vnv_4_t3drfo = self.get_study_file('vnv_4:T3DRFO')[0]
        vnv_5_t3drfo = self.get_study_file('vnv_5:T3DRFO')
        vnv_6_t3drfo = self.get_study_file('vnv_6:T3DRFO')[0]

        def simuTime(RFOs,P_RFOs,outFile) :
           import codecs

           Nschemes =  len(RFOs)
           SeqTime = []
           ParTime = []
           fSeq = []
           fPar = []
           for ff in range(Nschemes):
               fSeq.append(codecs.open(RFOs[ff],"r",encoding='utf-8'))
               fPar.append(codecs.open(P_RFOs[ff],"r",encoding='utf-8'))

           for ff in range(Nschemes):
               for line in fSeq[ff]:
                   if(line.startswith(" ELAPSE")):
                       SeqTime.append(line.split(":")[-1])
               for line in fPar[ff]:
                   if(line.startswith(" ELAPSE")):
                       ParTime.append(line.split(":")[-1])

               fSeq[ff].close()
               fPar[ff].close()


           #Write in Out file
           print(" "*8+"~> Writing "+outFile)
           f = codecs.open(outFile,"w", encoding='utf-8')

           f.write(SeqTime[0].split("H")[-1])
           for n in range(1,Nschemes-1):
               f.write('& ' + (SeqTime[n].split("H")[-1]))
           f.write('& ' + (SeqTime[Nschemes-1].split("H")[-1]) + ' \\\\ \n')


           f.write(ParTime[0].split("H")[-1])
           for n in range(1,Nschemes-1):
               f.write('& ' + (ParTime[n].split("H")[-1]))
           f.write('& ' + (ParTime[Nschemes-1].split("H")[-1]) + '\n')

           f.close

        simuTime([vnv_1_t3drfo, vnv_3_t3drfo, vnv_5_t3drfo],
                 [vnv_2_t3drfo, vnv_4_t3drfo, vnv_6_t3drfo],
                 'img/TimeSeqPar_Schemes.txt')

        def TracerMass(filesNames, legend, figName, Type) :
           import numpy as np
           import matplotlib.pyplot as plt
           import codecs
           from os import mkdir

           ##########################################################
           Nschemes = len(filesNames)
           fIn = []
           for ff in range(Nschemes):
               fIn.append(codecs.open(filesNames[ff],"r",encoding='utf-8'))

           Volumes = []
           VolumesConserved = []
           Times = []
           for ff in range(Nschemes):
               startSave = 0
               while(startSave == 0):
                   line=fIn[ff].readline()
                   if(line.startswith(" MASS")):
                       startSave = 1

               line = fIn[ff].readline()
               Volumes.append([])
               VolumesConserved.append([])
               Times.append([])
               while(line.startswith("TIME")):
                   dataString = line.split()
                   volumeString = dataString[-2]
                   createdString = dataString[3]
                   timeString = dataString[1]
                   Volumes[ff].append(float(volumeString))
                   VolumesConserved[ff].append(float(volumeString)+float(createdString))
                   Times[ff].append(float(timeString))
                   line = fIn[ff].readline()

                 ##########################################################
           T = 33300
           for ff in range(Nschemes):
               for i in range(len(Times[ff])):
                   Times[ff][i] = Times[ff][i]/T

           ##########################################################

           clr = ['b', 'g', 'r', 'c', 'm', 'y', 'k', 'orange']
           marker = ['*', 'o', '']
           fig = plt.figure()
           for s in range(Nschemes):
               if(Type=='Sum'):
                   plt.plot(Times[s], Volumes[s], clr[s], marker=marker[s])
               if(Type=='Conserved'):
                   plt.plot(Times[s], VolumesConserved[s], clr[s], marker=marker[s])

           plt.legend(legend, fontsize=22, ncol=2, loc=0)
           plt.xlabel("t/T", fontsize=26)
           plt.ylabel("Tracer mass", fontsize=26)
           plt.xticks(fontsize=26)
           plt.yticks(fontsize=26)
           fig.tight_layout()
           print(" "*8+"~> Plotting "+figName)
           plt.savefig(figName, format='png', dpi=400)
           plt.close('all')


        vnv_1_tracer = path.join(self.get_vnv_working_dir('vnv_1'), 'tracer.txt')
        vnv_3_tracer = path.join(self.get_vnv_working_dir('vnv_3'), 'tracer.txt')
        vnv_5_tracer = path.join(self.get_vnv_working_dir('vnv_5'), 'tracer.txt')

        TracerMass([vnv_1_tracer, vnv_3_tracer, vnv_5_tracer],
                   ['LEO POSTMA', 'CHAR', 'MURD'],
                   'img/TracerMassTime.png',
                   'Sum')

        TracerMass([vnv_1_tracer, vnv_3_tracer, vnv_5_tracer],
                   ['LEO POSTMA', 'CHAR', 'MURD'],
                   'img/ConservedTracerMassTime.png',
                   'Conserved')

        def volumeCons(RFOs, outFile) :
            import numpy as np
            import matplotlib.pyplot as plt
            import codecs

            Nschemes = len(RFOs)
            Ntimes = 112
            Times = []
            Volumes = []
            RelativeErr = np.zeros((Nschemes))
            VolumeLost = np.zeros((Nschemes))
            fIn = []
            for ff in range(Nschemes):
                fIn.append(codecs.open(RFOs[ff],"r",encoding='utf-8'))

            for ff in range(Nschemes):
                startSave = 0
                while(startSave == 0):
                    line=fIn[ff].readline()
                    if(line.startswith(" VOLUMES")):
                        startSave = 1

                line = fIn[ff].readline()
                Volumes.append([])
                Times.append([])
                while(line.startswith("TIME")):
                    dataString = line.split()
                    volumeString = dataString[-2]
                    timeString = dataString[1]
                    Volumes[ff].append(float(volumeString))
                    Times[ff].append(float(timeString))
                    line = fIn[ff].readline()

                VolumeLost[ff] = float(line.split()[-1])

                line = fIn[ff].readline()
                RelativeErr[ff] = float(line.split()[-1])

                fIn[ff].close()

            #Write in Out file
            print(" "*8+"~> Writing "+outFile)
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

            f.close

        volumeCons([vnv_1_t3drfo, vnv_3_t3drfo, vnv_5_t3drfo],
                   'img/volErrors_schemes.txt')

        volumeCons([vnv_2_t3drfo, vnv_4_t3drfo, vnv_6_t3drfo],
                   'img/volErrors_schemes_Parallel.txt')

        def VolTime(RFOs,legend,figName, Type) :
            import numpy as np
            import matplotlib.pyplot as plt
            import codecs
            from os import mkdir

            nschemes = len(RFOs)

            #PARAM
            ntimes = 112
            t = np.zeros((ntimes))
            t[0] = 0.
            dtGraph = 300.
            for i in range(1,ntimes):
                t[i] = t[i-1] + dtGraph

            Volumes = np.zeros((ntimes,nschemes))
            VolumesConserved = np.zeros((ntimes,nschemes))
            fIn = []
            for ff in range(nschemes):
                fIn.append(codecs.open(RFOs[ff],"r",encoding='utf-8'))

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
                    createdString = dataString[3]
                Volumes[i,ff] = float(volumeString)
                VolumesConserved[i,ff] = float(volumeString)+float(createdString)

                fIn[ff].close()

            ##########################ADIM#################################
            T = 33300 #Duration

            t[:] = t[:]/T
            for ff in range(nschemes):
                if Volumes[0, ff] != 0.0:
                    Volumes[:, ff] = Volumes[:, ff]/Volumes[0, ff]
                if VolumesConserved[0, ff] != 0.0:
                    VolumesConserved[:, ff] = \
                            VolumesConserved[:, ff]/VolumesConserved[0, ff]
            ###############################################################

            clr = ['b', 'g', 'r', 'c', 'm', 'y', 'k', 'orange']
            marker = ['*', 'o', '']
            fig = plt.figure()
            for s in range(nschemes):
                if(Type=='Sum'):
                    plt.plot(t,Volumes[:,s],clr[s], marker=marker[s])
                if(Type=='Conserved'):
                    plt.plot(t,VolumesConserved[:,s],clr[s], marker=marker[s])

            plt.legend(legend, fontsize=22, ncol=2, loc=0)
            plt.xlabel("t/T",fontsize = 26)
            plt.ylabel("Volume / Initial Volume",fontsize = 26)
            plt.xticks(fontsize = 26)
            plt.yticks(fontsize = 26)
            fig.tight_layout()
            print(" "*8+"~> Plotting "+figName)
            plt.savefig(figName,format='png',dpi=400)
            plt.close('all')

        #TODO: Check the division by zeor
        VolTime([vnv_1_t3drfo, vnv_3_t3drfo, vnv_5_t3drfo],
                ['LEO POSTMA','CHAR', 'MURD'],
                'img/VolTime.png',
                'Sum')

        VolTime([vnv_1_t3drfo, vnv_3_t3drfo, vnv_5_t3drfo],
                ['LEO POSTMA','CHAR', 'MURD'],
                'img/ConservedVolTime.png',
                'Conserved')

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

        res_vnv_1_t3dhyd = TelemacFile(self.get_study_file('vnv_1:T3DHYD'))
        res_vnv_2_t3dhyd = TelemacFile(self.get_study_file('vnv_2:T3DHYD'))
        res_vnv_3_t3dhyd = TelemacFile(self.get_study_file('vnv_3:T3DHYD'))
        res_vnv_4_t3dhyd = TelemacFile(self.get_study_file('vnv_4:T3DHYD'))
        res_vnv_5_t3dhyd = TelemacFile(self.get_study_file('vnv_5:T3DHYD'))
        res_vnv_6_t3dhyd = TelemacFile(self.get_study_file('vnv_6:T3DHYD'))

        new1 = res_vnv_1_t3dhyd.get_data_value('WATER DEPTH', -1)
        new2 = res_vnv_3_t3dhyd.get_data_value('WATER DEPTH', -1)
        new3 = res_vnv_5_t3dhyd.get_data_value('WATER DEPTH', -1)
        pnew1 = res_vnv_2_t3dhyd.get_data_value('WATER DEPTH', -1)
        pnew2 = res_vnv_4_t3dhyd.get_data_value('WATER DEPTH', -1)
        pnew3 = res_vnv_6_t3dhyd.get_data_value('WATER DEPTH', -1)

        mass_file = path.join(self.get_vnv_working_dir('vnv_1'), 'mass.txt')

        nonRegression('img/SeqPar.txt', mass_file, 'w', new1, pnew1)
        nonRegression('img/SeqPar.txt', mass_file, 'a', new2, pnew2)
        nonRegression('img/SeqPar.txt', mass_file, 'a', new3, pnew3)

        def negCheck(mins,outFile) :
           import numpy as np
           import matplotlib.pyplot as plt
           import codecs

           nschemes=len(mins)
           ntimes=111

           #Write in Out file
           print(" "*8+"~> Writting "+outFile)
           f = codecs.open(outFile,"w",encoding='utf-8')
           f.write('%.2E ' % (mins[0]))
           for s in range(1,nschemes-1):
               f.write('& %.2E ' % (mins[s]))
           f.write('& %.2E \\\\ \n' % (mins[nschemes-1]))


        # Computing min and max for all records and points
        min_h1 = 1.e10
        min_h2 = 1.e10
        min_h3 = 1.e10
        max_h1 = 0.
        max_h2 = 0.
        max_h3 = 0.
        for record in range(111):
            data = res_vnv_1_t3dhyd.get_data_value('WATER DEPTH', record)
            min_h1 = min(min(data), min_h1)
            max_h1 = max(max(data), max_h1)
            data = res_vnv_3_t3dhyd.get_data_value('WATER DEPTH', record)
            min_h2 = min(min(data), min_h2)
            max_h2 = max(max(data), max_h2)
            data = res_vnv_5_t3dhyd.get_data_value('WATER DEPTH', record)
            min_h3 = min(min(data), min_h3)
            max_h3 = max(max(data), max_h3)

        negCheck([min_h1, min_h2, min_h3], 'img/hmins.txt')
        negCheck([max_h1, max_h2, max_h3], 'img/hmaxs.txt')

        # Computing min and max for all records and points
        res_vnv_1_t3dres = TelemacFile(self.get_study_file('vnv_1:T3DRES'))
        res_vnv_3_t3dres = TelemacFile(self.get_study_file('vnv_3:T3DRES'))
        res_vnv_5_t3dres = TelemacFile(self.get_study_file('vnv_5:T3DRES'))
        min_T1 = 1.e10
        min_T2 = 1.e10
        min_T3 = 1.e10
        max_T1 = 0.
        max_T2 = 0.
        max_T3 = 0.
        for record in range(111):
            data = res_vnv_1_t3dres.get_data_value('TRACER 1', record)
            min_T1 = min(min(data), min_T1)
            max_T1 = max(max(data), max_T1)
            data = res_vnv_3_t3dres.get_data_value('TRACER 1', record)
            min_T2 = min(min(data), min_T2)
            max_T2 = max(max(data), max_T2)
            data = res_vnv_5_t3dres.get_data_value('TRACER 1', record)
            min_T3 = min(min(data), min_T3)
            max_T3 = max(max(data), max_T3)

        negCheck([min_T1, min_T2, min_T3], 'img/Tmins.txt')
        negCheck([max_T1, max_T2, max_T3], 'img/Tmaxs.txt')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t3dgeo,
                   plot_mesh=True,
                   fig_size=(20, 3),
                   fig_name='img/mesh')

        # Plotting BOTTOM over polyline over records res_vnv_1_t3dgeo.ntimestep
        vnv_plot1d_polylines(\
                'BOTTOM',
                res_vnv_1_t3dgeo,
                legend_labels="bottom",
                poly=[[0., 50.], [2000., 50.]],
                fig_size=(10, 10),
                fig_name='img/bathymetry1D')


        # Plotting water depth for all schemes for point (0., 50.)
        points = [[0., 50.]]

        data1 = res_vnv_1_t3dhyd.get_timeseries_on_points('WATER DEPTH', points)
        data2 = res_vnv_3_t3dhyd.get_timeseries_on_points('WATER DEPTH', points)
        data3 = res_vnv_5_t3dhyd.get_timeseries_on_points('WATER DEPTH', points)

        fig, ax = plt.subplots(1, 1, figsize=(15, 10))

        plot1d(ax, res_vnv_1_t3dhyd.times, data1[0, :],
               plot_label='LEO POSTMA',
               color='blue')

        plot1d(ax, res_vnv_3_t3dhyd.times, data2[0, :],
               plot_label='CHAR',
               color='orange')

        plot1d(ax, res_vnv_5_t3dhyd.times, data3[0, :],
               plot_label='MURD',
               color='Aqua')

        ax.legend()

        fig_name = 'img/offshoreH'
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        # Plotting velocity u for all schemes for point (0., 50.)
        points = [[0., 50.]]

        data1 = res_vnv_1_t3dhyd.get_timeseries_on_points('VELOCITY U', points)
        data2 = res_vnv_3_t3dhyd.get_timeseries_on_points('VELOCITY U', points)
        data3 = res_vnv_5_t3dhyd.get_timeseries_on_points('VELOCITY U', points)

        fig, ax = plt.subplots(1, 1, figsize=(15, 10))

        plot1d(ax, res_vnv_1_t3dhyd.times, data1[0, :],
               plot_label='LEO POSTMA',
               color='blue')

        plot1d(ax, res_vnv_3_t3dhyd.times, data2[0, :],
               plot_label='CHAR',
               color='orange')

        plot1d(ax, res_vnv_5_t3dhyd.times, data3[0, :],
               plot_label='MURD',
               color='Aqua')

        ax.legend()

        fig_name = 'img/offshoreU'
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        for record, record_val in [(1, 300), (6, 1800), (18, 5400), (48, 14400), (111, 33300)]:

            vnv_plot2d(\
                       'TRACER 1',
                       res_vnv_1_t3dres,
                       plane=5,
                       record=record,
                       cbar_label='Tracer',
                       filled_contours=True,
                       fig_size=(20, 3),
                       fig_name='img/Tracer{}_LEO_plane6'.format(record_val))

        vnv_plot2d(\
                  'TRACER 1',
                  res_vnv_1_t3dres,
                  plane=0,
                  record=111,
                  cbar_label='Tracer',
                  filled_contours=True,
                  fig_size=(20, 3),
                  fig_name='img/Tracer33300_LEO_plane1')

        vnv_plot2d(\
                  'TRACER 1',
                  res_vnv_3_t3dres,
                  plane=5,
                  record=111,
                  cbar_label='Tracer',
                  filled_contours=True,
                  fig_size=(20, 3),
                  fig_name='img/Tracer33300_CHAR_plane6')

        vnv_plot2d(\
                  'TRACER 1',
                  res_vnv_5_t3dres,
                  plane=5,
                  record=111,
                  cbar_label='Tracer',
                  filled_contours=True,
                  fig_size=(20, 3),
                  fig_name='img/Tracer33300_MURD_plane6')

        # Closing files
        res_vnv_1_t3dgeo.close()
        res_vnv_1_t3dhyd.close()
        res_vnv_2_t3dhyd.close()
        res_vnv_3_t3dhyd.close()
        res_vnv_4_t3dhyd.close()
        res_vnv_5_t3dhyd.close()
        res_vnv_6_t3dhyd.close()
        res_vnv_1_t3dres.close()
        res_vnv_3_t3dres.close()
        res_vnv_5_t3dres.close()
