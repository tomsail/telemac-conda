#!/usr/bin/env python3
"""@author TELEMAC-MASCARET Consortium

   @brief Scripts to Manipulate the dictionary using damocles
"""
# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
from os import remove, path, mkdir, system
from argparse import ArgumentParser
import re
# ~~> dependencies towards other pytel/modules
from utils.messages import Messages
from utils.files import get_file_content
from utils.exceptions import TelemacException
from config import add_config_argument, update_config, CFGS
# _____             ________________________________________________
# ____/ MAIN CALL  /_______________________________________________/
#

def eficas_translation(ts_file, new_ts_file, lang):
    """
    Apllying modification to the translation file for eficas

    @param ts_file The ts_file generated by damocles
    @param new_ts_file The modified one
    @param lang Language to be used
    """
    dico_cata_to_label = {}
    dico_cata_to_telemac = {}
    header = '<?xml version="1.0" encoding="utf-8"?>'
    header += '<!DOCTYPE TS><TS version="1.1" language="'+lang+'">'
    header += '<context>\n'
    header += '    <name>@default</name>\n'

    end = '</context>\n</TS>\n'

    pattern_in = re.compile(r'^\s*<source>(?P<ident>.*)</source>\s*$')
    pattern_out = \
            re.compile(r'^\s*<translation>(?P<traduit>.*)</translation>\s*$')
    pattern_in2 = \
            re.compile(r'^\s*<source2>(?P<ident>.*)</source2>\s*$')
    pattern_out2 = \
            re.compile(r'^\s*<translation2>(?P<traduit>.*)</translation2>\s*$')
    liste_maj = []
    liste_maj.append(('for h', 'for H'))
    liste_maj.append(('pour h', 'pour H'))
    liste_maj.append(('for u', 'for U'))
    liste_maj.append(('pour u', 'pour U'))
    liste_maj.append(('of k', 'of K'))
    liste_maj.append(('de k', 'de K'))
    liste_maj.append(('of h', 'of H'))
    liste_maj.append(('de h', 'de H'))
    liste_maj.append(('u and v', 'U and V'))
    liste_maj.append(('u et v', 'U et V'))
    liste_maj.append(('on h', 'on H'))
    liste_maj.append(('sur h', 'sur H'))
    liste_maj.append(('supg', 'SUPG'))
    liste_maj.append(('k and epsilon', 'K and Epsilon'))
    liste_maj.append(('k-epsilon', 'K-Epsilon'))
    liste_maj.append(('gmres', 'GMRES'))
    liste_maj.append(('cgstab', 'CGSTAB'))
    liste_maj.append(('q(z)', 'Q(Z)'))
    liste_maj.append(('z(q)', 'Z(Q)'))
    liste_maj.append(('wgs84', 'WGS84'))
    liste_maj.append(('wgs84', 'UTM'))
    liste_maj.append(('n-scheme', 'N-Scheme'))
    liste_maj.append(('scheme n', 'Scheme N'))
    liste_maj.append(('psi-scheme', 'PSI-Scheme'))
    liste_maj.append((' psi', ' PSI'))
    liste_maj.append(('f(t90)', 'F(T90)'))
    liste_maj.append(('(pa)', '(Pa)'))
    liste_maj.append(('h clipping', 'H clipping'))
    liste_maj.append(('delwaq', 'DELWAQ'))
    liste_maj.append(('tomawac', 'TOMAWAC'))
    liste_maj.append(('chezy', 'CHEZY'))
    liste_maj.append(('hllc', 'HLLC'))
    liste_maj.append(('c-u', 'C-U'))
    liste_maj.append(('c,u,v', 'C,U,V'))
    liste_maj.append(('h,u,v', 'H,U,V'))
    liste_maj.append(('previmer', 'PREVIMER'))
    liste_maj.append(('fes20xx', 'FES20XX'))
    liste_maj.append(('legos-nea', 'LEGOS-NEA'))
    liste_maj.append(('tpxo', 'TPXO'))
    liste_maj.append((' x', ' X'))
    liste_maj.append((' y', ' Y'))
    liste_maj.append(('waf', 'WAF'))
    liste_maj.append(('(w/kg)', '(W/kg)'))
    liste_maj.append(('(j/kg)', '(W/kg)'))
    liste_maj.append(('zokagoa', 'Zokagoa'))
    liste_maj.append(('nikuradse', 'Nikuradse'))
    liste_maj.append(('froude', 'Froude'))
    liste_maj.append(('gauss', 'Gauss'))
    liste_maj.append(('seidel', 'Seidel'))
    liste_maj.append(('leo', 'Leo'))
    liste_maj.append(('postma', 'Postma'))
    liste_maj.append(('crout', 'Crout'))
    liste_maj.append(('okada', 'Okada'))
    liste_maj.append(('jmj', 'JMJ'))
    liste_maj.append(('haaland', 'HAALAND'))
    liste_maj.append(('grad(u)', 'grad(U)'))
    liste_maj.append(('variable z', 'variable Z'))
    liste_maj.append(('variable r', 'variable R'))
    liste_maj.append(('ascii', 'ASCII'))

    with open(ts_file, 'r') as fobj:
        for ligne in fobj.readlines():
            if pattern_in.match(ligne):
                word = pattern_in.match(ligne)
                ident = word.group('ident')
            if pattern_out.match(ligne):
                word = pattern_out.match(ligne)
                traduit = word.group('traduit')
                dico_cata_to_telemac[ident] = traduit
                traduit_main = traduit.lower()
                for trad in liste_maj:
                    traduit = traduit_main.replace(trad[0], trad[1])
                    traduit_main = traduit
                chaine = traduit_main[0].upper() + traduit_main[1:]
                dico_cata_to_label[ident] = chaine
            if pattern_in2.match(ligne):
                word = pattern_in2.match(ligne)
                ident = word.group('ident')
            if pattern_out2.match(ligne):
                word = pattern_out2.match(ligne)
                traduit = word.group('traduit')
                dico_cata_to_telemac[ident] = traduit
                dico_cata_to_label[ident] = traduit

    with open(new_ts_file, 'w') as fobj:
        fobj.write(header)
        for k in dico_cata_to_telemac:
            text = "    <message>\n        <source>"
            text += k
            text += "</source>\n        <translation>"
            text += dico_cata_to_label[k]
            text += "</translation>\n    </message>\n"
            fobj.write(text)
        fobj.write(end)

    system("lrelease %s"%new_ts_file)


def run_damocles(exe_path, parame_file, log_file=''):
    """
       Running the damocles executable

       @param exe_path Path the damocles executable
       @param parame_file Path to the input aprameters file
       @param log_file Redirecting ouput to that file if present
    """
    if not path.exists(exe_path):
        raise TelemacException("You need to compile damocles to use it...")
    # Run Fortran program
    mes = Messages(size=10)
    try:
        if log_file == '':
            print("%s < %s " % (exe_path, parame_file))
            _, code = mes.run_cmd("%s < %s" % (exe_path, parame_file), False)
        else:
            print("%s < %s > %s" % (exe_path, parame_file, log_file))
            _, code = mes.run_cmd("%s < %s > %s" % (exe_path,
                                                    parame_file,
                                                    log_file),
                                  False)
    except OSError as exc:
        raise TelemacException(exc.strerror)
    if code != 0:
        raise TelemacException([\
              {'name':'damocles',
               'msg':'Could not execute damocles'\
                     +'\n\nHere is the log:\n'
                     +'\n'.join(get_file_content(log_file))
              }])

def gen_dump(exe_path, input_dict, output_dict):
    """
       Run damocles to generate a reordered dictionary by rubrique

       @param exe_path Path to the damocles executable
       @param input_dict Input Telemac dictionary
       @param output_dict Resorted dictionary
    """
    param_file = path.join(path.dirname(input_dict), 'damo.par')
    with open(param_file, 'w') as fobj:
        fobj.write('DUMP'+'\n')
        fobj.write(input_dict+'\n')
        fobj.write(output_dict)
    run_damocles(exe_path, param_file)
    remove(param_file)

def gen_dump2(exe_path, input_dict, output_dict):
    """
       Run damocles to generate a reordered dictionary by index/type

       @param exe_path Path to the damocles executable
       @param input_dict Input Telemac dictionary
       @param output_dict Resorted dictionary
    """
    param_file = path.join(path.dirname(input_dict), 'damo.par')
    with open(param_file, 'w') as fobj:
        fobj.write('DUMP2'+'\n')
        fobj.write(input_dict+'\n')
        fobj.write(output_dict)
    run_damocles(exe_path, param_file)
    remove(param_file)


def gen_cata(code_name, exe_path, input_dict, input_dep,
             cata_name, enum_name, ts_path):
    """
    Run damocles to generate an eficas catalogue

    @param code_name (string) Name of the code (telemac2d...)
    @param exe_path (string )Path to the damocles executable
    @param input_dict (string) Input Telemac dictionary
    @param input_dep (string) Input Telemac depnedancies file
    @param cata_name (string) Name of the eficas Catalogue
    @param enum_name (string) Name of the enum for CHOIX
    @param ts_path (string) Path for where the ts file will be generated
    """
    param_file = path.join(path.dirname(input_dict), 'damo.par')
    fancy_module = code_name.lower()
    with open(param_file, 'w') as fobj:
        fobj.write('CATA'+'\n')
        fobj.write(code_name+'\n')
        fobj.write(input_dict+'\n')
        fobj.write(input_dep+'\n')
        fobj.write(cata_name+'\n')
        fobj.write(enum_name+'\n')
        fobj.write(ts_path)
    ts_ori_en = fancy_module+"_cata_name2eng_name.ts"
    ts_ori_fr = fancy_module+"_cata_name2fra_name.ts"
    ts_en = fancy_module+"_labelCataToIhm_en.ts"
    ts_fr = fancy_module+"_labelCataToIhm_fr.ts"
    # Removing files if they exist
    if path.exists(cata_name):
        remove(cata_name)
    if path.exists(enum_name):
        remove(enum_name)
    if path.exists(ts_path+path.sep+ts_en):
        remove(ts_path+path.sep+ts_en)
    if path.exists(ts_path+path.sep+ts_fr):
        remove(ts_path+path.sep+ts_fr)

    run_damocles(exe_path, param_file)
    # Running modification on the ts files
    eficas_translation(path.join(ts_path, ts_ori_en),
                       path.join(ts_path, ts_en), "en")
    eficas_translation(path.join(ts_path, ts_ori_fr),
                       path.join(ts_path, ts_fr), "fr")
    remove(path.join(ts_path, ts_ori_en))
    remove(path.join(ts_path, ts_ori_fr))
    remove(param_file)

    # Dirty Fix for Tracer in enum
    with open(enum_name, 'r', encoding='utf-8') as f:
        tmp = f.read()

    #TODO: Build names automatically from TelemacDico
    # Creating values for all kNOMi variable
    # It will create the entry in the eficas catalog for each case [1-9] et *
    if code_name == 'TELEMAC2D':
        names = [
                ("Ti", "tracer i etc."),
                ]
    elif code_name == 'TELEMAC3D':
        names = [
                ("TAi", "concentrations for tracer i"),
                ("NAXi", "viscosity for tracer i along x axis (m2/s)"),
                ("NAYi", "viscosity for tracer i along y axis (m2/s)"),
                ("NAZi", "viscosity for tracer i along z axis (m2/s)"),
                ]
    elif code_name == 'GAIA':
        names = [
                ("kAi", "fraction of non cohesive sediment of class i, in k layer"),
                ("QSi", "solid transport load of class i"),
                ("CSi", "mass concentration of class i"),
                ("C2DSi", "mass concentration of class i for 2D graphic printouts"),
                ("kES", "thickness of the k layer"),
                ("kCONC", "concentration of bed layer k"),
                ("QSi", "bed load transport rate of sediment of class i"),
                ("C2DSi", "mass concentration of class i for 2D graphic printouts"),
                ("kRi", "fraction of cohesive sediment of class i, in k layer"),
                ("kSi", "mass of non cohesive sediment of class i, in k layer"),
                ("kMi", "mass of cohesive sediment of class i, in k layer"),
                ("SVXi", "sediment i viscosity along x axis (m2/s) - only 3D"),
                ("SVYi", "sediment i viscosity along y axis (m2/s) - only 3D"),
                ("SVZi", "sediment i viscosity along z axis (m2/s) - only 3D"),
                ]
    elif code_name == 'SISYPHE':
        names = [
                ("kAi", "fraction of sediment of class i in layer k"),
                ("QSi", "bed load transport rate of sediment of class i"),
                ("kES", "thickness of the k layer"),
                ("kCONC", "concentration of bed layer k"),
                ("CSi", "concentration volumic or mass concentration for class i"),
                ]
    elif code_name == 'KHIONE':
        names = [
                ("Fi", "CONCENTRATION OF FRAZIL FOR CLASS i"),
                ("Ni", "PARTICLE NUMBER OF FRAZIL FOR CLASS i"),
                ("SFi", "CONCENTRATION OF FRAZIL FOR CLASS i AT SURFACE"),
                ("SNi", "PARTICLE NUMBER OF FRAZIL FOR CLASS i AT SURFACE"),
                ]

    else:
        names = {}

    with open(enum_name, 'w') as f:

        for name, desc in names:
            list_k = list_i = ['*']
            if name[0] == 'k':
                list_k.extend([str(i) for i in range(1, 20)])
                list_k.append('**')
            elif name[-1] == 'i':
                list_i.extend([str(i) for i in range(1, 20)])
                list_i.append('**')
            else:
                raise TelemacException("Name should contain should match [k]?XXX[i]?")

            pattern = """    '{}':"{}",\n""".format(name, desc)
            string = ""
            for k in list_k:
                for i in list_i:
                    string += pattern.replace(" i", i).replace(" k", k)\
                                     .replace("i'", i+"'")\
                                     .replace("'k", "'"+k)
            tmp = tmp.replace(pattern, string)

        f.write(tmp)

    with open(cata_name, 'r', encoding='utf-8') as f:
        tmp = f.read()

    with open(cata_name, 'w') as f:

        for name, desc in names:
            list_k = list_i = ['*']
            if name[0] == 'k':
                list_k.extend([str(i) for i in range(1, 20)])
                list_k.append('**')
            elif name[-1] == 'i':
                list_i.extend([str(i) for i in range(1, 20)])
                list_i.append('**')
            else:
                raise TelemacException("Name should contain should match [k]?XXX[i]?")

            pattern = '"{}",'.format(desc)
            string = ""
            for k in list_k:
                for i in list_i:
                    string += pattern.replace(" i", i).replace(" k", k)
            tmp = tmp.replace(pattern, string)
        f.write(tmp)

    # Trying to import all generated Python to check them
    # Cannot check catalogue as you need eficas environement
    import importlib
    names = ["dicoCasEnToCata", "dicoCasFrToCata", "enum_auto"]
    for name in names:
        module_name = "eficas.{}_{}".format(fancy_module, name)
        print("  ~> Checking: {}".format(module_name))
        my_module = importlib.import_module(module_name)
        if name == "dicoCasEnToCata":
            assert hasattr(my_module, 'dicoCataToEngTelemac')
            assert hasattr(my_module, 'dicoCasEnToCata')
        elif name == "dicoCasFrToCata":
            assert hasattr(my_module, 'dicoCataToFrTelemac')
            assert hasattr(my_module, 'dicoCasFrToCata')
        elif name == "enum_auto":
            assert hasattr(my_module, 'TelemacdicoEn')
            assert hasattr(my_module, 'TelemacdicoFr')
            assert hasattr(my_module, 'DicoCasFrToCata')
            assert hasattr(my_module, 'DicoCasEnToCata')
            assert hasattr(my_module, 'DicoEnumCasFrToEnumCasEn')
        else:
            pass






def gen_latex(exe_path, input_dict, latex_name, lng):
    """
       Run damocles to generate an LaTeX file for the reference manual

       @param exe_path Path to the damocles executable
       @param input_dict Input Telemac dictionary
       @param latex_name Name of the LaTeX file
       @param lng Language of the documentation
    """
    param_file = path.join(path.dirname(input_dict), 'damo.par')
    with open(param_file, 'w') as fobj:
        fobj.write('LATEX'+'\n')
        fobj.write(input_dict+'\n')
        fobj.write(latex_name+'\n')
        fobj.write(lng)
    run_damocles(exe_path, param_file)
    remove(param_file)

def main():
    """
       Main program for the execution of damocles
    """
#   ~~ Reads config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print('\n\nLoading Options and Configurations\n'+'~'*72+'\n')
    parser = ArgumentParser()
    parser = add_config_argument(parser)
    parser.add_argument(\
                  "-m", "--modules",
                  dest="modules",
                  default='',
                  help="specify the list modules, default is "\
                      "taken from config file")
    parser.add_argument(\
                  "--dump",
                  action="store_true",
                  dest="dump",
                  default=False,
                  help="Will dump a reordered dictionary by rubrique")
    parser.add_argument(\
                  "--dump2",
                  action="store_true",
                  dest="dump2",
                  default=False,
                  help="Will dump a reordered dictionary by index")
    parser.add_argument(\
                  "--eficas",
                  action="store_true",
                  dest="eficas",
                  default=False,
                  help="Will generate the eficas Catalogue from the dictionary")
    parser.add_argument(\
                  "--latex",
                  action="store_true",
                  dest="latex",
                  default=False,
                  help="Will generate the LaTeX file for the reference manual")

#   ~~~~ Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    args = parser.parse_args()
    # path to the root


#   ~~~~ Works for all configurations unless specified ~~~~~~~~~~~~~~~
    update_config(args)
    cfg = CFGS.configs[CFGS.cfgname]
#   <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # Defining which modules to use
    if args.modules == '':
        module_list = ['artemis', 'postel3d', 'stbtel', 'sisyphe',
                       'telemac2d', 'telemac3d', 'tomawac', 'waqtel',
                       'gaia', 'khione']
    else:
        module_list = args.modules.split(';')
    # Identify Root value
    CFGS.compute_vnv_info()
    root = CFGS.get_root()
    exe_path = path.join(root, 'builds', CFGS.cfgname,\
                         'bin', 'damocles'+\
                         cfg['SYSTEM']['sfx_exe'])
    # Looping on all modules
    for module in module_list:
        module_path = path.join(root,
                                'sources',
                                module)
        if args.dump:
            input_dict = path.join(module_path, module+".dico")
            output_dict = path.join(module_path, module+"2.dico")
            gen_dump(exe_path, input_dict, output_dict)

        if args.dump2:
            input_dict = path.join(module_path, module+".dico")
            output_dict = path.join(module_path, module+"2.dico")
            gen_dump2(exe_path, input_dict, output_dict)

        if args.eficas:

            # Creating eficas folder and __init__ if it does not exists
            eficas_path = path.join(root, 'scripts', 'python3', 'eficas')
            if not path.exists(eficas_path):
                mkdir(eficas_path)
                with open(path.join(eficas_path, '__init__.py'), 'w') as fobj:
                    fobj.write("#! /usr/bin/env python")
                    fobj.write("# -*- coding: utf-8 -*")

            input_dict = path.join(module_path,
                                   module+".dico")
            input_dep = path.join(module_path,
                                  module+".dico.dep")
            fancy_module = module
            cata_name = path.join(eficas_path,
                                  fancy_module+"_cata_auto.py")
            enum_name = path.join(eficas_path,
                                  fancy_module+"_enum_auto.py")
            ts_path = eficas_path
            gen_cata(module.upper(), exe_path, input_dict, input_dep,
                     cata_name, enum_name, ts_path+path.sep)

        if args.latex:
            input_dict = path.join(module_path, module+".dico")
            latex_name = path.join(root, 'documentation', module, 'reference',\
                                  'latex', 'Corpus.tex')
            # English only
            lng = '2'
            gen_latex(exe_path, input_dict, latex_name, lng)

#   ~~~~ Compile the valiation documentation

    print('\n\n'+'~'*72)

#   ~~~~ Jenkins' success message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print('\n\nMy work is done\n\n')

    sys.exit(0)

if __name__ == "__main__":
    main()
