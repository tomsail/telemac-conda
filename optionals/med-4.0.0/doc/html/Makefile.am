include $(top_srcdir)/config/automake.common

SUFFIXES = .html .dox

#Oblige l'essai de reconstruction des .dox pour la cible phony html
html: all

#Construit les pages .dox à partir des sources .html (permet d'avoir les références doxygen \page
.html.dox:
	echo  "/*! \page "`echo $@ | @SED@ 's/\(.*\)\..*/\1/'`" "`cat $< | @SED@ -n 's/^<A NAME="____\(.*\)____">$$/\1/p'` > $@
	cat $< | sed '/<!DOCTYPE/,/\/HEAD>/d' | sed 's/<BODY LANG="fr-FR" DIR="LTR">//g' | sed 's,</BODY>,,g' | sed 's,</HTML>,,g' >> $@
	echo "*/" >> $@
#        cat $< | sed '/<!DOCTYPE/,/\/HEAD>/d' | sed '1{s/^.*\(<A.*\/A>\)/\1/g}' | sed 's,</BODY>,,g' | sed 's,</HTML>,,g' >> $@

#cat $< | tail +10 | sed '1,/"LTR">/{s/^.*\(<A.*\/A>\)/\1/g}' | sed 's,</BODY>,,g' | sed 's,</HTML>,,g' >> $@

root.dox: root.html
	echo  "/*!" > $@
	cat $< | sed '/<!DOCTYPE/,/\/HEAD>/d' | sed 's/<BODY LANG="fr-FR" DIR="LTR">//g' | sed 's,</BODY>,,g' | sed 's,</HTML>,,g' >> $@
	echo "*/" >> $@

#docdir = $(datadir)/doc/html

BUILT_SOURCES = \
./faq.dox\
./introduction.dox\
./root.dox\
./user_guide.dox\
./base.dox \
./avances.dox \
./dictionnaire.dox \
./connectivites.dox \
./modele_de_donnees.dox \
./fichier_utilisateur.dox \
./maillage_utilisateur.dox \
./champ_utilisateur.dox \
./scalaire_utilisateur.dox \
./fichier_avances.dox \
./maillage_avances.dox \
./champ_avances.dox

#./tutorial.dox
#./entite_utilisateur.dox
#./equivalence_utilisateur.dox
#./external_links.dox
#./famille_utilisateur.dox
#./include_c.dox
#./include_f.dox
#./joint_utilisateur.dox
#./med_dump.dox
#./medconforme.dox
#./noeud_utilisateur.dox
#./utilitaires.dox
#./medimport.dox
#./installation.dox
#./passageV2.1_V2.2.dox
#./passageV2.2_V2.3.dox
#./exemple_utilisateur.dox
#./point_de_gauss_utilisateur.dox
#./profil_utilisateur.dox

EXTRA_DIST = \
./faq.html\
./introduction.html\
./root.html\
./user_guide.html\
./base.html \
./avances.html \
./dictionnaire.html \
./connectivites.html \
./modele_de_donnees.html \
./fichier_utilisateur.html \
./maillage_utilisateur.html \
./champ_utilisateur.html \
./scalaire_utilisateur.html \
./fichier_avances.html \
./maillage_avances.html \
./champ_avances.html

#./tutorial.html
#./entite_utilisateur.html
#./equivalence_utilisateur.html
#./external_links.html
#./famille_utilisateur.html
#./include_c.html
#./include_f.html\
#./joint_utilisateur.html
#./med_dump.html
#./medconforme.html
#./modele_de_donnees.html
#./noeud_utilisateur.html
#./utilitaires.html
#./medimport.html
#./installation.html
#./passageV2.1_V2.2.html
#./passageV2.2_V2.3.html
#./exemple_utilisateur.html
#./point_de_gauss_utilisateur.html
#./profil_utilisateur.html

#Doxygen regénère ses propres pages .html (1.7.1)
#ou recopie lui-même les pages .html (1.6.1)
#La ligne suivante n'est donc pas necessaire.
#doc_DATA = $(EXTRA_DIST)

CLEANFILES = \
./faq.dox\
./introduction.dox\
./root.dox\
./user_guide.dox\
./base.dox \
./avances.dox \
./dictionnaire.dox \
./connectivites.dox \
./modele_de_donnees.dox \
./fichier_utilisateur.dox \
./maillage_utilisateur.dox \
./champ_utilisateur.dox \
./scalaire_utilisateur.dox \
./fichier_avances.dox \
./maillage_avances.dox \
./champ_avances.dox \
./root.html

#./tutorial.dox
#./entite_utilisateur.dox
#./equivalence_utilisateur.dox
#./external_links.dox
#./famille_utilisateur.dox
#./include_c.dox
#./include_f.dox
#./joint_utilisateur.dox
#./med_dump.dox
#./medconforme.dox
#./modele_de_donnees.dox
#./noeud_utilisateur.dox
#./utilitaires.dox
#./medimport.dox
#./installation.dox
#./passageV2.1_V2.2.dox
#./passageV2.2_V2.3.dox
#./exemple_utilisateur.dox
#./point_de_gauss_utilisateur.dox
#./profil_utilisateur.dox

# Ces dépendances sont inutiles, elles sont bien gérées par make
# ./faq.dox                  : ./faq.html
# ./introduction.dox	   : ./introduction.html
# ./root.dox		   : ./root.html
# ./user_guide.dox	   : ./user_guide.html
# ./base.dox		   : ./base.html
# ./avances.dox		   : ./avances.html
# ./dictionnaire.dox	   : ./dictionnaire.html
# ./connectivites.dox	   : ./connectivites.html
# ./modele_de_donnees.dox	   : ./modele_de_donnees.html
# ./fichier_utilisateur.dox  : ./fichier_utilisateur.html
# ./maillage_utilisateur.dox : ./maillage_utilisateur.html
# ./champ_utilisateur.dox	   : ./champ_utilisateur.html
# ./scalaire_utilisateur.dox : ./scalaire_utilisateur.html
# ./fichier_avances.dox	   : ./fichier_avances.html
# ./maillage_avances.dox	   : ./maillage_avances.html
# ./champ_avances.dox        : ./champ_avances.html
