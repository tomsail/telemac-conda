dnl  This file is part of MED.
dnl
dnl  COPYRIGHT (C) 1999 - 2019  EDF R&D, CEA/DEN
dnl  MED is free software: you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published by
dnl  the Free Software Foundation, either version 3 of the License, or
dnl  (at your option) any later version.
dnl
dnl  MED is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl  GNU Lesser General Public License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with MED.  If not, see <http://www.gnu.org/licenses/>.
dnl

AC_DEFUN([MED_CHECK_TYPEOF_INT64], [

## Guess where as int64 type is provided by --with-int64 option
## or auto-detection must be used

AC_ARG_WITH([int64],
            AC_HELP_STRING([--with-int64=<C type>],
	                   [Use <C type int64_t or long> for int64]),
	    [],
	    [withval=no])


med_have_int64="no"

dnl Si aucune directive n'est donnée, test  quel
dnl type d'entier il faut utiliser pour int64 (par compilation en C)
dnl Pour la cross-compilation, il ne faut pas lancer un test de compilation  
dnl  -> l'utilisateur spécifie le type à utiliser : int64_t ou long
if test x"$withval" = xno
then

#Le type int64_t est défini par la macro si stdint.h n'existe pas et qu'un type entier 64bits en complément à 2 existe sur la plateforme.
#La définition est utilisée dans confdefs.h pour les macro suivantes.
  AC_TYPE_INT64_T
#echo "------------------- ac_cv_c_int64_t" : $ac_cv_c_int64_t"  ---------------------"
# Attention : med_config.h n'est pas directement inclu dans med.h (de façon volontaire pour séparer headers publiques et privés ).
#Il faut allors utiliser une variable de substitution pour inclure dans le header med.h.in la définition du type int64_t s'il n'est pas disponible dans stdint.h.
#DONE : 1) Tester en mode 32 bits avec le compilateur : -m32 (mais un long long 64bits existe)
#TODO : 2) Tester dans un système 32 bits avec proc 32bits

  case "x$ac_cv_c_int64_t" in
     "xyes")
	MED_INT64="int64_t"
      ;;	
     "xno")
	MED_INT64="med_int"
        AC_MSG_WARN([There is no 64bits signed integer type, writing/reading MED_INT64 fields will not be possible.])
      ;;	
     *)
	MED_INT64="$ac_cv_c_int64_t"
        AC_MSG_WARN([A 64bits signed integer type has been found ($ac_cv_c_int64_t) and will be used for defining int64_t type in med.h and med_config.h.])
      ;;	
  esac

#Si int64_t est défini, positionne la définition PP HAVE_INT64 (pour l'option de compilation swig -DSWIGWORDSIZ6E4 )
#En fait swig se base sur __WORDSIZE == 32/i386 ou 64/x64 malheureusemnt même un linux32 sur proc64 où int64_t existe (long long) swig ne prend pas en charge les int64 car il n'accepte pas l'option SWIGWORDSIZE64
  test "x$ac_cv_c_int64_t" = "xno" && med_have_int64="no" || med_have_int64="yes" 
  AC_CHECK_SIZEOF(long)
  AC_CHECK_SIZEOF(long long)
  test "x$ac_cv_sizeof_long" = "x8" && med_swig_int64="yes" || med_swig_int64="no"
  
  test "x$ac_cv_sizeof_long" = "x8" && MED_H5T_INT64="H5T_NATIVE_LONG" &&
    (AC_DEFINE(MED_H5T_INT64,[H5T_NATIVE_LONG],[The hdf type to user for 64bits signed integer type.])) || (
        test "x$ac_cv_sizeof_long_long" = "x8" && MED_H5T_INT64="H5T_NATIVE_LLONG"
         AC_DEFINE(MED_H5T_INT64,[H5T_NATIVE_LLONG],[The hdf type to user for 64bits signed integer type.])
        )

  AC_TYPE_INT32_T
  case "x$ac_cv_c_int32_t" in
     "xyes") MED_INT32="int32_t"  ;;	
     "xno")  MED_INT32="med_int"
        AC_MSG_ERROR([We must found at least a four bytes signed integer type !])
      ;;	
     *)
	MED_INT32="$ac_cv_c_int32_t"
        AC_MSG_WARN([A 32bits signed integer type has been found ($ac_cv_c_int32_t) and will be used for defining int32_t type in med.h and med_config.h.])
      ;;	
  esac

else
  MED_INT64="$withval" 
  AC_MSG_NOTICE([Using type $withval for int64])
  med_have_int64="yes"
  MED_INT32="int"
  #Prévoir un paramétrage
  MED_H5T_INT64="H5T_NATIVE_LONG"
  AC_DEFINE(MED_H5T_INT64,[H5T_NATIVE_LONG],[The hdf type to use for 64bits signed integer type.])
fi

test "x$med_swig_int64" = "xyes" && (AC_DEFINE(MED_SWIG_INT64,[],[The size of a long imply the size of python integer via swig.]))
AM_CONDITIONAL([MED_SWIG_INT64],[test "x$med_swig_int64" = "xyes" ])

test "x$med_have_int64" = "xyes" && (AC_DEFINE(HAVE_INT64,[],[The size of a int64_t, as computed by sizeof.]))
AM_CONDITIONAL([HAVE_INT64],[test "x$med_have_int64" = "xyes" ])

AC_SUBST(MED_INT64)
AC_SUBST(MED_INT32)
    
])
