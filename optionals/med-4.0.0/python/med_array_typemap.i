%include "typemaps.i"
%include "cstring.i"

%{
/*OLD : (ancienne version de med_array_typemap) Doit apparaître avant la définition de traits_from_stdseq */
/*Permet de renvoyer des vecteurs swigés plutôt que des séquences python*/
#define SWIG_PYTHON_EXTRA_NATIVE_CONTAINERS
%}


/* %include "std_vector.i" */

//Macro1 appelée dans %med_array_typemaps
//Définition des opérateurs algébriques
%define %med_array_operator(TypeMed,CCOp1,StlOp1,CCOp2,SwgCharOp)
  std::vector<TypeMed> & operator CCOp1(const std::vector<TypeMed>  & value){
    std::cout << "self   "  << self   << std::endl;
    std::cout << "&value " << &value << std::endl;
    std::transform(self->begin(),self->end(),
		   value.begin(),self->begin(), std::StlOp1<TypeMed>() );
    return *self;
  }

  std::vector<TypeMed> operator CCOp2(const std::vector<TypeMed>  & value) const {
    /*
       %mangle(Type); Le mangling du nom de la méthode semble être générée par le parseur C
     J'ai l'impression que l'on a pas le moyen d'y accéder par une directive.
    */
    std::vector<TypeMed> v=std::vector<TypeMed>(*self);
    std_vector_Sl_##TypeMed##_Sg__operator_S##SwgCharOp##__Se_(&v,value);
    return v;
  }
%enddef
//Fin Macro1

//Macro2 appelée dans %med_array_typemaps (inutilisée)
//Définition des opérateurs de comparaison élément par élément.
%define %med_array_operator2(TypeMed,CCOp1,StlOp1)
    %newobject operator CCOp1;
    /* std::vector<std::StlOp1<TypeMed>::result_type>  */
    std::vector<bool> operator CCOp1(const std::vector<TypeMed>  & value) const {
    /* typedef std::StlOp1<TypeMed>::result_type result_type; */
    std::cout << "self   "  << self   << std::endl;
    std::cout << "&value " << &value << std::endl;
    /* std::vector<result_type> v=std::vector<result_type>(std::max(self->size(),value.size())); */
    std::vector<bool> v=std::vector<bool>(std::max(self->size(),value.size()));
    std::transform(self->begin(),self->end(),
		   value.begin(),v.begin(), std::StlOp1<TypeMed>() );
    return v;
  }
%enddef
//Fin Macro2

//Macro3 appelée dans %med_array_typemaps
//Définition des opérateurs de comparaison.
%define %med_array_operator3(TypeMed,CCOp1,StlOp1)
    bool operator CCOp1(const std::vector<TypeMed>  & value) const {
  return (*self) StlOp1 value;
  }
%enddef


/* %define %med_array_ltop(TypeMed,CCOp1,StlOp1) */
/*     bool operator CCOp1(const std::vector<TypeMed>  & value) const { */
/*    /\*implémentation correcte de < *\/ */
/*     return std::lexicographical_compare(self->begin(),self->end(), */
/*     				      value.begin(),value.end(), std::StlOp1<TypeMed>() ); */
/*   } */
/* %enddef */

/* %define %med_array_leop(TypeMed,CCOp1,StlOp1) */
/*     bool operator CCOp1(const std::vector<TypeMed>  & value) const { */
/*    /\*implémentation correcte de >= *\/ */
/*     return !std::lexicographical_compare(self->begin(),self->end(), */
/* 				      value.begin(),value.end(), std::StlOp1<TypeMed>() ); */
/*   } */
/* %enddef */


%include "std_vector.i"
%template(MEDBOOL) std::vector<bool>;
%{
typedef std::vector<bool> MEDBOOL;
%}
typedef std::vector<bool> MEDBOOL;
%pythoncode{
MEDBOOL.__str__= lambda self: str([x for x in self])
MEDBOOL.__repr__= lambda self: "MEDBOOL("+str([x for x in self])+")"
}

//Définition de %med_array_typemaps
%define %med_array_typemaps(TypeMed,Type,ParamName)

%include "std_vector.i"

%template(Type) std::vector<TypeMed>;

%{
#ifndef Type ## _H
#define Type ## _H
#include <vector>
typedef std::vector<TypeMed> Type;
#endif
%}

typedef std::vector<TypeMed> Type;

%extend std::vector<TypeMed> {

     %med_array_operator(TypeMed,+=,plus,+,a)
     %med_array_operator(TypeMed,-=,minus,-,s)
     %med_array_operator(TypeMed,*=,multiplies,*,m)
     %med_array_operator(TypeMed,/=,divides,/,d)

     %med_array_operator3(TypeMed,<=,<=)
     %med_array_operator3(TypeMed,<,<)
     %med_array_operator3(TypeMed,>,>)
     %med_array_operator3(TypeMed,>=,>=)
     %med_array_operator3(TypeMed,==,==)
     %med_array_operator3(TypeMed,!=,!=)

     /*Developper une fonction map en extension */

  /* L'implémentation suivante n'est pas possible en extend car les méthodes n'existent
     pas dans la classe std::vector mais existe sous forme de fontions template
     globales
  */

  /* std::vector<TypeMed>  operator +(const std::vector<TypeMed>  & value) const { */
  /*   std::cout << "self   "  << self   << std::endl; */
  /*   std::cout << "&value " << &value << std::endl; */
  /*   return std::vector<TypeMed>(*self)+=value; */
  /* } */

}

%pythoncode{
Type.__str__= lambda self: str([x for x in self])
Type.__repr__= lambda self: #Type +"("+str([x for x in self])+")"
}

// Gestion du type const TypeMed * const
// const TypeMed * const (IN) 1/4
%typemap(in, noblock=1) const TypeMed * const ParamName (void * argp = 0, int res = 0) {
  res = SWIG_ConvertPtr($input, &argp,$descriptor(Type &), 0 |  0 );
  if (!SWIG_IsOK(res)) {
    SWIG_exception_fail(SWIG_ArgError(res), "in method '" "$symname" "', argument "
                       "$argnum"" of type '" "Type &""'");
  }
  $1 =(TypeMed * ) ( & ( (*reinterpret_cast< Type * >(argp))[0]) );
}

// const TypeMed * const (IN) 2/4
// pour ne pas activer l'arginit du TypeMed * const pour un const TypeMed * const
// cf ci-dessous "Gestion du type TypeMed * const"
%typemap(freearg,noblock=1) const TypeMed * const ParamName {
}
// const TypeMed * const (IN) 3/4
// pour ne pas activer un out du TypeMed * const (par sécurité)
// cf ci-dessous "Gestion du type TypeMed * const"
%typemap(out,noblock=1) const TypeMed * const ParamName {
}
// const TypeMed * const (IN) 4/4
// pour ne pas activer un argout du TypeMed * const (par sécurité)
// cf ci-dessous "Gestion du type TypeMed * const"
%typemap(argout,noblock=1) const TypeMed * const ParamName {
}

// Gestion du type TypeMed * const
//  TypeMed * const ParamName : OUT 1/4 (l'allocation Type est faite ds Python)
%typemap(in, noblock=1) TypeMed * const ParamName (void * argp = 0, int res = 0, PyObject *o) {
  o=$input;
  res = SWIG_ConvertPtr($input, &argp,$descriptor(Type &), 0 |  0 );
  if (!SWIG_IsOK(res)) {
    SWIG_exception_fail(SWIG_ArgError(res), "in method '" "$symname" "', argument "
                       "$argnum"" of type '" "Type &""'");
  }
  $1 =(TypeMed * ) ( & ( (*reinterpret_cast< Type * >(argp))[0]) );
}

//  TypeMed * const ParamName : OUT 2/4 (l'allocation Type est faite ds Python)
%typemap(freearg) TypeMed * const ParamName {
  Py_INCREF(o$argnum);
  $result=SWIG_Python_AppendOutput($result, o$argnum);
}
// TypeMed * const (OUT) 3/4
// pour ne pas activer un out du TypeMed * const (par sécurité)
%typemap(out,noblock=1)  TypeMed * const ParamName {
}
// TypeMed * const (OUT) 4/4
//On définit le typemap argout sinon le  char * const ParamName (ARRAY) est mappé sur char * const (STRING) pour le argout (en l'abscence d'autres règles argout) !
%typemap(argout,noblock=1)  TypeMed * const ParamName {
}

%enddef

//fin de la définition de %med_array_typemaps(TypeMed,Type)

//Spécialisation pour unsigned char *
// const unsigned char * const (IN) 1/4
%typemap(in, noblock=1) const unsigned char * const value (void * argp = 0, int res = 0) {
  res = SWIG_ConvertPtr($input, &argp,$descriptor(std::vector<med_float> *), 0 |  0 );
  if (SWIG_IsOK(res)) {
    $1 =( unsigned char *  ) ( & ( (*reinterpret_cast< const std::vector<med_float> * >(argp))[0]) );
  }
  if (!SWIG_IsOK(res)) {
    res = SWIG_ConvertPtr($input, &argp,$descriptor(std::vector<med_int> *), 0 |  0 );
    if (SWIG_IsOK(res)) $1 =( unsigned char *  ) ( & ( (*reinterpret_cast< const std::vector<med_int> * >(argp))[0]) );
  }
  if (!SWIG_IsOK(res)) {
    res = SWIG_ConvertPtr($input, &argp,$descriptor(std::vector<med_int32> *), 0 |  0 );
    if (SWIG_IsOK(res)) $1 =( unsigned char *  ) ( & ( (*reinterpret_cast< const std::vector<med_int32> * >(argp))[0]) );
  }
  if (!SWIG_IsOK(res)) {
    res = SWIG_ConvertPtr($input, &argp,$descriptor(std::vector<med_int64> *), 0 |  0 );
    if (SWIG_IsOK(res)) $1 =( unsigned char *  ) ( & ( (*reinterpret_cast< const std::vector<med_int64> * >(argp))[0]) );
  }
  if (!SWIG_IsOK(res)) {
    res = SWIG_ConvertPtr($input, &argp,$descriptor(std::vector<med_float32> *), 0 |  0 );
    if (SWIG_IsOK(res)) $1 =( unsigned char *  ) ( & ( (*reinterpret_cast< const std::vector<med_float32> * >(argp))[0]) );
  }
  if (!SWIG_IsOK(res)) {
    res = SWIG_ConvertPtr($input, &argp,$descriptor(std::vector<long> *), 0 |  0 );
    if (SWIG_IsOK(res)) $1 =( unsigned char *  ) ( & ( (*reinterpret_cast< const std::vector<long> * >(argp))[0]) );
  }

  if (!SWIG_IsOK(res)) {
    SWIG_exception_fail(SWIG_ArgError(res), "in method '" "$symname" "', argument "
                       "$argnum"" of type '"
			"$descriptor(MEDFLOAT) or $descriptor(MEDINT32) or $descriptor(MEDINT64)"
			"'");
  }

}

// const unsigned char * const (IN) 2/4
// pour ne pas activer l'arginit du unsigned char * const (ci-dessous) pour un const unsigned char * const
%typemap(freearg,noblock=1) const unsigned char * const value  {
}
// const unsigned char * const (IN) 3/4
// pour ne pas activer un out du unsigned char * const (par sécurité)
%typemap(out,noblock=1) const unsigned char * const value  {
}
// const unsigned char * const (IN) 4/4
// pour ne pas activer un argout du unsigned char * const (par sécurité)
%typemap(argout,noblock=1) const unsigned char * const value  {
}

// ------------ Traitement pour le type tableau unsigned char * -------------------
//TODO: Mettre en commun le code (OUT) CAR IDEM (IN):
//      Modifier la macro précédente pour utiliser un fragment
//        qui spécialise le traitement pour unsigned char *

//  unsigned char * const : OUT 1/2 (l'allocation Type est faite ds Python)
%typemap(in, noblock=1) unsigned char * const (void * argp = 0, int res = 0, PyObject *o) {
  o=$input;
  res = SWIG_ConvertPtr(o, &argp,$descriptor(std::vector<med_float> *), 0 |  0 );
  if (SWIG_IsOK(res)) {
    $1 =( unsigned char *  ) ( & ( (*reinterpret_cast< const std::vector<med_float> * >(argp))[0]) );
  }
  if (!SWIG_IsOK(res)) {
    res = SWIG_ConvertPtr(o, &argp,$descriptor(std::vector<med_int> *), 0 |  0 );
    if (SWIG_IsOK(res)) $1 =( unsigned char *  ) ( & ( (*reinterpret_cast< const std::vector<med_int> * >(argp))[0]) );
  }
  if (!SWIG_IsOK(res)) {
    res = SWIG_ConvertPtr(o, &argp,$descriptor(std::vector<med_int32> *), 0 |  0 );
    if (SWIG_IsOK(res)) $1 =( unsigned char *  ) ( & ( (*reinterpret_cast< const std::vector<med_int32> * >(argp))[0]) );
  }
  if (!SWIG_IsOK(res)) {
    res = SWIG_ConvertPtr(o, &argp,$descriptor(std::vector<med_int64> *), 0 |  0 );
    if (SWIG_IsOK(res)) $1 =( unsigned char *  ) ( & ( (*reinterpret_cast< const std::vector<med_int64> * >(argp))[0]) );
  }
  if (!SWIG_IsOK(res)) {
    res = SWIG_ConvertPtr(o, &argp,$descriptor(std::vector<med_float32> *), 0 |  0 );
    if (SWIG_IsOK(res)) $1 =( unsigned char *  ) ( & ( (*reinterpret_cast< const std::vector<med_float32> * >(argp))[0]) );
  }
  if (!SWIG_IsOK(res)) {
    res = SWIG_ConvertPtr(o, &argp,$descriptor(std::vector<long> *), 0 |  0 );
    if (SWIG_IsOK(res)) $1 =( unsigned char *  ) ( & ( (*reinterpret_cast< const std::vector<long> * >(argp))[0]) );
  }

  if (!SWIG_IsOK(res)) {
    SWIG_exception_fail(SWIG_ArgError(res), "in method '" "$symname" "', argument "
                       "$argnum"" of type '"
			"$descriptor(MEDFLOAT) or $descriptor(MEDINT32) or $descriptor(MEDINT64)"
			"'");
  }
}

//  unsigned char * const : OUT 2/4 (l'allocation Type est faite ds Python)
%typemap(freearg) unsigned char * const {
  Py_INCREF(o$argnum);
  $result=SWIG_Python_AppendOutput($result, o$argnum);
}
// unsigned char * const (OUT) 3/4
// pour ne pas activer un out du unsigned char * const (par sécurité)
%typemap(out,noblock=1)  unsigned char * const {
}
// unsigned char * const (OUT) 4/4
//On définit le typemap argout sinon le  char * const ParamName (ARRAY) est mappé sur char * const (STRING) pour le argout (en l'abscence d'autres règles argout) !
%typemap(argout,noblock=1)  unsigned char * const {
}
