%include "typemaps.i"
%include "exception.i"
%include "cstring.i"

//début de la définition de %med_enum_typemaps(Module,TypeEnum,Type)
%define %med_enum_typemaps(Module,TypeEnum,Type)

%exception Type::Type {
  try {
    $action
  } catch(std::range_error& ex) {
    SWIG_exception_fail(SWIG_ValueError, ex.what());
  } catch(...) {
        SWIG_exception_fail(SWIG_RuntimeError,"Unknown exception");
  }
}

// définition de la classe C++ pour wrapper le type enum
%{

#include <Python.h>
#include <iostream>
#include <map>
#include <exception>
  
  class Type {
  public:
    int _val;
    typedef std::map<int,const char * const> Get__str__;
    typedef Get__str__::value_type Vt;
    static  Get__str__ _get__str__;

  Type():_val(0) {
      // std::cout << "Type Constructor (1) : " << this << " [_val:"<<_val<<"]" <<std::endl;
  }
    
  Type(int val):_val(val) {
      //std::cout << "Type Constructor (2) : " << this << " [_val:"<<_val<<"]"<< std::endl;
        Get__str__::iterator it = _get__str__.find( val );
	if ( it == _get__str__.end() ) throw std::range_error(std::string("Type constructor value out of range"));
  };
				    
  Type(const Type & foo):_val(foo._val) {
      //std::cout << "Type Copy Constructor " << this << " [_val:"<<_val<<"]" << std::endl;
  }
  friend std::ostream & operator <<(std::ostream & os, const Type & foo) {
    return os << "[Classe Type(): _val:"<<foo._val<<"]"<<std::endl;
  }
  Type & operator=(const Type & foo) {_val=foo._val;
    //std::cout << "Type Operator = :"<<this<< " [_val:"<<_val<<"]" <<std::endl; return *this;
  }

  };

  // Création De L'objet Type initialisé avec le tableau de pairs Type_init
  Type::Get__str__ Type::_get__str__(Type ##_init, Type ##_init + sizeof(Type ##_init)/ sizeof(*Type ##_init));

%}

//Demande à SWIG de wrapper la classe C++ (pour qu'elle existe dans le module python)
class Type {
public:
  %rename _val val;
     int _val;
     typedef std::map<int,const char * const> Get__str__;
     /* typedef Get__str__::value_type Vt; */
     /* static  Get__str__ _get__str__; */

     Type();
     Type(int val);
     Type(const Type & foo);
//     Type & operator=(const Type & foo);
//     friend std::ostream & operator <<(std::ostream & os, const Type & foo);
};



//Demande à SWIG d'ajouter une méthode pour afficher la valeur de l'enum sous forme de chaine de caractère en python.
%extend Type {
   char *__str__() {
       static char tmp[256];
       if (Type::_get__str__.find($self->_val) != Type::_get__str__.end() )
	 snprintf(tmp,256,"%s", Type::_get__str__[self->_val]);
       else
	 snprintf(tmp,256,"%s", #Type ": VALUE OUT OF RANGE!");
       return tmp;
   }
};

%pythoncode{
Type.__repr__= lambda self: #Type +"("+str(self.val)+")"
}

//Ce typemap utilise le proxy python généré par swig pour
//renvoyer un object proxy python et non un objet swig
//(car la construction du nouvel objet vient du C pas de python)
//TODO : Utiliser SWIG_NewPointerObj
%typemap(argout) TypeEnum * {
  PyObject *pmod, *pclass, *pargs, *pinst;
  Py_Initialize();
  pmod   = PyImport_ImportModule("med.Module");
  if (pmod   == NULL)
    pmod   = PyImport_ImportModule("Module");
  if (pmod   == NULL) {
    printf("%s\n","Can't load module med.Module nor Module");
  }
  pclass = PyObject_GetAttrString(pmod,  #Type );
  if (pclass == NULL) printf("%s\n","Can't get class $1_basetype");
  Py_DECREF(pmod);
  pargs  = Py_BuildValue("(i)",*$1);
  pinst  = PyEval_CallObject(pclass, pargs);
  if (pinst == NULL) printf("%s\n","Can't instanciate class $1_basetype");
  $result=SWIG_Python_AppendOutput($result, pinst);
}

%typemap(in,numinputs=0) TypeEnum * (TypeEnum temp) {
   $1 = &temp;
}

/* %typemap(doc) TypeEnum "v1: $1_name: $1_basetype"; */
/* %typemap(doc) TypeEnum * "v2: $1_name: $1_basetype"; */

%enddef
 //fin de la définition de %med_enum_typemaps(Module,Type)


// UTILISATION :

/* %{ */
/*   /\* typedef MED_MESH_TYPE::Vt Vt1 *\/ */
/*   typedef std::pair<int,const char * const> enum_; */
/*   const enum_ MED_MESH_TYPE_init[] = { */
/*     enum_(0,"MED_UNSTRUCTURED_MESH"), */
/*     enum_(1,"MED_STRUCTURED_MESH"), */
/*     enum_(-1,"MED_UNDEF_MESH_TYPE") */
/*   }; */
/* %} */
/* %med_enum_typemaps(mymed,med_mesh_type,MED_MESH_TYPE) */
/* void blah2(med_mesh_type *); */

/* %{ */
/*   typedef std::pair<int,const char * const> enum_; */
/* %} */

