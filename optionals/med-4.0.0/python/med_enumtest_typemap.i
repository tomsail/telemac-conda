%include "typemaps.i"


%define %SwigPyIteratorDef(Module)
#define SwigPyIterator Module ## _SwigPyIterator
/* cf #881 Multiple modules leads to unhandled exeption */
%include "pyiterators_881.i"
%enddef

//début de la définition de %med_enumtest_typemaps(Module,TypeEnum,Type)
%define %med_enumtest_typemaps(Module,TypeEnum,Type_)

/* #define SwigPyIterator Module ## _SwigPyIterator */
/* /\* cf #881 Multiple modules leads to unhandled exeption *\/ */
/* %include "pyiterators_881.i" */

%include "std_string.i"
%include "std_map.i"

%template(EnumMapType) std::map<int,std::string >;


// définition de la classe C++ pour wrapper le type enum
%{

#include <Python.h>
#include <string>
#include <iostream>
#include <map>

typedef std::map<int, std::string  > EnumMapType;

struct MEDENUM {};

class Type_ : public EnumMapType, public MEDENUM {

  public:
    int _val;
    typedef EnumMapType             map_type;
    typedef map_type::value_type    value_type;

    /* typedef std::map<int,char * const> Get__str__; */
    /* typedef Get__str__::value_type Vt; */
    /* static  Get__str__ _get__str__; */

   Type_():map_type(Type_ ##_init,
		    Type_ ##_init + sizeof(Type_ ##_init)/ sizeof(*Type_ ##_init))
          ,_val(0) {
      // std::cout << "Type_ Constructor (1) : " << this << " [_val:"<<_val<<"]" <<std::endl;
   }
   Type_(int val):map_type(Type_ ##_init,
		          Type_ ##_init + sizeof(Type_ ##_init)/ sizeof(*Type_ ##_init))
                 ,_val(val) {
      //std::cout << "Type_ Constructor (2) : " << this << " [_val:"<<_val<<"]"<< std::endl;
   }
   Type_(const Type_ & foo):map_type(map_type(foo)),_val(foo._val) {
      //std::cout << "Type_ Copy Constructor " << this << " [_val:"<<_val<<"]" << std::endl;
   }
  friend std::ostream & operator <<(std::ostream & os, const Type_ & foo) {
    return os << "[Classe Type_(): _val:"<<foo._val<<"]"<<std::endl;
  }
  Type_ & operator=(const Type_ & foo) {
    map_type & _map((map_type &)*this);

    _map=map_type(foo);
    _val=foo._val;
    //std::cout << "Type_ Operator = :"<<this<< " [_val:"<<_val<<"]" <<std::endl; return *this;
  }

};


  /* Type_::Get__str__ Type_::_get__str__(Type_ ##_init, Type_ ##_init + sizeof(Type_ ##_init)/ sizeof(*Type_ ##_init)); */

%}


//Demande à SWIG de wrapper la classe C++ (pour qu'elle existe dans le module python)
typedef std::map<int, std::string  > EnumMapType;

struct MEDENUM {};

class Type_ : public EnumMapType, public MEDENUM {
public:

  /* typedef int _Key; */
  /* typedef std::string _Tp; */
  /* typedef std::less<int> _Compare; */
  /* typedef std::allocator<std::pair<const int, char*> > _Alloc; */

  int _val;
  typedef EnumMapType              map_type;
  typedef EnumMapType::value_type  value_type;

     /* typedef std::map<int,const char *> Get__str__; */
     /* typedef Get__str__::value_type Vt; */
     /* static  Get__str__ _get__str__; */

     Type_();
     Type_(int val);
     Type_(const Type_ & foo);
};

//Demande à SWIG d'ajouter une méthode pour afficher la valeur de l'enum sous forme de chaine de caractère en python.
%extend Type_ {
   char *__str__() {
       static char tmp[256];
       Type_::map_type & _map((Type_::map_type &)*self);
       printf("-----------------1-----------\n");
       if ($self->find($self->_val) != self->end() )
	 snprintf(tmp,256,"%s", _map[self->_val].c_str());
       else
	 snprintf(tmp,256,"%s", #Type_ ": VALUE OUT OF RANGE!");
       return tmp;
   }
};

//Ce typemap utilise le proxy python généré par swig pour
//renvoyer un object proxy python et non un objet swig
//(car la construction du nouvel objet vient du C pas de python)
%typemap(argout) TypeEnum * {
  PyObject *pmod, *pclass, *pargs, *pinst;
  Py_Initialize();
  /* printf("%s\n","Loading module Module"); */
  pmod   = PyImport_ImportModule("Module");
  if (pmod   == NULL) printf("%s\n","Can't load module Module");
  pclass = PyObject_GetAttrString(pmod,  #Type_ );
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

