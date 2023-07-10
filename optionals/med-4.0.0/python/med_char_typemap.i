// Gestion des chaînes de caractères
// Les  lignes suivantes evitent à SWIG de mapper les const char * const avec
// les règles cstring_bounded_output des char * const
// Rem : Le paramètre ParamLength n'est utile que pour le char * const (OUT)
%define %med_char_typemaps(Module,ParamName,ParamLength)
%typemap(in,noblock=1) (const char * const ParamName) (int res, char *buf = 0, int alloc = 0)  {
  res = SWIG_AsCharPtrAndSize($input, &buf, NULL, &alloc);
  if (!SWIG_IsOK(res)) {
    %argument_fail(res, "(const char * const)", $symname, $argnum);
  }
  $1 = ($1_ltype) buf;
}
%typemap(freearg,noblock=1,match="in") (const char * const ParamName) {
  if (alloc$argnum == SWIG_NEWOBJ) %delete_array(buf$argnum);
}
%typemap(out,noblock=1) (const char * const ParamName) {
}
//On définit le typemap argout sinon le const char * const est mappé sur char * const pour le argout !
%typemap(argout,noblock=1) (const char * const ParamName) {
}
// typecheck (défaut char *) ?
//OUT :
%cstring_bounded_output(char * const ParamName, ParamLength);
%enddef
