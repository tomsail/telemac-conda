#ifndef  ENGINE_H_
#define ENGINE_H_ /* Only include once */


typedef struct  
  {
    char          *FileName; 
    int           CurOffset; 
    int           DataType; 
    int           NumPoints;
  } BlockClientDataValues_s;


typedef struct 
  {
    char          *FileName;
    int           PointOffset;
    int           CurOffset; 
    int           DataType; 
    int           NumPoints;
  } PointClientDataValues_s;

extern Boolean_t StopReadFile;
 


Boolean_t LoadBlock(FILE           *MyFile, 
               FileOffset_t    CurOffset, 
               FieldDataType_e DataType, 
               LgIndex_t       NumPoints, 
               FieldData_pa    FieldData);

Boolean_t STDCALL LoaderCallback(StringList_pa Instructions); /* in 'engine.c' */
void STDCALL LoaderSelectedCallback(void);

typedef enum
{
  LoadMethod_Immediate = 1,
  LoadMethod_Deferred, 
  LoadMethod_AutoLOD,
  LoadMethod_CustomLOD,
  LoadMethod_CustomVar, 
  
  END_LoadMethod_e,
  LoadMethod_Invalid = BadEnumValue
} LoadMethod_e;


#endif /* ENGINE_H_ */
