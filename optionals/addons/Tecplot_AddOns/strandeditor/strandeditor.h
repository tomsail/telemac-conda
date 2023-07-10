#ifndef STRANDEDITOR_H_ /* Only include once */
#define STRANDEDITOR_H_

typedef enum
  {
    TimeOptions_SingleValue   = 1, /* Explicitly defined to be used with GUI */
    TimeOptions_ConstantDelta = 2,
    END_TimeOptions_e,
    TimeOptions_Invalid       = BadEnumValue,
  } TimeOptions_e;

typedef enum
  {
    ZoneGrouping_ByTimeStep  = 1, /* Explicitly defined to be used with GUI */
    ZoneGrouping_ByStrandID  = 2,
    END_ZoneGrouping_e,
    ZoneGrouping_Invalid     = BadEnumValue,
  } ZoneGrouping_e;

typedef struct
  {
    Boolean_t         MultipleZonesPerTimestep;
    ZoneGrouping_e    ZoneGrouping;
    LgIndex_t         GroupSize;
  } DataSettings_s;

typedef struct
  {
    Boolean_t         Assign;
    LgIndex_t         Value;
  } StrandIDSettings_s;

typedef struct
  {
    Boolean_t        AssignSolutionTime;
    double           Value;
    double           Delta;
    TimeOptions_e    Option;
  } SolutionTimeSettings_s;

typedef struct
  {
    DataSettings_s          Data;
    StrandIDSettings_s      StrandID;
    SolutionTimeSettings_s  SolutionTime;
    StrandIDSettings_s      ParentZone;
  } GlobalSettings_s;
extern GlobalSettings_s GlobalSettings;

Boolean_t RedefineMultipleZones(const Set_pa                   ZoneSet,
                                const DataSettings_s          &DataSettings,
                                const StrandIDSettings_s      &StrandIDSettings,
                                const SolutionTimeSettings_s  &SolutionTimeSettings,
                                const StrandIDSettings_s      &ParentZoneSettings);
void InitGlobalSettings(void);                           
void UpdateDialog1Values(void);

#endif
