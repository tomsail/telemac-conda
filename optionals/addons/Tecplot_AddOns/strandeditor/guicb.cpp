#include "TECADDON.h"
#include "ADDGLBL.h"
#include <sstream>
#include <string>
#include "GUIDEFS.h"
#include "strandeditor.h"
#include "macro.h"

void InitGlobalSettings(void)
{
  GlobalSettings.Data.MultipleZonesPerTimestep    = FALSE;
  GlobalSettings.Data.ZoneGrouping                = ZoneGrouping_ByTimeStep;
  GlobalSettings.Data.GroupSize                   = 2;

  GlobalSettings.StrandID.Assign                  = FALSE;
  GlobalSettings.StrandID.Value                   = 1;

  GlobalSettings.SolutionTime.AssignSolutionTime  = FALSE;
  GlobalSettings.SolutionTime.Value               = 0.0;
  GlobalSettings.SolutionTime.Delta               = 1.0;
  GlobalSettings.SolutionTime.Option              = TimeOptions_SingleValue;

  GlobalSettings.ParentZone.Assign                = FALSE;
  GlobalSettings.ParentZone.Value                 = 0;
}

template <class T>
std::string Stringify(T value)
  {
    std::ostringstream o;
    o << value;
    return o.str();
  }

//
//  Update all interface items in the dialog to set sensitivity/visibility
//
void UpdateDialog1Controls(void)
{
  TecUtilLockStart(AddOnID);

  if (TecGUIToggleGet(MultiZone_TOG_D1))
    {
      TecGUISetSensitivity(Grouping_RADIO_D1, TRUE);
      TecGUISetSensitivity(GroupSize_TF_D1, TRUE);
      TecGUILabelSetText(StrandID_LBL_D1, "Base ID:");
    }
  else
    {
      TecGUISetSensitivity(Grouping_RADIO_D1, FALSE);
      TecGUISetSensitivity(GroupSize_TF_D1, FALSE);
      TecGUILabelSetText(StrandID_LBL_D1, "Value:");
    }

  if (TecGUIToggleGet(AssignStrandID_TOG_D1))
    {
      TecGUISetSensitivity(StrandID_TF_D1, TRUE);
    }
  else
    {
      // Desensitize all StrandID controls
      TecGUISetSensitivity(StrandID_TF_D1, FALSE);
    }

  if (TecGUIToggleGet(AssignSolutionTime_TOG_D1))
    {
      TecGUISetSensitivity(TimeOption_RADIO_D1, TRUE);
      TecGUISetSensitivity(SolutionTime_TF_D1, TRUE);
      switch (TecGUIRadioBoxGetToggle(TimeOption_RADIO_D1))
        {
          case TimeOptions_SingleValue:
            {
              // Desensitize the time delta controls
              TecGUISetSensitivity(TimeDelta_LBL_D1, FALSE);
              TecGUISetSensitivity(SolutionTimeDelta_TF_D1, FALSE);
              TecGUILabelSetText(TimeValue_LBL_D1, "Value:");
            } break;
          case TimeOptions_ConstantDelta:
            {
              // Sensitize the time delta controls
              TecGUISetSensitivity(TimeDelta_LBL_D1, TRUE);
              TecGUISetSensitivity(SolutionTimeDelta_TF_D1, TRUE);
              TecGUILabelSetText(TimeValue_LBL_D1, "Initial:");
            } break;
          default:
            CHECK(FALSE);
        }  
    }
  else
    {
      // Desensitize all Solution Time controls
      TecGUISetSensitivity(TimeOption_RADIO_D1, FALSE);
      TecGUISetSensitivity(SolutionTime_TF_D1, FALSE);
      TecGUISetSensitivity(SolutionTimeDelta_TF_D1, FALSE);
    }

  if (TecGUIToggleGet(AssignParent_TOG_D1))
    TecGUISetSensitivity(ParentZone_TF_D1, TRUE);
  else
    TecGUISetSensitivity(ParentZone_TF_D1, FALSE);

  // Apply button only sensitive if one of the assignment toggles is
  // active and at least one zone is selected
  LgIndex_t  *SelectedZoneArray   = NULL;
  LgIndex_t   SelectedZoneCount   = 0;
  TecGUIListGetSelectedItems(ZoneList_MLST_D1,
                             &SelectedZoneArray, &SelectedZoneCount);
  TecUtilArrayDealloc((void**)&SelectedZoneArray);
  if ((GlobalSettings.StrandID.Assign                  ||
       GlobalSettings.SolutionTime.AssignSolutionTime  ||
       GlobalSettings.ParentZone.Assign)               &&
       SelectedZoneCount > 0)
    TecGUISetSensitivity(Apply_BTN_D1, TRUE);
  else
    TecGUISetSensitivity(Apply_BTN_D1, FALSE);


  TecUtilLockFinish(AddOnID);
}

void UpdateDialog1Values(void)
{
  TecUtilLockStart(AddOnID);

  TecGUIToggleSet           (MultiZone_TOG_D1,        GlobalSettings.Data.MultipleZonesPerTimestep);
  TecGUIRadioBoxSetToggle   (Grouping_RADIO_D1,       GlobalSettings.Data.ZoneGrouping);
  TecGUITextFieldSetLgIndex (GroupSize_TF_D1,         GlobalSettings.Data.GroupSize, FALSE);

  TecGUIToggleSet           (AssignStrandID_TOG_D1,   GlobalSettings.StrandID.Assign);
  TecGUITextFieldSetLgIndex (StrandID_TF_D1,          GlobalSettings.StrandID.Value,     FALSE);

  TecGUIToggleSet           (AssignSolutionTime_TOG_D1, GlobalSettings.SolutionTime.AssignSolutionTime);
  TecGUIRadioBoxSetToggle   (TimeOption_RADIO_D1,       GlobalSettings.SolutionTime.Option);
  TecGUITextFieldSetDouble  (SolutionTime_TF_D1,        GlobalSettings.SolutionTime.Value, DoubleFormatString);
  TecGUITextFieldSetDouble  (SolutionTimeDelta_TF_D1,   GlobalSettings.SolutionTime.Delta, DoubleFormatString);

  TecGUITextFieldSetLgIndex (ParentZone_TF_D1, GlobalSettings.ParentZone.Value, FALSE);
  TecUtilLockFinish(AddOnID);
}


//
//  Determine if all the zones in the selected zones array have the same
//  StrandID, Solution Time, and/or Parent Zone.
//
static void DetermineMatchingInfo(const LgIndex_t  *SelectedZoneArray,
                                  LgIndex_t         SelectedZoneCount,
                                  Boolean_t        &StrandIDsMatch,
                                  Boolean_t        &SolutionTimesMatch,
                                  Boolean_t        &ParentZonesMatch)
{
  REQUIRE(IMPLICATION(SelectedZoneCount>0, VALID_REF(SelectedZoneArray)));

  Strand_t    StrandID      = 0;
  double      SolutionTime  = 0.0;
  EntIndex_t  ParentZone    = 0;

  StrandIDsMatch         = TRUE;
  SolutionTimesMatch     = TRUE;
  ParentZonesMatch       = TRUE;

  if (SelectedZoneCount > 0)
    {
      LgIndex_t SelectedZone = SelectedZoneArray[0];

      StrandID      = TecUtilZoneGetStrandID(SelectedZone);
      SolutionTime  = TecUtilZoneGetSolutionTime(SelectedZone);
      ParentZone    = TecUtilZoneGetParentZone(SelectedZone);

      for (EntIndex_t ii = 1; ii < SelectedZoneCount; ii++)
        {
          SelectedZone = SelectedZoneArray[ii];

          if (StrandIDsMatch)
            StrandIDsMatch = TecUtilZoneGetStrandID(SelectedZone) == StrandID;

          if (SolutionTimesMatch)
            SolutionTimesMatch = TecUtilZoneGetSolutionTime(SelectedZone) == SolutionTime;

          if (ParentZonesMatch)
            ParentZonesMatch = TecUtilZoneGetParentZone(SelectedZone) == ParentZone;
        }
    }
}


//
//  Fill the zone list with the names of the current zones in the active data set.
//  Return the number of zones in the data set.
//
EntIndex_t UpdateZoneList(void)
{
  TecUtilLockStart(AddOnID);

  TecGUIListDeleteAllItems(ZoneList_MLST_D1);

  EntIndex_t NumZones = 0;
  TecUtilDataSetGetInfo(NULL, /* Title */
                        &NumZones,
                        NULL); /* NumVars */

  if (NumZones > 0)
    {
      for (EntIndex_t ii = 1; ii <= NumZones; ii++)
        {
          char *ZoneName = NULL;
          if (TecUtilZoneGetName(ii, &ZoneName))
            {
              std::string ZoneNumAndName(Stringify(ii));
              ZoneNumAndName += ": ";
              ZoneNumAndName += ZoneName;

              TecGUIListAppendItem(ZoneList_MLST_D1,
                                   ZoneNumAndName.c_str());
              TecUtilStringDealloc(&ZoneName);
            }
        }
    }

  TecUtilLockFinish(AddOnID);
  return (NumZones);
}
// Copy of above for OnIdle callbacks.
void STDCALL UpdateZoneListOnIdle(ArbParam_t ClientData)
{
  UpdateZoneList();
  ZoneListUpdatePending = FALSE;
}

//
//  Update the StrandID and Solution Time labels based on
//  the current zone selection(s).
//
void UpdateZoneInfo()
{
  TecUtilLockStart(AddOnID);

  LgIndex_t  *SelectedZoneArray   = NULL;
  LgIndex_t   SelectedZoneCount   = 0;
  Boolean_t   StrandIDsMatch      = FALSE;
  Boolean_t   SolutionTimesMatch  = FALSE;
  Boolean_t   ParentZonesMatch    = FALSE;

  TecGUIListGetSelectedItems(ZoneList_MLST_D1,
                             &SelectedZoneArray, &SelectedZoneCount);
  if (SelectedZoneCount > 0)
    {
      DetermineMatchingInfo(SelectedZoneArray,
                            SelectedZoneCount,
                            StrandIDsMatch,
                            SolutionTimesMatch,
                            ParentZonesMatch);

      /* Update the StrandID text field. */
      if (StrandIDsMatch)
        TecGUILabelSetLgIndex(SelectedStrandID_LBL_D1,
                              TecUtilZoneGetStrandID(SelectedZoneArray[0]));
      else
        TecGUILabelSetText(SelectedStrandID_LBL_D1, MultipleString);

      /* Update the Solution Time text field. */
      if (SolutionTimesMatch)
        TecGUILabelSetDouble(SelectedSolutionTime_LBL_D1,
                             TecUtilZoneGetSolutionTime(SelectedZoneArray[0]),
                             DoubleFormatString);
      else
        TecGUILabelSetText(SelectedSolutionTime_LBL_D1, MultipleString);
    }
  else
    {
      TecGUILabelSetText(SelectedStrandID_LBL_D1, "N/A");
      TecGUILabelSetText(SelectedSolutionTime_LBL_D1, "N/A");
    }

  if (SelectedZoneArray != NULL)
    TecUtilArrayDealloc((void**)&SelectedZoneArray);

  TecUtilLockFinish(AddOnID);
}
// Copy of above for OnIdle callbacks.
void STDCALL UpdateZoneInfoOnIdle(ArbParam_t ClientData)
{
  UpdateZoneInfo();
  ZoneListUpdatePending = FALSE;
}


static void Apply_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);

  LgIndex_t  *SelectedZoneArray   = NULL;
  LgIndex_t   SelectedZoneCount   = 0;

  TecGUIListGetSelectedItems(ZoneList_MLST_D1,
                             &SelectedZoneArray, &SelectedZoneCount);
  if (SelectedZoneCount > 0)
    {
      // Convert the array into a set
      Set_pa    ZoneSet = TecUtilSetAlloc(FALSE);
      Boolean_t IsOk    = ZoneSet != NULL;
      for (LgIndex_t ii = 0; ii < SelectedZoneCount && IsOk; ii++)
        IsOk = TecUtilSetAddMember(ZoneSet,
                                   SelectedZoneArray[ii],
                                   FALSE);
      TecUtilArrayDealloc((void **)&SelectedZoneArray);
      if (IsOk)
        IsOk = RedefineMultipleZones(ZoneSet,
                                     GlobalSettings.Data,
                                     GlobalSettings.StrandID,
                                     GlobalSettings.SolutionTime,
                                     GlobalSettings.ParentZone);
      if (IsOk && TecUtilMacroIsRecordingActive())
        {
          std::string macroString;
          BuildMacroCommand(ZoneSet, macroString);
          TecUtilMacroRecordAddOnCommand(ADDON_NAME, macroString.c_str());
        }

      TecUtilSetDealloc(&ZoneSet);
    }
  TecUtilOnIdleQueueAddCallback(UpdateZoneInfoOnIdle, NULL);
  TecUtilLockFinish(AddOnID);
}

static void Dialog1CloseButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecGUIDialogDrop(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}

static void Dialog1Init_CB(void)
{
  TecUtilLockStart(AddOnID);

  UpdateDialog1Values();

  if (UpdateZoneList())
    TecGUIListSelectAllItems(ZoneList_MLST_D1);
  UpdateDialog1Controls();
  UpdateZoneInfo();

  TecGUISetInputFocus(MultiZone_TOG_D1);

  TecUtilLockFinish(AddOnID);
}

static LgIndex_t  StrandID_TF_D1_CB(const char *)
{
  LgIndex_t Value = 0;
  TecUtilLockStart(AddOnID);

  // Must be an integer >= 0
  if (TecGUITextFieldGetLgIndex(StrandID_TF_D1, &Value) &&
      Value >= 0)
    GlobalSettings.StrandID.Value = Value;
  else
    TecUtilDialogErrMsg("StrandID value must be an integer greater than or equal to zero.");
 
  TecGUITextFieldSetLgIndex(StrandID_TF_D1,
                            GlobalSettings.StrandID.Value,
                            FALSE);

  TecUtilLockFinish(AddOnID);
  return (1);
}


static LgIndex_t  GroupSize_TF_D1_CB(const char *)
{
  LgIndex_t Value = 0;
  TecUtilLockStart(AddOnID);

  // Must be an integer >= 2
  if (TecGUITextFieldGetLgIndex(GroupSize_TF_D1, &Value) &&
      Value >= 2)
    GlobalSettings.Data.GroupSize = Value;
  else
    TecUtilDialogErrMsg("Group size must be an integer greater than or equal to 2.");

  TecGUITextFieldSetLgIndex(GroupSize_TF_D1,
                            GlobalSettings.Data.GroupSize,
                            FALSE);

  TecUtilLockFinish(AddOnID);
  return (1);
}


static LgIndex_t  SolutionTime_TF_D1_CB(const char *)
{
  double    Value = 0;
  TecUtilLockStart(AddOnID);

  // Must be a floating point value
  if (TecGUITextFieldGetDouble(SolutionTime_TF_D1, &Value))
    GlobalSettings.SolutionTime.Value = Value;
  else
    TecUtilDialogErrMsg("Solution Time value must be a real number.");

  TecGUITextFieldSetDouble(SolutionTime_TF_D1,
                           GlobalSettings.SolutionTime.Value,
                           DoubleFormatString);

  TecUtilLockFinish(AddOnID);
  return (1);
}


static LgIndex_t  SolutionTimeDelta_TF_D1_CB(const char *)
{
  double    Value = 0;
  TecUtilLockStart(AddOnID);

  // Must be a floating point value > 0
  if (TecGUITextFieldGetDouble(SolutionTimeDelta_TF_D1, &Value) &&
      Value > 0)
    GlobalSettings.SolutionTime.Delta = Value;
  else
    TecUtilDialogErrMsg("Solution Time delta must be a positive number.");

  TecGUITextFieldSetDouble(SolutionTimeDelta_TF_D1,
                           GlobalSettings.SolutionTime.Delta,
                           DoubleFormatString);

  TecUtilLockFinish(AddOnID);
  return (1);
}


static LgIndex_t  ParentZone_TF_D1_CB(const char *)
{
  LgIndex_t Value = 0;
  TecUtilLockStart(AddOnID);

  // Must be an integer >= 0
  if (TecGUITextFieldGetLgIndex(ParentZone_TF_D1, &Value) &&
      Value >= 0)
    GlobalSettings.ParentZone.Value = Value;
  else
    TecUtilDialogErrMsg("Parent zone must be an integer greater than or equal to zero.");

  TecGUITextFieldSetLgIndex(ParentZone_TF_D1,
                            GlobalSettings.ParentZone.Value,
                            FALSE);

  TecUtilLockFinish(AddOnID);
  return (1);
}


static void ZoneList_MLST_D1_CB(const LgIndex_t *)
{
  UpdateZoneInfo();
  UpdateDialog1Controls();
}


static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}


static void TimeOption_RADIO_D1_CB(const LgIndex_t *NewValue)
{
  GlobalSettings.SolutionTime.Option = static_cast<TimeOptions_e>(*NewValue);
  UpdateDialog1Controls();
}

static void MultiZone_TOG_D1_CB(const LgIndex_t *NewValue)
{
  GlobalSettings.Data.MultipleZonesPerTimestep = (*NewValue == 1);
  UpdateDialog1Controls();
}

static void Grouping_RADIO_D1_CB(const LgIndex_t *NewValue)
{
  GlobalSettings.Data.ZoneGrouping = static_cast<ZoneGrouping_e>(*NewValue);
  UpdateDialog1Controls();
}

static void AssignSolutionTime_TOG_D1_CB(const LgIndex_t *NewValue)
{
  GlobalSettings.SolutionTime.AssignSolutionTime = (*NewValue == 1);
  UpdateDialog1Controls();
}

static void AssignStrandID_TOG_D1_CB(const LgIndex_t *NewValue)
{
  GlobalSettings.StrandID.Assign = (*NewValue == 1);
  UpdateDialog1Controls();
}

static void AssignParent_TOG_D1_CB(const LgIndex_t *NewValue)
{
  GlobalSettings.ParentZone.Assign = (*NewValue == 1);
  UpdateDialog1Controls();
}


#include "guibld.cpp"
