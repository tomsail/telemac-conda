#!MC 1100
$!VarSet |MFBD| = '\\salmon\pubusers\ben\strandeditor'
$!ADDONCOMMAND 
  ADDONID = 'Strand Editor' 
  COMMAND = 'ZoneSet=1,4,8-9,12;AssignStrands=TRUE;StrandValue=1;AssignSolutionTime=TRUE;TimeValue=0;TimeOption=ConstantDelta;DeltaValue=1;' 
$!RemoveVar |MFBD|
