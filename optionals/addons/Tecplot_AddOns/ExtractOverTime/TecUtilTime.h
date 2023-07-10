// TecUtilTime.h: interface for the CTecUtilTime class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_TECUTILTIME_H__9E994C36_9C90_4B8F_9085_967E0423D54D__INCLUDED_)
#define AFX_TECUTILTIME_H__9E994C36_9C90_4B8F_9085_967E0423D54D__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CTecUtilSet;

class CTecUtilTime  
{
public:
	CTecUtilTime();
	virtual ~CTecUtilTime();
  Strand_t GetMaxStrandID();
  void GetSolutionTimes(vector<double> &SolutionTimes);
  void GetSolutionTimes(Strand_t        StrandID,
                        vector<double> &SolutionTimes);
  CTecUtilSet *GetZonesAtSolutionTime(Strand_t StrandID,
                                      double   SolutionTime);
  Boolean_t ConcatenateZonesIntoStrand(Strand_t        StrandID,
                                       vector<double>  &SolutionTimes,
                                       vector<CTecUtilSet*> &ZoneSets);
  Boolean_t FrameHasTransientData(UniqueID_t FrameID);
protected:
  Boolean_t ZoneAddCopyToStrand(EntIndex_t Zone,
                                Strand_t   StrandID,
                                double     SolutionTime);
};

#endif // !defined(AFX_TECUTILTIME_H__9E994C36_9C90_4B8F_9085_967E0423D54D__INCLUDED_)
