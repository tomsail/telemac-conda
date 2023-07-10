// TecUtilSet.cpp: implementation of the CTecUtilSet class.
//
//////////////////////////////////////////////////////////////////////
#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TecUtilSet.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

/////////////////////////////////////
// Wrapper for a TecUtilSet
/////////////////////////////////////
CTecUtilSet::CTecUtilSet()
{
  m_Set = TecUtilSetAlloc(TRUE);
}

CTecUtilSet::CTecUtilSet(Set_pa set)
{
  m_Set = set;
}

CTecUtilSet::~CTecUtilSet()
{
  TecUtilSetDealloc(&m_Set);
}

void CTecUtilSet::Clear()
{
  TecUtilSetClear(m_Set);
}

void CTecUtilSet::Add(int N)
{
  TecUtilSetAddMember(m_Set,(SetIndex_t)N,TRUE);
}

Set_pa CTecUtilSet::GetSet()
{
  return m_Set;
}