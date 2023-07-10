// TPSet.h: interface for the CTecUtilSet class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_TPSET_H__9BDD3AD0_301C_4296_9A73_EBC74BA83A28__INCLUDED_)
#define AFX_TPSET_H__9BDD3AD0_301C_4296_9A73_EBC74BA83A28__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CTecUtilSet
{
private:
  Set_pa  m_Set;
  
public:
  CTecUtilSet();
  CTecUtilSet(Set_pa set);
  ~CTecUtilSet();

  void Clear();
  void Add(int N);
  Boolean_t IsEmpty() { return TecUtilSetIsEmpty(m_Set); }
  int Count() { return TecUtilSetGetMemberCount(m_Set); }

  Set_pa GetSet();

};

#endif // !defined(AFX_TPSET_H__9BDD3AD0_301C_4296_9A73_EBC74BA83A28__INCLUDED_)
