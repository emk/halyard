//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999,2000 Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// KRandom.h : Random number generator. Based on Scott Robert Ladd's code
//		from Components and Algorithms. The RandGen class defines a 
//		psuedo-random number generator that uses the linear congruential 
//		method.
//	Copyright 1992 Scott Robert Ladd. All rights reserved
//

#if !defined (_KRandom_h_)
#define _KRandom_h_

#include "KCommon.h"
#include <time.h>

class KRandom
{
public:
	KRandom();
	KRandom(uint32 seed);

	uint32	operator () (uint32 limit);
	void	SetSeed(uint32 seed = NewSeed());

private:
	static uint32		NewSeed();
	uint32		m_Seed;
};

inline uint32 KRandom::NewSeed()
{
	return ((uint32) time(NULL));
}

inline KRandom::KRandom()
{
	m_Seed = NewSeed();
}

inline KRandom::KRandom(uint32 seed)
{
	m_Seed = seed;
}

inline void KRandom::SetSeed(uint32 seed /* = NewSeed() */)
{
	m_Seed = seed;
}

//--------------------------------------------------------
// NOTE:
//      This generator depends upon overflow. It will NOT
//      work correctly in an environment where integer
//      arithmetic generates an overflow exception.
//--------------------------------------------------------
//
uint32 KRandom::operator () (uint32 limit)
{
	// get next seed value
	m_Seed = m_Seed * 5709421UL + 1UL;

	// return value from 0 to (lim - 1)
	return (uint32)((m_Seed >> 16UL) % limit);
}

#endif // _KRandom_h_

/*
 $Log$
 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
