//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999, Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// KString.h : 
//

#if !defined (_KString_h_)
#define _KString_h_

#include <iostream.h>

#include "KCommon.h"
#include "KObject.h"

typedef enum {KStringDate, KStringNumber, KStringString} StringType;

class KString : public KObject 
{
	public:
				KString();
				KString(const uint32 inSize);
				KString(const char *inStr);
				KString(const KString &inStr);

		virtual ~KString();

		uint32	Length() const;
		uint32	Size() const;
		void	Update();

		StringType	Type();

		bool	Equal(const char *inStr, bool inCaseSens = true);
		int		Compare(const char *inStr, bool inCaseSens = true) const;
		int		TypeCompare(KString &inStr, bool inCaseSens = false);

		int		operator < (const KString &inStr) const;
		int		operator > (const KString &inStr) const;
		int		operator <= (const KString &inStr) const;
		int		operator >= (const KString &inStr) const;
		int		operator == (const KString &inStr) const;
		int		operator == (const char *inStr) const;
		int		operator != (const KString &inStr) const;

		bool	StartsWith(const char *inStr, bool inCaseSens = true);
		bool	EndsWith(const char inCh, bool inCaseSens = true);
		bool	Contains(const char *inStr, bool inCaseSens = true);
		int32	Find(const char inCh, int32 inStartPos = 0, bool inCaseSens = true);
		int32	Find(const char *inStr, int32 inStartPos = 0, bool inCaseSens = true);

		bool	IsEmpty();
		void	Empty();

		void	RTrim(void);
		void	LTrim(void);

		KString	Mid(uint32 inStart, int32 inLen = -1);

		void	MakeLower();
		void	MakeUpper();
		KString	GetLower() const;
		KString	GetUpper() const;

		void	Resize(uint32 size);
		
		char	operator [] (uint32 inPos) const;
		char	operator () (uint32 inPos) const;

		const char	*GetString() const;
		char		*GetBuffer();

		operator const char *() const;
		operator int16();
		operator uint16();
		operator int32();
		operator uint32();
		operator double();

		KString &operator = (const char *inStr);
		KString &operator = (const KString &inStr);
		KString &operator = (const int32 inNum);
		KString &operator = (const uint32 inNum);
		KString &operator = (const int16 inNum);
		KString &operator = (const uint16 inNum);
		KString &operator = (const double inNum);
		KString &operator += (const char *inStr);
		KString &operator += (const KString &inStr);
		KString &operator += (const char inCh);
		KString &operator += (const long inNum);

		friend KString operator + (const KString &inStr1, const char *inStr2);
		friend KString operator + (const KString &inStr1, const KString &inStr2);
		friend KString operator + (const KString &inStr1, char inCh);

		// stream input/output methods
        friend ostream & operator << (ostream &inStream, const KString &inStr);
        friend istream & operator >> (istream &inStream, KString &inStr);

	protected:

		uint32		m_Length;
		uint32		m_Size;
		char		*m_String;

		bool		IsNumber();
		bool		IsDate();
		void		DoubleToString(double inNum, char *inStr);
		void		IntToString(int32 inNum, char *inStr);
		void		UIntToString(uint32 inNum, char *inStr);
};

#endif // _KString_h_

/*
 $Log$
 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
