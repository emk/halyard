/*****************************

	String class for 5L.

*****************************/

#ifndef _H_CSTRING
#define _H_CSTRING

#include <string.h>
#include "util.h"
#include "CObject.h"
#include "CTextFileStream.h"

enum DataType 
{
	isDate,
	isNumber,
	isString
};

class CString : public CObject 
{
 	protected:
		uint32		len;
		uint32		arraySize;
		char		*ptext;

	public:

		CString();
		CString(const uint32 newsize);
		CString(const char *s);
		CString(const CString &other);

		DataType	Type();

		char	*GetString() const { return ptext; }
		int		Equal(const CString &other, bool caseSens = TRUE);
		int		Equal(const char *other, bool caseSens = TRUE);
		int		Compare(const CString &other, bool caseSens = TRUE);
		int		Compare(const char *other, bool caseSens = TRUE);
		int		TypeCompare(CString &other, bool caseSens = FALSE);
		int		StartsWith(const CString &other, bool caseSens = TRUE);
		int		StartsWith(const char *other, bool caseSens = TRUE);
		bool	Contains(const CString &other, bool caseSens = TRUE);
		bool	Contains(const char *other, bool caseSens = TRUE);

		void	makelower() { strlower(ptext); }
		bool	empty() { return (len == 0); }
		void	resize(uint32 newlen);
		void	update() { len = strlen(ptext); }
		CString	substr(uint32 subpos, uint32 sublen);
		int		length() { return len; }
		char	getch(uint32 x) const;

		void	Decrypt(const char cipher);

		operator char *() { return ptext; }
		operator int16();
		operator uint16();
		operator int32();
		operator uint32();
		
		operator double();

		CString &operator=(const char *s);
		CString &operator=(const CString &other);
		CString &operator=(const int32 lnum);
		CString &operator=(const uint32 lnum);
		CString &operator=(const int16 num);
		CString &operator=(const uint16 num);
		CString &operator=(const double dnum);
		CString &operator+=(const char *s);
		CString &operator+=(const CString &other);
		CString &operator+=(const char ch);
		CString &operator+=(const long lnum);


		virtual ~CString();

		friend CString operator+ (const CString &s1, const char *s2);
		friend int operator== (const CString &s1, const char *s2);

	protected:

		int		IsNumber();
		int		IsDate();
		void	DoubleToString(double dnum, char *s);
};

CTextFileStream& operator>>(CTextFileStream &source, CString &dest);
CTextFileStream& operator<<(CTextFileStream &dest, CString &source);

#endif
