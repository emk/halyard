/**************************************

    CStream

    An input class based on TString.
    Allows >> style setting.

***************************************/

#ifndef _H_CSTREAM
#define _H_CSTREAM

#include "THeader.h"

#include "TString.h"
#include "TRect.h"
#include "TPoint.h"

BEGIN_NAMESPACE_FIVEL

class CStream : public FiveL::TString 
{
	
    protected:

      	uint32			pos;

        virtual bool 	whitespace(char ch);

    public:

        CStream();
        CStream(const int32 newsize);
        CStream(const char *s);
        CStream(const TString &other);
        CStream(const CStream &other);

        char 			curchar(void); 
        char        	nextchar(void);
        char 			prevchar(void) { return pos ? m_String[pos - 1] : 0; }
        
        uint32			GetPos(void) { return (pos); }

		bool			inEscape(int32 position); 			 
		bool			inEscape(void);

        virtual int  	eof(void) { return (pos >= m_Length); }
        virtual int     more(void);
        virtual void    reset(void);
        virtual void    skipwhite(void);

        virtual TString copystr(uint32 startPos, uint32 numChars);

        virtual void    scanword(void);
        virtual void    scanopen(void);
        virtual void    scanclose(void);
        virtual void    discard(void);

        virtual CStream&    operator>>(TString &dest);
        virtual CStream&    operator>>(CStream& (*_f)(CStream &));
        virtual CStream&    operator>>(int16 &dest);
        virtual CStream&    operator>>(int32 &dest);
        virtual CStream&	operator>>(uint32 &dest);
        virtual CStream&    operator>>(double &dest);
        virtual CStream&    operator>>(TRect &r);
        virtual CStream&    operator>>(TPoint &pt);
};

inline CStream& open(CStream &src) { src.scanopen(); return src; }
inline CStream& close(CStream &src) { src.scanclose(); return src; }
inline CStream& discard(CStream &src) { src.discard(); return src; }

END_NAMESPACE_FIVEL

#endif
