/**************************************

    CStream

    An input class based on KString.
    Allows >> style setting.

***************************************/

#ifndef _H_CSTREAM
#define _H_CSTREAM

#include "KHeader.h"

#include "KString.h"
#include "KRect.h"
#include "KPoint.h"

class CStream : public KString 
{

    protected:

      	uint32			pos;

        virtual bool 	whitespace(char ch);

    public:

        CStream();
        CStream(const int32 newsize);
        CStream(const char *s);
        CStream(const KString &other);
        CStream(const CStream &other);

        char 			curchar(void); 
        char        	nextchar(void);
        char 			prevchar(void) { return pos ? m_String[pos - 1] : 0; }
        
        uint32			GetPos(void) { return (pos); }

        virtual int  	eof(void) { return (pos >= m_Length); }
        virtual int     more(void);
        virtual void    reset(void);
        virtual void    skipwhite(void);

        virtual KString copystr(uint32 startPos, uint32 numChars);

        virtual void    scanword(void);
        virtual void    scanopen(void);
        virtual void    scanclose(void);
        virtual void    discard(void);

        virtual CStream&    operator>>(KString &dest);
        virtual CStream&    operator>>(CStream& (*_f)(CStream &));
        virtual CStream&    operator>>(int16 &dest);
        virtual CStream&    operator>>(int32 &dest);
        virtual CStream&	operator>>(uint32 &dest);
        virtual CStream&    operator>>(double &dest);
        virtual CStream&    operator>>(KRect &r);
        virtual CStream&    operator>>(KPoint &pt);
};

inline CStream& open(CStream &src) { src.scanopen(); return src; }
inline CStream& close(CStream &src) { src.scanclose(); return src; }
inline CStream& discard(CStream &src) { src.discard(); return src; }

#endif
