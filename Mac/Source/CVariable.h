/*****************************

	CVariable.h

	Variable support for 5L

*****************************/

#ifndef _H_CVARIABLE
#define _H_CVARIABLE

#include "THeader.h"

#include "TBTree.h"

#define DT_LONGDATE		1
#define DT_DATE			2
#define DT_TIME			3
#define DT_YEAR			4
#define DT_MONTH		5
#define DT_LONGMONTH	6
#define DT_DAY			7
#define DT_LONGDAY		8

BEGIN_NAMESPACE_FIVEL

//
//	This is a variable. It consists of the name and the contents
//	of the variable. It also knows how to convert to a number of
//	different formats.
//
class CVariable : public TBNode 
{
	public:
					CVariable(const char *name, const char *data = 0);
		virtual 	~CVariable() {}

		const char	*GetString(void) { return ((const char *) contents); }
		long		GetLong(void) { return ((long) contents); }
		uint32		GetULong(void) { return ((uint32) contents); }
		double		GetDouble(void) { return ((double) contents); }

		void		SetString(const char *data) { contents = data; }
		void		SetLong(const long data) { contents = data; }
		void		SetULong(const uint32 data) { contents = data; }
		void		SetDouble(const double data) { contents = data; }
		void		SetDate(uint32 date, int32 date_type);
		
	protected:
		TString		contents;
};

//
//	This class manages a binary tree of variables. Commands to
//	set and get variable values pass through this manager.
//
class CVariableManager : public TBTree 
{
	private:

		CVariable	*localroot;
		CVariable	*special;

		bool		IsSpecial(const char *name);

	public:
		
				CVariableManager();
		virtual ~CVariableManager();

		const char 	*GetString(const char *name);
		long 	GetLong(const char *name);
		double	GetDouble(const char *name);

		CVariable *FindVariable(const char *inName, bool inReading = TRUE);
		void	SetString(const char *name, const char *contents);
		void	SetLong(const char *name, const long contents);
		void	SetDouble(const char *name, const double contents);
		void	SetDate(const char *name, uint32 date, int32 date_type);
		
		CVariable *GetLocal();
		void	 SetLocal(CVariable *newroot);
};

extern CVariableManager gVariableManager;

END_NAMESPACE_FIVEL

#endif
