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

#if !defined (_Variable_h_)
#define _Variable_h_

#include "TBTree.h"

//
//	This is a variable. It consists of the name and the mValue
//	of the variable. It also knows how to convert to a number of
//	different formats.
//
/*-----------------------------------------------------------------

CLASS
    Variable

	A class for representing a 5L variable, which consists of a 
	name/value pair.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class Variable : public TBNode 
{
	public:
		//////////
		// Constructor.
		//
		// [in] inName - name of the variable
		// [in_optional] inValue - initial value (default NULL)
		//
		Variable(const char *inName, const char *inValue = NULL);
		
		//////////
		// Destructor.
		//
		virtual ~Variable() {}

		//////////
		// Get the value of this variable as a character string.
		//
		// [out] return - the value of this variable
		//
		const char	*GetString(void) { return (const char *) mValue; }
		
		//////////
		// Get the value of this variable as a long.
		//
		// [out] return - the value of this variable
		//
		int32	GetLong(void) { return (int32) mValue; }
		
		//////////
		// Get the value of this variable as unsigned long.
		//
		// [out] return - the value of this variable
		//
		uint32	GetULong(void) { return (uint32) mValue; }
		
		//////////
		// Get the value of this variable as a double.
		//
		// [out] return - the value of this variable
		//
		double	GetDouble(void)	{ return (double) mValue; }

		//////////
		// Set the value of this variable.
		//
		// [in] inValue - new value
		//
		void	SetString(const char *inValue) { mValue = inValue; }
		
		//////////
		// Set the value of this variable.
		//
		// [in] inValue - new value
		//
		void	SetString(const TString &inValue) { mValue = inValue; }
		
		//////////
		// Set the value of this variable.
		//
		// [in] inValue - new value
		//
		void	SetLong(const long inValue) { mValue = inValue; }
		
		//////////
		// Set the value of this variable.
		//
		// [in] inValue - new value
		//
		void	SetDouble(const double inValue)	{ mValue = inValue; }
		
		//////////
		// Fill this variable with a date string.
		//
		// [in] inDate - current time in seconds elapsed since 
		//				 midnight (00:00:00), January 1, 1970 
		// [in] inDateType - desired date/time format on of:<br>
		//				(DT_LONGDATE, DT_DATE, DT_TIME, DT_YEAR,
		//				DT_MONTH, DT_LONGMONTH, DT_DAY, DT_LONGDAY)  
		//
		void	SetDate(uint32 inDate, int32 inDateType);

	protected:
		//////////
		// The value of this variable.
		//
		TString		mValue;
		
		//////////
		// Is this varaible read-only?
		//
		bool		mReadOnly;

};

//
//	This class manages a binary tree of variables. Commands to
//	set and get variable values pass through this manager.
//
/*-----------------------------------------------------------------

CLASS
    VariableManager

	Manages a binary tree of Variable objects.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class VariableManager : public TBTree 
{
	public:		
		//////////
		// Constructor.
		//
		VariableManager();
		
		//////////
		// Destructor.
		//
		virtual		~VariableManager();
        
        //////////
		// Remove all variables from the tree.
		//
		void		RemoveAll(void);
        
		//////////
		// Get the value of the specified variable as a character string.
		//
		// [in] inName - name of the variable
		// [out] return - the value of the variable
		//
		const char 	*GetString(const char *inName);
		
		//////////
		// Get the value of the specified variable as a long.
		//
		// [in] inName - name of the variable
		// [out] return - the value of the variable
		//
		long 		GetLong(const char *inName);
		
		//////////
		// Get the value of the specified variable as a double.
		//
		// [in] inName - name of the variable
		// [out] return - the value of the variable
		//
		double		GetDouble(const char *inName);

		//////////
		// Find a variable by name.  If not found, create a new one.
		//
		// [in] inName - name of the variable
		// [in_optional] fReading - read-only access? (default true)
		//
		Variable	*FindVariable(const char *inName, int fReading = true);
		
		//////////
		// Set the value of the specified variable
		//
		// [in] inName - name of the variable
		// [in] inValue - the value
		//
		void		SetString(const char *inName, const char *inValue);
		
		//////////
		// Set the value of the specified variable
		//
		// [in] inName - name of the variable
		// [in] inValue - the value
		//
		void		SetLong(const char *inName, const long inValue);
		
		//////////
		// Set the value of the specified variable
		//
		// [in] inName - name of the variable
		// [in] inValue - the value
		//
		void		SetDouble(const char *inName, const double inValue);
        
		//////////
		// Set the value of the specified variable with a date string.
		//
		// [in] inName - name of the variable
		// [in] inDate - current time in seconds elapsed since 
		//				 midnight (00:00:00), January 1, 1970 
		// [in] inDateType - desired date/time format on of:<br>
		//				(DT_LONGDATE, DT_DATE, DT_TIME, DT_YEAR,
		//				DT_MONTH, DT_LONGMONTH, DT_DAY, DT_LONGDAY)  
		//
		void		SetDate(const char *inName, uint32 inDate, int32 inDateType);
        
		//////////
		// Get the root of the local variable tree (used by macros).
		//
		// [out] return - the root of the local variable tree
		//
		Variable	*GetLocal();
		
		//////////
		// Set the root of a local variable tree (used by macros).
		// Add() should be used to add variables to the local tree. <br>
		// NOTE: VariableManager will not clean up local trees!
		// It is up to whoever makes the tree to maintain it and delete it.
		// 
		// [in] newroot - new root of the local variable tree
		//
		void		SetLocal(Variable *newroot);

	private:
		//////////
		// Root of the local variable tree.
		//
		Variable	*localroot;
		
		//////////
		// Used to store value for special variables.
		//
		Variable	*special;

		//////////
		// Is the specified variable a "special variable"?
		//
		// [in] inName - name of the variable to check
		// [out] return - true if the variable is special, false otherwise
		//
		int			IsSpecial(const char *inName);
};

#endif // _Variable_h_

/*
 $Log$
 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.3  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
