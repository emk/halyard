/*  CVariable.cp

    Code for handling variables within 5L.

*/

#include "THeader.h"

#include <string.h>

#include <time.mac.h>	// looking for _mac_msl_epoch_offset_

#include "KLogger.h"

#include "CMac5LApp.h"

#include "CVariable.h"
#include "CCard.h"
#include "CFiles.h"

#include "Math64.h"

USING_NAMESPACE_FIVEL

//  Initialize the variable.
//
CVariable::CVariable(const char *name, const char *data) : TBNode(name)
{
    if (data)
        contents = data;
}

//
//	SetDate - Set the variable to a string the equals date (in seconds). Use
//		date_type to see what format to put it in.
//
void CVariable::SetDate(uint32 date, int32 date_type)
{	
	char		dateStr[255];
	TString		result;
	char		*strPtr;
	char		*strPtr2;

	// using internal metrowerks constant to convert from unix epoch to mac epoch
	date -= _mac_msl_epoch_offset_;
	
	dateStr[0] = 0;
	
	switch(date_type)
	{
		case DT_LONGDATE:
		case DT_LONGDAY:
		case DT_LONGMONTH:
		case DT_YEAR:
			::DateString(date, longDate, (unsigned char *) dateStr, nil);
			break;
		case DT_DATE:
		case DT_DAY:
		case DT_MONTH:
			::DateString(date, shortDate, (unsigned char *) dateStr, nil);
			break;
		case DT_TIME:
			::TimeString(date, false, (unsigned char *) dateStr, nil);
			break;
	}
	
	P2CStr((unsigned char *) dateStr);
	
	switch (date_type)
	{
		case DT_MONTH:
			// the month number is the set of characters before the first slash
			strPtr = strchr(dateStr, '\/');
			if (strPtr != NULL)
			{
				*strPtr = '\0';
				result = (const char *) dateStr;
			}
			break;
		case DT_DAY:
			// the day number is the set of characters between the slashes
			strPtr = strchr(dateStr, '\/');
			if (strPtr != NULL)
			{
				strPtr++;
				strPtr2 = strchr(strPtr, '\/');
				if (strPtr2 != NULL)
				{
					*strPtr2 = '\0';
					result = (const char *) strPtr;
				}
			}
			break;
		case DT_YEAR:
			// the year is the set of characters after the last comma
			strPtr = strrchr(dateStr, ',');
			if (strPtr != NULL)
			{
				strPtr++;
				if (*strPtr == ' ')
					strPtr++;
				result = (const char *) strPtr;
			}
			break;
		case DT_LONGDAY:
			// the day is the set of characters before the first comma
			strPtr = strchr(dateStr, ',');
			if (strPtr != NULL)
			{
				*strPtr = '\0';
				result = (const char *) dateStr;
			}
			break;
		case DT_LONGMONTH:
			// the month is the set of characters after the first comma until a space
			strPtr = strchr(dateStr, ',');
			if (strPtr != NULL)
			{
				strPtr++;
				if (*strPtr == ' ')
					strPtr++;
				strPtr2 = strchr(strPtr, ' ');
				if (strPtr2 != NULL)
				{
					*strPtr2 = '\0';
					result = (const char *) strPtr;
				}
			}
			break;
		default:
			result = (const char *) dateStr;
			break;
	}
			
	SetString(result.GetString());		
}	

/********************************

    VARIABLE MANAGER ROUTINES

********************************/

//  Initialize the local variable tree.
//
CVariableManager::CVariableManager() : TBTree()
{
    localroot = 0;

    //  The special variable is not in the tree so its name won't
    //  conflict with a 5L variable called "Special".
    //
    special = new CVariable("Special");
}

//  Normally the local tree will never exist. But in case we
//  exit in the middle of a macro (due to syntax, etc) we should
//  clean up.
//
CVariableManager::~CVariableManager()
{
    delete localroot;   //  Does nothing if 0.
    delete special;
}

/***********************************************************************
 * Function: CVariableManager::GetString
 *
 *  Parameter name
 * Return:
 *  Value of "name" as a string.
 * Comments:
 *  
 ***********************************************************************/
const char *CVariableManager::GetString(const char *name)
{
    CVariable    *var;

    var = FindVariable(name, TRUE);
    return (var->GetString());
}

/***********************************************************************
 * Function: CVariableManager::GetLong
 *
 *  Parameter name
 * Return:
 *  value of "name" as a long.
 * Comments:
 *
 ***********************************************************************/
long CVariableManager::GetLong(const char *name)
{
    CVariable    *var;

    var = FindVariable(name, TRUE);
    return (var->GetLong());
}

/***********************************************************************
 * Function: CVariableManager::GetDouble
 *
 *  Parameter name
 * Return:
 *  value of "name" as a double float.
 * Comments:
 *
 ***********************************************************************/
double CVariableManager::GetDouble(const char *name)
{
    CVariable    *var;

    var = FindVariable(name, TRUE);
    return (var->GetDouble());
}

/***********************************************************************
 * Function: CVariableManager::FindVariable
 *
 *  Parameter inName
 *  Parameter inReading
 * Return:
 *  CVariable name "name" or new variable created.
 * Comments:
 *  Search the tree for the variable. If it's not there create it.
 *  We always create variables if they don't exist. This is so a
 *  variable may be set in the command line (or not set in the
 *  command line) and still used in the script.
 ***********************************************************************/
CVariable *CVariableManager::FindVariable(const char *inName, bool inReading)
{
    CVariable    *theVar;

    //  First see if it is a special variable. If it is and fReading is
    //  false, complain because these are read-only variables. Otherwise
    //  return the special variable, which IsSpecial has set.
    //

	if (IsSpecial(inName))
	{
		if (not inReading)
			gLog.Caution("$%s is a read-only variable.", inName);
		else
			return (special);
	}

    //  Search the local tree.
    //
    if (localroot) 
    {
        theVar = (CVariable *) localroot->Find(inName);
        if (theVar != NULL) 
        	return (theVar);
    }

    //  Now check the global tree. It's ok to fail; we'll create the
    //  variable if it's not there.
    //
    theVar = (CVariable *) Find(inName);
    if (theVar == NULL) 
    {
    	// cbo - have changed behavior so that don't get warning when
    	// reading one that hasn't been set yet - so count on it returning
    	// 0 in this case
    	//
    	//if (inReading)
    		// we are getting the variable but it isn't in the tree, tell the user
    	//	gLog.Caution("Getting Variable <%s> before it has been set", inName);

#ifdef DEBUG
		//if (inReading)
		//	gDebugLog.Log("Getting Variable <%s> before it has been set", inName);
#endif
    	
    	theVar = new CVariable(inName);
    	Add(theVar);
    	
    	// make sure it has a value
    	theVar->SetLong(0);
    }

    return (theVar);
}

/***********************************************************************
 * Function: CVariableManager::SetString
 *
 *  Parameter name
 *  Parameter data
 * Return:
 *
 * Comments:
 *  Set "name" to "data"
 ***********************************************************************/
void CVariableManager::SetString(const char *name, const char *data)
{
    CVariable    *var;

    var = FindVariable(name, FALSE);
    var->SetString(data);
}

//
//	SetDate - Set the variable name to the date held in seconds in data
//		according to flag.
//
void CVariableManager::SetDate(const char *name, uint32 date, int32 date_type)
{
	CVariable	*var;
	
	var = FindVariable(name, FALSE);
	var->SetDate(date, date_type);
}
	
/***********************************************************************
 * Function: CVariableManager::SetLong
 *
 *  Parameter name
 *  Parameter data
 * Return:
 *
 * Comments:
 *  Set "name" to "data"
 ***********************************************************************/
void CVariableManager::SetLong(const char *name, const long data)
{
    CVariable    *var;

    var = FindVariable(name, FALSE);
    var->SetLong(data);
}

/***********************************************************************
 * Function: CVariableManager::SetDouble
 *
 *  Parameter name
 *  Parameter data
 * Return:
 *
 * Comments:
 *  Set "name" to "data"
 ***********************************************************************/
void CVariableManager::SetDouble(const char *name, const double data)
{
    CVariable    *var;

    var = FindVariable(name, FALSE);
    var->SetDouble(data);
}

//
//	IsSpecial - Is this a special variable? If so return its value.
//		By convention, all special variables start with "_".
//
bool CVariableManager::IsSpecial(const char *name)
{
	TString			vname(name);
	TString			str;
	static Str255	dateStr;
	UInt32			timeSecs;
	bool			retValue = false;
	
	vname.MakeLower();
	
	// do a quick check to see if it could be a special variable
	if (name[0] != '_')
		return (false);
	
	if (vname == (char *) "_date")
	{
		dateStr[0] = 0;
		::GetDateTime(&timeSecs);
		
		::DateString(timeSecs, shortDate, dateStr, nil);
		P2CStr(dateStr);
		
		special->SetString((const char *) dateStr);
		retValue = true;
	}
	else if (vname == (char *) "_longdate")
	{
		dateStr[0] = 0;
		::GetDateTime(&timeSecs);
		
		::DateString(timeSecs, longDate, dateStr, nil);
		P2CStr(dateStr);
		
		special->SetString((const char *) dateStr);
		retValue = true;
	}
	else if (vname == (char *) "_time")
	{
		dateStr[0] = 0;
		::GetDateTime(&timeSecs);
		
		::TimeString(timeSecs, TRUE, dateStr, nil);
		P2CStr(dateStr);
		
		special->SetString((const char *) dateStr);
		retValue = true;
	}
	else if (vname == (char *) "_seconds")
	{
		dateStr[0] = 0;
		::GetDateTime(&timeSecs);
		
		// cbo_fix - this has overflowed and we don't care about the absolute value
		//	anyway so we will subtract a very big number
		{
			UInt64	wide;
			
			wide = U64SetU(timeSecs);
			
			// using internal metrowerks constant to convert from mac epoch to unix epoch
			wide += _mac_msl_epoch_offset_;

			timeSecs = U32SetU(wide);
		}
		
		special->SetULong(timeSecs);		
		retValue = true;
	}
	else if (vname == (char *) "_system")
	{
		special->SetString("MacOS");
		retValue = true;
	}
	else if (vname == (char *) "_curcard")
	{
		special->SetString(gCardManager.CurCardName());
		retValue = true;
	}
	else if (vname == (char *) "_prevcard")
	{
		special->SetString(gCardManager.PrevCardName());
		retValue = true;
	}
	else if (vname == (char *) "_eof")
	{
		if (gFileManager.CurFileOpen())
		{
			if (gFileManager.CurFileAtEOF())
				special->SetLong(1);
			else
				special->SetLong(0);
		}
		else
		{
#ifdef DEBUG
			//gDebugLog.Caution("Trying to read _EOF and no file open!");
#endif
			special->SetLong(0);
		}
		retValue = true;
	}
		
	return (retValue);
}

/***********************************************************************
 * Function: CVariableManager::GetLocal
 *
 *  Parameter (null)
 * Return:
 *  root of local tree (for $1 etc. in a macro)
 * Comments:
 *  Methods to manage the local tree used by macros.
 *  NOTE: VariableManager will not clean up local trees!
 *  It is up to whoever makes the tree to maintain it
 *  and delete it.
 ***********************************************************************/
CVariable *CVariableManager::GetLocal()
{
    return localroot;
}

void CVariableManager::SetLocal(CVariable *newlocal)
{
    localroot = newlocal;
}
