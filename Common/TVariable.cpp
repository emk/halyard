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
// Variable.cpp : Code for handling variables within 5L.
//
//

#include "stdafx.h"

#include "LUtil.h"
//#include "LFiles.h"

#include "Variable.h"
#include "Card.h"
#include "Globals.h"

//  Initialize the variable.
//
Variable::Variable(const char *inName, const char *inValue) : TBNode(inName)
{
    if (inValue != NULL)
        mValue = inValue; 
	else
		mValue = (int32) 0;
}

void Variable::SetDate(uint32 inDate, int32 inDateType)
{
	TString		theDate;
	
	::SetDate(theDate, inDate, inDateType);
	SetString(theDate.GetString());
}

/********************************

    VARIABLE MANAGER ROUTINES

********************************/

//  Initialize the local variable tree.
//
VariableManager::VariableManager() : TBTree()
{
    localroot = NULL;

    //  The special variable is not in the tree so its name won't
    //  conflict with a 5L variable called "Special".
    //
    special = new Variable("Special");
}

//  Normally the local tree will never exist. But in case we
//  exit in the middle of a macro (due to syntax, etc) we should
//  clean up.
//
VariableManager::~VariableManager()
{ 
	RemoveAll();

	if (localroot != NULL)
	{
		localroot->RemoveAll(localroot);
		delete localroot;
	}

	if (special != NULL)
		delete special;
}

//
//	RemoveAll - Wipe out all the nodes.
//
void VariableManager::RemoveAll(void)
{
	if (localroot != NULL)
	{
		localroot->RemoveAll(localroot);
		delete localroot;
		localroot = NULL;
	}
	
	// don't delete special - there is no reason to
	
	TBTree::RemoveAll();
}

	
	
/***********************************************************************
 * Function: VariableManager::IsSpecial
 *
 *  Parameter name
 * Return:
 *  1 if special, 0 else.
 * Comments:
 *  Determine if the given variable is a "special" variable. If it is
 *  set the value of the special instance variable to the proper value.
 *  Return whether or not the name is a special variable.
 *  
 *  Special variables:
 *
 *      date        Current date in form 08/21/93
 *      longdate    Current date in form January 22, 1992
 *      time        Current time in form 12:34pm
 ***********************************************************************/
int VariableManager::IsSpecial(const char *name)
{
    TString     vname(name);
    TString     str;
 //   LDate       theDate;

    vname.MakeLower();
    
    // do a quick check to see if it could be a special variable
	if (not vname.StartsWith("_", false))
    	return (false);

	if (vname.Equal("_date"))
    {
    	GetDate(str, df_DATE);
    	
        //str = theDate.GetDate(df_DATE);
        special->SetString(str);
        return true;
    } 
	else if (vname.Equal("_longdate")) 
    {
    	GetDate(str, df_LONGDATE);
        
        //str = theDate.GetDate(df_LONGDATE);
        special->SetString(str);
        return true;
    } 
    else if (vname.Equal("_time")) 
    {
    	GetDate(str, df_TIME);
    	
        //str = theDate.GetDate(df_TIME);
        special->SetString(str);
        return true;
    } 
    else if (vname.Equal("_seconds"))
    {
    	GetDate(str, df_SECONDS);
    	
        //str = theDate.GetDate(df_SECONDS);
        special->SetString(str);
        return true;
    } 
    else if (vname.Equal("_system"))
    {
        special->SetString(gSysInfo.ShortString());
        return true;
    }
    else if (vname.Equal("_curcard"))
    {
    	special->SetString(gCardManager.CurCardName());
    	return (true);
    }
    else if (vname.Equal("_prevcard"))
    {
    	special->SetString(gCardManager.PrevCardName());
    	return (true);
    }
    else if (vname.Equal("_eof"))
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
			gDebugLog.Log("Trying to read _EOF and no file open!");
			special->SetLong(0);
		}
    	return (true);
    }

    return false;
}

/***********************************************************************
 * Function: VariableManager::GetString
 *
 *  Parameter name
 * Return:
 *  Value of "name" as a string.
 * Comments:
 *  
 ***********************************************************************/
const char *VariableManager::GetString(const char *name)
{
    Variable    *var;

    var = FindVariable(name, true);
    return (var->GetString());
}

/***********************************************************************
 * Function: VariableManager::GetLong
 *
 *  Parameter name
 * Return:
 *  value of "name" as a long.
 * Comments:
 *
 ***********************************************************************/
long VariableManager::GetLong(const char *name)
{
    Variable    *var;

    var = FindVariable(name, true);
    return var->GetLong();
}

/***********************************************************************
 * Function: VariableManager::GetDouble
 *
 *  Parameter name
 * Return:
 *  value of "name" as a double float.
 * Comments:
 *
 ***********************************************************************/
double VariableManager::GetDouble(const char *name)
{
    Variable    *var;

    var = FindVariable(name, true);
    return var->GetDouble();
}

/***********************************************************************
 * Function: VariableManager::FindVariable
 *
 *  Parameter name
 *  Parameter fReading
 * Return:
 *  Variable name "name" or new variable created.
 * Comments:
 *  Search the tree for the variable. If it's not there create it.
 *  We always create variables if they don't exist. This is so a
 *  variable may be set in the command line (or not set in the
 *  command line) and still used in the script.
 ***********************************************************************/
Variable *VariableManager::FindVariable(const char *name, int fReading)
{
    Variable    *var;

    //  First see if it is a special variable. If it is and fReading is
    //  false, complain because these are read-only variables. Otherwise
    //  return the special variable, which IsSpecial has set.
    //
    if (IsSpecial(name)) 
    {
        if (fReading)
			return (special);
		else
		{
            gLog.FatalError("Error: $%s is a read-only variable.", name);
		}
            
    }

    //  Search the local tree.
    //
    if (localroot) 
    {
        var = (Variable *)localroot->Find(name);
        if (var) 
        	return var;
    }

    //  Now check the global tree. It's ok to fail; we'll create the
    //  variable if it's not there.
    //
    var = (Variable *)Find(name);
    if (var == NULL) 
    { 
 		if (fReading)
 			gDebugLog.Log("Getting variable <%s> before it has been set.", name);
 
        var = new Variable(name);
        Add(var); 
    
		var->SetString("0");
    }

    return var;
}

/***********************************************************************
 * Function: VariableManager::SetString
 *
 *  Parameter name
 *  Parameter data
 * Return:
 *
 * Comments:
 *  Set "name" to "data"
 ***********************************************************************/
void VariableManager::SetString(const char *name, const char *data)
{
    Variable    *var;

    var = FindVariable(name, false);
    var->SetString(data);
}

/***********************************************************************
 * Function: VariableManager::SetLong
 *
 *  Parameter name
 *  Parameter data
 * Return:
 *
 * Comments:
 *  Set "name" to "data"
 ***********************************************************************/
void VariableManager::SetLong(const char *name, const long data)
{
    Variable    *var;

    var = FindVariable(name, false);
    var->SetLong(data);
}

/***********************************************************************
 * Function: VariableManager::SetDouble
 *
 *  Parameter name
 *  Parameter data
 * Return:
 *
 * Comments:
 *  Set "name" to "data"
 ***********************************************************************/
void VariableManager::SetDouble(const char *name, const double data)
{
    Variable    *var;

    var = FindVariable(name, false);
    var->SetDouble(data);
} 

void VariableManager::SetDate(const char *name, uint32 date, int32 date_type)
{
   Variable    *var;

    var = FindVariable(name, false);
    var->SetDate(date, date_type);
}


/***********************************************************************
 * Function: VariableManager::GetLocal
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
Variable *VariableManager::GetLocal()
{
    return localroot;
}

void VariableManager::SetLocal(Variable *newlocal)
{
    localroot = newlocal;
}


/*
 $Log$
 Revision 1.2  2002/02/19 12:35:12  tvw
 Bugs #494 and #495 are addressed in this update.

 (1) 5L.prefs configuration file introduced
 (2) 5L_d.exe will no longer be part of CVS codebase, 5L.prefs allows for
     running in different modes.
 (3) Dozens of compile-time switches were removed in favor of
     having a single executable and parameters in the 5L.prefs file.
 (4) CryptStream was updated to support encrypting/decrypting any file.
 (5) Clear file streaming is no longer supported by CryptStream

 For more details, refer to ReleaseNotes.txt

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.5  2000/08/08 19:03:41  chuck
 no message

 Revision 1.4  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.3  1999/11/02 17:16:37  chuck
 2.00 Build 8

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
