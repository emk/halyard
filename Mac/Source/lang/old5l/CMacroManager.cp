//
//	CMacroManager.h
//

#include "CMacroManager.h"

USING_NAMESPACE_FIVEL
/***********************************************************************
 * Function: CMacroManager::GetScript
 *
 *  Parameter macroname
 * Return:
 *
 * Comments:
 *  Overwrite the IndexManager's GetScript()
 ***********************************************************************/
const char *CMacroManager::GetScript(const char *macroname)
{
    TIndex   *mac;

    mac = (TIndex *) Find(macroname);
    return (mac->GetScript());
}

/***********************************************************************
 * Function: CMacroManager::MakeNewIndex
 *
 *  Parameter name
 *  Parameter start
 *  Parameter end
 * Return:
 *
 * Comments:
 *  Creates new macro entry into the macro tree
 ***********************************************************************/
void CMacroManager::MakeNewIndex(TIndexFile *inFile, const char *inName, 
	int32 inStart, int32 inEnd)
{
    TIndex   *newMacro = NULL;

    newMacro = new TIndex(inFile, inName, inStart, inEnd);
    
    if (newMacro->SetScript())
    	Add(newMacro);
}

