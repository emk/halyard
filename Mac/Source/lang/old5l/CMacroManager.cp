//
//	CMacroManager.h
//

#include "CMacroManager.h"

/**************************

    MACRO MANAGER STUFF

**************************/

/***********************************************************************
 * Function: CMacroManager::GetScript
 *
 *  Parameter macroname
 * Return:
 *
 * Comments:
 *  Overwrite the IndexManager's GetScript()
 ***********************************************************************/
char *CMacroManager::GetScript(const char *macroname)
{
    CIndex   *mac;

    mac = (CIndex *) FindNode(macroname);
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
void CMacroManager::MakeNewIndex(char *name, int32 start, int32 end)
{
    CIndex   *newMacro;

    newMacro = new CIndex(name, start, end);

    AddNode(newMacro);
    newMacro->SetScript();
}

