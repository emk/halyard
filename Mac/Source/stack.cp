/**********************************************************
    STACK.CPP
    Module for delayed command execution
    Currently delayed during Channel 1 Video Only segments are:
        - loadpic
        - text
**********************************************************/

#include <stdio.h>
#include "stack.h"
#include "CResource.h"
#include "CStream.h"
#include "CCard.h"
#include "util.h"

#define MAXPICTS 20
#define MAXPICLEN 10

//Structure for card buffering.
typedef struct  {                   
    CString nameCmd;
}  pictureElement;

static int currCmd = 0;
pictureElement ArrayOfCmds[MAXPICTS];

void InitCmd()  
{
    currCmd = 0;
}
    
void StackCmd(CString name)  
{ 
    ArrayOfCmds[currCmd].nameCmd = name;
    currCmd++; 
    if (currCmd >= MAXPICTS)
        prerror("More than MAXCMDs stacked!");
}

void FlushCmd()  
{
    int i;
    
    for (i = 0; i < currCmd; i++)  
        gCardManager.DoOneCommand(ArrayOfCmds[i].nameCmd);

    currCmd = 0;
}

void DeInitCmd()  
{
    int i;
    for (i=0;i<MAXPICTS;i++)
        ArrayOfCmds[i].nameCmd = "";      
}
