// ===========================================================================
//	LTimerTask.h					© 1995, ƒric Forget. All rights reserved.
// ===========================================================================
//	
//	************************************************************************
//	*                                                                      *
//	*	Before using this code you should read the "License Agreement"     *
//	*	document and agree with it.                                        *
//	*                                                                      *
//	************************************************************************
//
//	Instruction and usage notes are in the LTimerTask.cp file.
//
// ---------------------------------------------------------------------------


#pragma once


#ifndef __TIMER__
#include	<Timer.h>
#endif	// __TIMER__

#include	"LTask.h"


class LTimerTask;


// ---------------------------------------------------------------------------
//		¥ Structure STMInfoT
// ---------------------------------------------------------------------------

struct	STMInfoT {

	TMTask	task;
#if !GENERATINGCFM
	Int32	A5World;
#endif
	LTimerTask	*timerTask;
};


// ---------------------------------------------------------------------------
//		¥ Class LTimerTask
// ---------------------------------------------------------------------------

class LTimerTask :	public LTask {

private:
								LTimerTask() {}	// Impossible constructor!
	Int32						m_RealTime;
	
public:
								LTimerTask(Int32	inFirstDelay,
										Int32	inNextDelay,
										Boolean inDeleteOnCompletion);
	virtual						~LTimerTask();
	
	virtual void				StartTask();
	virtual void				StopTask();
	
protected:
	static TimerUPP				sTaskUPP;
	STMInfoT					mTMInfo;
	
	
#if GENERATINGCFM
	static pascal void			TaskUPP(STMInfoT *inTMInfoPtr);
#else
	static pascal void			TaskUPP(STMInfoT *inTMInfoPtr : __A1);
#endif
	virtual void				ContinueTask();
};