// ===========================================================================
//	LTimerTask.cp					© 1995, ƒric Forget. All rights reserved.
// ===========================================================================
//	
//	************************************************************************
//	*                                                                      *
//	*	Before using this code you should read the "License Agreement"     *
//	*	document and agree with it.                                        *
//	*                                                                      *
//	************************************************************************
//
//	LTimerTask is a wrapper class for the Time Manager. It hides all the
//	details for you.
//
// ---------------------------------------------------------------------------
//
//	Instruction Notes:
//	-----------------
//
//	1) Derive LTimerTask and override:
//
//			StartTaskSelf();
//			ExecuteTaskSelf();
//			StopTaskSelf();
//
// ---------------------------------------------------------------------------
//
//	Usage Notes:
//	-----------
//
//	1) Read the "Task Guidelines" on page 1-13 of Inside Macintosh: Process.
//
//	2) You should allocate the handle and pointer needed in StartTaskSelf()
//	   and lock all these handle needed for the ExecuteTaskSelf().
//
//	3) In ExecuteTaskSelf() you can use your global variable, A5 has the
//	   the correct value.
//
//	4) You should dispose of all memory kept by your class in StopTaskSelf().
//
//	5) As for the Time Manager the "delay" you pass to the constructor are
//	   intepreted in milliseconds if you pass a positive value and interpreted
//	   in microseconds if you pass a negative value.
//
// ---------------------------------------------------------------------------

#include	"LTimerTask.h"


// ---------------------------------------------------------------------------
//		¥ Static members
// ---------------------------------------------------------------------------

TimerUPP		LTimerTask::sTaskUPP = NewTimerProc(LTimerTask::TaskUPP);


// ---------------------------------------------------------------------------
//		¥ LTimerTask
// ---------------------------------------------------------------------------

LTimerTask::LTimerTask(
	Int32	inFirstDelay,
	Int32	inNextDelay,
	Boolean inDeleteOnCompletion)
		
		: LTask(inFirstDelay, inNextDelay, inDeleteOnCompletion)
{
	mTMInfo.task.tmAddr		= sTaskUPP;
	mTMInfo.task.tmWakeUp	= 0;
	mTMInfo.task.tmReserved = 0;
	
	m_RealTime = inFirstDelay;
		
#if !GENERATINGCFM
	mTMInfo.A5World = ::SetCurrentA5();
#endif
	mTMInfo.timerTask = this;
}


// ---------------------------------------------------------------------------
//		¥ ~LTimerTask
// ---------------------------------------------------------------------------

LTimerTask::~LTimerTask()
{
	
}


// ---------------------------------------------------------------------------
//		¥ StartTask
// ---------------------------------------------------------------------------

void
LTimerTask::StartTask()
{
	if(!IsExecuting()) {
	
		LTask::StartTask();
		
		::InsTime((QElemPtr)&mTMInfo);
		::PrimeTime((QElemPtr)&mTMInfo, m_RealTime);
		//::PrimeTime((QElemPtr)&mTMInfo, mFirstDelay);
	}
}


// ---------------------------------------------------------------------------
//		¥ StopTask
// ---------------------------------------------------------------------------

void
LTimerTask::StopTask()
{
	if(IsExecuting()) {
	
		::RmvTime((QElemPtr)&mTMInfo);
		
		LTask::StopTask();
	}
}


// ---------------------------------------------------------------------------
//		¥ TaskUPP
// ---------------------------------------------------------------------------

#if !GENERATINGCFM
	
	pascal void
	LTimerTask::TaskUPP(
		STMInfoT	*inTMInfoPtr : __A1)
	{
		Int32	oldA5 = ::SetA5(inTMInfoPtr->A5World);
		
		inTMInfoPtr->timerTask->ExecuteTask();
		
		::SetA5(oldA5);
	}
	
#else
	
	pascal void
	LTimerTask::TaskUPP(
		STMInfoT	*inTMInfoPtr)
	{
		inTMInfoPtr->timerTask->ExecuteTask();
	}
	
#endif


// ---------------------------------------------------------------------------
//		¥ ContinueTask
// ---------------------------------------------------------------------------

void
LTimerTask::ContinueTask()
{
	::PrimeTime((QElemPtr)&mTMInfo, mNextDelay);	
}
