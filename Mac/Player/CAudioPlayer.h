//
//	CAudioPlayer.h
//

#pragma once

class CAudioManager
{
	public:
					CAudioManager();
					~CAudioManager();
		
		void		SpendTime(const EventRecord &inMacEvent);
		
		void		Play(const char *inName, int32 inOffset, int32 inVolume,
							int32 inFadeTime, bool inLoop, bool inKill);
		void		Kill(int32 inFadeTime = 0, bool inLoops = false);
		void		Volume(int32 inVolLevel, int32 inFadeTime);
		void		Wait(int32 inWaitFrame);
		void		Pause(int32 inTenths);
		void		Resume(void);
		
		bool		Playing(void)
						{ return (m_Playing); }
		bool		Looping(void)
						{ return (m_Playing and m_Looping); }
		bool		PlayNoLoop(void)
						{ return (m_Playing and (not m_Looping)); }
		bool		Paused(void)
						{ return (m_Playing and m_Paused); }
	
	private:
		CAudio		*m_Loop;
		
};
		