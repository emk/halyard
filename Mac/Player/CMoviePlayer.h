//
// CMoviePlayer.h
//

#pragma once

#include <LPane.h>
#include <LCommander.h>

class  CMoviePlayer
{
	public:
      					CMoviePlayer();
						~CMoviePlayer();
		
       	void			SpendTime(const EventRecord &inMacEvent);

		void			Preroll(const char *inMovieName, bool inAudioOnly);
		void			Play(const char *inMovieName, int32 inWaitOffset, 
							bool inAudioOnly, const char *inPalHand, int32 inStartOffset);
		void			PlayLoop(const char *inMovieName, int32 inFadeTime);
		void			Pause(void);
		void			Resume(void);
		void			Kill(void);
		void			SetOrigin(Point inPoint);
		
		bool			AtEnd(TimeValue &inMovieTime);
		
		bool			Playing(void) { return (mInMovie); }
		bool			MoviePlaying(void) { return (mInMovie and (not mAudioOnly)); }
		bool			AudioPlaying(void) { return (mInMovie and mAudioOnly); }
		bool			Paused(void) { return (mInMovie and mPaused); }
		bool			FullScreenPlay(void) { return ((MoviePlaying()) and (not mHaveOrigin)); }
		bool			AudioLooping(void) { return ((AudioPlaying()) and (mLooping)); }
		bool			HaveOrigin(void) { return (mHaveOrigin); }		
		void			WakeCard(int32 inFrame);
		void			DrawSelf(void);
		
	private:
		bool			mInMovie;
		bool			mPaused;
		bool			mPrerolled;
		bool			mWakeCard;
		bool			mAudioOnly;
		bool			mHaveOrigin;
		bool			mHavePal;
		bool			mLooping;
		
		Point			mOrigin;
		
		TimeValue		mWaitTime;
		int32			mWaitOffset;
		int32			mStartOffset;
		int32			mLoopFadeTime;

		Movie			mMovie;
		FSSpec			mSpec;
		
		Fixed			mRate;
		TimeValue		mEndTime;
		TimeScale		mScale;
		
		//bool			Load(FSSpec *theSpec, bool inAudioOnly);
		bool			Load(const char *inMovieName, bool inAudioOnly);
		bool			DoPreroll(const char *inMovieName, bool inAudioOnly, bool inPreroll);
		void			Cleanup(void);
		void			FadeLoop(void);
		
};

// global
extern CMoviePlayer gMovieManager;
