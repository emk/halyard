// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef GeigerSynthElement_H
#define GeigerSynthElement_H

#include "InvisibleElement.h"
#include "MediaElement.h"
#include "TStateDB.h"

class GeigerAudioStream;
class VorbisAudioStream;

//////////
// A GeigerSynthElement manages a number of separate audio streams to
// acheive a complete Geiger-counter-like effect.
//
class GeigerSynthElement : public InvisibleElement, public IMediaElement,
                           public TStateListener
{
    typedef shared_ptr<VorbisAudioStream> VorbisAudioStreamPtr;
    typedef std::map<double,VorbisAudioStreamPtr> LoopMap;

    std::string mStatePath;
    shared_ptr<GeigerAudioStream> mGeigerAudioStream;
    LoopMap mLoopStreams;
    double mCurrentLoopCps;
    size_t mBufferSize;

    void SetChirpsPerSecond(double inCPS);

public:
	GeigerSynthElement(Stage *inStage, const wxString &inName,
                       const std::string &inStatePath,
                       const char *inChirpLocation,
                       size_t inBufferSize);
    ~GeigerSynthElement();

    void AddLoop(double inLoopCps, const char *inLoopLocation);
    void DoneAddingLoops();
    
    virtual void NotifyStateChanged();

    virtual bool IsLooping() { return true; }
	virtual void EndPlayback();
    virtual void Pause();
    virtual void Resume();
	virtual void SetVolume(const std::string &inChannel, double inVolume);
};

#endif // GeigerSynthElement_H
