// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef GeigerSynthElement_H
#define GeigerSynthElement_H

#include "InvisibleElement.h"

class GeigerAudioStream;
class VorbisAudioStream;

//////////
// A GeigerSynthElement manages a number of separate audio streams to
// acheive a complete Geiger-counter-like effect.
//
class GeigerSynthElement : public InvisibleElement {
    typedef boost::shared_ptr<VorbisAudioStream> VorbisAudioStreamPtr;
    typedef std::map<double,VorbisAudioStreamPtr> LoopMap;

    boost::shared_ptr<GeigerAudioStream> mGeigerAudioStream;
    LoopMap mLoopStreams;
    double mCurrentLoopCps;
    size_t mBufferSize;

public:
	GeigerSynthElement(Stage *inStage, const wxString &inName,
                       const char *inChirpLocation,
                       size_t inBufferSize);
    ~GeigerSynthElement();

    void AddLoop(double inLoopCps, const char *inLoopLocation);
    void DoneAddingLoops();

    void SetChirpsPerSecond(double inCPS);
};

#endif // GeigerSynthElement_H
