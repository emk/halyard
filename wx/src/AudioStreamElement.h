// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef AudioStreamElement_H
#define AudioStreamElement_H

#include "Stage.h"
#include "InvisibleElement.h"
#include "MediaElement.h"

class AudioStream;

//////////
// A widget represents a full-fledged wxWindow object hanging around on
// our stage.  It does its own event processing.
//
class AudioStreamElement : public InvisibleElement, public IMediaElement
{
	AudioStream *mStream;

public:
	AudioStreamElement(Stage *inStage, const wxString &inName,
					   AudioStream *inStream);
	virtual ~AudioStreamElement();

	AudioStream *GetAudioStream() { return mStream; }

    virtual bool IsLooping();
	virtual void EndPlayback();
    virtual void Pause();
    virtual void Resume();
	
	virtual void SetVolume(const std::string &inChannel, double inVolume);
};

#endif // AudioStreamElement_H
