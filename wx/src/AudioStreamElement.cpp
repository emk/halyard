// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"
#include "AudioStreamElement.h"
#include "AudioStream.h"


//=========================================================================
//  AudioStreamElement Methods
//=========================================================================

AudioStreamElement::AudioStreamElement(Stage *inStage, const wxString &inName,
									   AudioStream *inStream)
    : Element(inStage, inName), mStream(inStream)
{
    mStream->Start();
}

AudioStreamElement::~AudioStreamElement()
{
    mStream->Stop();
    delete mStream;
}

void AudioStreamElement::Pause()
{
    mStream->Stop();
}

void AudioStreamElement::Resume()
{
    mStream->Start();
}
