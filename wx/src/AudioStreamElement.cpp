// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"

#include "AudioStreamElement.h"
#include "AudioStream.h"


//=========================================================================
//  AudioStreamElement Methods
//=========================================================================

AudioStreamElement::AudioStreamElement(Stage *inStage, const wxString &inName,
									   AudioStream *inStream)
    : InvisibleElement(inStage, inName), mStream(inStream)
{
    mStream->Start();
}

AudioStreamElement::~AudioStreamElement()
{
    mStream->Stop();
    delete mStream;
}

void AudioStreamElement::EndPlayback()
{
	mStream->Stop();
}

void AudioStreamElement::Pause()
{
    mStream->Stop();
}

void AudioStreamElement::Resume()
{
    mStream->Start();
}

void AudioStreamElement::SetVolume(const std::string &inChannel,
								   double inVolume)
{
    mStream->SetChannelVolume(inChannel, inVolume);
}
