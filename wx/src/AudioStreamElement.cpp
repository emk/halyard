// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"
#include "TException.h"
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

void AudioStreamElement::SetVolume(const std::string &inChannel,
								   double inVolume)
{
	int channels = mStream->GetChannelCount();
	if (inChannel == "left" && channels >= 2)
		mStream->SetChannelVolume(AudioStream::LEFT_CHANNEL, inVolume);
	else if (inChannel == "right" && channels >= 2)
		mStream->SetChannelVolume(AudioStream::RIGHT_CHANNEL, inVolume);
	else if (inChannel == "all")
		mStream->SetVolume(inVolume);
	else
		THROW("Tried to set volume on an unknown channel");
}
