// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include "GeigerSynthElement.h"
#include "GeigerAudioStream.h"
#include "VorbisAudioStream.h"

GeigerSynthElement::GeigerSynthElement(Stage *inStage, const wxString &inName,
                                       const std::string &inStatePath,
                                       const char *inChirpLocation,
                                       size_t inBufferSize)
    : InvisibleElement(inStage, inName),
      mStatePath(inStatePath),
      mGeigerAudioStream(new GeigerAudioStream(inChirpLocation)),
      mCurrentLoopCps(0), mBufferSize(inBufferSize)
{
    NotifyStateChanged();
}

GeigerSynthElement::~GeigerSynthElement() {
    mGeigerAudioStream->Stop();
}

void GeigerSynthElement::AddLoop(double inLoopCps, const char *inLoopLocation)
{
    VorbisAudioStreamPtr stream(new VorbisAudioStream(inLoopLocation,
                                                      mBufferSize,
                                                      true));
    mLoopStreams.insert(LoopMap::value_type(inLoopCps, stream));
}

void GeigerSynthElement::DoneAddingLoops() {
    mGeigerAudioStream->Start();
}

void GeigerSynthElement::SetChirpsPerSecond(double inCPS) {
    mGeigerAudioStream->SetChirpsPerSecond(inCPS);
}

void GeigerSynthElement::NotifyStateChanged() {
    double cps(gStateDB.Get(this, mStatePath));
    SetChirpsPerSecond(cps);
}

void GeigerSynthElement::EndPlayback()
{
    mGeigerAudioStream->Stop();
}

void GeigerSynthElement::Pause()
{
    mGeigerAudioStream->Stop();
}

void GeigerSynthElement::Resume()
{
    mGeigerAudioStream->Start();
}

void GeigerSynthElement::SetVolume(const std::string &inChannel,
                                   double inVolume)
{
    mGeigerAudioStream->SetChannelVolume(inChannel, inVolume);
}
