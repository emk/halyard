// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include "GeigerSynthElement.h"
#include "GeigerAudioStream.h"
#include "VorbisAudioStream.h"

GeigerSynthElement::GeigerSynthElement(Stage *inStage, const wxString &inName,
                                       const char *inChirpLocation,
                                       size_t inBufferSize)
    : InvisibleElement(inStage, inName),
      mGeigerAudioStream(new GeigerAudioStream(inChirpLocation)),
      mCurrentLoopCps(0), mBufferSize(inBufferSize)
{
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
