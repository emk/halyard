// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef MediaElement_H
#define MediaElement_H

//////////
// An abstract interface for elements which play media streams.  This
// is used as a mixin class.
//
class IMediaElement
{
public:
    IMediaElement() {} 
	virtual ~IMediaElement() {}

	//////////
	// Cease playback, and make sure all future calls to
	// HasReachedFrame() return true.
	//
	virtual void EndPlayback() = 0;

    //////////
    // Pause the media stream.
    //
    // Note: This method may not be happy if the underlying movie layer
    // doesn't like to be paused.  In particular, streaming network
    // QuickTime is chancy.
    //
    virtual void Pause() = 0;

    //////////
    // Resume playing the media stream after a pause.
    //
    virtual void Resume() = 0;

    //////////
    // Set the volume of this media element.
    //
    // [in] inChannel - The channel to change.  Typical values include
    //                  "left", "right" and "all", although future media
    //                  frameworks may define more channels.
    // [in] inVolume - The volume.  0.0 is silent, 1.0 is the media's
    //                 normal volume.
    //
	virtual void SetVolume(const std::string &inChannel, double inVolume) = 0;
};

#endif // MediaElement_H
