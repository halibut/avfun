avfun
=====

This is a kind of learning project aimed at generating music based on machine learning techniques. It's definitely a WIP and will probably see significant changes in the techniques used and also the way the code is organized maintained.

The project can be broken down into a few high-level areas.

* __composition__ - Generation of the score, instruments, melodies, etc.
* __synthesis__ - Feeding the composition into Supercollider to produce a wav file
* __vizualization/playback__ - UI/automation around listening to the songs and visual feedback.


##Setup

1. Clone the repo.
2. Install Supercollider.
3. Make a copy of the `song-generator-config.json.example` file in the project root. Name the new file `song-generator-config.json`. Modify the file to point to your supercollider installation location. You can also modify the location where the generated songs (and machine learning data) will be stored.

##Make Some Music

Eventually this will all be combined into a single UI. But for now it's 2 separate steps. Generate songs by running the `avfun.musicgen.SongGenerator` class. Listen to the songs by running the `avfun.musicgen.MusigGenUI` class.


##Future Plans

###Composition Changes

I'm not entirely happy with how the app generates compositions. It's a genetic algorithm-based neural network, or set of neural networks. Each network is responsible for generating a part of the overall song. 
* Song structure stuff like BPM, # of instruments, key, etc.
* Pattern "stucture" by instrument - Note division, note range, etc.
* Note patterns - The actual notes produced for each instrument.
* Synthesizers - Generate a synthesizer for a particular instrument

All of this works okay to produce some esoteric music. But since training is based on a genetic algorithm and I don't have an automated way to give fitness feedback, it will probably be a long manual process to produce anything that sounds good without artifially adding some manual constraints (which I am guilty of doing already.)

Long story short, I'm looking into different ways of generating the compositions.


###Visualization

I've played around with this some, but I'd like for part of the machine learning algorithm produce a music visualizer.

###Crowd-sourced feedback

As I pointed out a couple sections prior to this one, training the algorithm to produce "good" music is a daunting task for 1 person. It would be neat to crowd source the training feedback. But obviously this requires additional infrastructure and plumbing to automate the training process. Idea: Create a "generation" of songs and upload to youtube. Use youtube's likes/dislikes as fitness values when generating the next generation. (Youtube because of unlimited videos, easily shareable, API to aid in automation of the process. Not necessarily married to youtube though.)
