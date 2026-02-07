A native Free Pascal class for playback of Impulse Tracker (IT) and Scream Tracker (S3M) modules.

Basically a port of 8bitbubsy's it2play, which is an accurate port of 
Impulse Tracker 2.15's original IT playroutine ported from assembly to C.

8bitbubsy's C port: https://github.com/8bitbubsy/it2play

Current state:
- Supports modules using samples or instruments, envelopes, filters
- SB16 and SB16-MMX drivers implemented; choice of interpolation, volume ramping and filter support
- Lots of low-level bit twiddling

Partial TODO:
- Example projects
- 8bitbubsy's HQ driver
- WAV writer
- Support for MMCMP compressed modules
- Refactor for more Pascalish code

Bugs:
- Filter routines may need tweaking
