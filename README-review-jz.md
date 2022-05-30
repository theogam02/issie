# Code Review - Jason

Link: [WaveSim.fs](https://github.com/tomcl/issie/blob/review-jz/src/Renderer/UI/WaveSim/WaveSim.fs#L704)

Lines 704 to the end.

1. This code is still due a proper clean-up before it is PR'd to master (which hopefully will happen some time this week).
2. Some code is legacy code from old waveform simulator which I have reused (either because I haven't gotten round to improving it, or because I see no reason to replace it).
3. I hope the naming of the functions makes it obvious as to how they correspond to what you see on screen...