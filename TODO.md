### Parsing
* Single dash works for long names and double dash works for short names. Also requires support in prediction.
- Multiple short names joined together.
- Using `=` to separate , using `--` to force pos arg, and gluing to short opts.

### Commands
* Args may signal an error or might indicate that help / usage should be printed.
* Add flexible pos args that get remaining values, use for write and echo.
* Add help arg to commands somehow.
* Print help / usage when required arg is omitted. Also when eval-ing and there is an unknown arg, instead of printing and error. Probably shouldn't print directly, should use UI somehow. Currently passing the print_error function to eval, which isn't great.
- Check / handle when different opt args have the same name? Currently I think the earlier one will take the value of the later one as the mapping is based on strings, which is bad.

### Prediction
* Showing possibilities with their doc, and allowing choice. Pressing tab after tab has already been pressed to do longest prefix should complete a full option.
- Better escaping - only escape existing string parts to string.
- Completing a short arg doesn't result in one dash. Short names should be joined on.

### History
- Can currently modify history, could have actual history and modified history in state.
- Need to not add if duplicate of last.
- Limiting size.
- Persisting the history to disk somewhere.
- Using in prediction and navigating history by searching with partially typed command.

### Unix / Notty UI
* When line wrapping, the cursor isn't displayed (as it's going off screen).
* Could split input at pos, draw left, save cursor pos, draw right.
* There's still a bug where if the last line isn't blank then it is overwritten. Need a way to get the column number.
- Add new line characters if input isn't terminated. Test with consecutive newlines.
- print_error needs to handle new lines, and remove other escape characters. Alternatively print "Error:" with Notty then print with something else.
- Insert key and keyboard shortcuts.
- Resizing doesn't cause a redraw.
