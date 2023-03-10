# Revision history for mindmap-hs

## 0.1.0.0 -- 2022-08-19

* First version. Released on an unsuspecting world.

## 0.1.1.0 -- 2022-08-19

* Added `dd` keybinding to delete currently selected bubble

## 0.1.2.0 -- 2022-08-19

* Added `u` keybinding to undo previous changes
* Added `C-r` keybinding to redo previously undone changes

## 0.1.2.1 -- YYYY-mm-dd

* Added `cc` keybind to change an entire bubble at once
* Added arrow key support
* Fixed cursor to not move when selecting a bubble
* Added `zz` to center the screen on the cursor
* Fixed issue where `dd` rearrange the tree
* Escape now clears the keystack
* Program no longer tries to render offscreen elements
* Fixed issue where connecting lines would be drawn inside of bubbles
* Added basic caching to reduce lag when moving the cursor around
