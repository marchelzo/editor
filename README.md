editor
======

it's a text editor that doesn't handle tabs at all

### About
 - Written in portable C11
 - Only dependency is ncurses (on Windows it can be built with pdcurses)


### Screenshot

![screenshot of the editor running in gnome-terminal](/screenshot.png?raw=true "Running in gnome-terminal")

### Features

 - It's fairly light on resources
 - Optional line numbers
 - Optional auto-indent
 - Multiple buffer support
 - vi-like keybinds
 - Modal (similar to vi)
 
### Tabs
 
In insert mode, pressing the tab key will insert spaces (4 by default).
There is no way to indent with tab characters instead.
If you open a file which contains tab characters, you will undoubtedly face problems.

### Future

Eventually I would like to add a mapping system similar to that of vim. Also, I would like to implement some form of runtime automation, either via a scripting language, macros, etc.

### Usage

`$ ./edit <filename>` will launch the editor and open `<filename>` in a new buffer.
Once inside, you can move around with vi-like motions.
 - currently supported are j k l h W f F 0 ^
 - eventually most if not all will be supported

Save with `:w<CR>`

Open a new buffer with `:e <absolute file path><CR>`

Close a buffer with `:q<CR> (quits the editor if one buffer is open)`

Enter insert mode with: `i a I A o O S`

Delete text with: `dd D (backspace in insert mode)`

Leave insert mode with: `<Esc> jk`
