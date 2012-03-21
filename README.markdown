# E2WM : Equilibrium Emacs Window Manager

E2WM is a window manager for Emacs.  It enables to customize the
place of pop-up window, how the windows are split, how the buffers
are located in the windows, keybinds to manipulate windows and
buffers, etc.  It also has plug-ins to help your Emacs life.

## Buffer history

Buffers that you edit is recorded in a special history.  It will
help you to go back and forth to edit these buffers.

## Perspective

Depending on the kind of your task, you can change how the windows are
split.  Following the term from Eclipse, it is called perspective.
E2WM has the following perspectives by default.

### Code perspective

![code perspective](http://cdn-ak.f.st-hatena.com/images/fotolife/k/kiwanami/20100527/20100527231754.png)

Place a buffer for reading/writing code at center and helper plug-ins
around of it.

### Two / HTwo perspective

![two perspective](http://cdn-ak.f.st-hatena.com/images/fotolife/k/kiwanami/20100527/20100527231758.png)

Show two buffers by splitting left and right (two) or top and bottom (htwo).

### Doc perspective

![doc perspective](http://cdn-ak.f.st-hatena.com/images/fotolife/k/kiwanami/20100527/20100527231753.png)

Read long code or document in the follow-mode.

### Dashboard perspective

![dashboard](http://cdn-ak.f.st-hatena.com/images/fotolife/k/kiwanami/20100527/20100527231752.png)

Put the buffers you want see occasionally.

### Array perspective

![array](http://cdn-ak.f.st-hatena.com/images/fotolife/k/kiwanami/20100527/20100527231751.png)

Show all the buffers you have and help searching for the buffer you
are looking for visually.

### Emacs DBI

![Emacs DBI](https://cacoo.com/diagrams/VdRPw8hjXiezJJud-23532.png?width=450)

Emacs DBI (Database Interface for Emacs Lisp) has it's own perspective
to manipulate database.

See also [emacs-edbi](https://github.com/kiwanami/emacs-edbi).


### Customize

Perspective is highly customizable.  You don't need to split windows
by yourself anymore; let E2WM do the job!


## Plug-ins

E2WM can have windows which have a specific function, something like
Eclipse's "view".  It is called plug-in.  E2WM has the following
plug-ins by default.  (Yes, you can create your own plug-ins also.)

### history-list

![history-list plug-in](http://cdn-ak.f.st-hatena.com/images/fotolife/k/kiwanami/20100527/20100527231755.png)

![history-list2 plug-in](http://cdn-ak.f.st-hatena.com/images/fotolife/k/kiwanami/20100607/20100607234419.png)

Show buffer history.


### files (dired)

![files plug-in](http://cdn-ak.f.st-hatena.com/images/fotolife/k/kiwanami/20100527/20100527231759.png)

A list of files in the current directory.


### imenu

![imenu plug-in](http://cdn-ak.f.st-hatena.com/images/fotolife/k/kiwanami/20100527/20100527231747.png)

Show the outline of the current code and the current position on it
using Imenu.


## Install

Put e2wm.el and
[window-layout.el](https://github.com/kiwanami/emacs-window-layout) in
your load-path.  This is a very simple setting example:

    (require 'e2wm)
    (global-set-key (kbd "M-+") 'e2wm:start-management)

Have a look at e2wm-config.el to see how to customize E2WM.

**Warning**: E2WM overrides Emacs lisp functions which is related to
window manipulation.  It is possible that E2WM does not work in your
environment.  It is recommended to test if E2WM works find before
use it to serious files.


## Usage

TODO...


## License

License
  GPL v3

Repository
  http://github.com/kiwanami/emacs-window-manager

SAKURAI, Masashi
m.sakurai atmark kiwanami.net
