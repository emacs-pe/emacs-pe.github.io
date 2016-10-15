Defines a minor mode which defines a menu bar item allowing a wide
range of maths/technical characters (roughly the LaTeX repertoire)
to be inserted into the buffer by selecting menu items.

Menu display only works properly under X with Gtk menus and if the
menu uses a font with a suitable repertoire.  (Lucid and Motif
toolkit menus can't display multilingual text.  I don't know about
MS Windows menus.)  It will work with tmm in tty mode iff the tty
displays Unicode.  The tmm version (via F10) is also useful under a
window system when the menus don't display the characters
correctly, but where the symbols have word syntax, tmm won't
provide an ASCII selector for them, which can be a pain to use
without a mouse.

See also the TeX and sgml Quail input modes.  These will probably
behave better if you can remember the input sequences.  For
instance, this minor mode won't give you the ability to insert into
the minibuffer via the menu, though presumably it could be added to
the minibuffer menu.
