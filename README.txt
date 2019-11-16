cage


Table of Contents
─────────────────

1. Important note
2. Introduction
3. The wrapper
.. 1. Parameters
.. 2. Function name
.. 3. Other differences
4. Issues
.. 1. Missing function
5. BUGS
6. License
7. NO WARRANTY


[http://quickdocs.org/badge/cl-pslib.svg]


[http://quickdocs.org/badge/cl-pslib.svg]
<http://quickdocs.org/cl-pslib/>


1 Important note
════════════════

  The 'ps' nickname for this package has been removed.

  This means that if you are using this library you should replace all
  occurences of the 'ps' package qualifier with 'cl-pslib'.

  Examples

  from:

  ┌────
  │ (ps:get-value doc "boxheight")
  └────

  to:

  ┌────
  │ (cl-pslib:get-value doc "boxheight")
  └────

  and from:

  ┌────
  │ (defpackage :foo
  │   (:use :ps))
  └────

  to:

  ┌────
  │ (defpackage :foo
  │   (:use :cl-pslib))
  └────

  Alternatively., if you are using ECL, ABCL, CCL or SBCL compiler you
  can setup a package local nickname as showed [here].


[here]
<http://www.sbcl.org/manual/index.html#Package_002dLocal-Nicknames>


2 Introduction
══════════════

  Cl-pslib is a (thin) wrapper around [pslib] a library for generating
  PostScript files.


[pslib] <http://pslib.sourceforge.net/>


3 The wrapper
═════════════

  Cl-pslib use CFFI and SWIG to interface with foreign (non lisp)
  library and generate the low-level lisp wrapper respectively.

  cl-pslib does not export the raw (CFFI) pslib API but use CLOS
  instead.

  A psdoc C struct pointer is wrapped in the class psdoc, most of the
  functions to manipulate this pointer are wrapped in lisp methods
  specialized for that class.


3.1 Parameters
──────────────

  Pslib use a lot of parameters to configure its behavior.

  Example:

  ┌────
  │ (get-value doc "boxheight")
  └────

  I found this very unpractical so i tried to generate lisp constants to
  match the corresponding string parameter

  ┌────
  │ (get-value doc +value-key-boxheight+)
  └────


3.2 Function name
─────────────────

  As general rule the methods for the class psdoc are the same of the
  ones of the pslib with the character "_" substituted by "-".

  There are some exceptions listed below.

  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   native name                                                   lisp name
  ───────────────────────────────────────────────────────────────────────────────────────
   `PS_set_border_color'                                         `set-border-link-color'
   `PS_set_border_dash'                                          `set-border-link-dash'
   `PS_set_border_style'                                         `set-border-link-style'
   `PS_open, PS_open_mem'                                        `open-doc'
   `PS_fill'                                                     `fill-path'
   `PS_show and PS_show2'                                        `show'
   `PS_show_xy and PS_show_xy2'                                  `show-xy'
   `PS_string_geometry', `PS_stringwidth' and `PS_stringwidth2'  `string-geometry'
   `PS_symbol'                                                   `font-symbol'
   `PS_symbol_name'                                              `font-symbol-name'
   `PS_symbol_width'                                             `font-symbol-width'
  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


3.3 Other differences
─────────────────────

  • `setcolor' can accept a `cl-color' object as parameter;
  • there is no '(ps_)boot' method; when an instance of `psdoc' is
    created the `initialize-instance' of that class will call `ps_boot'
    if needed.  The same is true for '(ps_)new' method;
  • `open-doc' method will pass the output of pslib (i.e.the PostScript
    file) to a lisp callback function if the second argument of the
    function is nil; by default the callback just copy the output in
    `cl-pslib:*callback-string*'.  This maybe can be useful for generate
    a file in a web application.

    There is another callback: `write-to-stream' that write the
    postscript code to a stream (by default the stream is bound to
    `*standard-output*')

    ┌────
    │ (let* ((cl-pslib:*callback-stream* *standard-output*)
    │        (doc (make-instance 'cl-pslib:psdoc
    │                            :writeproc (cffi:callback write-to-stream))))
    │   ;; rest of the code here
    │   )
    │
    └────

  • the `string-geometry' method return an instance of `text-metrics'
    class


4 Issues
════════

  Note that the whole library is in an alpha stage, testing is still in
  progress, please see [section below]

  • to use the template features `begin-template' and `and-template' a
    version >= 0.47 must be used.


[section below] See section 7

4.1 Missing function
────────────────────

  The high-level API does not still remap this functions:

  • `PS_free_glyph_list';
  • `PS_get_buffer';
  • `PS_glyph_list';
  • `PS_new2';
  • `PS_open_fp';
  • `PS_hyphenate';
  • `PS_list_parameters';
  • `PS_list_resources';
  • `PS_list_values';
  • `PS_set_gstate';
  • `PS_setdash' (use `set-polydash' instead).


5 BUGS
══════

  Please file bug report on the [issue tracker]


[issue tracker] <https://notabug.org/cage/cl-pslib/issues>


6 License
═════════

  This library is released under Lisp Lesser General Public license (see
  COPYING.LESSER file)

  Examples are released under GPL version 3 or later


7 NO WARRANTY
═════════════

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
