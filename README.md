# jade-mode

Emacs major mode for [jade template](http://github.com/visionmedia/jade) highlighting.

Still in very early stages.  Some simple highlighting of tags, ids, and classes works; however, it highlights incorrectly into the javascript code and plain text code as well.

Lines can be indented or un-indented (is that a word?) with tab, shift-tab respectively.  Indentation will look at the proceeding line and not indent more than 1 level of nesting below it.

    html
      body
        #container
        .content
      ^
      |----cursor anywhere on this line, press tab

    html
      body
        #container
          .content
      ^
      |---- cursor anywhere on this line, press tab again

    html
      body
        #container
    .content

Regions can be indentend in a similar way; however, this is still buggy as hell.

I would like to get the highlighting working better.  Also note javascript highlighting with either js2-mode or espresso-mode should be possible.

Since jade nesting is somewhat related to sexp layout I hope to have sexp related selection & manipulation working in the future.  Currently working on `jade-highlight-sexp`
      
