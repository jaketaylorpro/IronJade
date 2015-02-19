namespace IronJade
    type LexLine=
        |Root of Indentation*Compiled //Reserved for the first lexnode
        |TextBlockLine of string //only if the parent specifies it's child is all text with a dot '.'
        |DocType of string //if its a doctype specification 'doctype htnl'
        |TextLine of string //if it's a text line specified by '| '
        |Tag of LexTag //if it's an html tag specified by 'tagname[#id][.class1][.classn][:|.| text]'