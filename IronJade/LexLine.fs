namespace IronJade
    module LexLine=
        type LexLine=
            |Root //Reserved for the first lexnode
            |TextBlockLine of string //only if the parent specifies it's child is all text with a dot '.'
            |DocType of string //if its a doctype specification 'doctype htnl'
            |TextLine of string //if it's a text line specified by '| '
            |Tag of LexTag.LexTag //if it's an html tag specified by 'tagname[#id][.class1][.classn][:|.| text]'
        let buildLexLine (line:string) (inTextBlock:bool) :LexLine=
            if inTextBlock 
            then
                TextBlockLine(line) 
            else
                match line with
                | Util.Prefix "doctype " rest -> DocType(rest)
                | Util.Prefix "|" rest -> TextLine(rest)
                | Util.Prefix "//" rest -> Tag(LexTag.buildCommentLexTag rest)
                | _-> Tag(LexTag.buildLexTag line)