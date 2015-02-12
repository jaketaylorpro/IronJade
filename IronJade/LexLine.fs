namespace IronJade
open IronJade.Util
open IronJade.LexTag
    module LexLine=
        type LexLine=
            |Tag of LexTag
            |TextLine of string
            |TextBlockLine of string
            |Root
        let buildLexLine (line:string) :LexLine= //TODO block text not yet implemented
            match line with
            | Util.Prefix "|" rest -> TextLine(rest)
            | _-> Tag(buildLexTag line)

