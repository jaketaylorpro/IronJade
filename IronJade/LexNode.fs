namespace IronJade
open IronJade.Util
open IronJade.LexLine
    module LexNode=
        type LexNode(lexLine:LexLine,childNodes:List<LexNode>,indentation:int,lineNumber:int)=
            member this.LexLine=lexLine
            member this.ChildNodes=childNodes
            member this.Indentation=indentation
            member this.LineNumber=lineNumber

