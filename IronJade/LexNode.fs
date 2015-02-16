namespace IronJade
    module LexNode=
        type LexNode={LexLine:LexLine.LexLine;ChildNodes:List<LexNode>;LineNumber:int;Indentation:int}

