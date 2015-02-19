namespace IronJade
    type LexTagValue = {Name:string;Attributes:List<string*Option<string>>;LexInnerTag:LexInnerTag}
    type LexTag=
        |LexTagProper of LexTagValue
        |LexTagError of string //an error detected at the tag level
        |LexTagInnerError of string //an error detected at the inner tag level