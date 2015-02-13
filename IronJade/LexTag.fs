namespace IronJade
open IronJade.Util
    module LexTag=
        let REGEX_WORD_START_CHAR="[a-zA-Z]"
        let REGEX_WORD_CHAR="[a-zA-Z0-9_-]"
        let REGEX_WORD_PATTERN=REGEX_WORD_START_CHAR+REGEX_WORD_CHAR+"*"
        let REGEX_TAG_PATTERN=System.String.Format(
                                                    "^({0})"+ //start with the tag name
                                                    "(?:\#({0}))?"+ //optional id
                                                    "(?:\.({0}))*" //optional multiple classes
                                                    ,REGEX_WORD_PATTERN)
        let REGEX_DIV_ID_PATTERN=System.String.Format(
                                                    "^\#({0})"+ //start with the id
                                                    "(?:\.({0}))*" //optional multiple classes
                                                    ,REGEX_WORD_PATTERN)
        let REGEX_DIV_CLASS_PATTERN=System.String.Format(
                                                    "^\.({0})"+ //start with a class
                                                    "(?:\.({0}))*" //optional additional classes
                                                    ,REGEX_WORD_PATTERN)
        let REGEX_ATTR_PATTERN=System.String.Format(
                                                    "(?:\("+ //optional attributes, inside parens
                                                    "(?:({0})=\\\"(.*?)(?<!(?<!\\\\)\\\\)\\\",\s*)*"+ //multiple name="value" followed by comma;value is lazy matched with a negative look behind to allow escaped double quotes
                                                    "({0})=\\\"(.*?)(?<!(?<!\\\\)\\\\)\\\""+ //at least one name=value without comma
                                                    "\))" //closing paren
                                                    ,REGEX_WORD_PATTERN)
        let REGEX_TEXT_PATTERN=(" (.*)$")
        let FULL_TAG_PATTERN=REGEX_TAG_PATTERN+REGEX_ATTR_PATTERN+REGEX_TEXT_PATTERN
        let FULL_DIV_ID_PATTERN=REGEX_DIV_ID_PATTERN+REGEX_ATTR_PATTERN+REGEX_TEXT_PATTERN
        let FULL_DIV_CLASS_PATTERN=REGEX_DIV_CLASS_PATTERN+REGEX_ATTR_PATTERN+REGEX_TEXT_PATTERN
        
        type LexTagValue(tag:string,attributes:List<string*Option<string>>,text:Option<string>)=
                    member this.Name=tag
                    member this.Attributes=attributes
                    member this.Text=text
        type LexTag=
            |LexTagProper of LexTagValue
            |LexTagError
        let buildLexTag (line:string)=
            let compileAttributes (id:string) (classes:List<string>) (attrN:List<string>) (attrV:List<Option<string>>) :List<string*Option<string>>=
                let idPair=("id",Util.ifSomeTrimmed id)
                let classPair=("class",Util.ifSomeTrimmed (List.fold (fun acc c->if acc="" then c else acc+" "+c) "" classes))
                let attrPairs=List.zip attrN attrV
                idPair::classPair::attrPairs
                |>List.filter (fun (n,v)-> n.Trim() <> "" )
                |>List.filter (fun (n,v)-> if n.Trim().ToLower() = "id"
                                           then v.IsSome
                                           else true )
                |>List.filter (fun (n,v)-> if n.Trim().ToLower() = "class"
                                           then v.IsSome
                                           else true )
            match line with
            | Util.RegexMC FULL_TAG_PATTERN [[name];[id];classes;[firstAttrN];[firstAttrV];attrN;attrV;[text]] -> 
                LexTagProper(new LexTagValue(name ,(compileAttributes id classes (firstAttrN::attrN) ((firstAttrV::attrV)|>List.map Util.ifSomeTrimmed)),(Util.ifSomeTrimmed text)))
            | Util.RegexMC FULL_DIV_ID_PATTERN [[id];classes;[firstAttrN];[firstAttrV];attrN;attrV;[text]] -> 
                LexTagProper(new LexTagValue("div",(compileAttributes id classes (firstAttrN::attrN) ((firstAttrV::attrV)|>List.map Util.ifSomeTrimmed)),(Util.ifSomeTrimmed text)))
            | Util.RegexMC FULL_DIV_CLASS_PATTERN [classes;[firstAttrN];[firstAttrV];attrN;attrV;[text]] -> 
                LexTagProper(new LexTagValue("div",(compileAttributes "" classes (firstAttrN::attrN) ((firstAttrV::attrV)|>List.map Util.ifSomeTrimmed)),(Util.ifSomeTrimmed text)))
            | _-> LexTagError