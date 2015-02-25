namespace IronJade
open HtmlAgilityPack
    module Formatter=
        let formatLexNode (rootNode:LexNode) :string=
            let indType = match rootNode.LexLine with
                          | LexLine.Root(ind,_) -> ind
                          | _ -> failwith Constants.Text.FAIL_FORMAT_NON_ROOT_NODE
            let reindentText (text:string) (ind:int) :string=
                match indType with
                |Indentation.Space(n) -> new System.String(' ',n*ind)+text
                |Indentation.Tab -> new System.String('\t',ind)+text
            let mutable doc = new HtmlDocument()
            let rec h (n:LexNode) (parent:ref<HtmlNode>) (doc:ref<HtmlDocument>):unit=
                let hchildren (n:LexNode) (parent:ref<HtmlNode>)=
                    n.ChildNodes
                    |>List.fold (fun (acc:ref<HtmlNode>*ref<HtmlDocument>) v ->
                                                                                h v (fst acc) (snd acc)
                                                                                acc) (parent,doc)
                match n.LexLine with
                |LexLine.DocType(t) -> 
                    let c=doc.contents.CreateComment(System.String.Format("<!DOCTYPE {0}>",t))
                    ignore (parent.contents.PrependChild(c))
                |LexLine.TextBlockLine(text)|LexLine.TextLine(text) -> 
                    ignore (parent.contents.AppendChild(doc.contents.CreateTextNode("\n"+(reindentText text n.Indentation))))//TODO figure out rules about when to indent text and not
                |LexLine.Root(_,_) -> ignore (hchildren n parent)
                |LexLine.Tag(tag) -> match tag with
                                     |LexTag.LexTagError(errorMessage)|LexTag.LexTagInnerError(errorMessage)-> failwith (System.String.Format(Constants.Text.FAIL_TAG_ERROR_P2,n.LineNumber,errorMessage))
                                     |LexTag.LexTagProper({Name="//";Attributes=_;LexInnerTag=LexInnerTag.Inline(text)}) ->
                                        let c=doc.contents.CreateComment(System.String.Format("<!--{0}-->",text))
                                        ignore (parent.contents.AppendChild(c))
                                     |LexTag.LexTagProper({Name="//";Attributes=_;LexInnerTag=LexInnerTag.BlockText}) ->
                                        let text=n.ChildNodes
                                                 |>List.fold (fun acc cn->  let newline=match acc with
                                                                                        |"" -> ""
                                                                                        |_ -> "\n"
                                                                            let ct = match cn.LexLine with
                                                                                     |LexLine.TextBlockLine(t) -> (reindentText t n.Indentation)
                                                                                     |_ -> failwith (System.String.Format(Constants.Text.FAIL_COMMENT_BLOCK_INVALID_P1,cn.LexLine.ToString()))
                                                                            acc+newline+ct) ""
                                        let c=doc.contents.CreateComment(System.String.Format("<!--{0}-->",text))
                                        ignore (parent.contents.AppendChild(c))

                                     |LexTag.LexTagProper({Name="//-";Attributes=_;LexInnerTag=_}) ->()
                                     |LexTag.LexTagProper({Name=name;Attributes=attributes;LexInnerTag=lexInnerTag})->
                                         let mutable e=doc.contents.CreateElement name
                                         ignore (attributes|>List.fold (fun (acc:ref<HtmlNode>) (n,v)->
                                             ignore (acc.contents.SetAttributeValue(n,Util.ifNoneEmpty v))
                                             acc) (ref e))
                                         ignore (parent.contents.AppendChild(e))
                                         match lexInnerTag with
                                         |LexInnerTag.Inline(text) ->
                                             ignore (e.AppendChild(doc.contents.CreateTextNode(text)) )
                                         |LexInnerTag.BlockText|LexInnerTag.Normal ->
                                             ignore (hchildren n (ref e))
                                         |LexInnerTag.InnerLexTagError(_) -> () //TODO this isn't reachable, all inner errors cause the tag to not be a LexTagProper
            h rootNode (ref doc.DocumentNode) (ref doc)
            doc.DocumentNode.OuterHtml


