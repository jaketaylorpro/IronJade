namespace IronJade
open HtmlAgilityPack
    module Formatter=
        let formatLexNode (rootNode:LexNode.LexNode) :string=
            let mutable doc = new HtmlDocument()
            let rec h (n:LexNode.LexNode) (parent:ref<HtmlNode>) (doc:ref<HtmlDocument>):unit=
                match n.LexLine with
                |LexLine.DocType(t) -> 
                    let c=doc.contents.CreateComment(System.String.Format("<!DOCTYPE {0}>",t))
                    ignore (parent.contents.AppendChild(c))
                |LexLine.TextBlockLine(text)|LexLine.TextLine(text) -> 
                    ignore (parent.contents.AppendChild(doc.contents.CreateTextNode(text)))
                |LexLine.Root -> ()
                |LexLine.Tag(tag) -> match tag with
                                     |LexTag.LexTagError(_)|LexTag.LexTagInnerError(_)-> ()
                                     |LexTag.LexTagProper({Name="//";Attributes=_;LexInnerTag=_}) ->()//TODO we need to create commentNodes and insert them into the doc
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
                                             ignore (n.ChildNodes
                                                     |>List.fold (fun (acc:ref<HtmlNode>*ref<HtmlDocument>) v ->
                                                         h v (fst acc) (snd acc)
                                                         acc) ((ref e),doc))
                                         |LexInnerTag.InnerLexTagError(_) -> ()
            h rootNode (ref doc.DocumentNode) (ref doc)
            doc.DocumentNode.OuterHtml


