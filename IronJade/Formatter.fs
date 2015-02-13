namespace IronJade
open LexTag
open LexLine
open LexNode
open System.Xml
open HtmlAgilityPack
    module Formatter=
        let formatLexNode (rootNode:LexNode) :string=
            let mutable doc = new HtmlDocument()
            let rec h (n:LexNode) (parent:ref<HtmlNode>) (doc:ref<HtmlDocument>):unit=
                match n.LexLine with
                |LexLine.TextBlockLine(text)|LexLine.TextLine(text) -> 
                    parent.contents.AppendChild(doc.contents.CreateTextNode(text))
                    ()
                |LexLine.Root -> ()
                |LexLine.Tag(tag) -> match tag with
                                        |LexTag.LexTagError-> ()
                                        |LexTag.LexTagProper(v)->
                                        let mutable e=doc.contents.CreateElement v.Name
                                        v.Attributes|>List.fold (fun (acc:ref<HtmlNode>) (n,v)->
                                            acc.contents.SetAttributeValue(n,Util.ifNoneEmpty v)
                                            acc) (ref e)
                                        parent.contents.AppendChild(e)
                                        if v.Text.IsSome 
                                        then 
                                            e.AppendChild(doc.contents.CreateTextNode(v.Text.Value)) 
                                            ()
                                        else 
                                            n.ChildNodes
                                            |>List.fold (fun (acc:ref<HtmlNode>*ref<HtmlDocument>) v ->
                                                h v (fst acc) (snd acc)
                                                acc) ((ref e),doc)   
                                            ()             
                ()
            h rootNode (ref doc.DocumentNode) (ref doc)
            doc.DocumentNode.OuterHtml


