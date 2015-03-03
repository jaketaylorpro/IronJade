namespace IronJade.Mvc

type IronJadeView(jadeFilePath:string) = 
    let readEnvironmentFromDynamic (dyn:obj) :List<string*obj>=
        IronJade.Util.Reflection.objToKvp dyn
    member this.JadeNode = 
        let jadeLines=System.IO.File.ReadAllLines(jadeFilePath)
        IronJade.Lexer.lexLines(jadeLines)
    interface System.Web.Mvc.IView with
        member this.Render(viewContext:System.Web.Mvc.ViewContext,writer:System.IO.TextWriter) :unit=
            let env=readEnvironmentFromDynamic viewContext.ViewBag
            let compiledNode=IronJade.Compiler.compileLexNode env this.JadeNode
            IronJade.Formatter.formatLexNode compiledNode (ref writer)
