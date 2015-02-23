namespace IronJade.Test
open FsUnit
open NUnit.Framework
open Swensen.Unquote
open System.Text.RegularExpressions
open Newtonsoft.Json
open IronJade
    module ConstantsTests =
        let testFullTagPattern input=
            test <@ Regex.IsMatch(input,Constants.Regex.FULL_TAG_PATTERN)  @>
        //no attr or text
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern just tag`` () :unit=
            testFullTagPattern "table"
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern tag,id`` () :unit=
            testFullTagPattern "table#tableid"
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern tag,classes`` () :unit=
            testFullTagPattern "table.classa.classb.classc"
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern tag,id,classes`` () :unit=
            testFullTagPattern "table#tableid.classa.classb.classc"
        //with text no attr
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern just tag with text`` () :unit=
            testFullTagPattern "table text"
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern tag,id with text`` () :unit=
            testFullTagPattern "table#tableid text"
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern tag,classes with text`` () :unit=
            testFullTagPattern "table.classa.classb.classc text"
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern tag,id,classes with text`` () :unit=
            testFullTagPattern "table#tableid.classa.classb.classc text"
        //with attr no text
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern just tag with attr`` () :unit=
            testFullTagPattern "table(one=\"one\",two=\"two\")"
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern tag,id with attr`` () :unit=
            testFullTagPattern "table#tableid(one=\"one\",two=\"two\")"
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern tag,classes with attr`` () :unit=
            testFullTagPattern "table.classa.classb.classc(one=\"one\",two=\"two\")"
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern tag,id,classes with attr`` () :unit=
            testFullTagPattern "table#tableid.classa.classb.classc(one=\"one\",two=\"two\")"
        //with attr and block
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern just tag with attr,block`` () :unit=
            testFullTagPattern "table(one=\"one\",two=\"two\")."
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern tag,id with attr,block`` () :unit=
            testFullTagPattern "table#tableid(one=\"one\",two=\"two\")."
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern tag,classes with attr,block`` () :unit=
            testFullTagPattern "table.classa.classb.classc(one=\"one\",two=\"two\")."
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern tag,id,classes with attr,block`` () :unit=
            testFullTagPattern "table#tableid.classa.classb.classc(one=\"one\",two=\"two\")."
        //with text and attr
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern just tag with attr,text`` () :unit=
            testFullTagPattern "table(one=\"one\",two=\"two\") text"
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern tag,id with attr,text`` () :unit=
            testFullTagPattern "table#tableid(one=\"one\",two=\"two\") text"
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern tag,classes with attr,text`` () :unit=
            testFullTagPattern "table.classa.classb.classc(one=\"one\",two=\"two\") text"
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``tag pattern tag,id,classes with attr,text`` () :unit=
            testFullTagPattern "table#tableid.classa.classb.classc(one=\"one\",two=\"two\") text"
            
        let testIdTagPattern input=
            test <@ Regex.IsMatch(input,Constants.Regex.FULL_DIV_ID_PATTERN)  @>
        //no attr or text
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``id pattern id`` () :unit=
            testIdTagPattern "#tableid"
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``id pattern id,classes`` () :unit=
            testIdTagPattern "#tableid.classa.classb.classc"
        //with text no attr
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``id pattern id with text`` () :unit=
            testIdTagPattern "#tableid text"
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``id pattern id,classes with text`` () :unit=
            testIdTagPattern "#tableid.classa.classb.classc text"
        //with attr no text
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``id pattern id with attr`` () :unit=
            testIdTagPattern "#tableid(one=\"one\",two=\"two\")"
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``id pattern tag,id,classes with attr`` () :unit=
            testIdTagPattern "#tableid.classa.classb.classc(one=\"one\",two=\"two\")"
        //with attr and block
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``id pattern with attr,block`` () :unit=
            testIdTagPattern "#tableid(one=\"one\",two=\"two\")."
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``id pattern id,classes with attr,block`` () :unit=
            testIdTagPattern "#tableid.classa.classb.classc(one=\"one\",two=\"two\")."
        //with text and attr
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``id pattern id with attr,text`` () :unit=
            testIdTagPattern "#tableid(one=\"one\",two=\"two\") text"
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``id pattern id,classes with attr,text`` () :unit=
            testIdTagPattern "#tableid.classa.classb.classc(one=\"one\",two=\"two\") text"
            
        let testClassPattern input=
            test <@ Regex.IsMatch(input,Constants.Regex.FULL_DIV_CLASS_PATTERN)  @>
        //no attr or text
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``class pattern classes`` () :unit=
            testClassPattern ".classa.classb.classc"
        //with text no attr
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``class pattern classes with text`` () :unit=
            testClassPattern ".classa.classb.classc text"
        //with attr no text
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``class pattern classes with attr`` () :unit=
            testClassPattern ".classa.classb.classc(one=\"one\",two=\"two\")"
        //with attr and block
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``class pattern classes with attr,block`` () :unit=
            testClassPattern ".classa.classb.classc(one=\"one\",two=\"two\")."
        //with text and attr
        [<Test>]
        [<CategoryAttribute("Constants.Regex")>]
        let ``class pattern classes with attr,text`` () :unit=
            testClassPattern ".classa.classb.classc(one=\"one\",two=\"two\") text"