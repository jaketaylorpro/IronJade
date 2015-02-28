namespace IronJade.Constants
    module Regex=
        let private RegexWordPattern="[a-zA-Z][a-zA-Z0-9_-]*"
        let private RegexUnescapedPattern="(?<!(?<!\\\\)\\\\)"
        let public FullTagPattern=["^"; //begin
                                   "(?<name>{0})?"; //start with the tag name
                                   "(?:\#(?<id>{0}))?"; //optional id
                                   "(?:\.(?<class>{0}))*"; //optional multiple classes
                                   //attributes
                                   "(?:\("; //open parentisis
                                     "(?:"; //noncapture repeat
                                       "(?<attrn>{0})"; //attribute name
                                       "\s*=\s*"; //equals sign optionally surrounded by whitespace
                                       "\\\"(?<attrv>.*?){1}\\\""; //value is lazy matched with a negative look behind to allow escaped double quotes
                                       "((?=\s*\))\s*|\s*,\s*)";  //this is an if statement, if lookahead finds whitespace and then an close parentisis, no comma, otherwise, comma is needed. comma can be surrounded by whitespace
                                     ")*"; //\noncapture repeat
                                   "\))?"; //closing parentisis
                                   //\attributes
                                   "(?:(?<dot>\.)||(?<colon>\:)? (?<text>.*))"; //optional nesting (colon), than space, and text (or nested tag)]
                                   "$"] //end
                                  |>List.map (fun t->System.String.Format(t,RegexWordPattern,RegexUnescapedPattern))
                                  |>List.fold (fun s t->s+t) ""
        let public GroupName="name"
        let public GroupId="id"
        let public GroupClass="class"
        let public GroupAttrn="attrn"
        let public GroupAttrv="attrv"
        let public GroupDot="dot"
        let public GroupColon="colon"
        let public GroupText="text"
        let public TabLine="^[\\t\w].*$"