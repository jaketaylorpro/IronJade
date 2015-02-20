namespace IronJade
    module Constants=
        module Regex=
            let REGEX_WORD_START_CHAR="[a-zA-Z]"
            let REGEX_WORD_CHAR="[a-zA-Z0-9_-]"
            let REGEX_WORD_PATTERN=REGEX_WORD_START_CHAR+REGEX_WORD_CHAR+"*"
            let REGEX_UNESCAPED_PATTERN="(?<!(?<!\\\\)\\\\)"
            let REGEX_TAG_PATTERN=System.String.Format(
                                                        "^(?<name>{0})"+ //start with the tag name
                                                        "(?:\#(?<id>{0}))?"+ //optional id
                                                        "(?:\.(?<class>{0}))*" //optional multiple classes
                                                        ,REGEX_WORD_PATTERN)
            let REGEX_DIV_ID_PATTERN=System.String.Format(
                                                        "^\#(<?id>{0})"+ //start with the id
                                                        "(?:\.(?<class>{0}))*" //optional multiple classes
                                                        ,REGEX_WORD_PATTERN)
            let REGEX_DIV_CLASS_PATTERN=System.String.Format(
                                                        "^(?:\.(?<class>{0}))+" //at least one class specified
                                                        ,REGEX_WORD_PATTERN)
            let REGEX_ATTR_PATTERN=System.String.Format(
                                                        "(?:\("+ //optional attributes, inside parens
                                                        "(?:(?<attrn>{0})=\\\"(?<attrv>.*?){1}\\\",\s*)*"+ //multiple name="value" followed by comma;value is lazy matched with a negative look behind to allow escaped double quotes //TODO use forward match to make it so the comma isn't neccisary if it's the last
                                                                          //[\"\'] to enable single quote attributes, but use a backmatch to make sure the opening and closing match
                                                        "\))?" //closing paren
                                                        ,REGEX_WORD_PATTERN,REGEX_UNESCAPED_PATTERN)
            let REGEX_TEXT_PATTERN=("(?<text>.*)$")
            let FULL_TAG_PATTERN=REGEX_TAG_PATTERN+REGEX_ATTR_PATTERN+REGEX_TEXT_PATTERN
            let FULL_DIV_ID_PATTERN=REGEX_DIV_ID_PATTERN+REGEX_ATTR_PATTERN+REGEX_TEXT_PATTERN
            let FULL_DIV_CLASS_PATTERN=REGEX_DIV_CLASS_PATTERN+REGEX_ATTR_PATTERN+REGEX_TEXT_PATTERN
            let GROUP_NAME="name"
            let GROUP_ID="id"
            let GROUP_CLASS="class"
            let GROUP_ATTRN="attrn"
            let GROUP_ATTRV="attrv"
            let GROUP_TEXT="text"
            let TAB_LINE="^[\\t\w].*$"
        module Text=
            let ERR_NO_MATCH_P1="expression:`{0}` did not match a supported pattern"
            let FAIL_TAG_ERROR_P2="there was an error parsing a tag<line:{0}>:`{1}`"
            let FAIL_IMPOSSIBLE_REGEX_MATCH="unexpected regex capturing is causing an unforseen match case"
