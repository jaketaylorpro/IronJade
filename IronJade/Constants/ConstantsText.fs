namespace IronJade.Constants
    module Text=
        let ERR_NO_MATCH_P1="expression:\n\t{0} did not match a supported pattern"
        let FAIL_COMMENT_BLOCK_INVALID_P1="there was a child node in a comment block that did not have a BlockText lexline, instead it had\n\t{0}"
        let FAIL_TAG_ERROR_P2="there was an error parsing a tag<line:{0}>:\n\t{1}"
        let FAIL_IMPOSSIBLE_REGEX_MATCH="unexpected regex capturing is causing an unforseen match case"
        let FAIL_FORMAT_NON_ROOT_NODE="attempted to format a non root node"
        let FAIL_MIXED_INDENTATION_P3="mixed indentation found<line:{0}>:\n\t{1}\nexpecting {2} indentation"