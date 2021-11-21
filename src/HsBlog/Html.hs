module HsBlog.Html
    (
    -- * HTML EDSL
      Html (..)
    , html_
    -- ** Combinators used to construct the @\<head\>@ section
    , Header (..)
    , title_
    , meta_
    , stylesheet_
    -- ** Combinators used to construct the @\<body\>@ section
    , Structure (..)
    , p_
    , h_
    , ul_
    , ol_
    , code_
    -- ** Combinators used to construct content within structures
    , Content (..)
    , txt_
    , link_
    , img_
    , b_
    , i_
    -- ** Render HTML to String
    , render
    )
    where

import HsBlog.Html.Internal

