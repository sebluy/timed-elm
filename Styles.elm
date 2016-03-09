module Styles where

dateField : Result String String -> List (String, String)
dateField result =
  let style = case result of
                Ok _ ->
                  goodDate
                Err _ ->
                  badDate
  in style ++ inputBorderless


input =
  [ ("border", "0px") ]
  ++ inputBorderless

centeredInput =
  input ++ centered ++
  [ ("width", "100%") ]


inputBorderless =
    [ ("outline", "none")
    , ("font-family", "Arial sans-serif")
    , ("font-size", "16px")
    ]

green = "#00FF00"

red = "#FF0000"

bottomBorder color =
  [ ("border-bottom", "solid 3px " ++ color)
  , ("border-top", "none")
  , ("border-right", "none")
  , ("border-left", "none")
  ]


goodDate = bottomBorder green
badDate = bottomBorder red


tableNarrow =
  [ ("width", "50%")
  , ("margin", "auto")
  ]

tableWide =
  [ ("margin", "auto")]

tdText =
  ("font-size", "30px") :: td

td =
  [ ("padding", "20px") ] ++ centered

tr =
  [ ("padding", "20px") ]


centered =
  [ ("text-align", "center") ]


button =
  [ ("height", "40px")
  , ("border-radius", "5px")
  , ("float", "right")
  , ("margin-right", "20px")
  , ("padding-right", "20px")
  , ("padding-left", "20px")
  ]


div =
  [ ("border", "3px solid")
  , ("margin", "30px auto 30px auto")
  , ("padding", "20px")
  , ("width", "50%")
  ]

h1 = []
