type HTML = String
type Attributes = String
type TagName = String
type Props = [(String, String)]
type Children = [Element]
data Element = 
  Text String
  |
  Tag TagName Props Children
  deriving (Eq, Show)

renderProps :: Props -> Attributes
renderProps [] = ""
renderProps (x:xs) = attr ++ (renderProps xs)
  where
    attr = " " ++ attrName ++ "=\"" ++ attrValue ++ "\""
    (attrName, attrValue) = x

renderChildren :: Children -> HTML
renderChildren [] = ""
renderChildren (x:xs) = (renderToString x) ++ (renderChildren xs)

renderToString :: Element -> HTML
renderToString (Text textContent) = textContent
renderToString (Tag tagName props children) = html
  where
    html = openTag ++ childrenString ++ closeTag
    openTag = "<" ++ tagName ++ (renderProps props) ++ ">"
    closeTag = "</" ++ tagName ++ ">"
    childrenString = renderChildren children

header :: String -> String -> Element
header title description = Tag "header" [("class", "header")] 
  [
    Tag "h1" [("class", "title")] 
      [Text title],
    Tag "p" [("class", "description")]
      [Text description]
  ]

renderHeader :: String -> String -> HTML
renderHeader title description = renderToString (header title description)
    