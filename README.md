# anseml ![](https://github.com/akarpovskii/anseml/workflows/Clojure%20CI/badge.svg)
ANSEML is ANother S-Expression Markup Language inspired by XML format.

The library provides a simple API to work with documents and a tool to convert the document to HTML format.

The document format is pretty straightforward.
- Each ANSEML document has exactly one single root element, which encloses all the other elements.
- Each element may or may not have a set of attributes.

More specifically, the format is following:

```
(:tag-name
    attr-map?
    body)
```

Where `attr-map?` is an associative array of `{ key value }` pairs,
where `key` is `keyword` and `value` is one of the following _primitives_: `string, integer, float`; and `body` contains zero or more _primitives_ or enclosed elements.

Here are few examples:

```
(:root
    { :id "Root" }
    "Here is a number:" 123
    "And here is the text:"
    (:text
        "Some text")
    :br
    (:text
        "Another text"))
```  

Note that you don't need to put parenthesis around the element consisting only of a tag-name. 
