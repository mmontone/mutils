# html2who

Parse HTML and create cl-who source code.

[[source code]](../html2who.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1
- **Requires**: cl-who, cl-html5-parser


 Parse HTML and create cl-who source code.

 Usage:

    (html5-parser:parse-html5 #p"/vagrant/admin/index.html" :dom :who :strictp nil)
    (html5-parser:parse-html5-fragment #p"/vagrant/admin/index.html" :dom :who :strictp nil)
    (html5-parser:parse-html5 #p"/vagrant/admin/index.html" :dom :xmls :strictp nil)



