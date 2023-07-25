# html2who

Parse HTML and create cl-who source code.

[[source code]](../html2who.lisp)

- **Requires**: cl-who, cl-html5-parser
- **Version**: 0.1
- **Author**: Mariano Montone <marianomontone@gmail.com>


 Parse HTML and create cl-who source code.

 Usage:

   (html5-parser:parse-html5 #p"/vagrant/admin/index.html" :dom :who :strictp nil)
   (html5-parser:parse-html5-fragment #p"/vagrant/admin/index.html" :dom :who :strictp nil)
   (html5-parser:parse-html5 #p"/vagrant/admin/index.html" :dom :xmls :strictp nil)



