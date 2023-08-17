# image-dimensions-reader

Get image dimensions (PNG/JPG) without loading the file.

[[source code]](../image-dimensions-reader.lisp)

- **Author**: Mihai Bazon
- **Version**: 0.1


 Get image dimensions (PNG/JPG) without loading the file

 See: https://lisperator.net/blog/get-image-dimensions-png-jpg-without-loading-the-file-in-common-lisp/



## Generic-Functions
### image-size
Get the sizes of the image designated by INPUT, without loading the file.
INPUT can be a PATHNAME, a STRING or a STREAM.
The size of the image are returned in a list: (width height).

