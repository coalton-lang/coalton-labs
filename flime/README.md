# FLIME

Flime is a library designed to compile Coalton source code in real time and provide immediate error feedback during development.

Key features:

* **Automated compilation:** Unlike SLIME that requires to specify code snippets to compile, Flime continuously compiles the editing code, providing instant feedback without manual intervention.
* **Project-wide compilation**: Instead of focusing on individual files, Flime compiles entire projects, making it ideal for handling codebases that span multiple files.
* **Conflict-free dependencies**: Compilation is executed in a separate process, avoiding any conflicts with Flime’s own dependencies.
* **Multi-project support**: Each project is compiled in its own isolated subprocess, ensuring that multiple projects can be handled simultaneously without interference.

It provides a dedicated API specifically designed for integration with other tools, such as Language Server Protocol (LSP) servers.

Note that Flime is not intended to replace existing tools like SLIME or the LSP server, nor does it dismiss the value of interactive development styles on REPL. Instead, it complements them by providing continuous execution to help developers quickly notice errors as they arise.

## Warning

This software is still ALPHA quality. The APIs likely change.

## Usage

```common-lisp
(defvar *workspace* (flime:make-workspace))

(flime:open-file *workspace* #P"~/project/main.coal")
(flime:edit-file *workspace* #P"~/project/main.coal"
                 "fibonatti"
                 :start '(4 . 8)
                 :end '(4 . 12))

(flime:check-file *workspace* #P"~/project/main.coal")
;=> (#.(FLIME/CORE/PROJECT:COMPILERNOTE #.(FLIME/CORE/TEXT-DOCUMENT:RANGE #.(FLIME/CORE/TEXT-DOCUMENT:POINT 0 1) #.(FLIME/CORE/TEXT-DOCUMENT:POINT 0 1)) #P"~/project/main.coal" "Malformed package declaration" "package declarations must start with `package`"))

(flime:close-file *workspace* #P"~/project/main.coal")
;=> T
```

## See Also

* [coalton-lang/coalton-mode](https://github.com/coalton-lang/coalton-mode)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2025 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the MIT License. See the [LICENSE](./LICENSE) file.
