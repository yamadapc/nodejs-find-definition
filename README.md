# nodejs-find-definition
Work-in progress server to find the definition of some token in a Node.JS
module. Support for finding the definition of tokens local to a module is
paritally implemented and using module resolution, the `package.json` files and
ASTs of the dependencies, I'll try to implement finding the definition of any
token in a module, be it defined in another file, or inside of a dependency
nested need bellow `node_modules`.

## License
This code is licensed under the MIT license for Pedro Tacla Yamada. For more
information refer to the [LICENSE](/LICENSE) file.
