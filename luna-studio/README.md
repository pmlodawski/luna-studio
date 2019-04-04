This is main source folder of luna-studio, you can see here 4 subprojects here, which create luna-studio application:

* **[node-editor](node-editor)** - this is main project of node editor application, you can find most of application logi there
* **[node-editor-view](node-editor-view)** - this is a subproject responsible for content rendering. In the past it was rendering to react, now it is using basegl-ui
* **[text-editor](text-editor)** - this is deprecated project of a text-editor logic which was used in atom plugin. It has to be merged into node-editor
* **[lib](lib)** - this is a common library shared between node-editor and text-editor packages
