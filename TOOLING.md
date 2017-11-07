# WebGL Shader Tooling

## Preface

In order for a shader to be used in a WebGL program, it must be put in a string and compiled in the browser:

```js
const source = `
  void main() {
    gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
  }
`;
const shader = gl.createShader(gl.FRAGMENT_SHADER);
gl.shaderSource(shader, source);
gl.compileShader(shader);
if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
  const info = gl.getShaderInfoLog(shader);
  throw 'Oops, compilation failed:\n\n' + info;
}
```

Without proper tooling, this would've been a bad developer experience, because:

1. long time from editing the code to seing results in the browser;
2. shader’s code is not modularized;
3. compilation errors are only available in the browser;
4. shader’s code may have to be minified.

## Tooling

Existing tooling improves the developer experience by addressing these concerns.

### [glslify](https://github.com/glslify/glslify)

glslify is a preprocessing tool that allows to modularize shader’s code in a node.js-style. There are a lot of available packages on npm, check the [Shader Components](http://stack.gl/packages/) section.

The syntax is extended with `#pragma glslify` to allow import and export code, e.g.:

```glsl
#pragma glslify: hsl2rgb = require(glsl-hsl2rgb)
vec3 rgb = hsl2rgb(75.0 / 360.0, 0.5, 0.25);
```

glslify addresses the modularization, but it doesn't verify the correctness of glsl syntax. So devs have to rely on [console errors](https://youtu.be/Aq7diSfU9Cc?t=1263).

### In Browser Editing

Editing shaders in a browser not only provides fast visual feedback, but also verifies the correctness of syntax and highlights compilation errors. There is a bunch of online editors for the fragment shader’s code, e.g. [Shadertoy](https://www.shadertoy.com/), [GLSL Sandbox](http://glslsandbox.com/), [Shdr](http://shdr.bkcore.com/). Firefox Developer Tools include [Shader Editor](https://developer.mozilla.org/en-US/docs/Tools/Shader_Editor) that may be used to edit already linked vertext and fragment shaders in the running application. There is also [Shader Editor Extension](https://github.com/spite/ShaderEditorExtension) that adds live editing support to Google Chrome.

Being able to live edit shaders is powerful, but is only available in the browser, meaning you cannot get the feedback from your editor of choice.

### [glslx](http://evanw.github.io/glslx/)

glslx is a type checker and minifier for WebGL shaders. It can be integrated in [Visual Studio Code](https://github.com/evanw/glslx-vscode) editor and provides nice features, such as inline errors, type tooltips, go-to-definition, and symbol renaming. It is a powerful tool, that allows to eliminate potential errors straight in the source code, without looking in the browser. It also allows to minify glsl code which might be useful.

glslx actually is an extension on top of glsl, that supports exporting multiple shaders from the same file, but it doesn’t support modularization and is not able to parse glslify `#pragma` definitions.

