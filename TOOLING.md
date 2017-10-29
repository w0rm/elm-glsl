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

1. shader’s code is not modularized;
2. compilation errors are only available in the browser;
3. shader’s code may have to be minified.

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
