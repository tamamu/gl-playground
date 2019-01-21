
extern crate glfw;
use glfw::{Action, Context, Key};

extern crate gl;
use gl::types::*;

extern crate image;
use image::GenericImageView;

use std::sync::mpsc::Receiver;
use std::ffi::CString;
use std::ptr;
use std::str;
use std::mem;
use std::os::raw::c_void;
use std::collections::HashMap;
use std::path::Path;

const vertexShaderSource: &str = r#"
    #version 330 core
    layout (location = 0) in vec3 aPos;
    layout (location = 1) in vec2 aTexCoord;
    out vec2 TexCoord;
    void main() {
        gl_PointSize = 10.0;
        gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
        TexCoord = vec2(aTexCoord.x, aTexCoord.y);
    }
"#;

const fragmentShaderSource: &str = r#"
    #version 330 core
    out vec4 FragColor;
    in vec2 TexCoord;
    uniform sampler2D tex;
    void main() {
        //FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);
        FragColor = texture(tex, TexCoord);
    }
"#;

const vertexShaderSource2: &str = r#"
    #version 330 core
    layout (location = 0) in vec3 aPos;
    layout (location = 1) in vec2 aTexCoord;
    out vec2 TexCoord;
    out vec2 vPos;
    void main() {
        vPos = aPos.xy;
        gl_PointSize = 10.0;
        gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
        TexCoord = vec2(aTexCoord.x, aTexCoord.y);
    }
"#;

const fragmentShaderSource2: &str = r#"
    #version 330 core
    in vec2 vPos;
    out vec4 FragColor;
    in vec2 TexCoord;
    uniform sampler2D tex;
    uniform float t;
    uniform int showPoint;
    uniform int showLine;
    void calc_rectangle_position(in float t, out vec2 v) {
        float u = mod(t, 4.0);
        if (u > 3.0) {
            v = vec2(-0.5, 0.5 - (u - 3.0));
        } else if (u > 2.0) {
            v = vec2(0.5 - (u - 2.0), 0.5);
        } else if (u > 1.0) {
            v = vec2(0.5, -0.5 + (u - 1.0));
        } else {
            v = vec2(-0.5 + u, -0.5);
        }
    }
    float calc_opacity(float t) {
        return t * t;
    }
    void main() {
        vec2 p = vPos;
        vec2 a; calc_rectangle_position(t, a);
        vec2 b; calc_rectangle_position(3.14 + (t * 0.64), b);
        vec2 c; calc_rectangle_position(1.41 + (t * 1.28), c);
        vec2 ab = b - a;
        vec2 bp = p - b;
        vec2 bc = c - b;
        vec2 cp = p - c;
        vec2 ca = a - c;
        vec2 ap = p - a;
        float c1 = ab.x * bp.y - ab.y * bp.x;
        float c2 = bc.x * cp.y - bc.y * cp.x;
        float c3 = ca.x * ap.y - ca.y * ap.x;
        vec2 n = (vPos + 1.0) / 2.0;
        if (showPoint == 1 && (distance(p, a) < 0.015 || distance(p, b) < 0.015 || distance(p, c) < 0.015)) {
            FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);
        } else if (showLine == 1 && (abs(p.x)-0.499 > 0 || abs(p.y)-0.499 > 0)) {
            FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);
        } else {
            if ((c1 > 0 && c2 > 0 && c3 > 0) || (c1 < 0 && c2 < 0 && c3 < 0)) {
                //FragColor = vec4(1.0f, 0.5f, 0.2f, calc_opacity(mod(t, 2.0)-1.0));
                const float shakeLength = 0.1;
                const float shakeWidth = 0.01;
                const float speed = 1.0;

                float offsetX = sin(gl_FragCoord.x * shakeLength + t * speed) * shakeWidth;
                float offsetY = cos(gl_FragCoord.y * shakeLength + t * speed) * shakeWidth;
                vec4 color = texture(tex, vec2(TexCoord.x + offsetX , TexCoord.y + offsetY));
                FragColor = color;
            } else {
                FragColor = vec4(1.0f, 1.0f, 1.0f, 0.0f);
            }
        }
    }
"#;

static mut show_point: bool = true;
static mut show_line: bool = true;
static mut program_number: u32 = 0;

type Vertices1 = [f32; 9];
type Vertices2 = [f32;12];
type Vec2f4=[f32; 8];

enum GLType {
    Int,
    Float,
    Uint,
}

struct GLVariable {
    data_type: GLType,
    size: GLsizeiptr,
}

struct BufferPosition {
    offset: GLsizeiptr,
    size: GLsizeiptr,
}

struct VertexBufferObject<T> {
    id: GLuint,
    data_type: GLType,
    data: Box<[T]>,
    usage: GLenum,
    positions: Vec<BufferPosition>,
}

struct VertexArrayObject {
    id: GLuint,
    vbos: Vec<VertexBufferObject<f32>>,
    ebos: Vec<VertexBufferObject<u32>>,
}

struct Shader {
    program: GLuint,
    uniforms: HashMap<String, GLVariable>,
}

impl Shader {
    pub fn new(vertex_shader: &str, fragment_shader: &str) -> Self {
        unsafe {
            let vertexShader = gl::CreateShader(gl::VERTEX_SHADER);
            let fragmentShader = gl::CreateShader(gl::FRAGMENT_SHADER);
            let c_str_vert = CString::new(vertex_shader.as_bytes()).unwrap();
            let c_str_frag = CString::new(fragment_shader.as_bytes()).unwrap();
            let mut success = gl::FALSE as GLint;
            let mut infoLog = Vec::with_capacity(512);
            let shaderProgram = gl::CreateProgram();

            // compile vertex shader
            {
                gl::ShaderSource(vertexShader, 1, &c_str_vert.as_ptr(), ptr::null());
                gl::CompileShader(vertexShader);
                // check compile errors
                infoLog.set_len(512 - 1); // skip null character
                gl::GetShaderiv(vertexShader, gl::COMPILE_STATUS, &mut success);
                if success != gl::TRUE as GLint {
                    gl::GetShaderInfoLog(vertexShader, 512, ptr::null_mut(), infoLog.as_mut_ptr() as *mut GLchar);
                    println!("ERROR::SHADER::VERTEX::COMPILATION_FAILED\n{}", str::from_utf8(&infoLog).unwrap());
                }
            }

            // compile fragment shader
            {
                gl::ShaderSource(fragmentShader, 1, &c_str_frag.as_ptr(), ptr::null());
                gl::CompileShader(fragmentShader);
                // check compile errors
                gl::GetShaderiv(fragmentShader, gl::COMPILE_STATUS, &mut success);
                if success != gl::TRUE as GLint {
                    gl::GetShaderInfoLog(fragmentShader, 512, ptr::null_mut(), infoLog.as_mut_ptr() as *mut GLchar);
                    println!("ERROR::SHADER::FRAGMENT::COMPILATION_FAILED\n{}", str::from_utf8(&infoLog).unwrap());
                }
            }

            // link shader
            {
                gl::AttachShader(shaderProgram, vertexShader);
                gl::AttachShader(shaderProgram, fragmentShader);
                gl::LinkProgram(shaderProgram);
                // check link errors
                gl::GetProgramiv(shaderProgram, gl::LINK_STATUS, &mut success);
                if success != gl::TRUE as GLint {
                    gl::GetProgramInfoLog(shaderProgram, 512, ptr::null_mut(), infoLog.as_mut_ptr() as *mut GLchar);
                    println!("ERROR::SHADER::PROGRAM::COMPILATION_FAILED\n{}", str::from_utf8(&infoLog).unwrap());
                }
                gl::DeleteShader(vertexShader);
                gl::DeleteShader(fragmentShader);
            }
            Shader {
                program: shaderProgram,
                uniforms: HashMap::new(),
            }
        }
    }

    pub unsafe fn useProgram(&self) {
        gl::UseProgram(self.program);
    }
}

fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();
    glfw.window_hint(glfw::WindowHint::ContextVersion(3, 3));
    glfw.window_hint(glfw::WindowHint::OpenGlProfile(glfw::OpenGlProfileHint::Core));
    #[cfg(target_os = "macos")]
    glfw.window_hint(glfw::WindowHint::OpenGlForwardCompat(true));

    let (mut window, events) = glfw.create_window(800, 600, "TRIANGLE", glfw::WindowMode::Windowed)
        .expect("Failed to create GLFW window.");

    window.make_current();
    window.set_key_polling(true);
    window.set_framebuffer_size_polling(true);

    gl::load_with(|symbol| window.get_proc_address(symbol) as *const _);
    unsafe {
        gl::Enable(gl::PROGRAM_POINT_SIZE);
        //gl::Enable(gl::DEPTH_TEST);
        gl::Enable(gl::BLEND);
        gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);
        gl::LineWidth(8.0);
    }

    let mut delta_time: f32;
    let mut last_frame: f32 = 0.0;

    let mut tri_vertices: Vertices1 = [
        -0.5, -0.5, 0.0,
         0.5, -0.5, 0.0,
         0.0,  0.5, 0.0
    ];
    let mut rect_vertices: Vertices2 = [
        -0.5, -0.5, 0.0,
         0.5, -0.5, 0.0,
         0.5,  0.5, 0.0,
        -0.5,  0.5, 0.0
    ];
    let tex_coords: Vec2f4 = [
        1.0, 1.0,
        0.0, 1.0,
        0.0, 0.0,
        1.0, 0.0,
    ];
    let fill_indices = [
        0, 1, 3, 2
    ];

    let program1 = Shader::new(vertexShaderSource, fragmentShaderSource);
    let (mut VBO1_1, mut VBO1_2, mut VAO1) = (0, 0, 0);
    let mut texture1 = 0;
    unsafe {
        gl::GenVertexArrays(1, &mut VAO1);
        gl::GenBuffers(1, &mut VBO1_1);
        gl::GenBuffers(1, &mut VBO1_2);

        // bind VAO first, then bind and set vertex buffer, and then configure vertex attribute
        gl::BindVertexArray(VAO1);
        // link VBO to variable of vertex shader at location 0
        {
            // send data to GPU
            {
                gl::BindBuffer(gl::ARRAY_BUFFER, VBO1_1);
                gl::BufferData(gl::ARRAY_BUFFER,
                               (tri_vertices.len() * mem::size_of::<GLfloat>()) as GLsizeiptr,
                               tri_vertices.as_ptr() as *const c_void,
                               gl::DYNAMIC_DRAW);
            }

            // set location 0 of vertex shader to 3 floats
            gl::VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, 3 * mem::size_of::<GLfloat>() as GLsizei, ptr::null());
            gl::EnableVertexAttribArray(0);

            {
                gl::BindBuffer(gl::ARRAY_BUFFER, VBO1_2);
                gl::BufferData(gl::ARRAY_BUFFER,
                               (tex_coords.len() * mem::size_of::<GLfloat>()) as GLsizeiptr,
                               tex_coords.as_ptr() as *const c_void,
                               gl::STATIC_DRAW);
            }

            gl::VertexAttribPointer(1, 2, gl::FLOAT, gl::FALSE, 2 * mem::size_of::<GLfloat>() as GLsizei, ptr::null());
            gl::EnableVertexAttribArray(1);
        }

        gl::GenTextures(1, &mut texture1);
        gl::BindTexture(gl::TEXTURE_2D, texture1);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::REPEAT as i32); // set texture wrapping to gl::REPEAT (default wrapping method)
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::REPEAT as i32);
        // set texture filtering parameters
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR as i32);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);

        let img = image::open(&Path::new("sea.png")).expect("Failed to load texture");
        let data = img.raw_pixels();
        gl::TexImage2D(gl::TEXTURE_2D,
                       0,
                       gl::RGBA as i32,
                       img.width() as i32,
                       img.height() as i32,
                       0,
                       gl::RGBA,
                       gl::UNSIGNED_BYTE,
                       &data[0] as *const u8 as *const c_void);
        gl::GenerateMipmap(gl::TEXTURE_2D);

        gl::BindBuffer(gl::ARRAY_BUFFER, 0);

        gl::BindVertexArray(0);
    }


    let program2 = Shader::new(vertexShaderSource, fragmentShaderSource);
    let (mut VBO2_1, mut VBO2_2, mut VAO2) = (0, 0, 0);
    let mut texture2 = 0;
    unsafe {
        gl::GenVertexArrays(1, &mut VAO2);
        gl::GenBuffers(1, &mut VBO2_1);

        // bind VAO first, then bind and set vertex buffer, and then configure vertex attribute
        gl::BindVertexArray(VAO2);
        // link VBO to variable of vertex shader at location 0
        {
            // send data to GPU
            {
                gl::BindBuffer(gl::ARRAY_BUFFER, VBO2_1);
                gl::BufferData(gl::ARRAY_BUFFER,
                               (rect_vertices.len() * mem::size_of::<GLfloat>()) as GLsizeiptr,
                               rect_vertices.as_ptr() as *const c_void,
                               gl::STATIC_DRAW);
            }

            // set location 0 of vertex shader to 3 floats
            gl::VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, 3 * mem::size_of::<GLfloat>() as GLsizei, ptr::null());
            gl::EnableVertexAttribArray(0);
        }

        gl::BindBuffer(gl::ARRAY_BUFFER, 0);

        gl::BindVertexArray(0);
    }

    let program3 = Shader::new(vertexShaderSource2, fragmentShaderSource2);
    let (mut VBO3, mut VAO3, mut EBO3) = (0, 0, 0);
    let mut texture3 = 0;
    unsafe {
        gl::GenVertexArrays(1, &mut VAO3);
        gl::GenBuffers(1, &mut VBO3);
        gl::GenBuffers(1, &mut EBO3);

        // bind VAO first, then bind and set vertex buffer, and then configure vertex attribute
        gl::BindVertexArray(VAO3);
        // link VBO to variable of vertex shader at location 0
        {
            // send data to GPU
            {
                gl::BindBuffer(gl::ARRAY_BUFFER, VBO3);
                gl::BufferData(gl::ARRAY_BUFFER,
                               (rect_vertices.len() * mem::size_of::<GLfloat>()) as GLsizeiptr,
                               rect_vertices.as_ptr() as *const c_void,
                               gl::STATIC_DRAW);
            }
            {
                gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, EBO3);
                gl::BufferData(gl::ELEMENT_ARRAY_BUFFER,
                               (fill_indices.len() * mem::size_of::<GLfloat>()) as GLsizeiptr,
                               fill_indices.as_ptr() as *const c_void,
                               gl::STATIC_DRAW);
            }

            // set location 0 of vertex shader to 3 floats
            gl::VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, 3 * mem::size_of::<GLfloat>() as GLsizei, ptr::null());
            gl::EnableVertexAttribArray(0);

            {
                gl::BindBuffer(gl::ARRAY_BUFFER, VBO1_2);
                gl::BufferData(gl::ARRAY_BUFFER,
                               (tex_coords.len() * mem::size_of::<GLfloat>()) as GLsizeiptr,
                               tex_coords.as_ptr() as *const c_void,
                               gl::STATIC_DRAW);
            }

            gl::VertexAttribPointer(1, 2, gl::FLOAT, gl::FALSE, 2 * mem::size_of::<GLfloat>() as GLsizei, ptr::null());
            gl::EnableVertexAttribArray(1);
        }

        gl::GenTextures(1, &mut texture3);
        gl::BindTexture(gl::TEXTURE_2D, texture3);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::REPEAT as i32); // set texture wrapping to gl::REPEAT (default wrapping method)
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::REPEAT as i32);
        // set texture filtering parameters
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR as i32);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);

        let img = image::open(&Path::new("sea.png")).expect("Failed to load texture");
        let data = img.raw_pixels();
        gl::TexImage2D(gl::TEXTURE_2D,
                       0,
                       gl::RGBA as i32,
                       img.width() as i32,
                       img.height() as i32,
                       0,
                       gl::RGBA,
                       gl::UNSIGNED_BYTE,
                       &data[0] as *const u8 as *const c_void);
        gl::GenerateMipmap(gl::TEXTURE_2D);

        gl::BindBuffer(gl::ARRAY_BUFFER, 0);

        gl::BindVertexArray(0);
    }

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_events(&mut window, &events);

        unsafe {
            gl::ClearColor(0.2, 0.3, 0.3, 1.0);
            gl::Clear(gl::COLOR_BUFFER_BIT);

            match program_number {
                0 => {
                    program1.useProgram();
                    // update triangle position
                    {
                        let (x1, y1) = calc_rectangle_position(last_frame);
                        let (x2, y2) = calc_rectangle_position(3.14 + last_frame * 0.64);
                        let (x3, y3) = calc_rectangle_position(1.41 + last_frame * 1.28);
                        tri_vertices[0] = x1;
                        tri_vertices[1] = y1;
                        tri_vertices[3] = x2;
                        tri_vertices[4] = y2;
                        tri_vertices[6] = x3;
                        tri_vertices[7] = y3;
                        gl::BindBuffer(gl::ARRAY_BUFFER, VBO1_1);
                        gl::BufferSubData(gl::ARRAY_BUFFER,
                                          0,
                                          (tri_vertices.len() * mem::size_of::<GLfloat>()) as GLsizeiptr,
                                          &tri_vertices[0] as *const f32 as *const c_void);
                        gl::BindBuffer(gl::ARRAY_BUFFER, 0);
                    }
                    // render a triangle and points
                    {
                        gl::BindTexture(gl::TEXTURE_2D, texture1);
                        gl::BindVertexArray(VAO1);
                        gl::DrawArrays(gl::TRIANGLES, 0, 3);
                        if show_point { gl::DrawArrays(gl::POINTS, 0, 3); }
                    }
                    // render lines
                    if show_line {
                        program2.useProgram();
                        gl::BindVertexArray(VAO2);
                        gl::DrawArrays(gl::LINE_LOOP, 0, 4);
                    }
                }
                1 => {
                    program3.useProgram();
                    // send t as current time
                    {
                        let t = CString::new("t").unwrap();
                        let t_location = gl::GetUniformLocation(program3.program, t.as_ptr());
                        gl::Uniform1f(t_location, last_frame % 10000.0);
                        let showPoint = CString::new("showPoint").unwrap();
                        let showPoint_location = gl::GetUniformLocation(program3.program, showPoint.as_ptr());
                        gl::Uniform1i(showPoint_location, if show_point {1} else {0});
                        let showLine = CString::new("showLine").unwrap();
                        let showLine_location = gl::GetUniformLocation(program3.program, showLine.as_ptr());
                        gl::Uniform1i(showLine_location, if show_line {1} else {0});
                    }
                    // render with tri_vertices
                    {
                        gl::BindTexture(gl::TEXTURE_2D, texture3);
                        gl::BindVertexArray(VAO3);
                        gl::DrawElements(gl::TRIANGLE_STRIP, 4, gl::UNSIGNED_INT, ptr::null());
                    }
                }
                _ => {}
            }
        }

        window.swap_buffers();

        glfw.poll_events();
    }
}

fn calc_rectangle_position(t: f32) -> (f32, f32) {
    let u = t % 4.0;
    if u > 3.0 {
        (-0.5, 0.5 - (u - 3.0))
    } else if u > 2.0 {
        (0.5 - (u - 2.0), 0.5)
    } else if u > 1.0 {
        (0.5, -0.5 + (u - 1.0))
    } else {
        (-0.5 + u, -0.5)
    }
}

fn process_events(window: &mut glfw::Window, events: &Receiver<(f64, glfw::WindowEvent)>) {
    for (i, event) in glfw::flush_messages(&events) {
        println!("{:?}, {:?}", i, event);
        match event {
            glfw::WindowEvent::FramebufferSize(width, height) => {
                unsafe { gl::Viewport(0, 0, width, height) }
            }
            glfw::WindowEvent::Key(Key::Escape, _, Action::Press, _) => window.set_should_close(true),
            glfw::WindowEvent::Key(Key::Enter, _, Action::Press, _) => {
                unsafe {
                    program_number += 1;
                    if program_number > 1 {
                        program_number = 0;
                    }
                }
            }
            glfw::WindowEvent::Key(Key::Space, _, Action::Press, _) => {
                unsafe { show_point = !show_point; }
            }
            glfw::WindowEvent::Key(Key::LeftShift, _, Action::Press, _) => {
                unsafe { show_line = !show_line; }
            }
            _ => {}
        }
    }
}

