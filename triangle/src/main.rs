
extern crate glfw;
use glfw::{Action, Context, Key};

extern crate gl;
use gl::types::*;

use std::sync::mpsc::Receiver;
use std::ffi::CString;
use std::ptr;
use std::str;
use std::mem;
use std::os::raw::c_void;

const vertexShaderSource: &str = r#"
    #version 330 core
    layout (location = 0) in vec3 aPos;
    void main() {
        gl_PointSize = 10.0;
        gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
    }
"#;

const fragmentShaderSource: &str = r#"
    #version 330 core
    out vec4 FragColor;
    void main() {
        FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);
    }
"#;

type Vertices1 = [f32; 9];
type Vertices2 = [f32;12];

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
    unsafe { gl::Enable(gl::PROGRAM_POINT_SIZE); }

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

    let (shaderProgram1, VAO1, VBO1) = create_program_and_vao(&tri_vertices, gl::DYNAMIC_DRAW);
    let (shaderProgram2, VAO2, VBO2) = create_program_and_vao(&rect_vertices, gl::STATIC_DRAW);

    while !window.should_close() {
        let current_frame = glfw.get_time() as f32;
        delta_time = current_frame - last_frame;
        last_frame = current_frame;

        process_events(&mut window, &events);

        unsafe {
            gl::ClearColor(0.2, 0.3, 0.3, 1.0);
            gl::Clear(gl::COLOR_BUFFER_BIT);

            {
            gl::UseProgram(shaderProgram1);
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
                    gl::BindBuffer(gl::ARRAY_BUFFER, VBO1);
                    gl::BufferSubData(gl::ARRAY_BUFFER,
                                      0,
                                      (tri_vertices.len() * mem::size_of::<GLfloat>()) as GLsizeiptr,
                                      &tri_vertices[0] as *const f32 as *const c_void);
                    gl::BindBuffer(gl::ARRAY_BUFFER, 0);
                }
                // render with tri_vertices
                {
                    gl::BindVertexArray(VAO1);
                    gl::DrawArrays(gl::TRIANGLES, 0, 3);
                    gl::DrawArrays(gl::POINTS, 0, 3);
                }
            }
            // render with rect_vertices
            {
                gl::UseProgram(shaderProgram2);
                gl::BindVertexArray(VAO2);
                gl::DrawArrays(gl::LINE_LOOP, 0, 4);
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
            _ => {}
        }
    }
}


fn create_program_and_vao(vertices: &[f32], draw_type: GLenum) -> (u32, u32, u32) {
    unsafe {
        let vertexShader = gl::CreateShader(gl::VERTEX_SHADER);
        let fragmentShader = gl::CreateShader(gl::FRAGMENT_SHADER);
        let c_str_vert = CString::new(vertexShaderSource.as_bytes()).unwrap();
        let c_str_frag = CString::new(fragmentShaderSource.as_bytes()).unwrap();
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

        let (mut VBO, mut VAO) = (0, 0);
        gl::GenVertexArrays(1, &mut VAO);
        gl::GenBuffers(1, &mut VBO);

        // bind VAO first, then bind and set vertex buffer, and then configure vertex attribute
        gl::BindVertexArray(VAO);
        // link VBO to variable of vertex shader at location 0
        {
            // send data to GPU
            {
                gl::BindBuffer(gl::ARRAY_BUFFER, VBO);
                gl::BufferData(gl::ARRAY_BUFFER,
                               (vertices.len() * mem::size_of::<GLfloat>()) as GLsizeiptr,
                               vertices.as_ptr() as *const c_void,
                               draw_type);
            }

            // set location 0 of vertex shader to 3 floats
            gl::VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, 3 * mem::size_of::<GLfloat>() as GLsizei, ptr::null());
            gl::EnableVertexAttribArray(0);
        }

        gl::BindBuffer(gl::ARRAY_BUFFER, 0);

        gl::BindVertexArray(0);

        (shaderProgram, VAO, VBO)
    }
}
