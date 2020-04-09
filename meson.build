 project('dparse', 'd',
    meson_version: '>=0.44',
    license: 'BSL-1.0',
    version: '0.10.4'
)

project_soversion = '0'

pkgc = import('pkgconfig')
allocator_dep = dependency('stdx-allocator', version: '>= 2.77', fallback: ['stdx-allocator', 'allocator_dep'])

#
# Sources
#
dparse_src = [
    'src/dparse/ast.d',
    'src/dparse/entities.d',
    'src/dparse/formatter.d',
    'src/dparse/lexer.d',
    'src/dparse/parser.d',
    'src/dparse/rollback_allocator.d',
    'src/dparse/stack_buffer.d',
    'src/dparse/trivia.d',
    'src/std/experimental/lexer.d',
]

src_dir = include_directories('src/')

#
# Targets
#
dparse_lib = library('dparse',
        [dparse_src],
        include_directories: [src_dir],
        install: true,
        version: meson.project_version(),
        soversion: project_soversion,
        dependencies: [allocator_dep]
)

pkgc.generate(name: 'dparse',
              libraries: [dparse_lib],
              subdirs: 'd/dparse',
              requires: ['stdx-allocator'],
              version: meson.project_version(),
              description: 'Library for lexing and parsing D source code.'
)

# for use by others which embed this as subproject
dparse_dep = declare_dependency(
    link_with: [dparse_lib],
    include_directories: [src_dir],
    dependencies: [allocator_dep]
)

#
# Tests
#
if meson.get_compiler('d').get_id() == 'llvm'
  extra_args = ['-main', '-link-defaultlib-shared']
else
  extra_args = ['-main']
endif

dparse_test_exe = executable('test_dparse',
    [dparse_src],
    include_directories: [src_dir],
    dependencies: [allocator_dep],
    d_unittest: true,
    link_args: extra_args,
)
test('test_dparse', dparse_test_exe)

#
# Install
#
install_subdir('src/dparse/', install_dir: 'include/d/dparse/')
install_subdir('src/std/', install_dir: 'include/d/dparse/')
