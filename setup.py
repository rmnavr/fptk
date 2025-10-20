from setuptools                 import setup, find_packages
from setuptools.command.install import install 

proj_version = '0.3.3.dev2'

libs_required = [
    'hy >= 1',
    'hyrule >= 1', 
    'pydantic >= 2', 
    'lenses >= 1.2.0',
    'funcy >= 2.0',
]

# install class ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

# copy-paste from «hyrule» install:
class install(install):
    def run(self):
        super().run()
        import py_compile
        import hy  
        for path in set(self.get_outputs()):
            if path.endswith('.hy'):
                py_compile.compile(
                    path,
                    invalidation_mode=py_compile.PycInvalidationMode.CHECKED_HASH
                )

# _____________________________________________________________________________/ }}}1
# setup ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

setup(
    name              = 'fptk',
    version           = proj_version,
    setup_requires    = ['wheel'] + libs_required,
    install_requires  = libs_required,
    packages          = find_packages(exclude = ["private*"], where='src'),
    package_data      = {'': ['*.hy']},
    package_dir       = {'': 'src'},
    author            = 'Roman Averyanov',
    author_email      = 'averrmn@gmail.com',
    description       = 'Curated list of functional-programming funcs/classes/modules/macros for hy lang',
    url               = 'https://github.com/rmnavr/fptk',
    python_requires   = '>=3.9',
    classifiers       = [ 'Programming Language :: Hy',
                          'Operating System :: OS Independent',
                        ],
    long_description  = open('README.md').read(),
    long_description_content_type = 'text/markdown',
)

# _____________________________________________________________________________/ }}}1

