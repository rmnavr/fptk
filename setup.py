#!/usr/bin/env python

import setuptools
from setuptools import setup, find_packages
from setuptools.command.install import install

# copy-paste from «hyrule» install:
class install(install):
    def run(self):
        super().run()
        import py_compile
        import hy  
        for path in set(self.get_outputs()):
            if path.endswith(".hy"):
                py_compile.compile(
                    path,
                    invalidation_mode = py_compile.PycInvalidationMode.CHECKED_HASH,
                )

libs_required = [ 'hy >= 1',
                  'funcy >= 2',
                  'returns >= 0.23',
                  'pydantic >= 1.10',
                  'lenses >= 1.2'
                ]

setup(
    name              = 'fptk',
    version           = '0.2.2', 
    setup_requires    = ['wheel'] + libs_required,
    install_requires  = libs_required,
    packages          = setuptools.find_packages(exclude = ["private*"]),
    package_data      = {'': ['*.hy']},
    author            = 'Roman Averyanov',
    author_email      = 'averrmn@gmail.com',
    description       = 'Curated list of functional-programming funcs and classes for hy lang',
    url               = 'https://github.com/rmnavr/fptk',
    python_requires   = '>=3.9',
    classifiers       = [ 'Programming Language :: Hy',
                          'Operating System :: OS Independent',
                        ],
    long_description  = open('README.md').read(),
    long_description_content_type = 'text/markdown',
)
