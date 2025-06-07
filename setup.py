from setuptools import setup, find_packages

setup(
    name='fptk',
    version='0.0.1',
    packages=find_packages(include=['fptk', 'fptk.*']),
    install_requires=[
        'hy >= 1'
    ],
    author='Roman Averyanov',
    author_email='averrmn@gmail.com',
    description='Curated list of functional-programming imports for hy lang',
    long_description=open('README.md').read(),
    long_description_content_type='text/markdown',
    url='https://github.com/rmnavr/fptk',
    classifiers=[
        'Programming Language :: Hy',
        'Operating System :: OS Independent',
    ],
    python_requires='>=3.9', 
)
