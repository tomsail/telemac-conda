#!/usr/bin/env python3
"""
Script to move continuation line symbol in f90 style
"""
import re
from argparse import ArgumentParser
from os import remove, path

F77_CONT = re.compile(r"\n(?P<code>(\s*[^ !&]+.*))\n(?P<comments>((\s*!.*\n)*))[ ]{5}&")

def replace(text):
    """
    Replace text continuation symbol first occurence

    @param text (str) Input text

    @returns (str, bool) Updated text and True if it was modified
    """
    comp = re.search(F77_CONT, text)

    if comp:
        old_txt = '{code}\n{comments}     &'\
                  .format(code=comp.group('code'),
                          comments=comp.group('comments'))
        new_txt = '{code} &\n{comments}      '\
                  .format(code=comp.group('code'),
                          comments=comp.group('comments'))

        return text.replace(old_txt, new_txt), True

    return text, False

def f77_to_f90(f77_file, f90_file):
    """
    Converting f77_file into a f90_file

    @param f77_file (str) Path to the f77 style file
    @param f90_file (str) Path to the f90 style file
    """

    with open(f77_file, 'r') as f:
        text = f.read()

    modified = True
    while modified:
        text, modified = replace(text)

    if path.exists(f90_file):
        remove(f90_file)
    with open(f90_file, 'w') as f:
        f.write(text)

def main():
    """
    Main script
    """
    parser = ArgumentParser("Move continuation symbol from f77 style to f90")

    parser.add_argument("f77_file", help="f77 style file")
    parser.add_argument("f90_file", help="f90 style file")

    options = parser.parse_args()

    f77_to_f90(options.f77_file, options.f90_file)

if __name__ == "__main__":
    main()
