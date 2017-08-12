#!/usr/bin/env python3
from sh import pandoc,rm,cp,mkdir
import glob
import os

def compile(input_file_path, output_file_path):
    print("compiling: %s to %s " % (input_file_path, output_file_path))
    pandoc(input_file_path, c="pandoc.css", f="markdown+lhs", o=output_file_path)

def mk_output_path(iname):
    return "docs/" + iname.lower().split(".lhs")[0] + ".html"

def mk_input_path(iname):
    return "src/" + iname

def get_input_file_names():
    names = []
    for f in  os.listdir("src/"):
        if f.endswith("hs"):
            names.append(f)

    return names


if __name__ == "__main__":
    rm("-rf", "docs/")
    mkdir("docs")
    cp("pandoc.css", "docs/pandoc.css")
    for iname in get_input_file_names():
        compile(mk_input_path(iname), mk_output_path(iname))

