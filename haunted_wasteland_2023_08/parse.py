import re
import os

line_re = re.compile(r'^([A-Z]{3}) = \(([A-Z]{3}),([A-Z]{3})\)$', re.MULTILINE)

def parse_file(path: str | os.PathLike):
    