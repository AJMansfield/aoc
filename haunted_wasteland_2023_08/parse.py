import re
import os
from dataclasses import dataclass, InitVar
from typing import ClassVar, Union

@dataclass
class CaseData:
    dirs_re: ClassVar[re.Pattern] = re.compile(r'^[LR]*$', re.MULTILINE)
    routes_re: ClassVar[re.Pattern] = re.compile(r'^([A-Z]{3}) = \(([A-Z]{3}), ([A-Z]{3})\)$', re.MULTILINE)
    alphabet: ClassVar[str] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    path: InitVar[Union[str,os.PathLike]]
    dirs: list[int] = None
    routes: dict[str, tuple[str,str]] = None
    name_list: list[str] = None
    # name_map: dict[str, int] = None

    def __post_init__(self, path: str | os.PathLike):
        with open(path) as f:
            dirs = self.dirs_re.findall(f.readline())[0]
            routes = self.routes_re.findall(f.read())
        
        self.routes = {k:(l,r) for k,l,r in routes}
        self.dirs = ["LR".index(c) for c in dirs]

        self.name_list = sorted(self.routes.keys(), key=self.name_to_num)
        # self.name


    @classmethod
    def name_to_num(cls, name: str) -> int:
        num = 0
        for char in reversed(name):
            num = num * len(cls.alphabet) + cls.alphabet.index(char)
        return num

    @classmethod
    def num_to_name(cls, num: int) -> str:
        name = ""
        while num > 0:
            num, digit = divmod(num, len(cls.alphabet))
            name = name + cls.alphabet[digit]
        return name
