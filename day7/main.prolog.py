from dataclasses import dataclass
from typing import ClassVar
import re

@dataclass
class AndRule:
    left: str
    right: str
    output: str
    regex: ClassVar[re.Pattern[str]] = re.compile(r"(\w+) AND (\w+) -> (\w+)")

    def export_prolog(self, f):
        try:
            left = int(self.left)
            f.write(f"wire({self.output}, X) :- {left} = A, wire({self.right}, B), X is A /\\ B.\n")
        except:
            f.write(f"wire({self.output}, X) :- wire({self.left}, A), wire({self.right}, B), X is A /\\ B.\n")

@dataclass
class OrRule:
    left: str
    right: str
    output: str
    regex: ClassVar[re.Pattern[str]] = re.compile(r"(\w+) OR (\w+) -> (\w+)")

    def export_prolog(self, f):
        f.write(f"wire({self.output}, X) :- wire({self.left}, A), wire({self.right}, B), X is A \\/ B.\n")

@dataclass
class LShiftRule:
    left: str
    right: int
    output: str
    regex: ClassVar[re.Pattern[str]] = re.compile(r"(\w+) LSHIFT (\d+) -> (\w+)")

    def export_prolog(self, f):
        f.write(f"wire({self.output}, X) :- wire({self.left}, A), X is A << {self.right}.\n")

@dataclass
class RShiftRule:
    left: str
    right: int
    output: str
    regex: ClassVar[re.Pattern[str]] = re.compile(r"(\w+) RSHIFT (\d+) -> (\w+)")

    def export_prolog(self, f):
        f.write(f"wire({self.output}, X) :- wire({self.left}, A), X is A >> {self.right}.\n")

@dataclass
class NotRule:
    input: str
    output: str
    regex: ClassVar[re.Pattern[str]] = re.compile(r"NOT (\w+) -> (\w+)")

    def export_prolog(self, f):
        f.write(f"wire({self.output}, X) :- wire({self.input}, A), X is \\ A.\n")

@dataclass
class AliasRule:
    input: str
    output: str
    regex: ClassVar[re.Pattern[str]] = re.compile(r"(\w+) -> (\w+)")

    def export_prolog(self, f):
        try:
            input = int(self.input)
            f.write(f"wire({self.output}, {input}).\n")
        except:
            f.write(f"wire({self.output}, X) :- wire({self.input}, X).\n")

def main():
    with open("input") as f:
        lines = f.readlines()

    rules = map(parse_line, lines)

    with open("wires.pl", "w") as f:
        f.write(":- use_module(library(tabling)).\n:- table wire/2.\n")
        for rule in rules:
            rule.export_prolog(f)

def parse_line(line):
    if matches := AndRule.regex.match(line):
        return AndRule(left=matches.group(1), right=matches.group(2), output=matches.group(3))
    elif matches := OrRule.regex.match(line):
        return OrRule(left=matches.group(1), right=matches.group(2), output=matches.group(3))
    elif matches := LShiftRule.regex.match(line):
        return LShiftRule(left=matches.group(1), right=int(matches.group(2)), output=matches.group(3))
    elif matches := RShiftRule.regex.match(line):
        return RShiftRule(left=matches.group(1), right=int(matches.group(2)), output=matches.group(3))
    elif matches := NotRule.regex.match(line):
        return NotRule(input=matches.group(1), output=matches.group(2))
    elif matches := AliasRule.regex.match(line):
        return AliasRule(input=matches.group(1), output=matches.group(2))
    else:
        raise Exception("Can't parse")

if __name__ == "__main__":
    main()
