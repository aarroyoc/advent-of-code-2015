from dataclasses import dataclass
from typing import ClassVar
import re
from z3 import *

@dataclass
class AndRule:
    left: str
    right: str
    output: str
    regex: ClassVar[re.Pattern[str]] = re.compile(r"(\w+) AND (\w+) -> (\w+)")

    def export_z3(self, solver):
        try:
            left = int(self.left)
        except:
            left = BitVec(self.left, 16)
        
        right = BitVec(self.right, 16)
        output = BitVec(self.output, 16)
        solver.add(left & right == output)

@dataclass
class OrRule:
    left: str
    right: str
    output: str
    regex: ClassVar[re.Pattern[str]] = re.compile(r"(\w+) OR (\w+) -> (\w+)")

    def export_z3(self, solver):
        left = BitVec(self.left, 16)
        right = BitVec(self.right, 16)
        output = BitVec(self.output, 16)
        solver.add(left | right == output)

@dataclass
class LShiftRule:
    left: str
    right: int
    output: str
    regex: ClassVar[re.Pattern[str]] = re.compile(r"(\w+) LSHIFT (\d+) -> (\w+)")

    def export_z3(self, solver):
        left = BitVec(self.left, 16)
        output = BitVec(self.output, 16)
        solver.add(left << self.right == output)

@dataclass
class RShiftRule:
    left: str
    right: int
    output: str
    regex: ClassVar[re.Pattern[str]] = re.compile(r"(\w+) RSHIFT (\d+) -> (\w+)")

    def export_z3(self, solver):
        left = BitVec(self.left, 16)
        output = BitVec(self.output, 16)
        solver.add(left >> self.right == output)    

@dataclass
class NotRule:
    input: str
    output: str
    regex: ClassVar[re.Pattern[str]] = re.compile(r"NOT (\w+) -> (\w+)")

    def export_z3(self, solver):
        input = BitVec(self.input, 16)
        output = BitVec(self.output, 16)
        solver.add(~input == output)    

@dataclass
class AliasRule:
    input: str
    output: str
    regex: ClassVar[re.Pattern[str]] = re.compile(r"(\w+) -> (\w+)")

    def export_z3(self, solver):
        try:
            input = int(self.input)
        except:
            input = BitVec(self.input, 16)
        
        output = BitVec(self.output, 16)
        solver.add(input == output)

def main():
    with open("input2") as f:
        lines = f.readlines()

    rules = map(parse_line, lines)

    solver = Solver()
    for rule in rules:
        rule.export_z3(solver)

    if not solver.check():
        raise Exception("Not solvable")

    model = solver.model()
    a = BitVec("a", 16)
    print(f"Value of A: {model[a]}")

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
